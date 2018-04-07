unit DART_ProcessingThread;

{$INCLUDE DART_defs.inc}

interface

uses
  Classes,
  SyncThread,
  DART_PRocessingSettings, DART_Common, DART_Repairer;

type
  TDARTArchiveProgressEvent = procedure(Sender: TObject; ArchiveIndex: Integer; Progress: Double) of object;

  TDARTProcessingThread = class(TSyncThread)
  private
    sync_Progress:              Double;
    fSyncDispatcher:            TSyncThreadDispatcher;
    fProgressFactorCoef:        Integer;
    fPreviousProgressFactor:    Integer;
    fArchiveIndex:              Integer;
    fArchiveProcessingSettings: TDARTArchiveProcessingSettings;
    fPauseControlObject:        TDARTPauseObject;
    fHeartbeat:                 PInteger;
    fRepairer:                  TDARTRepairer;
    fResultInfo:                TDARTResultInfo;
    fOnArchiveProgress:         TDARTArchiveProgressEvent;
  protected
    procedure sync_DoProgress; virtual;
    procedure ProgressHandler(Sender: TObject; Progress: Double); virtual;
    procedure CreateRepairer; virtual;
    procedure Execute; override;
  public
    constructor Create(ArchiveIndex: Integer; ArchiveProcessingSettings: TDARTArchiveProcessingSettings; Heartbeat: PInteger; SyncDispatcher: TSyncThreadDispatcher);
    destructor Destroy; override;
    procedure StartProcessing; virtual;
    procedure PauseProcessing; virtual;
    procedure ResumeProcessing; virtual;
    procedure StopProcessing; virtual;
    property ArchiveProcessingSettings: TDARTArchiveProcessingSettings read fArchiveProcessingSettings;
    property ResultInfo: TDARTResultInfo read fResultInfo;
  published
    property ProgressFactorCoefficient: Integer read fProgressFactorCoef write fProgressFactorCoef;
    property ArchiveIndex: Integer read fArchiveIndex;
    property OnArchiveProgress: TDARTArchiveProgressEvent read fOnArchiveProgress write fOnArchiveProgress;
  end;

implementation

uses
  SysUtils,
  DART_Repairer_SCS_Rebuild, DART_Repairer_SCS_Extract, DART_Repairer_SCS_Convert_ZIP,
  DART_Repairer_ZIP_Rebuild, DART_Repairer_ZIP_Extract, DART_Repairer_ZIP_Convert_SCS;

procedure TDARTProcessingThread.sync_DoProgress;
begin
If Assigned(fOnArchiveProgress) then
  fOnArchiveProgress(Self,fArchiveIndex,sync_Progress);
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingThread.ProgressHandler(Sender: TObject; Progress: Double);
var
  ProgressFactor: Integer;
begin
// limit number of calls to synchronize
ProgressFactor := Trunc(Progress * fProgressFactorCoef);
If (ProgressFactor <= 0) or (ProgressFactor >= fProgressFactorCoef) or
  (ProgressFactor <> fPreviousProgressFactor) then
  begin
    sync_Progress := Progress;  
    fPreviousProgressFactor := ProgressFactor;
    If ((Progress < 0.0) or (Progress > 1.0)) and Assigned(fRepairer) then
      fResultInfo := fRepairer.ResultInfo;
    If Assigned(fSyncDispatcher) then
      Synchronize(sync_DoProgress,fSyncDispatcher)
    else
      Synchronize(sync_DoProgress);
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingThread.CreateRepairer;
begin
// select proper repairer according to archive type and requested operation
case fArchiveProcessingSettings.Common.SelectedArchiveType of
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  atSCS_sig,atSCS_frc:
    case fArchiveProcessingSettings.Common.RepairMethod of
      rmRebuild:  fRepairer := TDARTRepairer_SCS_Rebuild.Create(fPauseControlObject,fArchiveProcessingSettings,True);
      rmExtract:  fRepairer := TDARTRepairer_SCS_Extract.Create(fPauseControlObject,fArchiveProcessingSettings,True);
      rmConvert:  case fArchiveProcessingSettings.Common.ConvertTo of
                    katSCS:
                      fRepairer := TDARTRepairer_SCS_Rebuild.Create(fPauseControlObject,fArchiveProcessingSettings,True);
                    katZIP:
                      fRepairer := TDARTRepairer_SCS_Convert_ZIP.Create(fPauseControlObject,fArchiveProcessingSettings,True);
                  else
                    raise Exception.CreateFmt('TDARTProcessingThread.Create: Unknown target archive type (%d).',
                                              [Ord(fArchiveProcessingSettings.Common.ConvertTo)]);
                  end;
    else
      raise Exception.CreateFmt('TDARTProcessingThread.Create: Unknown repair method (%d).',
                                [Ord(fArchiveProcessingSettings.Common.RepairMethod)]);
    end;
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  atZIP_sig,atZIP_frc,atZIP_dft:
    case fArchiveProcessingSettings.Common.RepairMethod of
      rmRebuild:  fRepairer := TDARTRepairer_ZIP_Rebuild.Create(fPauseControlObject,fArchiveProcessingSettings,True);
      rmExtract:  fRepairer := TDARTRepairer_ZIP_Extract.Create(fPauseControlObject,fArchiveProcessingSettings,True);
      rmConvert:  case fArchiveProcessingSettings.Common.ConvertTo of
                    katSCS:
                      fRepairer := TDARTRepairer_ZIP_Convert_SCS.Create(fPauseControlObject,fArchiveProcessingSettings,True);
                    katZIP:
                      fRepairer := TDARTRepairer_ZIP_Rebuild.Create(fPauseControlObject,fArchiveProcessingSettings,True);
                  else
                    raise Exception.CreateFmt('TDARTProcessingThread.Create: Unknown target archive type (%d).',
                                              [Ord(fArchiveProcessingSettings.Common.ConvertTo)]);
                  end;
    else
      raise Exception.CreateFmt('TDARTProcessingThread.Create: Unknown repair method (%d).',
                                [Ord(fArchiveProcessingSettings.Common.RepairMethod)]);
    end;
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -    
else                             
  raise Exception.CreateFmt('TDARTProcessingThread.Create: Unknown archive type (%d).',
                            [Ord(fArchiveProcessingSettings.Common.SelectedArchiveType)]);
end;
fRepairer.Heartbeat := fHeartbeat;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingThread.Execute;
begin
try
  CreateRepairer;
  try
    fRepairer.OnProgress := ProgressHandler;
    fRepairer.Run;
  finally
    FreeAndNil(fRepairer);
  end;
except
  on E: Exception do
    begin
      fResultInfo.ResultState := rsError;
      fResultInfo.RepairerInfo := '(nil)';
      fResultInfo.ErrorInfo.FaultObjectRef := Self;
      fResultInfo.ErrorInfo.FaultObjectClass := Self.ClassName;
      fResultInfo.ErrorInfo.ExceptionClass := E.ClassName;
      fResultInfo.ErrorInfo.ExceptionText := E.Message;
      ProgressHandler(Self,-1.0);
    end;
end;
end;

//==============================================================================

constructor TDARTProcessingThread.Create(ArchiveIndex: Integer; ArchiveProcessingSettings: TDARTArchiveProcessingSettings; Heartbeat: PInteger; SyncDispatcher: TSyncThreadDispatcher);
begin
inherited Create(True);
FreeOnTerminate := False;
sync_Progress := 0.0;
fSyncDispatcher := SyncDispatcher;
fProgressFactorCoef := 2000;
fPreviousProgressFactor := 0;
fArchiveIndex := ArchiveIndex;
fArchiveProcessingSettings := ArchiveProcessingSettings;
EnsureThreadSafety(fArchiveProcessingSettings);
fPauseControlObject := TDARTPauseObject.Create;
fHeartbeat := Heartbeat;
// creation of repairer is deffered to when the thread is started
fRepairer := nil;
fResultInfo := DART_DefaultResultInfo;
fOnArchiveProgress := nil;
end;

//------------------------------------------------------------------------------

destructor TDARTProcessingThread.Destroy;
begin
fPauseControlObject.Free;
If Assigned(fSyncDispatcher) then
  fSyncDispatcher.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingThread.StartProcessing;
begin
fPauseControlObject.SetEvent;
{$IF Defined(FPC) or (CompilerVersion >= 21)} // Delphi 2010+
Start;
{$ELSE}
Resume;
{$IFEND}
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingThread.PauseProcessing;
begin
fPauseControlObject.ResetEvent;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingThread.ResumeProcessing;
begin
fPauseControlObject.SetEvent;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingThread.StopProcessing;
begin
fRepairer.Stop;
end;

end.
