unit DART_ProcessingThread;

{$INCLUDE DART_defs.inc}

interface

uses
  Classes,
  DART_PRocessingSettings, DART_Common, DART_Repairer;

type
  TDARTArchiveProgressEvent = procedure(Sender: TObject; ArchiveIndex: Integer; Progress: Single) of object;

  TDARTProcessingThread = class(TThread)
  private
    sync_Progress:              Single;
    fArchiveIndex:              Integer;
    fArchiveProcessingSettings: TDARTArchiveProcessingSettings;
    fPauseControlObject:        TDARTPauseObject;
    fRepairer:                  TDARTRepairer;
    fResultInfo:                TDARTResultInfo;
    fRepairerCreated:           Boolean;
    fOnArchiveProgress:         TDARTArchiveProgressEvent;
  protected
    procedure sync_DoProgress; virtual;
    procedure ProgressHandler(Sender: TObject; Progress: Single); virtual;
    procedure CreateRepairer; virtual;
    procedure Execute; override;
  public
    constructor Create(ArchiveIndex: Integer; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
    destructor Destroy; override;
    procedure StartProcessing; virtual;
    procedure PauseProcessing; virtual;
    procedure ResumeProcessing; virtual;
    procedure StopProcessing; virtual;
    property ArchiveProcessingSettings: TDARTArchiveProcessingSettings read fArchiveProcessingSettings;
    property ResultInfo: TDARTResultInfo read fResultInfo;
  published
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

procedure TDARTProcessingThread.ProgressHandler(Sender: TObject; Progress: Single);
begin
sync_Progress := Progress;
If (Progress < 0.0) or (Progress > 1.0) then
  If fRepairerCreated then
    fResultInfo := fRepairer.ResultInfo;
{$message 'somehow limit call frequency'}
Synchronize(sync_DoProgress);
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingThread.CreateRepairer;
begin
// select proper repairer according to archive type and requested operation
case fArchiveProcessingSettings.Common.SelectedArchiveType of

  atSCS_sig,atSCS_frc:            // - - - - - - - - - - - - - - - - - - - - - -
    case fArchiveProcessingSettings.Common.RepairMethod of
      rmRebuild:  fRepairer := TDARTRepairer_SCS_Rebuild.Create(fPauseControlObject,fArchiveProcessingSettings,True);
      rmExtract:  fRepairer := TDARTRepairer_SCS_Extract.Create(fPauseControlObject,fArchiveProcessingSettings,True);
      rmConvert:  case fArchiveProcessingSettings.Common.ConvertTo of
                    atSCS_sig,atSCS_frc:
                      fRepairer := TDARTRepairer_SCS_Rebuild.Create(fPauseControlObject,fArchiveProcessingSettings,True);
                    atZIP_sig,atZIP_frc,atZIP_dft:
                      fRepairer := TDARTRepairer_SCS_Convert_ZIP.Create(fPauseControlObject,fArchiveProcessingSettings,True);
                  else
                    raise Exception.CreateFmt('TDARTProcessingThread.Create: Unknown target archive type (%d).',
                                              [Ord(fArchiveProcessingSettings.Common.ConvertTo)]);
                  end;
    else
      raise Exception.CreateFmt('TDARTProcessingThread.Create: Unknown repair method (%d).',
                                [Ord(fArchiveProcessingSettings.Common.RepairMethod)]);
    end;

  atZIP_sig,atZIP_frc,atZIP_dft:  // - - - - - - - - - - - - - - - - - - - - - -
    case fArchiveProcessingSettings.Common.RepairMethod of
      rmRebuild:  fRepairer := TDARTRepairer_ZIP_Rebuild.Create(fPauseControlObject,fArchiveProcessingSettings,True);
      rmExtract:  fRepairer := TDARTRepairer_ZIP_Extract.Create(fPauseControlObject,fArchiveProcessingSettings,True);
      rmConvert:  case fArchiveProcessingSettings.Common.ConvertTo of
                    atSCS_sig,atSCS_frc:
                      fRepairer := TDARTRepairer_ZIP_Convert_SCS.Create(fPauseControlObject,fArchiveProcessingSettings,True);
                    atZIP_sig,atZIP_frc,atZIP_dft:
                      fRepairer := TDARTRepairer_ZIP_Rebuild.Create(fPauseControlObject,fArchiveProcessingSettings,True);
                  else
                    raise Exception.CreateFmt('TDARTProcessingThread.Create: Unknown target archive type (%d).',
                                              [Ord(fArchiveProcessingSettings.Common.ConvertTo)]);
                  end;
    else
      raise Exception.CreateFmt('TDARTProcessingThread.Create: Unknown repair method (%d).',
                                [Ord(fArchiveProcessingSettings.Common.RepairMethod)]);
    end;
    
else                             
  raise Exception.CreateFmt('TDARTProcessingThread.Create: Unknown archive type (%d).',
                            [Ord(fArchiveProcessingSettings.Common.SelectedArchiveType)]);
end;
fRepairerCreated := True;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingThread.Execute;
begin
try
  CreateRepairer;
  fRepairer.OnProgress := ProgressHandler;
  fRepairer.Run;
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

constructor TDARTProcessingThread.Create(ArchiveIndex: Integer; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
inherited Create(True);
FreeOnTerminate := True;
fArchiveIndex := ArchiveIndex;
fArchiveProcessingSettings := ArchiveProcessingSettings;
EnsureThreadSafety(fArchiveProcessingSettings);
fPauseControlObject := TDARTPauseObject.Create;
// creation of repairer is deffered to when the thread is started
fResultInfo := DART_DefaultResultInfo;
fRepairerCreated := False;
fOnArchiveProgress := nil;
end;

//------------------------------------------------------------------------------

destructor TDARTProcessingThread.Destroy;
begin
If fRepairerCreated then
  fRepairer.Free;
fPauseControlObject.Free;
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
