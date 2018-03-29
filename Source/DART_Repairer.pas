unit DART_Repairer;

{$INCLUDE DART_defs.inc}

interface

uses
  SysUtils,
  DART_Common, DART_ProcessingSettings, DART_Loader;

{===============================================================================
--------------------------------------------------------------------------------
                                 TDARTRepairer
--------------------------------------------------------------------------------
===============================================================================}

type
  TDARTResultState = (rsUndefined,rsNormal,rsWarning,rsError);

  TDARTResultInfo_Error = record
    FaultObjectRef:     TObject;
    FaultObjectClass:   String;
    FaultFunctionIndex: Integer;
    FaultFunctionName:  String;
    ExceptionClass:     String;
    ExceptionText:      String;
  end;

  TDART_RI_Warnings = record
    Arr:    array of string;
    Count:  Integer;
  end;

  TDARTResultInfo_Warning = record
    Warnings: TDART_RI_Warnings;
  end;

  TDARTResultInfo = record
    ResultState:  TDARTResultState;
    RepairerInfo: String;
    WarningInfo:  TDARTResultInfo_Warning;
    ErrorInfo:    TDARTResultInfo_Error;
  end;

const
  DART_DefaultResultInfo: TDARTResultInfo = (
    ResultState:  rsUndefined;
    RepairerInfo: '';
    WarningInfo: (
      Warnings: (
        Arr:    nil;
        Count:  0));
    ErrorInfo: (
      FaultObjectRef:     nil;
      FaultObjectClass:   '';
      FaultFunctionIndex: -1;
      FaultFunctionName:  'unknown function';
      ExceptionClass:     '';
      ExceptionText:      ''));

   // termination flag values
   DART_TERMFLAG_TERMINATED = -1;
   DART_TERMFLAG_CONTINUE   = 0;

   // progress stages
   DART_PROGSTAGE_ID_REP_Loading        = 1;  // loading of metadata (headers) and/or loading on input archive into memory
   DART_PROGSTAGE_ID_REP_MetaProcessing = 2;  // processing of metadata (eg. correction of data in headers)
   DART_PROGSTAGE_ID_REP_Processing     = 3;  // data processing (moving compressed data and corrected metadata from input to output archive)
   DART_PROGSTAGE_ID_REP_Saving         = 4;  // saving of in-memory output archive

{===============================================================================
   TDARTRepairer - class declaration
===============================================================================}
type
  TDARTRepairer = class(TDARTProcessingObject)
  private
    fTerminatedFlag:  Integer;  // 0 = continue, <>0 = terminated
    fResultInfo:      TDARTResultInfo;
    Function GetTerminated: Boolean;
    procedure SetTerminated(Value: Boolean);
  protected
    fPauseControlObject:  TDARTPauseObject;
    fTerminating:         Boolean;
    fLoader:              TDARTLoader;
    // initialization and finalization methods
    procedure InitializeProcessingSettings; override;
    procedure InitializeProgress; override;
    procedure FinalizeProgress; override;
    procedure FinalizeProcessingSettings; override;
    // flow control and progress report methods
    procedure DoProgress(StageID: Integer; Data: Single); override;
    procedure DoWarning(const WarningText: String); override;
    procedure DoTerminate; virtual;
    // exceptions processing
    procedure ProcessException(E: Exception); virtual;
    // processing methods
    procedure MainProcessing; override;
    procedure ArchiveProcessing; virtual; abstract; // <- specific for each archive type, all the fun must happen here
    // loader/saver progress handlers
    procedure LoaderProgressHandler(Sender: TObject; Data: Single); virtual;
    // loader/saver creation
    Function CreateLoader(ArchiveType: TDARTArchiveType): TDARTLoader; virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
    constructor Create(PauseControlObject: TDARTPauseObject; APS: TDARTArchiveProcessingSettings);
    destructor Destroy; override;
    procedure Run; virtual;
    procedure Stop; virtual;
    property ResultInfo: TDARTResultInfo read fResultInfo;
  published
    property Terminated: Boolean read GetTerminated write SetTerminated;
  end;

implementation

uses
  Windows;

{===============================================================================
--------------------------------------------------------------------------------
                                 TDARTRepairer
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTRepairer - private methods
-------------------------------------------------------------------------------}

Function TDARTRepairer.GetTerminated: Boolean;
begin
Result := InterlockedExchangeAdd(fTerminatedFlag,0) <> DART_TERMFLAG_CONTINUE;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.SetTerminated(Value: Boolean);
begin
If Value then
  InterlockedExchange(fTerminatedFlag,DART_TERMFLAG_TERMINATED)
else
  InterlockedExchange(fTerminatedFlag,DART_TERMFLAG_CONTINUE);
end;

{-------------------------------------------------------------------------------
    TDARTRepairer - protected methods
-------------------------------------------------------------------------------}

procedure TDARTRepairer.InitializeProcessingSettings;
begin
inherited;
RectifyArchiveProcessingSettings(fArchiveProcessingSettings);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.InitializeProgress;
begin
inherited;
fProgressTracker.Add(10,DART_PROGSTAGE_ID_REP_Loading);
fProgressTracker.Add(10,DART_PROGSTAGE_ID_REP_MetaProcessing);
fProgressTracker.Add(10,DART_PROGSTAGE_ID_REP_Processing);
fProgressTracker.Add(10,DART_PROGSTAGE_ID_REP_Saving);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.FinalizeProgress;
begin
fProgressTracker.Clear;
inherited;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.FinalizeProcessingSettings;
begin
// nothing to do here
inherited;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.DoProgress(StageID: Integer; Data: Single);
begin
fPauseControlObject.WaitFor;
If Terminated then
  begin
    fTerminating := True;
    Terminated := False;
    DoTerminate;
    DoError(0,'Processing terminated. Data can be in an inconsistent state.');
  end;
If fProgressTracker.IndexOf(StageID) <= 0 then
  begin
    If Data > 1.0 then Data := 1.0;
    inherited DoProgress(StageID,Data);
  end
else
  begin
    If StageID <> DART_PROGSTAGE_ID_NoProgress then
      fProgressTracker.Progress := Data;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.DoWarning(const WarningText: String);
begin
inherited;
fResultInfo.ResultState := rsWarning;
with fResultInfo.WarningInfo do
  begin
    If Length(fResultInfo.WarningInfo.Warnings.Arr) >= fResultInfo.WarningInfo.Warnings.Count then
      SetLength(fResultInfo.WarningInfo.Warnings.Arr,Length(fResultInfo.WarningInfo.Warnings.Arr) + 128); 
    Warnings.Arr[Warnings.Count] := WarningText;
    Inc(Warnings.Count);
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.DoTerminate;
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.ProcessException(E: Exception);
begin
fResultInfo.ResultState := rsError;
fResultInfo.ErrorInfo.ExceptionClass := E.ClassName;
fResultInfo.ErrorInfo.ExceptionText := E.Message;
DoProgress(DART_PROGSTAGE_ID_Direct,-1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.MainProcessing;
begin
inherited;
fResultInfo.ResultState := rsNormal;
try
  DoProgress(DART_PROGSTAGE_ID_Direct,0.0);
  fLoader := CreateLoader(fArchiveProcessingSettings.Common.SelectedArchiveType);
  try
    AllocateMemoryBuffers;
    try
      ArchiveProcessing;
      DoProgress(DART_PROGSTAGE_ID_Direct,1.0);
    finally
      FreeMemoryBuffers;
    end;
  finally
    fLoader.Free;
  end;
  DoProgress(DART_PROGSTAGE_ID_Direct,2.0);
except
  on E: EDARTProcessingException do
    begin
      fResultInfo.ErrorInfo.FaultObjectRef := E.FaultObjectRef;
      fResultInfo.ErrorInfo.FaultObjectClass := E.FaultObjectClass;
      fResultInfo.ErrorInfo.FaultFunctionIndex := E.FaultFunctionIdx;
      fResultInfo.ErrorInfo.FaultFunctionName := E.FaultFunctionName;
      ProcessException(E)
    end;
  on E: Exception do
    ProcessException(E);
end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.LoaderProgressHandler(Sender: TObject; Data: Single);
begin
DoProgress(DART_PROGSTAGE_ID_REP_Loading,Data);
end;

//------------------------------------------------------------------------------

Function TDARTRepairer.CreateLoader(ArchiveType: TDARTArchiveType): TDARTLoader;
begin
If Assigned(fLoader) then
  fLoader.OnProgress := LoaderProgressHandler;
end;

{-------------------------------------------------------------------------------
    TDARTRepairer - public methods
-------------------------------------------------------------------------------}

class Function TDARTRepairer.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
inherited;
case MethodIndex of
  0:  Result := 'Stop*';
else
  Result := 'unknown method';
end;
end;

//------------------------------------------------------------------------------

constructor TDARTRepairer.Create(PauseControlObject: TDARTPauseObject; APS: TDARTArchiveProcessingSettings);
begin
inherited Create(APS);
fTerminatedFlag := DART_TERMFLAG_CONTINUE;
fResultInfo := DART_DefaultResultInfo;
fResultInfo.RepairerInfo := Format('%s(0x%p)',[Self.ClassName,Pointer(Self)]);
fPauseControlObject := PauseControlObject;
fPauseControlObject.SetEvent;
fTerminating := False;
end;

//------------------------------------------------------------------------------

destructor TDARTRepairer.Destroy;
begin
inherited;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.Run;
begin
Terminated := False;
MainProcessing;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.Stop;
begin
Terminated := True;
end;

end.
