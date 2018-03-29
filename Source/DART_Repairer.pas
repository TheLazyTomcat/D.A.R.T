unit DART_Repairer;

{$INCLUDE DART_defs.inc}

interface

uses
  DART_Common, DART_ProcessingSettings;

{===============================================================================
--------------------------------------------------------------------------------
                                 TDARTRepairer
--------------------------------------------------------------------------------
===============================================================================}

type
  TDARTResultState = (rsUndefined,rsNormal,rsWarning,rsError);

  TDARTResultInfo_Error = record
    ErrorObjectRef:     TObject;
    ErrorObjectClass:   String;
    ErrorFunctionIndex: Integer;
    ErrorFunctionName:  String;
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
      ErrorObjectRef:     nil;
      ErrorObjectClass:   '';
      ErrorFunctionIndex: -1;
      ErrorFunctionName:  'unknown function';
      ExceptionClass:     '';
      ExceptionText:      ''));

   DART_TERMFLAG_TERMINATED = -1;
   DART_TERMFLAG_CONTINUE   = 0;

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
    // initialization and finalization methods
    procedure InitializeProcessingSettings; override;
    procedure FinalizeProcessingSettings; override;
    // flow control and progress report methods
    procedure DoProgress(StageID: Integer; Data: Single); override;
    procedure DoWarning(const WarningText: String); override;
    procedure DoTerminate; virtual;
    // processing methods
    //procedure MainProcessing; override;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
    constructor Create(PauseControlObject: TDARTPauseObject; APS: TDARTArchiveProcessingSettings);
    destructor Destroy; override;
    property ResultInfo: TDARTResultInfo read fResultInfo;
  published
    property Terminated: Boolean read GetTerminated write SetTerminated;
  end;

implementation

uses
  Windows, SysUtils;

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

procedure TDARTRepairer.FinalizeProcessingSettings;
begin
// nothing to do here
inherited;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.DoProgress(StageID: Integer; Data: Single);
begin
// paused?
fPauseControlObject.WaitFor;
// terminated?
If Terminated then
  begin
    fTerminating := True;
    Terminated := False;
    DoTerminate;
    DoError(0,'Processing terminated. Data can be in an inconsistent state.');
  end;
// progress

end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.DoWarning(const WarningText: String);
begin
// do not call inherited code
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
end;

{-------------------------------------------------------------------------------
    TDARTRepairer - public methods
-------------------------------------------------------------------------------}

class Function TDARTRepairer.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
// do not call inherited code
Result := 'unknown method';
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

end.
