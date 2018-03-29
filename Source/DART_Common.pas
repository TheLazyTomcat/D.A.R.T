unit DART_Common;

{$INCLUDE DART_defs.inc}

interface

uses
  SysUtils,
  WinSyncObjs, ProgressTracker,
  DART_ProcessingSettings;

{===============================================================================
--------------------------------------------------------------------------------
                            EDARTProcessingException
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    EDARTProcessingException - class declaration
===============================================================================}
type
  EDARTProcessingException = class(Exception)
  private
    fFaultObjectRef:    TObject;
    fFaultObjectClass:  String;
    fFaultFunctionIdx:  Integer;
    fFaultFunctionName: String;
  public
    constructor Create(const Text: String; ObjectRef: TObject; FunctionIdx: Integer; const FunctionName: String);
  published
    property FaultObjectRef: TObject read fFaultObjectRef;
    property FaultObjectClass: String read fFaultObjectClass;
    property FaultFunctionIdx: Integer read fFaultFunctionIdx;
    property FaultFunctionName: String read fFaultFunctionName;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TDARTPauseObject
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTPauseObject - class declaration
===============================================================================}

  TDARTPauseObject = WinSyncObjs.TEvent;

{===============================================================================
--------------------------------------------------------------------------------
                              TDARTProcessingObject                             
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTProcessingObject - class declaration
===============================================================================}

  TDARTProgressEvent = procedure(Sender: TObject; Progress: Single) of object;

  TDARTProcessingObject = class(TObject)
  protected
    fLocalFormatSettings:         TFormatSettings;
    fProgressTracker:             TProgressTracker;
    fArchiveProcessingSettings:   TDARTArchiveProcessingSettings;
    fOnProgress:                  TDARTProgressEvent;
    // initialization and finalization methods
    procedure InitializeProcessingSettings; virtual; abstract;
    procedure InitializeData; virtual; abstract;
    procedure InitializeProgress; virtual; abstract;
    procedure FinalizeProcessingSettings; virtual; abstract;
    procedure FinalizeData; virtual; abstract;
    procedure FinalizeProgress; virtual; abstract;
    // flow control and progress report methods
    procedure DoProgress(StageID: Integer; Data: Single); virtual;
    procedure DoWarning(const WarningText: String); virtual;
    procedure DoError(MethodIndex: Integer; const ErrorText: String; Values: array of const); overload; virtual;
    procedure DoError(MethodIndex: Integer; const ErrorText: String); overload; virtual;
    // memory buffers management
    procedure AllocateMemoryBuffers; virtual;
    procedure FreeMemoryBuffers; virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; virtual; abstract;
    constructor Create(ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
    destructor Destroy; override;
    property OnProgress: TDARTProgressEvent read fOnProgress write fOnProgress;
  end;

implementation

uses
  Windows;

{===============================================================================
--------------------------------------------------------------------------------
                            EDARTProcessingException
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    EDARTProcessingException - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    EDARTProcessingException - public methods
-------------------------------------------------------------------------------}

constructor EDARTProcessingException.Create(const Text: String; ObjectRef: TObject; FunctionIdx: Integer; const FunctionName: String);
begin
inherited Create(Text);
fFaultObjectRef := ObjectRef;
fFaultObjectClass := ObjectRef.ClassName;
fFaultFunctionIdx := FunctionIdx;
fFaultFunctionName := FunctionName;
end;

{===============================================================================
--------------------------------------------------------------------------------
                              TDARTProcessingObject                             
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTProcessingObject - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTProcessingObject - protected methods
-------------------------------------------------------------------------------}

procedure TDARTProcessingObject.DoProgress(StageID: Integer; Data: Single);
begin
fProgressTracker.SetStageProgress(StageID,Data);
If Assigned(fOnProgress) then
  fOnProgress(Self,fProgressTracker.Progress);
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingObject.DoWarning(const WarningText: String);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingObject.DoError(MethodIndex: Integer; const ErrorText: String; Values: array of const);
begin
raise EDARTProcessingException.Create(Format(ErrorText,Values,fLocalFormatSettings),Self,MethodIndex,GetMethodNameFromIndex(MethodIndex));
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingObject.DoError(MethodIndex: Integer; const ErrorText: String);
begin
DoError(MethodIndex,ErrorText,[]);
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingObject.AllocateMemoryBuffers;
begin
// no buffer for allocation
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingObject.FreeMemoryBuffers;
begin
// no buffer to free
end;

{-------------------------------------------------------------------------------
    TDARTProcessingObject - public methods
-------------------------------------------------------------------------------}

constructor TDARTProcessingObject.Create(ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
inherited Create;
{$WARN SYMBOL_PLATFORM OFF}
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,{%H-}fLocalFormatSettings);
{$WARN SYMBOL_PLATFORM ON}
fProgressTracker := TProgressTracker.Create;
fProgressTracker.ConsecutiveStages := True;
fProgressTracker.GrowOnly := True;
fArchiveProcessingSettings := ArchiveProcessingSettings;
fOnProgress := nil;
AllocateMemoryBuffers;
InitializeProcessingSettings;
InitializeProgress;
InitializeData;
end;

//------------------------------------------------------------------------------

destructor TDARTProcessingObject.Destroy;
begin
FinalizeData;
FinalizeProgress;
FinalizeProcessingSettings;
FreeMemoryBuffers;
fProgressTracker.Free;
inherited;
end;

end.
