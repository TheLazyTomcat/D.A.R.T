unit DART_Common;

{$INCLUDE DART_defs.inc}

interface

uses
  SysUtils,
  WinSyncObjs, ProgressTracker;

{===============================================================================
--------------------------------------------------------------------------------
                             EDARTRepairerException
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    EDARTRepairerException - class declaration
===============================================================================}
type
  EDARTRepairerException = class(Exception)
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
    fLocalFormatSettings: TFormatSettings;
    fProgressTracker:     TProgressTracker;
    fOnProgress:          TDARTProgressEvent;
    // flow control and progress report methods
    procedure DoProgress(StageID: Integer; Data: Single); virtual;
    procedure DoWarning(const WarningText: String); virtual; abstract;
    procedure DoError(MethodIndex: Integer; const ErrorText: String; Values: array of const); overload; virtual;
    procedure DoError(MethodIndex: Integer; const ErrorText: String); overload; virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; virtual; abstract;
    constructor Create;
    destructor Destroy; override;
    property OnProgress: TDARTProgressEvent read fOnProgress write fOnProgress;
  end;

implementation

uses
  Windows;

{===============================================================================
--------------------------------------------------------------------------------
                             EDARTRepairerException                             
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    EDARTRepairerException - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    EDARTRepairerException - public methods
-------------------------------------------------------------------------------}

constructor EDARTRepairerException.Create(const Text: String; ObjectRef: TObject; FunctionIdx: Integer; const FunctionName: String);
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

procedure TDARTProcessingObject.DoError(MethodIndex: Integer; const ErrorText: String; Values: array of const);
begin
raise EDARTRepairerException.Create(Format(ErrorText,Values,fLocalFormatSettings),Self,MethodIndex,GetMethodNameFromIndex(MethodIndex));
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingObject.DoError(MethodIndex: Integer; const ErrorText: String);
begin
DoError(MethodIndex,ErrorText,[]);
end;

{-------------------------------------------------------------------------------
    TDARTProcessingObject - public methods
-------------------------------------------------------------------------------}

constructor TDARTProcessingObject.Create;
begin
inherited;
{$WARN SYMBOL_PLATFORM OFF}
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,{%H-}fLocalFormatSettings);
{$WARN SYMBOL_PLATFORM ON}
fProgressTracker := TProgressTracker.Create;
fProgressTracker.ConsecutiveStages := True;
end;

//------------------------------------------------------------------------------

destructor TDARTProcessingObject.Destroy;
begin
fProgressTracker.Free;
inherited;
end;

end.
