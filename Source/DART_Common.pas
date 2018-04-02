unit DART_Common;

{$INCLUDE DART_defs.inc}

interface

uses
  SysUtils,
  WinSyncObjs, CRC32, ProgressTracker,
  DART_ProcessingSettings;

type
  TDARTKnownPath = record
    Path:       AnsiString;
    Directory:  Boolean;
    Hash:       TCRC32;
    Hash64:     UInt64;
  end;

  TDARTKnownPaths = record
    Arr:    array of TDARTKnownPath;
    Count:  Integer;
  end;

type
  TDARTProgressStageInfo = record
    ParentStage:  TProgressTracker;
    StageIndex:   Integer;
  end;

  TDART_PSI = TDARTProgressStageInfo;

Function DARTProgressStageInfo(ParentStage: TProgressTracker; StageIndex: Integer): TDARTProgressStageInfo; {$IFDEF CanInline}inline;{$ENDIF}

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

implementation

uses
  Windows;

Function DARTProgressStageInfo(ParentStage: TProgressTracker; StageIndex: Integer): TDARTProgressStageInfo;
begin
Result.ParentStage := ParentStage;
Result.StageIndex := StageIndex;
end;

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

end.
