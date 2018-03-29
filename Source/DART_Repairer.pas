unit DART_Repairer;

{$INCLUDE DART_defs.inc}

interface

uses
  SysUtils, Classes,
  AuxTypes, ProgressTracker, MemoryBuffer, StrRect,
  DART_Common, DART_ProcessingSettings;

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
   DART_PROGSTAGE_ID_NoProgress     = -100;
   DART_PROGSTAGE_ID_Direct         = -1;
   DART_PROGSTAGE_ID_Default        = 0;
   DART_PROGSTAGE_ID_Loading        = 1;  // loading of metadata (headers) and/or loading on input archive into memory
   DART_PROGSTAGE_ID_MetaProcessing = 2;  // processing of metadata (eg. correction of data in headers)
   DART_PROGSTAGE_ID_Processing     = 3;  // data processing (moving compressed data and corrected metadata from input to output archive)
   DART_PROGSTAGE_ID_Saving         = 4;  // saving of in-memory output archive

{===============================================================================
   TDARTRepairer - class declaration
===============================================================================}
type
  TDARTProgressEvent = procedure(Sender: TObject; Progress: Single) of object;

  TDARTRepairer = class(TObject)
  private
    fTerminatedFlag:  Integer;  // 0 = continue, <>0 = terminated
    fResultInfo:      TDARTResultInfo;
    fOnProgress:      TDARTProgressEvent;
    Function GetTerminated: Boolean;
    procedure SetTerminated(Value: Boolean);
  protected
    fLocalFormatSettings:       TFormatSettings;
    fPauseControlObject:        TDARTPauseObject;
    fArchiveProcessingSettings: TDARTArchiveProcessingSettings;
    fProgressTracker:           TProgressTracker;
    fTerminating:               Boolean;
    fInputArchiveStream:        TStream;
    fExpectedSignature:         UInt32;
    // preallocated buffers
    fBuffer_IO:                 TMemoryBuffer;
    // initialization methods
    procedure InitializeProcessingSettings; virtual;
    procedure InitializeData; virtual; abstract;
    procedure InitializeProgress; virtual;
    // memory buffers management
    procedure AllocateMemoryBuffers; virtual;
    procedure FreeMemoryBuffers; virtual;
    // flow control and progress report methods
    procedure DoProgress(StageID: Integer; Data: Single); virtual;
    procedure DoWarning(const WarningText: String); virtual;
    procedure DoError(MethodIndex: Integer; const ErrorText: String; Values: array of const); overload; virtual;
    procedure DoError(MethodIndex: Integer; const ErrorText: String); overload; virtual;
    procedure DoTerminate; virtual;
    // exceptions processing
    procedure ProcessException(E: Exception); virtual;
    // common processing methods
    procedure CheckArchiveSignature; virtual;
    Function FindSignature(Signature: UInt32; SearchFrom: Int64 = -1; SearchBack: Boolean = False; Limited: Boolean = False; ProgressStage: Integer = DART_PROGSTAGE_ID_NoProgress): Int64; virtual;
    procedure ProgressedLoadFile(const FileName: String; Stream: TStream; ProgressStage: Integer); virtual;
    // processing methods
    procedure MainProcessing; virtual;
    procedure ArchiveProcessing; virtual; abstract; // <- specific for each archive type, all the fun must happen here
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; virtual;
    constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
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

const
  DART_BUFFERSIZE_IO = 1024 * 1024;   // 1MiB, used for I/O operations

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
RectifyArchiveProcessingSettings(fArchiveProcessingSettings);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.InitializeProgress;
begin

end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.AllocateMemoryBuffers;
begin
AllocBuffer(fBuffer_IO,DART_BUFFERSIZE_IO);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.FreeMemoryBuffers;
begin
FreeBuffer(fBuffer_IO);
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
    fProgressTracker.SetStageProgress(StageID,Data);
    If Assigned(fOnProgress) then
      fOnProgress(Self,fProgressTracker.Progress);
  end
else
  begin
    If StageID <> DART_PROGSTAGE_ID_NoProgress then
      begin
        fProgressTracker.Progress := Data;
        If Assigned(fOnProgress) then
          fOnProgress(Self,fProgressTracker.Progress);
      end;
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


procedure TDARTRepairer.DoError(MethodIndex: Integer; const ErrorText: String; Values: array of const);
begin
raise EDARTProcessingException.Create(Format(ErrorText,Values,fLocalFormatSettings),Self,MethodIndex,GetMethodNameFromIndex(MethodIndex));
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.DoError(MethodIndex: Integer; const ErrorText: String);
begin
DoError(MethodIndex,ErrorText,[]);
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

procedure TDARTRepairer.CheckArchiveSignature;
var
  Signature: UInt32;
begin
fInputArchiveStream.Seek(0,soBeginning);
If fInputArchiveStream.Read({%H-}Signature,SizeOf(Signature)) >= SizeOf(Signature) then
  begin
    If Signature <> fExpectedSignature then
      DoError(2,'Bad file signature (0x%.8x).',[Signature]);
  end
else DoError(2,'File is too small to contain any valid signature (%d bytes).',[fInputArchiveStream.Size]);
end;

//------------------------------------------------------------------------------

Function TDARTRepairer.FindSignature(Signature: UInt32; SearchFrom: Int64 = -1; SearchBack: Boolean = False; Limited: Boolean = False; ProgressStage: Integer = DART_PROGSTAGE_ID_NoProgress): Int64;
const
  BufferOverlap = SizeOf(Signature) - 1;
var
  CurrentOffset:  Int64;
  BytesRead:      Integer;
  i:              Integer;
begin
Result := -1;
If SearchFrom >= 0 then
  begin
    If SearchBack then
      CurrentOffset := fInputArchiveStream.Size - SearchFrom
    else
      CurrentOffset := SearchFrom;
  end
else CurrentOffset := 0;
If fInputArchiveStream.Size > 0 then
  repeat
    If SearchBack then
      fInputArchiveStream.Seek(-(CurrentOffset + Int64(fBuffer_IO.Size)),soFromEnd)
    else
      fInputArchiveStream.Seek(CurrentOffset,soFromBeginning);
    BytesRead := fInputArchiveStream.Read(fBuffer_IO.Memory^,fBuffer_IO.Size);
    If BytesRead >= SizeOf(Signature) then
      For i := 0 to (BytesRead - SizeOf(Signature)) do
        If {%H-}PUInt32({%H-}PtrUInt(fBuffer_IO.Memory) + PtrUInt(i))^ = Signature then
          begin
            Result := fInputArchiveStream.Position - BytesRead + i;
            Exit;
          end;
    Inc(CurrentOffset,fBuffer_IO.Size - BufferOverlap);
    DoProgress(ProgressStage,CurrentOffset / fInputArchiveStream.Size);
  until (CurrentOffset > fInputArchiveStream.Size) or Limited;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.ProgressedLoadFile(const FileName: String; Stream: TStream; ProgressStage: Integer);
var
  FileStream: TFileStream;
  BytesRead:  Integer;
begin
DoProgress(ProgressStage,0.0);
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
try
  Stream.Size := FileStream.Size; // prevents reallocations
  Stream.Seek(0,soFromBeginning);
  FileStream.Seek(0,soFromBeginning);
  If FileStream.Size > 0 then
    repeat
      BytesRead := FileStream.Read(fBuffer_IO.Memory^,fBuffer_IO.Size);
      Stream.Write(fBuffer_IO.Memory^,BytesRead);
      DoProgress(ProgressStage,FileStream.Position / FileStream.Size);
    until TMemSize(BytesRead) <= fBuffer_IO.Size;
finally
  FileStream.Free;
end;
DoProgress(ProgressStage,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.MainProcessing;
begin
inherited;
fResultInfo.ResultState := rsNormal;
try
  DoProgress(DART_PROGSTAGE_ID_Direct,0.0);
  //fLoader := CreateLoader(fArchiveProcessingSettings.Common.SelectedArchiveType);
  try
    AllocateMemoryBuffers;
    try
      ArchiveProcessing;
      DoProgress(DART_PROGSTAGE_ID_Direct,1.0);
    finally
      FreeMemoryBuffers;
    end;
  finally
    //fLoader.Free;
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

{-------------------------------------------------------------------------------
    TDARTRepairer - public methods
-------------------------------------------------------------------------------}

class Function TDARTRepairer.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
inherited;
case MethodIndex of
  0:  Result := 'Stop*';
  1:  Result := 'MainProcessing';
  2:  Result := 'CheckArchiveSignature';
else
  Result := 'unknown method';
end;
end;

//------------------------------------------------------------------------------

constructor TDARTRepairer.Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
inherited Create;
fTerminatedFlag := DART_TERMFLAG_CONTINUE;
fResultInfo := DART_DefaultResultInfo;
fResultInfo.RepairerInfo := Format('%s(0x%p)',[Self.ClassName,Pointer(Self)]);
fOnProgress := nil;
{$WARN SYMBOL_PLATFORM OFF}
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,{%H-}fLocalFormatSettings);
{$WARN SYMBOL_PLATFORM ON}
fPauseControlObject := PauseControlObject;
fPauseControlObject.SetEvent;
fArchiveProcessingSettings := ArchiveProcessingSettings;
fProgressTracker := TProgressTracker.Create;
fProgressTracker.ConsecutiveStages := True;
fProgressTracker.GrowOnly := True;
fTerminating := False;
InitializeProcessingSettings;
InitializeProgress;
InitializeData;
end;

//------------------------------------------------------------------------------

destructor TDARTRepairer.Destroy;
begin
fProgressTracker.Free;
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
