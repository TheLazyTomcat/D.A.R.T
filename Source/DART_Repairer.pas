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
    Arr:    array of String;
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

  // progress stages
  DART_PROGSTAGE_ID_NoProgress = 1;
  DART_PROGSTAGE_ID_Direct     = 2;
  DART_PROGSTAGE_ID_Loading    = 10;
  DART_PROGSTAGE_ID_Processing = 11;
  DART_PROGSTAGE_ID_Saving     = 12;
  DART_PROGSTAGE_ID_Max        = 99;

  PSID_Processing = DART_PROGSTAGE_ID_Processing;

{===============================================================================
   TDARTRepairer - class declaration
===============================================================================}
type
  TDARTProgressEvent = procedure(Sender: TObject; Progress: Single) of object;

  TDARTRepairer = class(TObject)
  private
    fCatchExceptions: Boolean;
    fTerminatedFlag:  Integer;  // 0 = continue, <>0 = terminated
    fResultInfo:      TDARTResultInfo;
    fOnProgress:      TDARTProgressEvent;
    Function GetTerminated: Boolean;
    procedure SetTerminated(Value: Boolean);
  protected
    fHeartbeat:                 PInteger;
    fLocalFormatSettings:       TFormatSettings;
    fPauseControlObject:        TDARTPauseObject;
    fArchiveProcessingSettings: TDARTArchiveProcessingSettings;
    fProgressTracker:           TProgressTracker;
    fTerminating:               Boolean;
    fInputArchiveStream:        TStream;
    fExpectedSignature:         UInt32;
    // preallocated buffers
    fBuffer_IO:                 TMemoryBuffer;
    fBuffer_Entry:              TMemoryBuffer;
    // for (de)compression
    fCompProgressStage:         array of Integer;
    // progress nodes
    fProcessingProgNode:        TProgressTracker;
    fEntriesProcProgNode:       TProgressTracker;
    fEntryProcProgNode:         TProgressTracker;
    // initialization methods
    procedure InitializeProcessingSettings; virtual;
    procedure InitializeData; virtual; abstract;
    procedure InitializeProgress; virtual;
    // memory buffers management
    procedure AllocateMemoryBuffers; virtual;
    procedure FreeMemoryBuffers; virtual;
    // flow control and progress report methods
    procedure ForwardedProgressHandler(Sender: TObject; Progress: Single); virtual;
    procedure DoProgress(StageIDs: array of Integer; Data: Single); virtual;
    procedure DoWarning(const WarningText: String); virtual;
    procedure DoError(MethodIndex: Integer; const ErrorText: String; Values: array of const); overload; virtual;
    procedure DoError(MethodIndex: Integer; const ErrorText: String); overload; virtual;
    procedure DoTerminate; virtual;
    // exceptions processing
    procedure ProcessException(E: Exception); virtual;
    // common processing methods
    procedure CheckArchiveSignature; virtual;
    Function FindSignature(Signature: UInt32; ProgressStage: array of Integer; SearchFrom: Int64 = -1; SearchBack: Boolean = False; Limited: Boolean = False): Int64; virtual;
    procedure ProgressedLoadFile(const FileName: String; Stream: TStream; ProgressStage: array of Integer); virtual;
    procedure ProgressedSaveFile(const FileName: String; Stream: TStream; ProgressStage: array of Integer); virtual;
    procedure ProgressedStreamRead(Stream: TStream; Buffer: Pointer; Size: TMemSize; ProgressStage: array of Integer); virtual;
    procedure ProgressedStreamWrite(Stream: TStream; Buffer: Pointer; Size: TMemSize; ProgressStage: array of Integer); virtual;
    procedure CompressorProgressHandler(Sender: TObject; Progress: Single); virtual;
    procedure ProgressedDecompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; WindowBits: Integer; ProgressStage: array of Integer); virtual;
    procedure ProgressedCompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; WindowBits: Integer; ProgressStage: array of Integer); virtual;
    // methods for content parsing
    Function IndexOfEntry(const EntryFileName: AnsiString): Integer; virtual; abstract;
    Function GetEntryData(EntryIndex: Integer; out Data: Pointer; out Size: TMemSize): Boolean; overload; virtual; abstract;
    Function GetEntryData(const EntryFileName: AnsiString; out Data: Pointer; out Size: TMemSize): Boolean; overload; virtual;
    // methods working with known paths
    class Function IndexOfKnownPath(const Path: AnsiString; const KnownPaths: TDART_KnownPaths): Integer; virtual;
    // processing methods
    procedure MainProcessing; virtual;
    procedure ArchiveProcessing; virtual; abstract; // <- specific for each archive type, all the fun must happen here
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; virtual;
    constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings; CatchExceptions: Boolean = True);
    destructor Destroy; override;
    procedure Run; virtual;
    procedure Stop; virtual;
    Function GetAllKnownPaths(var KnownPaths: TDART_KnownPaths): Integer; virtual; abstract;
    property Heartbeat: PInteger read fHeartbeat write fHeartbeat;
    property ResultInfo: TDARTResultInfo read fResultInfo;
  published
    property Terminated: Boolean read GetTerminated write SetTerminated;
    property OnProgress: TDARTProgressEvent read fOnProgress write fOnProgress;
  end;

implementation

uses
  Windows, Math,
  CRC32, ZLibUtils;

{===============================================================================
--------------------------------------------------------------------------------
                                 TDARTRepairer
--------------------------------------------------------------------------------
===============================================================================}

const
  DART_BUFFERSIZE_IO    = 1024 * 1024;       // 1MiB, used for I/O operations
  DART_BUFFERSIZE_ENTRY = 1024 * 1024 * 16;  // 16MiB, used for entry data storage

  DART_METHOD_ID_STOP      = 0000;
  DART_METHOD_ID_MAINPROC  = 0001;
  DART_METHOD_ID_CHARCHSIG = 0002;

   // termination flag values
   DART_TERMFLAG_TERMINATED = -1;
   DART_TERMFLAG_CONTINUE   = 0;

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
fProgressTracker.BeginUpdate;
try
  fProgressTracker.Clear;
  If fArchiveProcessingSettings.Common.InMemoryProcessing then
    begin
      case fArchiveProcessingSettings.Common.RepairMethod of
        rmRebuild,
        rmConvert:  begin
                      fProgressTracker.Add(300,DART_PROGSTAGE_ID_Loading);
                      fProgressTracker.Add(400,DART_PROGSTAGE_ID_Processing);
                      fProgressTracker.Add(300,DART_PROGSTAGE_ID_Saving);
                    end;
        rmExtract:  begin
                      fProgressTracker.Add(400,DART_PROGSTAGE_ID_Loading);
                      fProgressTracker.Add(600,DART_PROGSTAGE_ID_Processing);
                    end;
      end;
    end
  else fProgressTracker.Add(1000,DART_PROGSTAGE_ID_Processing)
finally
  fProgressTracker.EndUpdate;
end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.AllocateMemoryBuffers;
begin
AllocBuffer(fBuffer_IO,DART_BUFFERSIZE_IO);
AllocBuffer(fBuffer_Entry,DART_BUFFERSIZE_ENTRY);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.FreeMemoryBuffers;
begin
FreeBuffer(fBuffer_Entry);
FreeBuffer(fBuffer_IO);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.ForwardedProgressHandler(Sender: TObject; Progress: Single);
begin
DoProgress([DART_PROGSTAGE_ID_NoProgress],Progress);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.DoProgress(StageIDs: array of Integer; Data: Single);
var
  CurrentStageNode: TProgressTracker;
  i,Index:          Integer;
begin
If Assigned(fHeartbeat) then InterlockedIncrement(fHeartbeat^);
fPauseControlObject.WaitFor;
If Terminated then
  begin
    fTerminating := True;
    Terminated := False;
    DoTerminate;
    DoError(DART_METHOD_ID_STOP,'Processing terminated. Data can be in an inconsistent state.');
  end;
// actual progress...
{$message 'progress needs serious optimization'}  
case Length(StageIDs) of
  0:  If Assigned(fOnProgress) then
        fOnProgress(Self,fProgressTracker.Progress);
  1:  begin
        If StageIDs[0] <= 0 then
          begin
            Index := Abs(StageIDs[0]);
            If (Index < fProgressTracker.LowIndex) or (Index > fProgressTracker.HighIndex) then
              Index := -1;
          end
        else Index := fProgressTracker.IndexOf(StageIDs[0]);
        If Index >= 0 then
          begin
            If Data > 1.0 then Data := 1.0;
            fProgressTracker.SetStageProgress(Index,Data);
            If Assigned(fOnProgress) then
              fOnProgress(Self,fProgressTracker.Progress);
          end
        else
          If Assigned(fOnProgress) and (StageIDs[0] <> DART_PROGSTAGE_ID_NoProgress) then
            case StageIDs[0] of
              DART_PROGSTAGE_ID_Direct: fOnProgress(Self,Data);
            else
              fOnProgress(Self,fProgressTracker.Progress);
            end;
      end;
else
  // length > 1
  CurrentStageNode := fProgressTracker;
  For i := Low(StageIDs) to High(StageIDs) do
    begin
      If StageIDs[i] <= 0 then
        begin
          Index := Abs(StageIDs[i]);
          If (Index < CurrentStageNode.LowIndex) or (Index > CurrentStageNode.HighIndex) then
            Index := -1;
        end
      else Index := CurrentStageNode.IndexOf(StageIDs[i]);
      If Index < 0 then
        begin
          CurrentStageNode := nil;
          Break{For i};
        end
      else CurrentStageNode := CurrentStageNode.StageObjects[Index];
    end;
  If Assigned(CurrentStageNode) then
    begin
      If Data > 1.0 then Data := 1.0;
      CurrentStageNode.Progress := Data;
      If Assigned(fOnProgress) then
        fOnProgress(Self,fProgressTracker.Progress);
    end
  else
    If Assigned(fOnProgress) and (StageIDs[0] <> DART_PROGSTAGE_ID_NoProgress) then
      case StageIDs[0] of
        DART_PROGSTAGE_ID_Direct: fOnProgress(Self,Data);
      else
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
DoProgress([DART_PROGSTAGE_ID_Direct],-1.0);
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
      DoError(DART_METHOD_ID_CHARCHSIG,'Bad file signature (0x%.8x).',[Signature]);
  end
else DoError(DART_METHOD_ID_CHARCHSIG,'File is too small to contain any valid signature (%d bytes).',[fInputArchiveStream.Size]);
end;

//------------------------------------------------------------------------------

Function TDARTRepairer.FindSignature(Signature: UInt32; ProgressStage: array of Integer; SearchFrom: Int64 = -1; SearchBack: Boolean = False; Limited: Boolean = False): Int64;
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
      fInputArchiveStream.Seek(-(CurrentOffset + Int64(fBuffer_IO.Size)),soEnd)
    else
      fInputArchiveStream.Seek(CurrentOffset,soBeginning);
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

procedure TDARTRepairer.ProgressedLoadFile(const FileName: String; Stream: TStream; ProgressStage: array of Integer);
var
  FileStream: TFileStream;
  BytesRead:  Integer;
begin
DoProgress(ProgressStage,0.0);
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
try
  Stream.Size := FileStream.Size; // prevents reallocations
  Stream.Seek(0,soBeginning);
  FileStream.Seek(0,soBeginning);
  If FileStream.Size > 0 then
    repeat
      BytesRead := FileStream.Read(fBuffer_IO.Memory^,fBuffer_IO.Size);
      Stream.Write(fBuffer_IO.Memory^,BytesRead);
      DoProgress(ProgressStage,FileStream.Position / FileStream.Size);
    until TMemSize(BytesRead) < fBuffer_IO.Size;
  Stream.Size := Stream.Position;  
finally
  FileStream.Free;
end;
DoProgress(ProgressStage,1.0);
end;
 
//------------------------------------------------------------------------------

procedure TDARTRepairer.ProgressedSaveFile(const FileName: String; Stream: TStream; ProgressStage: array of Integer);
var
  FileStream: TFileStream;
  BytesRead:  Integer;
begin
DoProgress(ProgressStage,0.0);
FileStream := TFileStream.Create(StrToRTL(FileName),fmCreate or fmShareExclusive);
try
  FileStream.Size := Stream.Size;
  Stream.Seek(0,soBeginning);
  FileStream.Seek(0,soBeginning);
  If Stream.Size > 0 then
    repeat
      BytesRead := Stream.Read(fBuffer_IO.Memory^,fBuffer_IO.Size);
      FileStream.Write(fBuffer_IO.Memory^,BytesRead);
      DoProgress(ProgressStage,Stream.Position / Stream.Size);
    until TMemSize(BytesRead) < fBuffer_IO.Size;
  FileStream.Size := FileStream.Position;  
finally
  FileStream.Free;
end;
DoProgress(ProgressStage,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.ProgressedStreamRead(Stream: TStream; Buffer: Pointer; Size: TMemSize; ProgressStage: array of Integer);
var
  i,Max:  PtrUInt;
begin
DoProgress(ProgressStage,0.0);
Max := Ceil(Size / fBuffer_IO.Size);
For i := 1 to Max do
  begin
    Dec(Size,TMemSize(Stream.Read(Buffer^,Min(Int64(fBuffer_IO.Size),Int64(Size)))));
    Buffer := {%H-}Pointer({%H-}PtrUInt(Buffer) + PtrUInt(fBuffer_IO.Size));
    DoProgress(ProgressStage,i / Max);
  end;
DoProgress(ProgressStage,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.ProgressedStreamWrite(Stream: TStream; Buffer: Pointer; Size: TMemSize; ProgressStage: array of Integer);
var
  i,Max:  PtrUInt;
begin
DoProgress(ProgressStage,0.0);
Max := Ceil(Size / fBuffer_IO.Size);
For i := 1 to Max do
  begin
    Dec(Size,TMemSize(Stream.Write(Buffer^,Min(Int64(fBuffer_IO.Size),Int64(Size)))));
    Buffer := {%H-}Pointer({%H-}PtrUInt(Buffer) + PtrUInt(fBuffer_IO.Size));
    DoProgress(ProgressStage,i / Max);
  end;
DoProgress(ProgressStage,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.CompressorProgressHandler(Sender: TObject; Progress: Single);
begin
DoProgress(fCompProgressStage,Progress);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.ProgressedDecompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; WindowBits: Integer; ProgressStage: array of Integer);
var
  i:  Integer;
begin
SetLength(fCompProgressStage,Length(ProgressStage));
For i := Low(ProgressStage) to High(ProgressStage) do
  fCompProgressStage[i] := ProgressStage[i];
with TZDecompressionBuffer.Create(InBuff,InSize,WindowBits) do
try
  FreeResult := False;
  OnProgress := CompressorProgressHandler;
  Process;
  OutBuff := ResultMemory;
  OutSize := ResultSize;
finally
  Free;
end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.ProgressedCompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; WindowBits: Integer; ProgressStage: array of Integer);
var
  i:  Integer;
begin
SetLength(fCompProgressStage,Length(ProgressStage));
For i := Low(ProgressStage) to High(ProgressStage) do
  fCompProgressStage[i] := ProgressStage[i];
with TZCompressionBuffer.Create(InBuff,InSize,zclDefault,WindowBits) do
try
  FreeResult := False;
  OnProgress := CompressorProgressHandler;
  Process;
  OutBuff := ResultMemory;
  OutSize := ResultSize;
finally
  Free;
end;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer.GetEntryData(const EntryFileName: AnsiString; out Data: Pointer; out Size: TMemSize): Boolean;
var
  Index:  Integer;
begin
Index := IndexOfEntry(EntryFileName);
If Index >= 0 then
  Result := GetEntryData(Index,Data,Size)
else
  Result := False;
end;

//------------------------------------------------------------------------------

class Function TDARTRepairer.IndexOfKnownPath(const Path: AnsiString; const KnownPaths: TDART_KnownPaths): Integer;
var
  PathHash: TCRC32;
  i:        Integer;
begin
PathHash := AnsiStringCRC32(AnsiLowerCase(Path));
Result := -1;
For i := Low(KnownPaths.Arr) to Pred(KnownPaths.Count) do
  If KnownPaths.Arr[i].Hash = PathHash then
    If AnsiSameText(Path,KnownPaths.Arr[i].Path) then
      begin
        Result := i;
        Break{For i};
      end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer.MainProcessing;
begin
fResultInfo.ResultState := rsNormal;
try
  DoProgress([DART_PROGSTAGE_ID_Direct],0.0);
  AllocateMemoryBuffers;
  try
    If fArchiveProcessingSettings.Common.InMemoryProcessing then
      fInputArchiveStream := TMemoryStream.Create
    else
      fInputArchiveStream := TFileStream.Create(StrToRTL(fArchiveProcessingSettings.Common.ArchivePath),fmOpenRead or fmShareDenyWrite);
    try
      If fArchiveProcessingSettings.Common.InMemoryProcessing then
        ProgressedLoadFile(fArchiveProcessingSettings.Common.ArchivePath,fInputArchiveStream,[DART_PROGSTAGE_ID_Loading]);
      If fInputArchiveStream.Size <= 0 then
        DoError(DART_METHOD_ID_MAINPROC,'Input file does not contain any data.');
      If not fArchiveProcessingSettings.Common.IgnoreFileSignature then
        CheckArchiveSignature;
      ArchiveProcessing;  // <- processing happens here
      DoProgress([DART_PROGSTAGE_ID_Direct],1.0);
    finally
      fInputArchiveStream.Free;
    end;
  finally
    FreeMemoryBuffers;
  end;
  DoProgress([DART_PROGSTAGE_ID_Direct],2.0);
except
  on E: EDARTProcessingException do
    If fCatchExceptions then
      begin
        fResultInfo.ErrorInfo.FaultObjectRef := E.FaultObjectRef;
        fResultInfo.ErrorInfo.FaultObjectClass := E.FaultObjectClass;
        fResultInfo.ErrorInfo.FaultFunctionIndex := E.FaultFunctionIdx;
        fResultInfo.ErrorInfo.FaultFunctionName := E.FaultFunctionName;
        ProcessException(E)
      end
    else raise;
  on E: Exception do
    If fCatchExceptions then ProcessException(E)
      else raise;
end;
end;

{-------------------------------------------------------------------------------
    TDARTRepairer - public methods
-------------------------------------------------------------------------------}

class Function TDARTRepairer.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  DART_METHOD_ID_STOP:      Result := 'Stop*';
  DART_METHOD_ID_MAINPROC:  Result := 'MainProcessing';
  DART_METHOD_ID_CHARCHSIG: Result := 'CheckArchiveSignature';
else
  Result := 'unknown method';
end;
end;

//------------------------------------------------------------------------------

constructor TDARTRepairer.Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings; CatchExceptions: Boolean = True);
begin
inherited Create;
fCatchExceptions := CatchExceptions;
fTerminatedFlag := DART_TERMFLAG_CONTINUE;
fResultInfo := DART_DefaultResultInfo;
fResultInfo.RepairerInfo := Format('%s(0x%p)',[Self.ClassName,Pointer(Self)]);
fOnProgress := nil;
fHeartbeat := nil;
{$WARN SYMBOL_PLATFORM OFF}
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,{%H-}fLocalFormatSettings);
{$WARN SYMBOL_PLATFORM ON}
fPauseControlObject := PauseControlObject;
fPauseControlObject.SetEvent;
fArchiveProcessingSettings := ArchiveProcessingSettings;
fProgressTracker := TProgressTracker.Create;
fProgressTracker.ConsecutiveStages := True;
fProgressTracker.StrictlyGrowing := True;
fTerminating := False;
fExpectedSignature := 0;
fCompProgressStage := nil;
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
