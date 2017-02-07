{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer;

{$INCLUDE DART_defs.inc}

interface

uses
  SysUtils, Classes,
  AuxTypes, WinSyncObjs,
  DART_MemoryBuffer, DART_ProcessingSettings;

{==============================================================================}
{------------------------------------------------------------------------------}
{                              ERepairerException                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   ERepairerException - class declaration                                     }
{==============================================================================}
type
  ERepairerException = class(Exception)
  private
    fSourceObject:        TObject;
    fSourceObjectClass:   String;
    fSourceFunctionIdx:   Integer;
    fSourceFunctionName:  String;
  public
    constructor Create(const Text: String; SrcObject: TObject; SrcFunctionIdx: Integer; const SrcFunctionName: String);
  published
    property SourceObject: TObject read fSourceObject;
    property SourceObjectClass: String read fSourceObjectClass;
    property SourceFunctionIdx: Integer read fSourceFunctionIdx;
    property SourceFunctionName: String read fSourceFunctionName;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                                   TRepairer                                  }
{------------------------------------------------------------------------------}
{==============================================================================}

const
  WINDOWBITS_Raw  = -15;
  WINDOWBITS_ZLib = 15;
  WINDOWBITS_GZip = 31;

type
  TResultState = (rsUndefined,rsNormal,rsWarning,rsError);

  TResultInfoError = record
    ErrorSourceObject:        TObject;
    ErrorSourceObjectClass:   String;
    ErrorSourceFunctionIndex: Integer;
    ErrorSourceFunctionName:  String;
    ExceptionClass:           String;
    ExceptionText:            String;
  end;

  TResultInfoWarning = record
    Warnings: array of String;
  end;

  TResultInfo = record
    ResultState:  TResultState;
    RepairerInfo: String;
    WarningInfo:  TResultInfoWarning;
    ErrorInfo:    TResultInfoError;
  end;

const
  DefaultResultInfo: TResultInfo = (
    ResultState:  rsUndefined;
    RepairerInfo: '';
    WarningInfo: (
      Warnings: nil);
    ErrorInfo: (
      ErrorSourceObject:        nil;
      ErrorSourceObjectClass:   '';
      ErrorSourceFunctionIndex: -1;
      ErrorSourceFunctionName:  'unknown function';
      ExceptionClass:           '';
      ExceptionText:            ''));

type
  TPathsArray = array of AnsiString; // used to return paths obtained by content parsing

  TProgressEvent = procedure(Sender: TObject; Progress: Single) of object;

  TProgressStage = record
    Offset: Single;
    Range:  Single;
  end;

const
  PROGSTAGEIDX_NoProgress = -100;
  PROGSTAGEIDX_Direct     = -1;
  PROGSTAGEIDX_Default    = 0;
  PROGSTAGEIDX_Custom     = 1;
  PROGSTAGEIDX_Loading    = 2;
  PROGSTAGEIDX_Processing = 3;
  PROGSTAGEIDX_Saving     = 4;
  PROGSTAGEIDX_Max        = PROGSTAGEIDX_Saving;

{==============================================================================}
{   TRepairer - class declaration                                              }
{==============================================================================}
type
  TRepairer = class(TObject)
  private
    fCatchExceptions:         Boolean;
    fTerminatedFlag:          Integer;  // 0 = continue, <>0 = terminated
    fLastProgressData:        Single;
    fResultInfo:              TResultInfo;
    fOnProgress:              TProgressEvent;
  protected
    fPauseControlObject:      TEvent;
    fExpectedSignature:       UInt32;
    fTerminating:             Boolean;
    fArchiveStream:           TStream;
    fLocalFormatSettings:     TFormatSettings;
    fFileProcessingSettings:  TFileProcessingSettings;
    fProgressStages:          array of TProgressStage;
    // preallocated memory buffers
    fIO_Buffer:               TMemoryBuffer;
    fCED_Buffer:              TMemoryBuffer;
    fUED_BUffer:              TMemoryBuffer;
    // event called when the processing is terminated
    fOnTerminate:             TNotifyEvent;
    // initialization methods
    procedure RectifyFileProcessingSettings; virtual; abstract;
    procedure InitializeData; virtual; abstract;
    procedure InitializeProgress; virtual;
    // flow control and progress report methods
    procedure DoProgress(ProgressStageIdx: Integer; Data: Single); virtual;
    procedure DoWarning(const WarningText: String); virtual;
    procedure DoError(MethodIndex: Integer; const ErrorText: String; Values: array of const); overload; virtual;
    procedure DoError(MethodIndex: Integer; const ErrorText: String); overload; virtual;
    // common processing functions
    procedure CheckArchiveSignature; virtual;
    Function FindSignature(Signature: UInt32; SearchFrom: Int64 = -1; SearchBack: Boolean = False; Limited: Boolean = False; ProgressStage: Integer = PROGSTAGEIDX_NoProgress): Int64; virtual;
    procedure ProgressedLoadFile(const FileName: String; Stream: TStream; ProgressStage: Integer); virtual;
    procedure ProgressedSaveFile(const FileName: String; Stream: TStream; ProgressStage: Integer); virtual;
    procedure ProgressedStreamRead(Stream: TStream; Buffer: Pointer; Size: TMemSize; ProgressStage: Integer); virtual;
    procedure ProgressedStreamWrite(Stream: TStream; Buffer: Pointer; Size: TMemSize; ProgressStage: Integer); virtual;
    procedure ProgressedDecompressBuffer(InBuff: Pointer; InSize: Integer; out OutBuff: Pointer; out OutSize: Integer; ProgressStage: Integer; EntryName: String; WindowBits: Integer); virtual;
    procedure ProgressedCompressBuffer(InBuff: Pointer; InSize: Integer; out OutBuff: Pointer; out OutSize: Integer; ProgressStage: Integer; EntryName: String; WindowBits: Integer); virtual;
    // content parsing for paths resolving
    //procedure ParseContentForPaths(out Paths: TPathsArray; DirsTrailingDelimiter: Boolean); virtual; abstract;
    //Function GetEntryData(const EntryPath: AnsiString; out Data: Pointer; out Size: TMemSize): Boolean; virtual; abstract;
    // memory buffers management
    procedure AllocateMemoryBuffers; virtual;
    procedure FreeMemoryBuffers; virtual;
    // helper for managing exceptions
    procedure ManageException(E: Exception); virtual;
    // main processing methods
    procedure MainProcessing; virtual;
    procedure ArchiveProcessing; virtual; abstract; // <- all the fun must happen here
    // flow control
    procedure Resume; virtual;
  public
    // helper methods
    class Function CreateFileStream(const FileName: String; Mode: Word): TFileStream; virtual;
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; virtual;
    constructor Create(PauseControlObject: TEvent; FileProcessingSettings: TFileProcessingSettings; CatchExceptions: Boolean);
    destructor Destroy; override;
    procedure Run; virtual;
    procedure Stop; virtual;
    Function Terminated: Boolean; virtual;
    property ResultInfo: TResultInfo read fResultInfo;
  published
    property ExpectedSignature: UInt32 read fExpectedSignature;
    property OnProgress: TProgressEvent read fOnProgress write fOnProgress;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                              Auxiliary functions                             }
{------------------------------------------------------------------------------}
{==============================================================================}

// Returns text identifying what method is used to call zlib
Function zlib_method_str: String;

implementation

uses
  Windows, Math
{$IFDEF FPC}
  {$IFDEF FPC_Internal_ZLib}
  , PasZLib
  {$ENDIF}
  , laz_zlib  // used in both internal and external (DLL) mode
{$ELSE FPC}
  , ZLibExAPI // Delphi
{$ENDIF FPC}
{$IFDEF FPC_NonUnicode}
  , LazUTF8
{$ENDIF};

{==============================================================================}
{------------------------------------------------------------------------------}
{                              Auxiliary functions                             }
{------------------------------------------------------------------------------}
{==============================================================================}

Function zlib_method_str: String;
begin
{$IFDEF FPC}
{$IFDEF FPC_Internal_ZLib}
Result := 'S';  // PasZLib is used for compression, statically linked zlib for decompression
{$ELSE FPC_Internal_ZLib}
Result := 'D';  // ZLib is loaded from a DLL
{$ENDIF FPC_Internal_ZLib}
{$ELSE FPC}
Result := 'E';  // ZLibEx (delphi zlib) is used (statically linked)
{$ENDIF FPC}
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                              ERepairerException                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   ERepairerException - class implementation                                  }
{==============================================================================}

{------------------------------------------------------------------------------}
{   ERepairerException - public methods                                        }
{------------------------------------------------------------------------------}

constructor ERepairerException.Create(const Text: String; SrcObject: TObject; SrcFunctionIdx: Integer; const SrcFunctionName: String);
begin
inherited Create(Text);
fSourceObject := SrcObject;
fSourceObjectClass := SrcObject.ClassName;
fSourceFunctionIdx := SrcFunctionIdx;
fSourceFunctionName := SrcFunctionName;
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                                   TRepairer                                  }
{------------------------------------------------------------------------------}
{==============================================================================}

(*
  Following check is here because placing it right after uses clause will cause
  internal error 200501152 in FPC.
*)
{$IFDEF FPC}
  {$IFDEF FPC_Internal_ZLib}
    {$IF zlib_dll}
      {$MESSAGE FATAL 'laz_zlib must not be in a DLL mode.'}
    {$IFEND}
  {$ELSE}
    {$IF not zlib_dll}
      {$MESSAGE FATAL 'laz_zlib is not in a DLL mode.'}
    {$IFEND}
  {$ENDIF}
{$ENDIF}

const
  // Size of the buffer used in progress-aware stream reading and writing
  IO_BufferSize  = $100000; {1MiB}

  // Initial size of buffer used to hold compressed entry data
  CED_BufferSize = $100000 * 8; {8MiB}

  // Initial size of buffer used to hold uncompressed entry data
  UED_BufferSize = $100000 * 16; {16MiB}

{==============================================================================}
{   TRepairer - class implementation                                           }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TRepairer - protected methods                                              }
{------------------------------------------------------------------------------}

procedure TRepairer.InitializeProgress;
begin
SetLength(fProgressStages,Succ(PROGSTAGEIDX_Max));
// all values that are not explicitly set are equal to 0.0
fProgressStages[PROGSTAGEIDX_Default].Range := 1.0;
If fFileProcessingSettings.Common.InMemoryProcessing then
  begin
    case fFileProcessingSettings.Common.RepairMethod of
      rmRebuild,
      rmConvert: begin
                   fProgressStages[PROGSTAGEIDX_Loading].Range := 0.3;
                   fProgressStages[PROGSTAGEIDX_Saving].Range := 0.3;
                 end;
      rmExtract: fProgressStages[PROGSTAGEIDX_Loading].Range := 0.4;
    end;
    fProgressStages[PROGSTAGEIDX_Saving].Offset := 1.0 - fProgressStages[PROGSTAGEIDX_Saving].Range;
  end;
fProgressStages[PROGSTAGEIDX_Processing].Offset := fProgressStages[PROGSTAGEIDX_Loading].Range;
fProgressStages[PROGSTAGEIDX_Processing].Range := 1.0 -
  fProgressStages[PROGSTAGEIDX_Loading].Range -
  fProgressStages[PROGSTAGEIDX_Saving].Range;
end;

//------------------------------------------------------------------------------

procedure TRepairer.DoProgress(ProgressStageIdx: Integer; Data: Single);
begin
fPauseControlObject.WaitFor;
If Terminated then
  begin
    fTerminating := True;
    Resume;
    If Assigned(fOnTerminate) then fOnTerminate(Self);
    DoError(0,'Processing terminated. Data can be in inconsistent state.');
  end;
If (ProgressStageIdx >= Low(fProgressStages)) and (ProgressStageIdx <= High(fProgressStages)) then
  begin
    If Data > 1.0 then Data := 1.0;
    Data := fProgressStages[ProgressStageIdx].Offset + (fProgressStages[ProgressStageIdx].Range * Data);
    // make sure progress goes only up, not down
    If Data >= fLastProgressData then
      begin
        If Assigned(fOnProgress) then fOnProgress(Self,Data);
        fLastProgressData := Data;
      end;
  end
else
  begin
    If ProgressStageIdx <> PROGSTAGEIDX_NoProgress then
      If Assigned(fOnProgress) then fOnProgress(Self,Data);
  end;
end;

//------------------------------------------------------------------------------

procedure TRepairer.DoWarning(const WarningText: String);
begin
fResultInfo.ResultState := rsWarning;
with fResultInfo.WarningInfo do
  begin
    SetLength(Warnings,Length(Warnings) + 1);
    Warnings[High(Warnings)] := WarningText;
  end;
end;

//------------------------------------------------------------------------------

procedure TRepairer.DoError(MethodIndex: Integer; const ErrorText: String; Values: array of const);
begin
raise ERepairerException.Create(Format(ErrorText,Values,fLocalFormatSettings),
                                Self,MethodIndex,GetMethodNameFromIndex(MethodIndex));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TRepairer.DoError(MethodIndex: Integer; const ErrorText: String);
begin
DoError(MethodIndex,ErrorText,[]);
end;

//------------------------------------------------------------------------------

procedure TRepairer.CheckArchiveSignature;
var
  Signature: UInt32;
begin
fArchiveStream.Seek(0,soBeginning);
If fArchiveStream.Read({%H-}Signature,SizeOf(Signature)) >= SizeOf(Signature) then
  begin
    If Signature <> fExpectedSignature then
      DoError(2,'Bad file signature (0x%.8x).',[Signature]);
  end
else DoError(2,'File is too small to contain valid signature (%d bytes).',[fArchiveStream.Size]);
end;

//------------------------------------------------------------------------------

Function TRepairer.FindSignature(Signature: UInt32; SearchFrom: Int64 = -1; SearchBack: Boolean = False; Limited: Boolean = False; ProgressStage: Integer = PROGSTAGEIDX_NoProgress): Int64;
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
      CurrentOffset := fArchiveStream.Size - SearchFrom
    else
      CurrentOffset := SearchFrom;
  end
else CurrentOffset := 0;
If fArchiveStream.Size > 0 then
  repeat
    If SearchBack then
      fArchiveStream.Seek(-(CurrentOffset + Int64(fIO_Buffer.Size)),soFromEnd)
    else
      fArchiveStream.Seek(CurrentOffset,soFromBeginning);
    BytesRead := fArchiveStream.Read(fIO_Buffer.Memory^,fIO_Buffer.Size);
    If BytesRead >= SizeOf(Signature) then
      For i := 0 to (BytesRead - SizeOf(Signature)) do
        If {%H-}PUInt32({%H-}PtrUInt(fIO_Buffer.Memory) + PtrUInt(i))^ = Signature then
          begin
            Result := fArchiveStream.Position - BytesRead + i;
            Exit;
          end;
    Inc(CurrentOffset,fIO_Buffer.Size - BufferOverlap);
    DoProgress(ProgressStage,CurrentOffset / fArchiveStream.Size);
  until (CurrentOffset > fArchiveStream.Size) or Limited;
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProgressedLoadFile(const FileName: String; Stream: TStream; ProgressStage: Integer);
var
  FileStream: TFileStream;
  BytesRead:  Integer;
begin
DoProgress(ProgressStage,0.0);
FileStream := CreateFileStream(FileName,fmOpenRead or fmShareDenyWrite);
try
  Stream.Size := FileStream.Size; // prevents reallocations
  Stream.Seek(0,soFromBeginning);
  FileStream.Seek(0,soFromBeginning);
  If FileStream.Size > 0 then
    repeat
      BytesRead := FileStream.Read(fIO_Buffer.Memory^,fIO_Buffer.Size);
      Stream.Write(fIO_Buffer.Memory^,BytesRead);
      DoProgress(ProgressStage,FileStream.Position / FileStream.Size);
    until BytesRead <= 0;
finally
  FileStream.Free;
end;
DoProgress(ProgressStage,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProgressedSaveFile(const FileName: String; Stream: TStream; ProgressStage: Integer);
var
  FileStream: TFileStream;
  BytesRead:  Integer;
begin
DoProgress(ProgressStage,0.0);
FileStream := CreateFileStream(FileName,fmCreate or fmShareDenyWrite);
try
  FileStream.Size := Stream.Size;
  Stream.Seek(0,soFromBeginning);
  FileStream.Seek(0,soFromBeginning);
  If Stream.Size > 0 then
    repeat
      BytesRead := Stream.Read(fIO_Buffer.Memory^,fIO_Buffer.Size);
      FileStream.Write(fIO_Buffer.Memory^,BytesRead);
      DoProgress(ProgressStage,Stream.Position / Stream.Size);
    until BytesRead <= 0;
  FileStream.Size := FileStream.Position;  
finally
  FileStream.Free;
end;
DoProgress(ProgressStage,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProgressedStreamRead(Stream: TStream; Buffer: Pointer; Size: TMemSize; ProgressStage: Integer);
var
  i,Max:  Integer;
begin
DoProgress(ProgressStage,0.0);
Max := Ceil(Size / fIO_Buffer.Size);
For i := 1 to Max do
  begin
    Dec(Size,Stream.Read(Buffer^,Min(Int64(fIO_Buffer.Size),Int64(Size))));
    Buffer := {%H-}Pointer({%H-}PtrUInt(Buffer) + fIO_Buffer.Size);
    DoProgress(ProgressStage,i / Max);
  end;
DoProgress(ProgressStage,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProgressedStreamWrite(Stream: TStream; Buffer: Pointer; Size: TMemSize; ProgressStage: Integer);
var
  i,Max:  Integer;
begin
DoProgress(ProgressStage,0.0);
Max := Ceil(Size / fIO_Buffer.Size);
For i := 1 to Max do
  begin
    Dec(Size,Stream.Write(Buffer^,Min(Int64(fIO_Buffer.Size),Int64(Size))));
    Buffer := {%H-}Pointer({%H-}PtrUInt(Buffer) + fIO_Buffer.Size);
    DoProgress(ProgressStage,i / Max);
  end;
DoProgress(ProgressStage,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProgressedDecompressBuffer(InBuff: Pointer; InSize: Integer; out OutBuff: Pointer; out OutSize: Integer; ProgressStage: Integer; EntryName: String; WindowBits: Integer);
type
{$IFDEF FPC}
  TZStream = laz_zlib.TZStream;
{$ELSE}
  // Delphi
  TZStream = ZLibExAPI.TZStreamRec;
{$ENDIF}
var
  ZStream:    TZStream;
  SizeDelta:  Integer;
  ResultCode: Integer;

  Function RaiseDecompressionError(ResCode: Integer; RaiseBuffError: Boolean = True): Integer;
  begin
    If (ResCode < 0) and ((ResCode <> Z_BUF_ERROR) or RaiseBuffError) then
      begin
        If Assigned(ZStream.msg) then
          DoError(3,'zlib: %s - %s ("%s")',[{$IFDEF FPC}laz_zlib.{$ENDIF}z_errmsg[2 - ResCode],String(ZStream.msg),EntryName])
        else
          DoError(3,'zlib: %s ("%s")',[{$IFDEF FPC}laz_zlib.{$ENDIF}z_errmsg[2 - ResCode],EntryName]);
      end;
    Result := ResCode;
  end;

begin
DoProgress(ProgressStage,0.0);
OutBuff := nil;
OutSize := 0;
If InSize > 0 then
  begin
    FillChar({%H-}ZStream,SizeOf(TZStream),0);
    SizeDelta := (InSize + 255) and not 255;
    OutSize := SizeDelta;
    RaiseDecompressionError({$IFDEF FPC}laz_zlib.{$ENDIF}InflateInit2(ZStream,WindowBits));
    try
      ResultCode := Z_OK;
      ZStream.next_in := InBuff;
      ZStream.avail_in := InSize;
      while ResultCode <> Z_STREAM_END do
        repeat
          ReallocateMemoryBuffer(fUED_Buffer,OutSize);
          ZStream.next_out := {%H-}Pointer({%H-}PtrUInt(fUED_Buffer.Memory) + PtrUInt(ZStream.total_out));
          ZStream.avail_out := fUED_Buffer.Size - ZStream.total_out;
          ResultCode := RaiseDecompressionError({$IFDEF FPC}laz_zlib.{$ENDIF}Inflate(ZStream,Z_NO_FLUSH),False);
          DoProgress(ProgressStage,ZStream.total_in / InSize);
          Inc(OutSize,SizeDelta);
        until (ResultCode = Z_STREAM_END) or (ZStream.avail_out > 0);
      // copy decompressed data into output
      OutSize := ZStream.total_out;
      GetMem(OutBuff,OutSize);
      Move(fUED_Buffer.Memory^,OutBuff^,OutSize);
    finally
      RaiseDecompressionError({$IFDEF FPC}laz_zlib.{$ENDIF}InflateEnd(ZStream));
    end;
  end;
DoProgress(ProgressStage,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProgressedCompressBuffer(InBuff: Pointer; InSize: Integer; out OutBuff: Pointer; out OutSize: Integer; ProgressStage: Integer; EntryName: String; WindowBits: Integer);
type
{$IFDEF FPC}
{$IFDEF FPC_Internal_ZLib}
  // FPC, internal
  TZStream = PasZLib.TZStream;
{$ELSE}
  // FPC, external (DLL)
  TZStream = laz_zlib.TZStream;
{$ENDIF}
{$ELSE}
  // Delphi
  TZStream = ZLibExAPI.TZStreamRec;
{$ENDIF}
var
  ZStream:    TZStream;
  SizeDelta:  Integer;
  ResultCode: Integer;

  Function RaiseCompressionError(ResCode: Integer; RaiseBuffError: Boolean = True): Integer;
  begin
    If (ResCode < 0) and ((ResCode <> Z_BUF_ERROR) or RaiseBuffError) then
      begin
      {$IF Defined(FPC) and Defined(FPC_Internal_ZLib)}
        If Length(ZStream.msg) > 0 then
      {$ELSE}
        If Assigned(ZStream.msg) then
      {$IFEND}
          DoError(4,'zlib: %s - %s ("%s")',[{$IFDEF FPC}laz_zlib.{$ENDIF}z_errmsg[2 - ResCode],String(ZStream.msg),EntryName])
        else
          DoError(4,'zlib: %s ("%s")',[{$IFDEF FPC}laz_zlib.{$ENDIF}z_errmsg[2 - ResCode],EntryName]);
      end;
    Result := ResCode;
  end;

begin
DoProgress(ProgressStage,0.0);
OutBuff := nil;
OutSize := 0;
If InSize > 0 then
  begin
    FillChar({%H-}ZStream,SizeOf(TZStream),0);
    SizeDelta := ((InSize div 4) + 255) and not 255;
    OutSize := SizeDelta;
    RaiseCompressionError({$IFDEF FPC}{$IFDEF FPC_Internal_ZLib}PasZLib.{$ELSE}laz_zlib.{$ENDIF}{$ENDIF}
      DeflateInit2(ZStream,Z_DEFAULT_COMPRESSION,Z_DEFLATED,WindowBits,8,Z_DEFAULT_STRATEGY));
    try
      ZStream.next_in := InBuff;
      ZStream.avail_in := InSize;
      repeat
        ReallocateMemoryBuffer(fCED_Buffer,OutSize);
        ZStream.next_out := {%H-}Pointer({%H-}PtrUInt(fCED_Buffer.Memory) + PtrUInt(ZStream.total_out));
        ZStream.avail_out := fCED_Buffer.Size - ZStream.total_out;
        ResultCode := RaiseCompressionError({$IFDEF FPC}{$IFDEF FPC_Internal_ZLib}PasZLib.{$ELSE}laz_zlib.{$ENDIF}{$ENDIF}
                        Deflate(ZStream,Z_NO_FLUSH),False);
        DoProgress(ProgressStage,ZStream.total_in / InSize);
        Inc(OutSize, SizeDelta);
      until (ResultCode = Z_STREAM_END) or (ZStream.avail_in = 0);
      // flush what is left in zlib internal buffer
      while ResultCode <> Z_STREAM_END do
        begin
          ReallocateMemoryBuffer(fCED_Buffer,OutSize);
          ZStream.next_out := {%H-}Pointer({%H-}PtrUInt(fCED_Buffer.Memory) + PtrUInt(ZStream.total_out));
          ZStream.avail_out := fCED_Buffer.Size - ZStream.total_out;
          ResultCode := RaiseCompressionError({$IFDEF FPC}{$IFDEF FPC_Internal_ZLib}PasZLib.{$ELSE}laz_zlib.{$ENDIF}{$ENDIF}
                          Deflate(ZStream,Z_FINISH),False);
          DoProgress(ProgressStage,ZStream.total_in / InSize);
          Inc(OutSize, SizeDelta);
        end;
      // copy compressed data into output
      OutSize := ZStream.total_out;
      GetMem(OutBuff,OutSize);
      Move(fCED_Buffer.Memory^,OutBuff^,OutSize);
    finally
      RaiseCompressionError({$IFDEF FPC}{$IFDEF FPC_Internal_ZLib}PasZLib.{$ELSE}laz_zlib.{$ENDIF}{$ENDIF}
        DeflateEnd(ZStream));
    end;
  end;
DoProgress(ProgressStage,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.AllocateMemoryBuffers;
begin
AllocateMemoryBuffer(fIO_Buffer,IO_BufferSize);
AllocateMemoryBuffer(fCED_Buffer,CED_BufferSize);
AllocateMemoryBuffer(fUED_BUffer,UED_BUfferSize);
end;

//------------------------------------------------------------------------------

procedure TRepairer.FreeMemoryBuffers;
begin
FreeMemoryBuffer(fIO_Buffer);
FreeMemoryBuffer(fCED_Buffer);
FreeMemoryBuffer(fUED_BUffer);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ManageException(E: Exception);
begin
fResultInfo.ResultState := rsError;
fResultInfo.ErrorInfo.ExceptionClass := E.ClassName;
fResultInfo.ErrorInfo.ExceptionText := E.Message;
DoProgress(PROGSTAGEIDX_Direct,-1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.MainProcessing;
begin
fResultInfo.ResultState := rsNormal;
try
  DoProgress(PROGSTAGEIDX_Direct,0.0);
  AllocateMemoryBuffers;
  try
    If fFileProcessingSettings.Common.InMemoryProcessing then
      fArchiveStream := TMemoryStream.Create
    else
      fArchiveStream := CreateFileStream(fFileProcessingSettings.Common.FilePath,fmOpenRead or fmShareDenyWrite);
    try
      If fFileProcessingSettings.Common.InMemoryProcessing then
        ProgressedLoadFile(fFileProcessingSettings.Common.FilePath,fArchiveStream,PROGSTAGEIDX_Loading);
      If fArchiveStream.Size <= 0 then
        DoError(1,'Input file does not contain any data.');
      If not fFileProcessingSettings.Common.IgnoreFileSignature then
        CheckArchiveSignature;
      ArchiveProcessing;  // <- processing happens here
      DoProgress(PROGSTAGEIDX_Direct,1.0);
    finally
      fArchiveStream.Free;
    end;
  finally
    FreeMemoryBuffers;
  end;
  DoProgress(PROGSTAGEIDX_Direct,2.0);
except
  on E: ERepairerException do
    If fCatchExceptions then
      begin
        fResultInfo.ErrorInfo.ErrorSourceObject := E.SourceObject;
        fResultInfo.ErrorInfo.ErrorSourceObjectClass := E.SourceObjectClass;
        fResultInfo.ErrorInfo.ErrorSourceFunctionIndex := E.SourceFunctionIdx;
        fResultInfo.ErrorInfo.ErrorSourceFunctionName := E.SourceFunctionName;
        ManageException(E)
      end
    else raise;
  on E: Exception do
    If fCatchExceptions then ManageException(E)
      else raise;
end;
end;

//------------------------------------------------------------------------------

procedure TRepairer.Resume;
begin
InterlockedExchange(fTerminatedFlag,0);
end;

{------------------------------------------------------------------------------}
{   TRepairer - public methods                                                 }
{------------------------------------------------------------------------------}

class Function TRepairer.CreateFileStream(const FileName: String; Mode: Word): TFileStream;
begin
{$IFDEF FPC_NonUnicode}
Result := TFileStream.Create(UTF8ToSys(FileName),Mode);
{$ELSE}
Result := TFileStream.Create(FileName,Mode);
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TRepairer.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  0:  Result := 'Stop*';
  1:  Result := 'MainProcessing';
  2:  Result := 'CheckArchiveSignature;';
  3:  Result := 'ProgressedDecompressBuffer';
  4:  Result := 'ProgressedCompressBuffer';
else
  Result := 'unknown method';
end;
end;

//------------------------------------------------------------------------------

constructor TRepairer.Create(PauseControlObject: TEvent; FileProcessingSettings: TFileProcessingSettings; CatchExceptions: Boolean);
begin
inherited Create;
fCatchExceptions := CatchExceptions;
fTerminatedFlag := 0;
fLastProgressData := 0.0;
fResultInfo := DefaultResultInfo;
fResultInfo.RepairerInfo := Format('%s(0x%p)',[Self.ClassName,Pointer(Self)]);
fPauseControlObject := PauseControlObject;
fExpectedSignature := 0;
fTerminating := False;
{$WARN SYMBOL_PLATFORM OFF}
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,{%H-}fLocalFormatSettings);
{$WARN SYMBOL_PLATFORM ON}
fFileProcessingSettings := FileProcessingSettings;
SetLength(fProgressStages,0);
// initialization
RectifyFileProcessingSettings;
InitializeData;
InitializeProgress;
end;

//------------------------------------------------------------------------------

destructor TRepairer.Destroy;
begin
inherited;
end;

//------------------------------------------------------------------------------

procedure TRepairer.Run;
begin
Resume;
MainProcessing;
end;

//------------------------------------------------------------------------------

procedure TRepairer.Stop;
begin
InterlockedExchange(fTerminatedFlag,-1);
end;

//------------------------------------------------------------------------------

Function TRepairer.Terminated: Boolean;
begin
Result := InterlockedExchangeAdd(fTerminatedFlag,0) <> 0;
end;

//==============================================================================

{$IFDEF FPC}
initialization
  laz_zlib.Initialize;

finalization
  laz_zlib.Finalize;
{$ENDIF}

end.
