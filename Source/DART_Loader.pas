unit DART_Loader;

{$INCLUDE DART_defs.inc}

interface

uses
  Classes,
  AuxTypes, MemoryBuffer,
  DART_Common;

type
  TDARTLoader = class(TDARTProcessingObject)
  private
  protected
    fInputArchiveStream:  TStream;
    fExpectedSignature:   UInt32;
    fBuffer_IO:           TMemoryBuffer;  
  (*
    // initialization and finalization methods
    procedure InitializeProcessingSettings; virtual; abstract;
    procedure InitializeData; virtual; abstract;
    procedure InitializeProgress; virtual; abstract;
    procedure FinalizeProgress; virtual; abstract;
    procedure FinalizeData; virtual; abstract;
    procedure FinalizeProcessingSettings; virtual; abstract;
  *)
    // memory buffers management
    procedure AllocateMemoryBuffers; override;
    procedure FreeMemoryBuffers; override;
    // processing methods
    //procedure MainProcessing; virtual; abstract;
    // loading-specific methods
    procedure CheckArchiveSignature; virtual;
    Function FindSignature(Signature: UInt32; SearchFrom: Int64 = -1; SearchBack: Boolean = False; Limited: Boolean = False; ProgressStage: Integer = DART_PROGSTAGE_ID_NoProgress): Int64; virtual;
    procedure ProgressedLoadFile(const FileName: String; Stream: TStream; ProgressStage: Integer); virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
    procedure Load; virtual; abstract;
  published
  end;

implementation

uses
  SysUtils,
  StrRect;

const
  DART_LOADER_BUFFERSIZE_IO = 1024 * 1024;  // 1MiB

procedure TDARTLoader.AllocateMemoryBuffers;
begin
inherited;
AllocBuffer(fBuffer_IO,DART_LOADER_BUFFERSIZE_IO);
end;

//------------------------------------------------------------------------------

procedure TDARTLoader.FreeMemoryBuffers;
begin
FreeBuffer(fBuffer_IO);
inherited;
end;

//------------------------------------------------------------------------------

procedure TDARTLoader.CheckArchiveSignature;
var
  Signature: UInt32;
begin
fInputArchiveStream.Seek(0,soBeginning);
If fInputArchiveStream.Read({%H-}Signature,SizeOf(Signature)) >= SizeOf(Signature) then
  begin
    If Signature <> fExpectedSignature then
      DoError(0,'Bad file signature (0x%.8x).',[Signature]);
  end
else DoError(0,'File is too small to contain any valid signature (%d bytes).',[fInputArchiveStream.Size]);
end;

//------------------------------------------------------------------------------

Function TDARTLoader.FindSignature(Signature: UInt32; SearchFrom: Int64 = -1; SearchBack: Boolean = False; Limited: Boolean = False; ProgressStage: Integer = DART_PROGSTAGE_ID_NoProgress): Int64;
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

procedure TDARTLoader.ProgressedLoadFile(const FileName: String; Stream: TStream; ProgressStage: Integer);
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

//==============================================================================

class Function TDARTLoader.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
inherited;
case MethodIndex of
  0:  Result := 'CheckArchiveSignature';
else
  Result := 'unknown method';
end;
end;

end.
