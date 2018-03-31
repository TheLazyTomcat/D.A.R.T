unit DART_Repairer_ZIP_Rebuild;

{$INCLUDE DART_defs.inc}

interface

uses
  Classes,
  DART_Repairer_ZIP;

type
  TDARTRepairer_ZIP_Rebuild = class(TDARTRepairer_ZIP_ProcessingBase)
  protected
    fRebuildArchiveStream:  TStream;
    procedure ArchiveProcessing; override;
    // zip-rebuilding specific methods
    procedure ZIP_RebuildArchive; virtual;
    procedure ZIP_WriteEntry(Index: Integer); virtual;
    procedure ZIP_WriteCentralDirectory; virtual;
    procedure ZIP_WriteEndOfCentralDirectory; virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
  end;

implementation

uses
  SysUtils,
  AuxTypes, MemoryBuffer, CRC32, ZLibCommon,
  DART_Auxiliary, DART_Format_ZIP, DART_Repairer;

const
  DART_METHOD_ID_ZIP_REB_ARCHPROC = 1100;

procedure TDARTRepairer_ZIP_Rebuild.ArchiveProcessing;
begin
// check if target <> source
If AnsiSameText(fArchiveProcessingSettings.Common.ArchivePath,fArchiveProcessingSettings.Common.TargetPath) then
  DoError(DART_METHOD_ID_ZIP_REB_ARCHPROC,'Output is directed into an input file, cannot proceed.');
inherited;
ZIP_RebuildArchive;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Rebuild.ZIP_RebuildArchive;
var
  i:  Integer;
begin
DoProgress([PSID_Processing,PSID_Z_EntriesProcessing],0.0);
// create directory where the rebuild file will be stored
DART_ForceDirectories(ExtractFileDir(fArchiveProcessingSettings.Common.TargetPath));
// create output stream
If fArchiveProcessingSettings.Common.InMemoryProcessing then
  fRebuildArchiveStream := TMemoryStream.Create
else
  fRebuildArchiveStream := TFileStream.Create(fArchiveProcessingSettings.Common.TargetPath,fmCreate or fmShareDenyWrite);
try
  // prepare output stream
  If fArchiveProcessingSettings.Common.InMemoryProcessing then
    fRebuildArchiveStream.Size := Trunc(fInputArchiveStream.Size * 1.1);
  fRebuildArchiveStream.Seek(0,soBeginning);
  // save individual entries
  For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
    ZIP_WriteEntry(i);
  // current position in output is where the central directory starts
  fArchiveStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset := UInt32(fRebuildArchiveStream.Position);
  ZIP_WriteCentralDirectory;  
  ZIP_WriteEndOfCentralDirectory;
  // finalize
  fRebuildArchiveStream.Size := fRebuildArchiveStream.Position;
  DoProgress([PSID_Processing,PSID_Z_EntriesProcessing],1.0);
  If fArchiveProcessingSettings.Common.InMemoryProcessing then
    ProgressedSaveFile(fArchiveProcessingSettings.Common.TargetPath,fRebuildArchiveStream,[DART_PROGSTAGE_ID_Saving]);
finally
  fRebuildArchiveStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Rebuild.ZIP_WriteEntry(Index: Integer);
var
  TempOffset:       Int64;
  DecompressedBuff: Pointer;
  DecompressedSize: TMemSize;
begin
with fArchiveStructure.Entries.Arr[Index] do
try
  // store offset of entry local header it has in input
  UtilityData.OriginalLocalHeaderOffset := CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader;

  // obtain compressed size of the entry if required
  If UtilityData.NeedsSizes then
    begin
      // need to obtain sizes of entry - getting compressed size here
      If Index < Pred(fArchiveStructure.Entries.Count) then
        // inner entry (ie. not a last one), compressed size is calculated as a difference
        // between data start (obtained earlier) and start of local header of next entry
        LocalHeader.BinPart.CompressedSize := UInt32(Int64(fArchiveStructure.Entries.Arr[Index + 1].CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader) - UtilityData.DataOffset)
      else
        begin
          // last entry, find CD or EOCD
          TempOffset := FindSignature(DART_ZIP_CentralDirectoryFileHeaderSignature,[]);
          If TempOffset < 0 then
            TempOffset := FindSignature(DART_ZIP_EndOfCentralDirectorySignature,[]);
          If TempOffset >= UtilityData.DataOffset then
            // CD or EOCD found, let's assume it marks an end of entry data
            LocalHeader.BinPart.CompressedSize := UInt32(TempOffset - UtilityData.DataOffset)
          else
            // CD or EOCD not found, assuming entry data are continuing to EOF
            LocalHeader.BinPart.CompressedSize := UInt32(fInputArchiveStream.Size - UtilityData.DataOffset);
        end;
      // store obtained compressed size
      DataDescriptor.CompressedSize := LocalHeader.BinPart.CompressedSize;
      CentralDirectoryHeader.BinPart.CompressedSize := LocalHeader.BinPart.CompressedSize;
    end;

  // assume compression method if required
  If fProcessingSettings.AssumeCompressionMethods then
    begin
      If (LocalHeader.BinPart.CompressedSize > 0) and (LocalHeader.BinPart.CompressionMethod <> DART_ZCM_Store) then
        begin
          // compressed entry has non-zero size and stored compression method
          // differs from 0 (store) =>  assuming compression method 8 (deflate)
          LocalHeader.BinPart.CompressionMethod := DART_ZCM_Deflate;
          CentralDirectoryHeader.BinPart.CompressionMethod := DART_ZCM_Deflate;
        end
      else
        begin
          LocalHeader.BinPart.CompressionMethod := DART_ZCM_Store;
          CentralDirectoryHeader.BinPart.CompressionMethod := DART_ZCM_Store;
        end;
    end;

  DoProgress([PSID_Processing,PSID_Z_EntriesProcessing,-Index],0.0);
  // prepare buffer that will hold entry data
  ReallocBufferKeep(fBuffer_Entry,LocalHeader.BinPart.CompressedSize);
  // load compressed data
  fInputArchiveStream.Seek(UtilityData.DataOffset,soBeginning);
  ProgressedStreamRead(fInputArchiveStream,fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,
                       [PSID_Processing,PSID_Z_EntriesProcessing,-Index,PSID_Z_EntryLoading]);

  // deciding whether entry data needs to be decompressed for further processing
  If (UtilityData.NeedsCRC32 or UtilityData.NeedsSizes) and (LocalHeader.BinPart.CompressionMethod <> 0) then
    begin
      // data needs to be decompressed for further processing
      ProgressedDecompressBuffer(fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,DecompressedBuff,DecompressedSize,
                                 WBITS_RAW,[PSID_Processing,PSID_Z_EntriesProcessing,-Index,PSID_Z_EntryDecompression]);
      try
        If UtilityData.NeedsCRC32 then
          begin
            LocalHeader.BinPart.CRC32 := BufferCRC32(DecompressedBuff^,DecompressedSize);
            DataDescriptor.CRC32 := LocalHeader.BinPart.CRC32;
            CentralDirectoryHeader.BinPart.CRC32 := LocalHeader.BinPart.CRC32;
          end;
        If UtilityData.NeedsSizes then
          begin
            LocalHeader.BinPart.UncompressedSize := DecompressedSize;
            DataDescriptor.UncompressedSize := DecompressedSize;
            CentralDirectoryHeader.BinPart.UncompressedSize := DecompressedSize;
          end;
      finally
        FreeMem(DecompressedBuff,DecompressedSize);
      end;
    end
  else
    begin
      // decompression is not needed (data are without compression or
      // crc32 calculation and/or decompressed size are not required)
      If UtilityData.NeedsCRC32 then
        begin
          LocalHeader.BinPart.CRC32 := BufferCRC32(fBuffer_Entry.Memory^,LocalHeader.BinPart.CompressedSize);
          DataDescriptor.CRC32 := LocalHeader.BinPart.CRC32;
          CentralDirectoryHeader.BinPart.CRC32 := LocalHeader.BinPart.CRC32;
        end;
      If UtilityData.NeedsSizes then
        begin
          LocalHeader.BinPart.UncompressedSize := LocalHeader.BinPart.CompressedSize;
          DataDescriptor.UncompressedSize := DataDescriptor.CompressedSize;
          CentralDirectoryHeader.BinPart.UncompressedSize :=  CentralDirectoryHeader.BinPart.CompressedSize;
        end;
    end;

  // current position in output corresponds to offset of local header
  CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader := UInt32(fRebuildArchiveStream.Position);
  // write local header
  fRebuildArchiveStream.WriteBuffer(LocalHeader.BinPart,SizeOf(TDART_ZIP_LocalFileHeaderRecord));
  fRebuildArchiveStream.WriteBuffer(PAnsiChar(LocalHeader.FileName)^,LocalHeader.BinPart.FileNameLength);
  fRebuildArchiveStream.WriteBuffer(PAnsiChar(LocalHeader.ExtraField)^,LocalHeader.BinPart.ExtraFieldLength);
  // write entry data
  ProgressedStreamWrite(fRebuildArchiveStream,fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,
                        [PSID_Processing,PSID_Z_EntriesProcessing,-Index,PSID_Z_EntrySaving]);
  // write data descriptor
  If (LocalHeader.BinPart.GeneralPurposeBitFlag and DART_ZBF_DataDescriptor) <> 0 then
    fRebuildArchiveStream.WriteBuffer(DataDescriptor,SizeOf(TDART_ZIP_DataDescriptorRecord));
  DoProgress([PSID_Processing,PSID_Z_EntriesProcessing,-Index],1.0);
  // if we are here, then there was no error during processing
  UtilityData.Erroneous := False;
except
  on E: Exception do
    begin
      UtilityData.Erroneous := True;
      If fArchiveProcessingSettings.Common.IgnoreErroneousEntries and not fTerminating then
        begin
          Terminated := False;
          DoWarning(Format('%s: %s',[E.ClassName,E.Message]));
        end
      else raise;
    end;
end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Rebuild.ZIP_WriteCentralDirectory;
var
  i:  Integer;
begin
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  begin
    If fArchiveStructure.Entries.Arr[i].UtilityData.Erroneous then
      // entry is erroneous (there was error in processing that was ignored), do not write this entry
      with fArchiveStructure.EndOfCentralDirectory.BinPart,fArchiveStructure.Entries.Arr[i].CentralDirectoryHeader.BinPart do
        begin
          Dec(EntriesOnDisk);
          Dec(Entries);
          Dec(CentralDirectorySize,SizeOf(TDART_ZIP_CentralDirectoryFileHeaderRecord));
          Dec(CentralDirectorySize,FileNameLength);
          Dec(CentralDirectorySize,ExtraFieldLength);
          Dec(CentralDirectorySize,FileCommentLength);
        end
    else
      // writing entry data
      with fArchiveStructure.Entries.Arr[i].CentralDirectoryHeader do
        begin
          fRebuildArchiveStream.WriteBuffer(BinPart,SizeOf(TDART_ZIP_CentralDirectoryFileHeaderRecord));
          fRebuildArchiveStream.WriteBuffer(PAnsiChar(FileName)^,BinPart.FileNameLength);
          fRebuildArchiveStream.WriteBuffer(PAnsiChar(ExtraField)^,BinPart.ExtraFieldLength);
          fRebuildArchiveStream.WriteBuffer(PAnsiChar(FileComment)^,BinPart.FileCommentLength);
        end;
    DoProgress([DART_PROGSTAGE_ID_NoProgress],0.0);
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Rebuild.ZIP_WriteEndOfCentralDirectory;
begin
with fArchiveStructure.EndOfCentralDirectory do
  begin
    fRebuildArchiveStream.WriteBuffer(BinPart,SizeOf(TDART_ZIP_EndOfCentralDirectoryRecord));
    fRebuildArchiveStream.WriteBuffer(PAnsiChar(Comment)^,BinPart.CommentLength);
  end;
end;

//==============================================================================

class Function TDARTRepairer_ZIP_Rebuild.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  DART_METHOD_ID_ZIP_REB_ARCHPROC:  Result := 'ArchiveProcessing';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;


end.
