{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_ZIP_Rebuild;

{$INCLUDE DART_defs.inc}

interface

uses
  Classes,
  DART_Repairer_ZIP;

{===============================================================================
--------------------------------------------------------------------------------
                           TDARTRepairer_ZIP_Rebuild
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_ZIP_Rebuild - class declaration
===============================================================================}
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
  end;

implementation

uses
  SysUtils,
  AuxTypes, MemoryBuffer, CRC32, ZLibCommon, StrRect,
  DART_Auxiliary, DART_Common, DART_Format_ZIP, DART_Repairer;

{===============================================================================
--------------------------------------------------------------------------------
                           TDARTRepairer_ZIP_Rebuild
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_ZIP_Rebuild - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTRepairer_ZIP_Rebuild - protected methods
-------------------------------------------------------------------------------}

procedure TDARTRepairer_ZIP_Rebuild.ArchiveProcessing;
begin
inherited;
ZIP_RebuildArchive;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Rebuild.ZIP_RebuildArchive;
var
  i:  Integer;
begin
DoProgress(fProcessingProgNode,PSIDX_Z_EntriesProcessing,0.0);
// create directory where the rebuild archive will be stored
DART_ForceDirectories(ExtractFileDir(fArchiveProcessingSettings.Common.TargetPath));
// create output stream
If fArchiveProcessingSettings.Common.InMemoryProcessing then
  fRebuildArchiveStream := TMemoryStream.Create
else
  fRebuildArchiveStream := TFileStream.Create(StrToRTL(fArchiveProcessingSettings.Common.TargetPath),fmCreate or fmShareDenyWrite);
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
  DoProgress(fProcessingProgNode,PSIDX_Z_EntriesProcessing,1.0);  
  If fArchiveProcessingSettings.Common.InMemoryProcessing then
    ProgressedSaveFile(fArchiveProcessingSettings.Common.TargetPath,fRebuildArchiveStream,
                       ProgressStageInfo(fProgressTracker,DART_PROGSTAGE_IDX_Saving));
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
          TempOffset := FindSignature(DART_ZIP_CentralDirectoryFileHeaderSignature,DART_PROGSTAGE_INFO_NoProgress);
          If TempOffset < 0 then
            TempOffset := FindSignature(DART_ZIP_EndOfCentralDirectorySignature,DART_PROGSTAGE_INFO_NoProgress);
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
      // correct for 0-size entries
      If LocalHeader.BinPart.CompressedSize = 0 then
        begin
          LocalHeader.BinPart.CompressionMethod := DART_ZCM_Store;
          CentralDirectoryHeader.BinPart.CompressionMethod := DART_ZCM_Store;
          LocalHeader.BinPart.UncompressedSize := 0;
          DataDescriptor.UncompressedSize := 0;
          CentralDirectoryHeader.BinPart.UncompressedSize := 0;
        end;
    end;

  // prepare progress
  fEntryProcessingProgNode := fEntriesProcessingProgNode.StageObjects[Index];
  DoProgress(fEntriesProcessingProgNode,Index,0.0);
  // prepare buffer that will hold entry data
  ReallocBufferKeep(fBuffer_Entry,LocalHeader.BinPart.CompressedSize);
  // load compressed data
  fInputArchiveStream.Seek(UtilityData.DataOffset,soBeginning);
  ProgressedStreamRead(fInputArchiveStream,fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,
                       ProgressStageInfo(fEntryProcessingProgNode,PSIDX_Z_EntryLoading));

  // deciding whether entry data needs to be decompressed for further processing
  If (UtilityData.NeedsCRC32 or UtilityData.NeedsSizes) and (LocalHeader.BinPart.CompressionMethod <> DART_ZCM_Store) then
    begin
      // data needs to be decompressed for further processing
      ProgressedDecompressBuffer(fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,DecompressedBuff,DecompressedSize,
                                 WBITS_RAW,ProgressStageInfo(fEntryProcessingProgNode,PSIDX_Z_EntryDecompression));
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
                        ProgressStageInfo(fEntryProcessingProgNode,PSIDX_Z_EntrySaving));
  // write data descriptor
  If (LocalHeader.BinPart.GeneralPurposeBitFlag and DART_ZBF_DataDescriptor) <> 0 then
    fRebuildArchiveStream.WriteBuffer(DataDescriptor,SizeOf(TDART_ZIP_DataDescriptorRecord));
  DoProgress(fEntriesProcessingProgNode,Index,1.0);
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
    DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0);
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

end.
