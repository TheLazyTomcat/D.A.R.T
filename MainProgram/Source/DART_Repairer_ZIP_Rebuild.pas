{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_ZIP_Rebuild;

{$INCLUDE DART_defs.inc}

interface

uses
  DART_Repairer_ZIP;

type
  TRepairer_ZIP_Rebuild = class(TRepairer_ZIP)
  protected
    procedure ZIP_RebuildArchive; virtual;
    procedure ArchiveProcessing; override;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;    
  end;

{$IF not Declared(FPC_FULLVERSION)}
const
  FPC_FULLVERSION = Integer(0);
{$IFEND}

implementation

uses
  SysUtils, Classes,
  AuxTypes, CRC32, 
  DART_MemoryBuffer, DART_Repairer
{$IF Defined(FPC)}
  , LazUTF8
{$IF FPC_FULLVERSION < 20701}
  , LazFileUtils
{$IFEND}
{$IFEND};

procedure TRepairer_ZIP_Rebuild.ZIP_RebuildArchive;
var
  RebuildArchiveStream:     TStream;
  i:                        Integer;
  TempOffset:               Int64;
  DecompressedBuff:         Pointer;
  DecompressedSize:         Integer;
begin
DoProgress(PROCSTAGEIDX_ZIP_EntriesProcessing,0.0);
// create directory where the rebuild file will be stored
{$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
ForceDirectoriesUTF8(ExtractFileDir(fFileProcessingSettings.Common.TargetPath));
{$ELSE}
ForceDirectories(ExtractFileDir(fFileProcessingSettings.Common.TargetPath));
{$IFEND}
// create output stream
If fFileProcessingSettings.Common.InMemoryProcessing then
  RebuildArchiveStream := TMemoryStream.Create
else
  RebuildArchiveStream := CreateFileStream(fFileProcessingSettings.Common.TargetPath,fmCreate or fmShareDenyWrite);
try
  // prepare output stream
  If fFileProcessingSettings.Common.InMemoryProcessing then
    RebuildArchiveStream.Size := Trunc(fArchiveStream.Size * 1.1);
  RebuildArchiveStream.Seek(0,soFromBeginning);  
  For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
    with fArchiveStructure.Entries[i] do
      try
        // store offset of entry LH it has in input
        UtilityData.OriginalLocalHeaderOffset := CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader;
        If UtilityData.NeedsSizes then
          begin
            // need to obtain sizes of entry - getting compressed size here
            If i < High(fArchiveStructure.Entries) then
              // inner entry (ie. not a last one), comp. size is calculated as difference
              // between data start (obtained earlier) and start of local header of next entry
              LocalHeader.BinPart.CompressedSize := UInt32((Int64(fArchiveStructure.Entries[i + 1].CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader) - UtilityData.DataOffset))
            else
              begin
                // last entry, find CD or EOCD
                TempOffset := FindSignature(ZIP_CentralDirectoryFileHeaderSignature);
                If TempOffset < 0 then
                  TempOffset := FindSignature(ZIP_EndOfCentralDirectorySignature);
                If TempOffset >= UtilityData.DataOffset then
                  // CD or EOCD found, let's assume it marks end of entry data
                  LocalHeader.BinPart.CompressedSize := UInt32(TempOffset - UtilityData.DataOffset)
                else
                  // CD or EOCD not found, assuming entry data are continuing to EOF
                  LocalHeader.BinPart.CompressedSize := UInt32(fArchiveStream.Size - UtilityData.DataOffset);
              end;
            // store obtained compressed size  
            DataDescriptor.CompressedSize := LocalHeader.BinPart.CompressedSize;
            CentralDirectoryHeader.BinPart.CompressedSize := LocalHeader.BinPart.CompressedSize;
          end;
        If fProcessingSettings.AssumeCompressionMethods then
          begin
            If (LocalHeader.BinPart.CompressedSize > 0) and (LocalHeader.BinPart.CompressionMethod <> 0) then
              begin
                // compressed entry has non-zero size and stored compression method
                // differs from 0 (store) =>  assuming compression method 8 (deflate)
                LocalHeader.BinPart.CompressionMethod := 8;
                CentralDirectoryHeader.BinPart.CompressionMethod := 8;
              end
            else
              begin
                LocalHeader.BinPart.CompressionMethod := 0; // store (no compression)
                CentralDirectoryHeader.BinPart.CompressionMethod := 0;
              end;
          end;
        // calculating progress info for processing of current entry
        ZIP_PrepareEntryProgressInfo(i);
        DoProgress(PROCSTAGEIDX_ZIP_EntryProcessing,0.0);
        // prepare buffer that will hold compressed data
        ReallocateMemoryBuffer(fCED_Buffer,LocalHeader.BinPart.CompressedSize);
        // load compressed data
        fArchiveStream.Seek(UtilityData.DataOffset,soFromBeginning);
        ProgressedStreamRead(fArchiveStream,fCED_Buffer.Memory,LocalHeader.BinPart.CompressedSize,PROCSTAGEIDX_ZIP_EntryLoading);
        // deciding whether entry data needs to be decompressed for further processing
        If (UtilityData.NeedsCRC32 or UtilityData.NeedsSizes) and (LocalHeader.BinPart.CompressionMethod <> 0) then
          begin
            // data needs to be decompressed for further processing
            ProgressedDecompressBuffer(fCED_Buffer.Memory,LocalHeader.BinPart.CompressedSize,
              DecompressedBuff,DecompressedSize,PROCSTAGEIDX_ZIP_EntryDecompressing,
              {$IFDEF FPC}WinCPToUTF8(CentralDirectoryHeader.FileName),
              {$ELSE}CentralDirectoryHeader.FileName,{$ENDIF}WINDOWBITS_Raw);
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
            // crc32 calculation and decompressed size are not required)
            If UtilityData.NeedsCRC32 then
              begin
                LocalHeader.BinPart.CRC32 := BufferCRC32(fCED_Buffer.Memory^,LocalHeader.BinPart.CompressedSize);
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
        // current position in output corresponds to offset of LH
        CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader := UInt32(RebuildArchiveStream.Position);
        // write local header
        RebuildArchiveStream.WriteBuffer(LocalHeader.BinPart,SizeOf(TZIP_LocalFileHeaderRecord));
        RebuildArchiveStream.WriteBuffer(PAnsiChar(LocalHeader.FileName)^,LocalHeader.BinPart.FileNameLength);
        RebuildArchiveStream.WriteBuffer(PAnsiChar(LocalHeader.ExtraField)^,LocalHeader.BinPart.ExtraFieldLength);
        // write entry data
        ProgressedStreamWrite(RebuildArchiveStream,fCED_Buffer.Memory,LocalHeader.BinPart.CompressedSize,PROCSTAGEIDX_ZIP_EntrySaving);
        // write data descriptor
        If (LocalHeader.BinPart.GeneralPurposeBitFlag and ZBF_DataDescriptor) <> 0 then
          RebuildArchiveStream.WriteBuffer(DataDescriptor,SizeOf(TZIP_DataDescriptorRecord));
        DoProgress(PROCSTAGEIDX_ZIP_EntryProcessing,1.0);
        // if we are here, then there was no error during processing
        UtilityData.Erroneous := False;
      except
        on E: Exception do
          begin
            UtilityData.Erroneous := True;
            If fFileProcessingSettings.Common.IgnoreErroneousEntries and not fTerminating then
              begin
                Resume;
                DoWarning(Format('%s: %s',[E.ClassName,E.Message]));
              end
            else raise;
          end;
      end;
  // current position in output is where the central directory starts
  fArchiveStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset := UInt32(RebuildArchiveStream.Position);
  // write central directory
  For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
    begin
      If fArchiveStructure.Entries[i].UtilityData.Erroneous then
        // entry is erroneous (there was error in processing that was ignored), do not write this entry
        with fArchiveStructure.EndOfCentralDirectory.BinPart,fArchiveStructure.Entries[i].CentralDirectoryHeader.BinPart do
          begin
            Dec(EntriesOnDisk);
            Dec(Entries);
            Dec(CentralDirectorySize,SizeOf(TZIP_CentralDirectoryFileHeaderRecord));
            Dec(CentralDirectorySize,FileNameLength);
            Dec(CentralDirectorySize,ExtraFieldLength);
            Dec(CentralDirectorySize,FileCommentLength);
        end
      else
        // writing entry data
        with fArchiveStructure.Entries[i].CentralDirectoryHeader do
          begin
            RebuildArchiveStream.WriteBuffer(BinPart,SizeOf(TZIP_CentralDirectoryFileHeaderRecord));
            RebuildArchiveStream.WriteBuffer(PAnsiChar(FileName)^,BinPart.FileNameLength);
            RebuildArchiveStream.WriteBuffer(PAnsiChar(ExtraField)^,BinPart.ExtraFieldLength);
            RebuildArchiveStream.WriteBuffer(PAnsiChar(FileComment)^,BinPart.FileCommentLength);
          end;
      DoProgress(PROCSTAGEIDX_NoProgress,0.0);
    end;
  // write end of central directory
  with fArchiveStructure.EndOfCentralDirectory do
    begin
      RebuildArchiveStream.WriteBuffer(BinPart,SizeOf(TZIP_EndOfCentralDirectoryRecord));
      RebuildArchiveStream.WriteBuffer(PAnsiChar(Comment)^,BinPart.CommentLength);
    end;
  // finalize
  RebuildArchiveStream.Size := RebuildArchiveStream.Position;
  DoProgress(PROCSTAGEIDX_ZIP_EntriesProcessing,1.0);
  If fFileProcessingSettings.Common.InMemoryProcessing then
    ProgressedSaveFile(fFileProcessingSettings.Common.TargetPath,RebuildArchiveStream,PROCSTAGEIDX_Saving);
finally
  RebuildArchiveStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TRepairer_ZIP_Rebuild.ArchiveProcessing;
begin
// check if target <> source
If AnsiSameText(fFileProcessingSettings.Common.FilePath,fFileProcessingSettings.Common.TargetPath) then
  DoError(200,'Output is directed into an input file, cannot proceed.');
inherited;
ZIP_RebuildArchive;
end;

//==============================================================================

class Function TRepairer_ZIP_Rebuild.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  200:  Result := 'ArchiveProcessing';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

end.
