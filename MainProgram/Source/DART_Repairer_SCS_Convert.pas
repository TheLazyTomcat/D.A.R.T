{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_SCS_Convert;

{$INCLUDE DART_defs.inc}

interface

uses
  DART_Format_ZIP, DART_Repairer_SCS_Rebuild;

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TRepairer_SCS_Convert                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TRepairer_SCS_Convert - class declaration                                  }
{==============================================================================}
type
  TRepairer_SCS_Convert = class(TRepairer_SCS_FileProcBase)
  protected
    fZIPArchiveStructure: TZIP_ArchiveStructure;
    procedure SCS_SortConvertEntries; virtual;
    procedure SCS_PrepareConversion; virtual;
    procedure SCS_ConvertArchive; virtual;
    procedure ArchiveProcessing; override;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
  end;

implementation

uses
  Windows, SysUtils, Classes,
  AuxTypes, CRC32, BitOps,
  DART_MemoryBuffer, DART_Format_SCS, DART_Repairer, DART_Repairer_SCS;

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TRepairer_SCS_Convert                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TRepairer_SCS_Convert - class implementation                               }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TRepairer_SCS_Rebuild - protected methods                                  }
{------------------------------------------------------------------------------}

procedure TRepairer_SCS_Convert.SCS_SortConvertEntries;

  procedure QuickSort(LeftIdx,RightIdx: Integer);
  var
    Pivot:  AnsiString;
    Idx,i:  Integer;

    procedure ExchangeEntries(Idx1,Idx2: Integer);
    var
      TempEntry:  TZIP_Entry;
    begin
      If (Idx1 <> Idx2) then
        begin
          If (Idx1 < Low(fZIPArchiveStructure.Entries)) or (Idx1 > High(fZIPArchiveStructure.Entries)) then
            DoError(300,'Index 1 (%d) out of bounds.',[Idx1]);
          If (Idx2 < Low(fZIPArchiveStructure.Entries)) or (Idx2 > High(fZIPArchiveStructure.Entries)) then
            DoError(300,'Index 2 (%d) out of bounds.',[Idx1]);
          TempEntry := fZIPArchiveStructure.Entries[Idx1];
          fZIPArchiveStructure.Entries[Idx1] := fZIPArchiveStructure.Entries[Idx2];
          fZIPArchiveStructure.Entries[Idx2] := TempEntry;
        end;
    end;

  begin
    DoProgress(PROCSTAGEIDX_NoProgress,0.0);
    If LeftIdx < RightIdx then
      begin
        ExchangeEntries((LeftIdx + RightIdx) shr 1,RightIdx);
        Pivot := fZIPArchiveStructure.Entries[RightIdx].CentralDirectoryHeader.FileName;
        Idx := LeftIdx;
        For i := LeftIdx to Pred(RightIdx) do
          If AnsiCompareText(Pivot,fZIPArchiveStructure.Entries[i].CentralDirectoryHeader.FileName) > 0 then
            begin
              ExchangeEntries(i,idx);
              Inc(Idx);
            end;
        ExchangeEntries(Idx,RightIdx);
        QuickSort(LeftIdx,Idx - 1);
        QuickSort(Idx + 1,RightIdx);
      end;
  end;

begin
If Length(fZIPArchiveStructure.Entries) > 1 then
  QuickSort(0,High(fZIPArchiveStructure.Entries));
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS_Convert.SCS_PrepareConversion;
var
  i:  Integer;

  procedure CopyCentralToLocal(Index: Integer);
  begin
    with fZIPArchiveStructure.Entries[Index] do
      begin
        LocalHeader.BinPart.Signature := ZIP_LocalFileHeaderSignature;
        LocalHeader.BinPart.VersionNeededToExtract := CentralDirectoryHeader.BinPart.VersionNeededToExtract;
        LocalHeader.BinPart.OSNeededForExtraction := CentralDirectoryHeader.BinPart.OSNeededForExtraction;
        LocalHeader.BinPart.GeneralPurposeBitFlag := CentralDirectoryHeader.BinPart.GeneralPurposeBitFlag;
        LocalHeader.BinPart.CompressionMethod := CentralDirectoryHeader.BinPart.CompressionMethod;
        LocalHeader.BinPart.LastModFileTime := CentralDirectoryHeader.BinPart.LastModFileTime;
        LocalHeader.BinPart.LastModFileDate := CentralDirectoryHeader.BinPart.LastModFileDate;
        LocalHeader.BinPart.CRC32 := CentralDirectoryHeader.BinPart.CRC32;
        LocalHeader.BinPart.CompressedSize := CentralDirectoryHeader.BinPart.CompressedSize;
        LocalHeader.BinPart.UncompressedSize := CentralDirectoryHeader.BinPart.UncompressedSize;
        LocalHeader.BinPart.FileNameLength := CentralDirectoryHeader.BinPart.FileNameLength;
        LocalHeader.BinPart.ExtraFieldLength := CentralDirectoryHeader.BinPart.ExtraFieldLength;
        LocalHeader.FileName := CentralDirectoryHeader.FileName;
        LocalHeader.ExtraField := CentralDirectoryHeader.ExtraField;
      end;
  end;

begin
SetLength(fZIPArchiveStructure.Entries,Length(fArchiveStructure.Entries));
For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
  begin
    DoProgress(PROCSTAGEIDX_NoProgress,0.0);
    {
      DataOffset in utility data is used to store original index.
      It is done because entries gets sorted which will break their index
      entanglement with original SCS entries.
    }
    fZIPArchiveStructure.Entries[i].UtilityData.DataOffset := Int64(i);
    with fZIPArchiveStructure.Entries[i].CentralDirectoryHeader do
      begin
        BinPart.Signature := ZIP_CentralDirectoryFileHeaderSignature;
        BinPart.VersionMadeBy := 20;
        BinPart.HostOS := 0;
        BinPart.VersionNeededToExtract := 20;
        BinPart.OSNeededForExtraction := 0;
        BinPart.GeneralPurposeBitFlag := 0;
        BinPart.LastModFileTime := DateTimeToFileDate(Now) and $FFFF;
        BinPart.LastModFileDate := DateTimeToFileDate(Now) shr 16;
        BinPart.ExtraFieldLength := 0;
        BinPart.FileCommentLength := 0;
        BinPart.DiskNumberStart := 0;
        BinPart.InternalFileAttributes := 0;
        BinPart.RelativeOffsetOfLocalHeader := 0; // will be filled on saving
        If GetFlagState(fArchiveStructure.Entries[i].Bin.Flags,SCS_FLAG_Directory) then
          begin
            BinPart.CompressionMethod := 0;
            BinPart.CRC32 := 0;
            BinPart.CompressedSize := 0;
            BinPart.UncompressedSize := 0;
            BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_DIRECTORY
          end
        else
          begin
            If GetFlagState(fArchiveStructure.Entries[i].Bin.Flags,SCS_FLAG_Compressed) then
              BinPart.CompressionMethod := 8 {deflate}
            else
              BinPart.CompressionMethod := 0 {store};
            BinPart.CRC32 := fArchiveStructure.Entries[i].Bin.CRC32;
            BinPart.CompressedSize := fArchiveStructure.Entries[i].Bin.CompressedSize;
            BinPart.UncompressedSize := fArchiveStructure.Entries[i].Bin.UncompressedSize;
            BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_ARCHIVE;
          end;        
        FileName := fArchiveStructure.Entries[i].FileName;
        If GetFlagState(fArchiveStructure.Entries[i].Bin.Flags,SCS_FLAG_Directory) and (Length(FileName) > 0) then
          If FileName[Length(FileName)] <> ZIP_PathDelim then
            FileName := FileName + ZIP_PathDelim;
        BinPart.FileNameLength := Length(FileName);
        SetLength(ExtraField,0);
        SetLength(FileComment,0);
      end;
    CopyCentralToLocal(i);
  end;
// fill EOCD
with fZIPArchiveStructure.EndOfCentralDirectory do
  begin
    BinPart.Signature := ZIP_EndOfCentralDirectorySignature;
    BinPart.NumberOfThisDisk := 0;
    BinPart.CentralDirectoryStartDiskNumber := 0;
    BinPart.EntriesOnDisk := 0;           // will be filled on saving
    BinPart.Entries := 0;                 // --//--
    BinPart.CentralDirectorySize := 0;    // --//--
    BinPart.CentralDirectoryOffset := 0;  // --//--
    BinPart.CommentLength := 0;
    SetLength(Comment,0);
  end;
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS_Convert.SCS_ConvertArchive;
var
  ProcessedBytes:       UInt64;
  ConvertArchiveStream: TStream;
  i:                    Integer;
  SCS_Index:            Integer;
  DecompressedBuff:     Pointer;
  DecompressedSize:     Integer;
begin
DoProgress(PROCSTAGEIDX_SCS_EntriesProcessing,0.0);
// create directory for the convert file
{$IFDEF FPC_NonUnicode_NoUTF8RTL}
ForceDirectoriesUTF8(ExtractFileDir(fFileProcessingSettings.Common.TargetPath));
{$ELSE}
ForceDirectories(ExtractFileDir(fFileProcessingSettings.Common.TargetPath));
{$ENDIF}
// create output stream
If fFileProcessingSettings.Common.InMemoryProcessing then
  ConvertArchiveStream := TMemoryStream.Create
else
  ConvertArchiveStream := CreateFileStream(fFileProcessingSettings.Common.TargetPath,fmCreate or fmShareDenyWrite);
try
  // prepare output stream
  If fFileProcessingSettings.Common.InMemoryProcessing then
    ConvertArchiveStream.Size := Trunc(fArchiveStream.Size * 1.1);
  ConvertArchiveStream.Seek(0,soFromBeginning);
  ProcessedBytes := 0;
  // traverse entries and process them...
  For i := Low(fZIPArchiveStructure.Entries) to High(fZIPArchiveStructure.Entries) do
    with fZIPArchiveStructure.Entries[i] do
      try
        SCS_Index := Integer(UtilityData.DataOffset);
        // calculate entry processing progress info
        SCS_PrepareEntryProgressInfo(SCS_Index,ProcessedBytes);
        DoProgress(PROCSTAGEIDX_SCS_EntryProcessing,0.0);
        // prepare buffer that will hold compressed data
        ReallocateMemoryBuffer(fCED_Buffer,LocalHeader.BinPart.CompressedSize);
        // load compressed data from input file
        fArchiveStream.Seek(fArchiveStructure.Entries[SCS_Index].Bin.DataOffset,soFromBeginning);
        ProgressedStreamRead(fArchiveStream,fCED_Buffer.Memory,LocalHeader.BinPart.CompressedSize,PROCSTAGEIDX_SCS_EntryLoading);
        If fArchiveStructure.Entries[SCS_Index].UtilityData.Resolved then
          begin
            // save only entries that have name
            If Length(LocalHeader.FileName) > 0 then
              begin
                // entry is resolved, save it into ZIP output
                // calculate and store new CRC32 if needed
                If (LocalHeader.BinPart.UncompressedSize <> 0) and SameCRC32(LocalHeader.BinPart.CRC32,0) then
                  begin
                   If GetFlagState(fArchiveStructure.Entries[SCS_Index].Bin.Flags,SCS_FLAG_Compressed) then
                      begin
                        // entry is compressed, decompress data
                        ProgressedDecompressBuffer(fCED_Buffer.Memory,LocalHeader.BinPart.CompressedSize,
                          DecompressedBuff,DecompressedSize,PROCSTAGEIDX_SCS_EntryDecompressing,
                        {$IFDEF FPC}
                          {$IFDEF Unicode}
                            UTF8Decode(fArchiveStructure.Entries[SCS_Index].FileName),
                          {$ELSE}
                            fArchiveStructure.Entries[SCS_Index].FileName,
                          {$ENDIF}
                        {$ELSE}
                          String(UTF8ToAnsi(fArchiveStructure.Entries[SCS_Index].FileName)),
                        {$ENDIF}
                          WINDOWBITS_ZLib);
                        try
                          CentralDirectoryHeader.BinPart.CRC32 := BufferCRC32(DecompressedBuff^,DecompressedSize);
                          LocalHeader.BinPart.CRC32 := CentralDirectoryHeader.BinPart.CRC32;
                        finally
                          FreeMem(DecompressedBuff,DecompressedSize);
                        end;
                      end
                    else
                      begin
                        // entry is not compressed, calculate directly
                        CentralDirectoryHeader.BinPart.CRC32 := BufferCRC32(fCED_Buffer.Memory^,LocalHeader.BinPart.CompressedSize);
                        LocalHeader.BinPart.CRC32 := CentralDirectoryHeader.BinPart.CRC32;
                      end;
                  end;
                // current position in output is stored as offset of local header
                CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader := UInt32(ConvertArchiveStream.Position);
                // save local header
                ConvertArchiveStream.WriteBuffer(LocalHeader.BinPart,SizeOf(TZIP_LocalFileHeaderRecord));
                ConvertArchiveStream.WriteBuffer(PAnsiChar(LocalHeader.FileName)^,LocalHeader.BinPart.FileNameLength);
                ConvertArchiveStream.WriteBuffer(PAnsiChar(LocalHeader.ExtraField)^,LocalHeader.BinPart.ExtraFieldLength);
                // save entry data (only for files)
                If not GetFlagState(fArchiveStructure.Entries[SCS_Index].Bin.Flags,SCS_FLAG_Directory) then
                  begin
                    If GetFlagState(fArchiveStructure.Entries[SCS_Index].Bin.Flags,SCS_FLAG_Compressed) then
                      begin
                      {
                        Entry is a compressed ZLib stream.                        
                        Header (2 bytes) and footer (4 bytes) of ZLib stream is
                        removed to create pure deflate stream.
                        We are assuming there is no dictionary ID stored.
                      }
                        If LocalHeader.BinPart.CompressedSize >= 6 then
                          ProgressedStreamWrite(ConvertArchiveStream,
                            {%H-}Pointer({%H-}PtrUInt(fCED_Buffer.Memory) + 2),
                            LocalHeader.BinPart.CompressedSize - 6,
                            PROCSTAGEIDX_SCS_EntrySaving)
                        else
                          DoError(301,'Entry is too small (%d) to be valid.',[LocalHeader.BinPart.CompressedSize]);
                      end
                    else ProgressedStreamWrite(ConvertArchiveStream,fCED_Buffer.Memory,LocalHeader.BinPart.CompressedSize,PROCSTAGEIDX_SCS_EntrySaving)
                  end;
                // if we are here, the entry can be assumed to be ok
                UtilityData.Erroneous := False;
              end
            // entry does not have name, remove it from output  
            else UtilityData.Erroneous := True;
          end
        else
          begin
            // entry is not resolved, extract it if required
            UtilityData.Erroneous := True;
            If fProcessingSettings.PathResolve.ExtractedUnresolvedEntries then
              begin
                If GetFlagState(fArchiveStructure.Entries[SCS_Index].Bin.Flags,SCS_FLAG_Compressed) then
                  begin
                    // entry is compressed, decompress data
                    ProgressedDecompressBuffer(fCED_Buffer.Memory,LocalHeader.BinPart.CompressedSize,
                      DecompressedBuff,DecompressedSize,PROCSTAGEIDX_SCS_EntryDecompressing,
                    {$IFDEF FPC}
                      {$IFDEF Unicode}
                        UTF8Decode(fArchiveStructure.Entries[SCS_Index].FileName),
                      {$ELSE}
                        fArchiveStructure.Entries[SCS_Index].FileName,
                      {$ENDIF}
                    {$ELSE}
                      String(UTF8ToAnsi(fArchiveStructure.Entries[SCS_Index].FileName)),
                    {$ENDIF}
                      WINDOWBITS_ZLib);
                    try
                      SCS_SaveUnresolvedEntry(SCS_Index,DecompressedBuff,DecompressedSize);
                    finally
                      FreeMem(DecompressedBuff,DecompressedSize);
                    end;
                  end
                // entry is not compressed, save data
                else SCS_SaveUnresolvedEntry(SCS_Index,fCED_Buffer.Memory,LocalHeader.BinPart.CompressedSize);
              end
            else DoWarning(Format('File name of entry #%d (0x%.16x) could not be resolved, it will be dropped.',[SCS_Index,fArchiveStructure.Entries[SCS_Index].Bin.Hash]));
          end;
        If not GetFlagState(fArchiveStructure.Entries[SCS_Index].Bin.Flags,SCS_FLAG_Directory) then
          Inc(ProcessedBytes,LocalHeader.BinPart.CompressedSize);
        DoProgress(PROCSTAGEIDX_SCS_EntryProcessing,1.0);
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
  fZIPArchiveStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset := UInt32(ConvertArchiveStream.Position);
  // write central directory
  For i := Low(fZIPArchiveStructure.Entries) to High(fZIPArchiveStructure.Entries) do
    begin
      If not fZIPArchiveStructure.Entries[i].UtilityData.Erroneous then
        begin
          with fZIPArchiveStructure.Entries[i].CentralDirectoryHeader do
            begin
              ConvertArchiveStream.WriteBuffer(BinPart,SizeOf(TZIP_CentralDirectoryFileHeaderRecord));
              ConvertArchiveStream.WriteBuffer(PAnsiChar(FileName)^,BinPart.FileNameLength);
              ConvertArchiveStream.WriteBuffer(PAnsiChar(ExtraField)^,BinPart.ExtraFieldLength);
              ConvertArchiveStream.WriteBuffer(PAnsiChar(FileComment)^,BinPart.FileCommentLength);
            end;
          Inc(fZIPArchiveStructure.EndOfCentralDirectory.BinPart.Entries);
        end;
      DoProgress(PROCSTAGEIDX_NoProgress,0.0);
    end;
  // write end of central directory
  with fZIPArchiveStructure.EndOfCentralDirectory do
    begin
      // get size of central directory (current position - CD offset)
      BinPart.CentralDirectorySize := UInt32(ConvertArchiveStream.Position) - BinPart.CentralDirectoryOffset;
      BinPart.EntriesOnDisk := BinPart.Entries;
      ConvertArchiveStream.WriteBuffer(BinPart,SizeOf(TZIP_EndOfCentralDirectoryRecord));
      ConvertArchiveStream.WriteBuffer(PAnsiChar(Comment)^,BinPart.CommentLength);
    end;
  // finalize
  ConvertArchiveStream.Size := ConvertArchiveStream.Position;
  DoProgress(PROCSTAGEIDX_SCS_EntriesProcessing,1.0);
  If fFileProcessingSettings.Common.InMemoryProcessing then
    ProgressedSaveFile(fFileProcessingSettings.Common.TargetPath,ConvertArchiveStream,PROCSTAGEIDX_Saving);
finally
  ConvertArchiveStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS_Convert.ArchiveProcessing;
begin
inherited;
SCS_PrepareConversion;
SCS_SortConvertEntries;
SCS_ConvertArchive;
end;

{------------------------------------------------------------------------------}
{   TRepairer_SCS_Convert - public methods                                     }
{------------------------------------------------------------------------------}

class Function TRepairer_SCS_Convert.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  300:  Result := 'SCS_SortConvertEntries.QuickSort.ExchangeEntries';
  301:  Result := 'SCS_ConvertArchive';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

end.
