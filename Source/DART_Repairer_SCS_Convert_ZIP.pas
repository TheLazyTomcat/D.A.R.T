{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_SCS_Convert_ZIP;

{$INCLUDE DART_defs.inc}

interface

uses
  DART_Format_ZIP, DART_Repairer_SCS_Convert;

{===============================================================================
--------------------------------------------------------------------------------
                         TDARTRepairer_SCS_Convert_ZIP                         
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_SCS_Convert_ZIP - class declaration
===============================================================================}
type
  TDARTRepairer_SCS_Convert_ZIP = class(TDARTRepairer_SCS_Convert)
  protected
    fZIPArchiveStructure: TDART_ZIP_ArchiveStructure;
    procedure ConvertArchiveStructure; override;
    procedure ConvertArchive; override;
    // conversion-specific methods
    procedure SCS_Conv_ZIP_SortConvertedEntries; virtual;
    procedure SCS_Conv_ZIP_WriteConvertedEntry(Index: Integer); virtual;
    procedure SCS_Conv_ZIP_WriteCentralDirectory; virtual;
    procedure SCS_Conv_ZIP_WriteEndOfCentralDirectory; virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
  end;

implementation

uses
  Windows, SysUtils, Classes,
  AuxTypes, BitOps, CRC32, StrRect, MemoryBuffer, ZLibCommon,
  DART_Auxiliary, DART_Common, DART_Format_SCS, DART_Repairer, DART_Repairer_SCS;

{===============================================================================
--------------------------------------------------------------------------------
                         TDARTRepairer_SCS_Convert_ZIP
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_SCS_Convert_ZIP - method indexing constants
===============================================================================}

const
  DART_METHOD_ID_SCS_CONV_ZIP_SCEQSEE = $01030200;
  DART_METHOD_ID_SCS_CONV_ZIP_WRCNVEN = $01030201;

{===============================================================================
    TDARTRepairer_SCS_Convert_ZIP - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTRepairer_SCS_Convert_ZIP - protected methods
-------------------------------------------------------------------------------}

procedure TDARTRepairer_SCS_Convert_ZIP.ConvertArchiveStructure;
var
  i:  Integer;

  // copy data from central directory headers to local headers
  procedure CopyCentralToLocal(Index: Integer);
  begin
    with fZIPArchiveStructure.Entries.Arr[Index] do
      begin
        LocalHeader.BinPart.Signature := DART_ZIP_LocalFileHeaderSignature;
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
DoProgress(fProcessingProgNode,PSIDX_C_EntriesConverting,0.0);
SetLength(fZIPArchiveStructure.Entries.Arr,fArchiveStructure.Entries.Count);
fZIPArchiveStructure.Entries.Count := fArchiveStructure.Entries.Count;
// process entries, ignore whether the entry is resolved or not for now
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  begin
    {
      Converted etries gets sorted which will break their index entanglement
      with original SCS entries - preserve the index.
    }
    fZIPArchiveStructure.Entries.Arr[i].UtilityData.Index := i;
    // prepare offset of data in input archive
    fZIPArchiveStructure.Entries.Arr[i].UtilityData.DataOffset :=
      Int64(fArchiveStructure.Entries.Arr[i].BinPart.DataOffset);
    // fill central directory
    with fZIPArchiveStructure.Entries.Arr[i].CentralDirectoryHeader do
      begin
        BinPart.Signature := DART_ZIP_CentralDirectoryFileHeaderSignature;
        BinPart.VersionMadeBy := DART_ZIP_DefVersionMadeBy;
        BinPart.HostOS := DART_ZIP_DefHostOS;
        BinPart.VersionNeededToExtract := DART_ZIP_DefVersionMadeBy;
        BinPart.OSNeededForExtraction := DART_ZIP_DefHostOS;
        BinPart.GeneralPurposeBitFlag := 0;
        BinPart.LastModFileTime := DateTimeToFileDate(Now) and $FFFF;
        BinPart.LastModFileDate := DateTimeToFileDate(Now) shr 16;
        BinPart.ExtraFieldLength := 0;
        BinPart.FileCommentLength := 0;
        BinPart.DiskNumberStart := 0;
        BinPart.InternalFileAttributes := 0;
        BinPart.RelativeOffsetOfLocalHeader := 0; // will be filled when saving the data
        If GetFlagState(fArchiveStructure.Entries.Arr[i].BinPart.Flags,DART_SCS_FLAG_Directory) then
          begin
            // entry is a directory
            BinPart.CompressionMethod := DART_ZCM_Store;
            BinPart.CRC32 := InitialCRC32;
            BinPart.CompressedSize := 0;
            BinPart.UncompressedSize := 0;
            BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
          end
        else
          begin
            // entry is a file
            If GetFlagState(fArchiveStructure.Entries.Arr[i].BinPart.Flags,DART_SCS_FLAG_Compressed) then
              BinPart.CompressionMethod := DART_ZCM_Deflate
            else
              BinPart.CompressionMethod := DART_ZCM_Store;
            BinPart.CRC32 := fArchiveStructure.Entries.Arr[i].BinPart.CRC32;
            BinPart.CompressedSize := fArchiveStructure.Entries.Arr[i].BinPart.CompressedSize;
            BinPart.UncompressedSize := fArchiveStructure.Entries.Arr[i].BinPart.UncompressedSize;
            BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_ARCHIVE;
          end;        
        FileName := fArchiveStructure.Entries.Arr[i].FileName;
        // include trailing path delimiter for directories
        If GetFlagState(fArchiveStructure.Entries.Arr[i].BinPart.Flags,DART_SCS_FLAG_Directory) and (Length(FileName) > 0) then
          If FileName[Length(FileName)] <> DART_ZIP_PathDelim then
            FileName := FileName + DART_ZIP_PathDelim;
        BinPart.FileNameLength := Length(FileName);
        SetLength(ExtraField,0);
        SetLength(FileComment,0);
      end;
    // fill local file headers
    CopyCentralToLocal(i);
    DoProgress(fProcessingProgNode,PSIDX_C_EntriesConverting,(i + 1) / fArchiveStructure.Entries.Count);
  end;
// fill end of central directory
with fZIPArchiveStructure.EndOfCentralDirectory do
  begin
    BinPart.Signature := DART_ZIP_EndOfCentralDirectorySignature;
    BinPart.NumberOfThisDisk := 0;
    BinPart.CentralDirectoryStartDiskNumber := 0;
    BinPart.EntriesOnDisk := 0;           // will be filled on saving
    BinPart.Entries := 0;                 // --//--
    BinPart.CentralDirectorySize := 0;    // --//--
    BinPart.CentralDirectoryOffset := 0;  // --//--
    BinPart.CommentLength := 0;
    SetLength(Comment,0);
  end;
// sort converted entries
SCS_Conv_ZIP_SortConvertedEntries;
DoProgress(fProcessingProgNode,PSIDX_C_EntriesConverting,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS_Convert_ZIP.ConvertArchive;
var
  i:  Integer;
begin
DoProgress(fProcessingProgNode,PSIDX_C_EntriesProcessing,0.0);
// create directory where the converted archive will be stored
DART_ForceDirectories(ExtractFileDir(fArchiveProcessingSettings.Common.TargetPath));
// create output stream
If fArchiveProcessingSettings.Common.InMemoryProcessing then
  fConvertedArchiveStream := TMemoryStream.Create
else
  fConvertedArchiveStream := TFileStream.Create(StrToRTL(fArchiveProcessingSettings.Common.TargetPath),fmCreate or fmShareDenyWrite);
try
  // prepare output stream
  If fArchiveProcessingSettings.Common.InMemoryProcessing then
    fConvertedArchiveStream.Size := Trunc(fInputArchiveStream.Size * 1.1);
  fConvertedArchiveStream.Seek(0,soBeginning);
  // traverse entries and save them
  For i := Low(fZIPArchiveStructure.Entries.Arr) to Pred(fZIPArchiveStructure.Entries.Count) do
    SCS_Conv_ZIP_WriteConvertedEntry(i);
  // current position in output is where the central directory starts
  fZIPArchiveStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset := UInt32(fConvertedArchiveStream.Position);
  // write central directory
  SCS_Conv_ZIP_WriteCentralDirectory;
  // write end of central directory
  SCS_Conv_ZIP_WriteEndOfCentralDirectory;
  // finalize
  fConvertedArchiveStream.Size := fConvertedArchiveStream.Position;
  DoProgress(fProcessingProgNode,PSIDX_C_EntriesProcessing,1.0);  
  If fArchiveProcessingSettings.Common.InMemoryProcessing then
    ProgressedSaveFile(fArchiveProcessingSettings.Common.TargetPath,fConvertedArchiveStream,
                       ProgressStageInfo(fProgressTracker,DART_PROGSTAGE_IDX_Saving));
finally
  fConvertedArchiveStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS_Convert_ZIP.SCS_Conv_ZIP_SortConvertedEntries;
// sort converted entries alphabetically

  procedure QuickSort(LeftIdx,RightIdx: Integer);
  var
    Pivot:  AnsiString;
    Idx,i:  Integer;

    procedure ExchangeEntries(Idx1,Idx2: Integer);
    var
      TempEntry:  TDART_ZIP_Entry;
    begin
      If (Idx1 <> Idx2) then
        begin
          If (Idx1 < Low(fZIPArchiveStructure.Entries.Arr)) or (Idx1 >= fZIPArchiveStructure.Entries.Count) then
            DoError(DART_METHOD_ID_SCS_CONV_ZIP_SCEQSEE,'Index 1 (%d) out of bounds.',[Idx1]);
          If (Idx2 < Low(fZIPArchiveStructure.Entries.Arr)) or (Idx2 >= fZIPArchiveStructure.Entries.Count) then
            DoError(DART_METHOD_ID_SCS_CONV_ZIP_SCEQSEE,'Index 2 (%d) out of bounds.',[Idx1]);
          TempEntry := fZIPArchiveStructure.Entries.Arr[Idx1];
          fZIPArchiveStructure.Entries.Arr[Idx1] := fZIPArchiveStructure.Entries.Arr[Idx2];
          fZIPArchiveStructure.Entries.Arr[Idx2] := TempEntry;
        end;
    end;

  begin
    DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0);
    If LeftIdx < RightIdx then
      begin
        ExchangeEntries((LeftIdx + RightIdx) shr 1,RightIdx);
        Pivot := fZIPArchiveStructure.Entries.Arr[RightIdx].CentralDirectoryHeader.FileName;
        Idx := LeftIdx;
        For i := LeftIdx to Pred(RightIdx) do
          If AnsiCompareText(Pivot,fZIPArchiveStructure.Entries.Arr[i].CentralDirectoryHeader.FileName) > 0 then
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
If fZIPArchiveStructure.Entries.Count > 1 then
  QuickSort(Low(fZIPArchiveStructure.Entries.Arr),Pred(fZIPArchiveStructure.Entries.Count));
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS_Convert_ZIP.SCS_Conv_ZIP_WriteConvertedEntry(Index: Integer);
var
  SCS_Index:        Integer;
  DecompressedBuff: Pointer;
  DecompressedSize: TMemSize;

  procedure ManageCompressedEntryData;
  begin
  {
    Entry is a compressed ZLib stream.
    Header (2 bytes) and footer (4 bytes) of ZLib stream is removed to create
    a pure deflate stream.
  }
    with fZIPArchiveStructure.Entries.Arr[Index] do
      If LocalHeader.BinPart.CompressedSize >= 6 then
        begin
          // check FDICT flag of zlib stream
          If BT(PUInt16(fBuffer_Entry.Memory)^,13) then
            begin
              If LocalHeader.BinPart.CompressedSize >= 10 then
                begin
                  // dictionary ID is stored in the compressed stream
                  If fProcessingSettings.Entry.IgnoreDictionaryID then
                    begin
                      DoWarning(Format('Entry #%d compressed data stream contains a dictionary ID (0x%.8x).',
                                       [Index,{%H-}PUInt32({%H-}PtrUInt(fBuffer_Entry.Memory) + 2)^]));
                      ProgressedStreamWrite(fConvertedArchiveStream,{%H-}Pointer({%H-}PtrUInt(fBuffer_Entry.Memory) + 6),
                        LocalHeader.BinPart.CompressedSize - 10,ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntrySaving));
                    end
                  else DoError(DART_METHOD_ID_SCS_CONV_ZIP_WRCNVEN,'Entry #%d compressed data stream contains a dictionary ID (0x%.8x).',
                               [Index,{%H-}PUInt32({%H-}PtrUInt(fBuffer_Entry.Memory) + 2)^]);
                end
              else DoError(DART_METHOD_ID_SCS_CONV_ZIP_WRCNVEN,'Entry #%d is too small (%d) to contain a valid zlib dictionary ID.',
                           [Index,LocalHeader.BinPart.CompressedSize]);
            end
          // dictionary ID is not present
          else ProgressedStreamWrite(fConvertedArchiveStream,{%H-}Pointer({%H-}PtrUInt(fBuffer_Entry.Memory) + 2),
                 LocalHeader.BinPart.CompressedSize - 6,ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntrySaving));
        end
      else DoError(DART_METHOD_ID_SCS_CONV_ZIP_WRCNVEN,'Entry #%d is too small (%d) to be a valid zlib stream.',
                   [Index,LocalHeader.BinPart.CompressedSize]);
  end;

begin
with fZIPArchiveStructure.Entries.Arr[Index] do
try
  // prepare progress
  fEntryProcessingProgNode := fEntriesProcessingProgNode.StageObjects[Index];
  DoProgress(fEntriesProcessingProgNode,Index,0.0);
  // prepare index of corresponding entry in input SCS# archive
  SCS_Index := UtilityData.Index;

  // prepare buffer that will hold compressed data
  ReallocBufferKeep(fBuffer_Entry,LocalHeader.BinPart.CompressedSize);
  // load compressed data from input archive
  fInputArchiveStream.Seek(UtilityData.DataOffset,soBeginning);
  ProgressedStreamRead(fInputArchiveStream,fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,
    ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntryLoading));

  If fArchiveStructure.Entries.Arr[SCS_Index].UtilityData.Resolved then
    begin
      // entry seems to have been resolved
      // save only entries that have a name
      If Length(LocalHeader.FileName) > 0 then
        begin
          // entry is resolved, save it into ZIP output
          // calculate and store new CRC32 if needed
          If (LocalHeader.BinPart.UncompressedSize <> 0) and SameCRC32(LocalHeader.BinPart.CRC32,0) then
            begin
              If GetFlagState(fArchiveStructure.Entries.Arr[SCS_Index].BinPart.Flags,DART_SCS_FLAG_Compressed) then
                begin
                  // entry is compressed, decompress data
                  ProgressedDecompressBuffer(fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,DecompressedBuff,DecompressedSize,WBITS_ZLIB,
                    ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntryDecompression));
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
                  CentralDirectoryHeader.BinPart.CRC32 := BufferCRC32(fBuffer_Entry.Memory^,LocalHeader.BinPart.CompressedSize);
                  LocalHeader.BinPart.CRC32 := CentralDirectoryHeader.BinPart.CRC32;
                end;
            end;
          // current position in output is stored as offset of local header
          CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader := UInt32(fConvertedArchiveStream.Position);
          // save local header
          fConvertedArchiveStream.WriteBuffer(LocalHeader.BinPart,SizeOf(TDART_ZIP_LocalFileHeaderRecord));
          fConvertedArchiveStream.WriteBuffer(PAnsiChar(LocalHeader.FileName)^,LocalHeader.BinPart.FileNameLength);
          fConvertedArchiveStream.WriteBuffer(PAnsiChar(LocalHeader.ExtraField)^,LocalHeader.BinPart.ExtraFieldLength);
          // save entry data (only files)
          If not GetFlagState(fArchiveStructure.Entries.Arr[SCS_Index].BinPart.Flags,DART_SCS_FLAG_Directory) then
            begin
              If not GetFlagState(fArchiveStructure.Entries.Arr[SCS_Index].BinPart.Flags,DART_SCS_FLAG_Compressed) then
                // data are not compressed, save them directly
                ProgressedStreamWrite(fConvertedArchiveStream,fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,
                  ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntrySaving))
              else
                ManageCompressedEntryData;
            end;
        end
      else
        begin
          // entry does not have a name, mark it as erroneous and remove it from output
          UtilityData.Erroneous := True;
        {
          The warning is disabled because entry with no name is present in every SCS# archive - the root.
          It stays here for completeness and future reference.
        }
          //DoWarning(Format('Converted entry #%d does not have a name, it will be dropped.',[Index]));
        end;
    end
  else
    begin
      // entry was not resolved, mark it as erroneous...
      UtilityData.Erroneous := True;
      // ...and extract it if required
      If fProcessingSettings.PathResolve.ExtractedUnresolvedEntries then
        begin
          If GetFlagState(fArchiveStructure.Entries.Arr[SCS_Index].BinPart.Flags,DART_SCS_FLAG_Compressed) then
            begin
              // entry is compressed, decompress data
              ProgressedDecompressBuffer(fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,DecompressedBuff,DecompressedSize,WBITS_ZLIB,
                ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntryDecompression));
              try
                SCS_SaveEntryAsUnresolved(SCS_Index,DecompressedBuff,DecompressedSize,
                  ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntrySaving));
              finally
                FreeMem(DecompressedBuff,DecompressedSize);
              end;
            end
          // entry is not compressed, save data
          else SCS_SaveEntryAsUnresolved(SCS_Index,fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,
                 ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntrySaving));
        end
      else DoWarning(Format('File name of entry #%d (0x%.16x) could not be resolved, it will be dropped.',
                            [SCS_Index,fArchiveStructure.Entries.Arr[SCS_Index].BinPart.Hash]));
    end;

  DoProgress(fEntriesProcessingProgNode,Index,1.0);
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

procedure TDARTRepairer_SCS_Convert_ZIP.SCS_Conv_ZIP_WriteCentralDirectory;
var
  i:  Integer;
begin
For i := Low(fZIPArchiveStructure.Entries.Arr) to Pred(fZIPArchiveStructure.Entries.Count) do
  begin
    If not fZIPArchiveStructure.Entries.Arr[i].UtilityData.Erroneous then
      begin
        with fZIPArchiveStructure.Entries.Arr[i].CentralDirectoryHeader do
          begin
            fConvertedArchiveStream.WriteBuffer(BinPart,SizeOf(TDART_ZIP_CentralDirectoryFileHeaderRecord));
            fConvertedArchiveStream.WriteBuffer(PAnsiChar(FileName)^,BinPart.FileNameLength);
            fConvertedArchiveStream.WriteBuffer(PAnsiChar(ExtraField)^,BinPart.ExtraFieldLength);
            fConvertedArchiveStream.WriteBuffer(PAnsiChar(FileComment)^,BinPart.FileCommentLength);
          end;
        Inc(fZIPArchiveStructure.EndOfCentralDirectory.BinPart.Entries);
      end;
    DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0);
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS_Convert_ZIP.SCS_Conv_ZIP_WriteEndOfCentralDirectory;
begin
with fZIPArchiveStructure.EndOfCentralDirectory do
  begin
    // use difference between stored central directory offset and current position as a central directory size
    BinPart.CentralDirectorySize := UInt32(fConvertedArchiveStream.Position) - BinPart.CentralDirectoryOffset;
    BinPart.EntriesOnDisk := BinPart.Entries;
    fConvertedArchiveStream.WriteBuffer(BinPart,SizeOf(TDART_ZIP_EndOfCentralDirectoryRecord));
    fConvertedArchiveStream.WriteBuffer(PAnsiChar(Comment)^,BinPart.CommentLength);
  end;
end;

{-------------------------------------------------------------------------------
    TDARTRepairer_SCS_Convert_ZIP - public methods
-------------------------------------------------------------------------------}

class Function TDARTRepairer_SCS_Convert_ZIP.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  DART_METHOD_ID_SCS_CONV_ZIP_SCEQSEE:  Result := 'SCS_Conv_ZIP_SortConvertedEntries.QuickSort.ExchangeEntries';
  DART_METHOD_ID_SCS_CONV_ZIP_WRCNVEN:  Result := 'SCS_Conv_ZIP_WriteConvertedEntry';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

end.
