unit DART_Repairer_SCS_Rebuild;

{$INCLUDE DART_defs.inc}

interface

uses
  Classes,
  DART_Repairer_SCS;

type
  TDARTRepairer_SCS_Rebuild = class(TDARTRepairer_SCS_ProcessingBase)
  protected
    fRebuildArchiveStream:  TStream;
    procedure ArchiveProcessing; override;
    // scs-rebuilding specific methods
    procedure SCS_RebuildArchive; virtual;
    procedure SCS_WriteHeaderAndEntryTable(Placeholder: Boolean); virtual;
    procedure SCS_WriteEntry(Index: Integer); virtual;
  end;

implementation

uses
  SysUtils,
  AuxTypes, StrRect, BitOps, CRC32, MemoryBuffer, StaticMemoryStream,
  ExplicitStringListsBase, ExplicitStringLists, ZLibCommon,
  DART_Auxiliary, DART_Common, DART_Format_SCS, DART_Repairer;

procedure TDARTRepairer_SCS_Rebuild.ArchiveProcessing;
begin
inherited;
SCS_RebuildArchive;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS_Rebuild.SCS_RebuildArchive;
var
  i:  Integer;
begin
DoProgress(fProcessingProgNode,PSIDX_C_EntriesProcessing,0.0);
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
  // reserve place for entry table
  SCS_WriteHeaderAndEntryTable(True);
  // save individual entries
  For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
    SCS_WriteEntry(i);
  fRebuildArchiveStream.Size := fRebuildArchiveStream.Position;
  // write header and entry table
  fRebuildArchiveStream.Seek(0,soBeginning);
  SCS_WriteHeaderAndEntryTable(False);
  // finalize
  DoProgress(fProcessingProgNode,PSIDX_C_EntriesProcessing,1.0);
  If fArchiveProcessingSettings.Common.InMemoryProcessing then
    ProgressedSaveFile(fArchiveProcessingSettings.Common.TargetPath,fRebuildArchiveStream,
                       ProgressStageInfo(fProgressTracker,DART_PROGSTAGE_IDX_Saving));
finally
  fRebuildArchiveStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS_Rebuild.SCS_WriteHeaderAndEntryTable(Placeholder: Boolean);
var
  EntryTableSize: TMemSize;
  EntryTable:     TWritableStaticMemoryStream;

  Function WriteEntriesToStream(Stream: TStream): Integer;
  var
    i:  Integer;
  begin
    Result := fArchiveStructure.Entries.Count;
    For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
      If not fArchiveStructure.Entries.Arr[i].UtilityData.Erroneous then
        Stream.WriteBuffer(fArchiveStructure.Entries.Arr[i].BinPart,SizeOf(TDART_SCS_EntryRecord))
      else
        Dec(Result);
  end;

begin
EntryTableSize := TMemSize(fArchiveStructure.Entries.Count * SizeOf(TDART_SCS_EntryRecord));
If Placeholder then
  begin
    // header and entry table is not actually written, instead write appropriate number of zeroes
    ReallocBufferKeep(fBuffer_Entry,DART_SCS_DefaultEntryTableOffset + EntryTableSize);
    FillChar(fBuffer_Entry.Memory^,DART_SCS_DefaultEntryTableOffset + EntryTableSize,0);
    fRebuildArchiveStream.WriteBuffer(fBuffer_Entry.Memory^,DART_SCS_DefaultEntryTableOffset + EntryTableSize);
  end
else
  begin
    // write actual header and entry table
    fRebuildArchiveStream.Seek(DART_SCS_DefaultEntryTableOffset,soBeginning);
    If fProcessingSettings.EntryTabInMem and not fArchiveProcessingSettings.Common.InMemoryProcessing then
      begin
        // build entire entry table in memory and write it as one block
        ReallocBufferKeep(fBuffer_Entry,EntryTableSize);
        FillChar(fBuffer_Entry.Memory^,EntryTableSize,0);
        EntryTable := TWritableStaticMemoryStream.Create(fBuffer_Entry.Memory,EntryTableSize);
        try
          EntryTable.Seek(0,soBeginning);
          fArchiveStructure.ArchiveHeader.EntryCount := WriteEntriesToStream(EntryTable);
          fRebuildArchiveStream.WriteBuffer(fBuffer_Entry.Memory^,EntryTableSize);
        finally
          EntryTable.Free;
        end;
      end
    else fArchiveStructure.ArchiveHeader.EntryCount := WriteEntriesToStream(fRebuildArchiveStream);
    // fill header with corrected data (erroneous entries discarded)...
    If fArchiveProcessingSettings.Common.IgnoreArchiveSignature then
      fArchiveStructure.ArchiveHeader.Signature := DART_SCS_ArchiveSignature;
    fArchiveStructure.ArchiveHeader.Unknown := 1;
    If fProcessingSettings.PathResolve.AssumeCityHash then
      fArchiveStructure.ArchiveHeader.HashType := DART_SCS_HASH_City;
  //fArchiveStructure.ArchiveHeader.EntryCount := ;   // was set earlier
    fArchiveStructure.ArchiveHeader.EntryTableOffset := DART_SCS_DefaultEntryTableOffset;
    fArchiveStructure.ArchiveHeader.UnknownOffset := DART_SCS_DefaultUnknownOffset;
    // ...and write it
    fRebuildArchiveStream.Seek(0,soBeginning);
    fRebuildArchiveStream.WriteBuffer(fArchiveStructure.ArchiveHeader,SizeOf(TDART_SCS_ArchiveHeader));
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS_Rebuild.SCS_WriteEntry(Index: Integer);
var
  i:                Integer;
  DirBuffer:        TMemoryStream;
  CompressedBuff:   Pointer;
  CompressedSize:   TMemSize;
  DecompressedBuff: Pointer;
  DecompressedSize: TMemSize;
begin
with fArchiveStructure.Entries.Arr[Index] do
try
  // prepare progress
  fEntryProcessingProgNode := fEntriesProcessingProgNode.StageObjects[Index];
  DoProgress(fEntriesProcessingProgNode,Index,0.0);
  If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) and UtilityData.Resolved then
    begin
      // entry is a resolved directory
      // build the entry data
      DirBuffer := TMemoryStream.Create;
      try
        with TAnsiStringList.Create do
        try
          TrailingLineBreak := False;
          LineBreakStyle := lbsLF;
          AddStringsDef(UtilityData.DirContent);
          SaveToStream(DirBuffer);
        finally
          Free;
        end;
        // fill missing info
        BinPart.DataOffset := UInt64(fRebuildArchiveStream.Position);
        BinPart.CRC32 := StreamCRC32(DirBuffer);
        BinPart.UncompressedSize := UInt32(DirBuffer.Size);
        If BinPart.UncompressedSize > DART_SCS_MaxUncompDirEntrySize then
          begin
            ProgressedCompressBuffer(DirBuffer.Memory,DirBuffer.Size,CompressedBuff,CompressedSize,WBITS_ZLIB,
              ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntryDecompression));
            try
              BinPart.CompressedSize := UInt32(CompressedSize) ;
              SetFlagValue(BinPart.Flags,DART_SCS_FLAG_Compressed);
              ProgressedStreamWrite(fRebuildArchiveStream,CompressedBuff,CompressedSize,
                ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntrySaving));
            finally
              FreeMem(CompressedBuff,CompressedSize);
            end;
          end
        else
          begin
            BinPart.CompressedSize := BinPart.UncompressedSize;
            ResetFlagValue(BinPart.Flags,DART_SCS_FLAG_Compressed);
            ProgressedStreamWrite(fRebuildArchiveStream,DirBuffer.Memory,DirBuffer.Size,
              ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntrySaving));
          end;
      finally
        DirBuffer.Free;
      end;
    end
  else
    begin
      // entry is a file or unresolved directory
      // preserve original data offset (not really needed, but better to be sure)
      UtilityData.DataOffset := BinPart.DataOffset;
      // prepare buffer that will hold compressed data
      ReallocBufferKeep(fBuffer_Entry,BinPart.CompressedSize);
      // load compressed data from input
      fInputArchiveStream.Seek(BinPart.DataOffset,soBeginning);
      ProgressedStreamRead(fInputArchiveStream,fBuffer_Entry.Memory,BinPart.CompressedSize,
        ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntryLoading));
      If ((BinPart.UncompressedSize <> 0) and SameCRC32(BinPart.CRC32,0)) or not UtilityData.Resolved then
        begin
          // a new CRC32 checksum is required or the entry is not resolved (might need extraction)
          If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed) then
            begin
              // data needs to be decompressed for CRC32 calculation
              ProgressedDecompressBuffer(fBuffer_Entry.Memory,BinPart.CompressedSize,DecompressedBuff,DecompressedSize,WBITS_ZLIB,
                ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntryDecompression));
              try
                If not UtilityData.Resolved then
                  SCS_SaveEntryAsUnresolved(i,DecompressedBuff,DecompressedSize,
                    ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntrySaving));
                If (BinPart.UncompressedSize <> 0) and SameCRC32(BinPart.CRC32,0) then
                  BinPart.CRC32 := BufferCRC32(DecompressedBuff^,DecompressedSize);
              finally
                FreeMem(DecompressedBuff,DecompressedSize);
              end;
            end
          else
            begin
              If not UtilityData.Resolved then
                SCS_SaveEntryAsUnresolved(Index,fBuffer_Entry.Memory,BinPart.CompressedSize,
                  ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntrySaving));
              If (BinPart.UncompressedSize <> 0) and SameCRC32(BinPart.CRC32,0) then
                BinPart.CRC32 := BufferCRC32(fBuffer_Entry.Memory^,BinPart.CompressedSize);
            end;
        end;
      // new data offset is set to current position in the output stream
      BinPart.DataOffset := UInt64(fRebuildArchiveStream.Position);
      // bin part does not need any more changes, save the data to output
      ProgressedStreamWrite(fRebuildArchiveStream,fBuffer_Entry.Memory,BinPart.CompressedSize,
        ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_SCS_EntrySaving));
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

end.
