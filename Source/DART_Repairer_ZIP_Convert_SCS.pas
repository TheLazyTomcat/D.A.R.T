unit DART_Repairer_ZIP_Convert_SCS;

{$INCLUDE DART_defs.inc}

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                                    WARNING

    implementation of TDARTRepairer_ZIP_Convert_SCS is completely untested

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

interface

uses
  AuxTypes, ProgressTracker,
  DART_Format_SCS, DART_Repairer_ZIP_Convert;

var
  // progress stages
  DART_PROGSTAGE_IDX_ZIP_CONV_SCS_EntriesCopy:     Integer = -1;
  DART_PROGSTAGE_IDX_ZIP_CONV_SCS_DirsReconstruct: Integer = -1;
  DART_PROGSTAGE_IDX_ZIP_CONV_SCS_ProgressReset:   Integer = -1;

  PSIDX_Z_EntriesCopy:     Integer = -1;
  PSIDX_Z_DirsReconstruct: Integer = -1;
  PSIDX_Z_ProgressReset:   Integer = -1;

type
  TDARTRepairer_ZIP_Convert_SCS = class(TDARTRepairer_ZIP_Convert)
  protected
    fSCSArchiveStructure:       TDART_SCS_ArchiveStructure;
    fEntriesConvertingProgNode: TProgressTracker;
    // initialization methods
    procedure InitializeProgress; override;
    // conversion methods
    procedure ConvertArchiveStructure; override;
    procedure ConvertArchive; override;
    // conversion-specific methods
    procedure ZIP_Conv_SCS_InitializeConvertedArchive; virtual;
    Function ZIP_Conv_SCS_EntryFileNameHash(const EntryFileName: AnsiString): UInt64; virtual;
    Function ZIP_Conv_SCS_HashCompare(A,B: UInt64): Integer; virtual;
    procedure ZIP_Conv_SCS_SortConvertedEntries; virtual;
    procedure ZIP_Conv_SCS_ReconstructDirectories; virtual;
    procedure ZIP_Conv_SCS_ResetProgress; virtual;
    procedure ZIP_Conv_SCS_WriteHeaderAndEntryTable(Placeholder: Boolean); virtual;
    procedure ZIP_Conv_SCS_WriteEntry(Index: Integer); virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;    
  end;

implementation

uses
  Windows, SysUtils, Classes, StrUtils,
  City, StrRect, MemoryBuffer, ExplicitStringListsBase, ExplicitStringLists,
  BitOps, CRC32, ZLibCommon,
  DART_Auxiliary, DART_Common, DART_PathDeconstructor, DART_Format_ZIP,
  DART_Repairer, DART_Repairer_ZIP;

const
  DART_METHOD_ID_ZIP_CONV_SCS_ENFNHSH = $01030100;
  DART_METHOD_ID_ZIP_CONV_SCS_SCEQSEE = $01030101;

procedure TDARTRepairer_ZIP_Convert_SCS.InitializeProgress;
begin
inherited;
fEntriesConvertingProgNode := fProcessingProgNode.StageObjects[PSIDX_Z_EntriesConverting];
fEntriesConvertingProgNode.BeginUpdate;
try
  DART_PROGSTAGE_IDX_ZIP_CONV_SCS_EntriesCopy     := fEntriesConvertingProgNode.Add(25);
  DART_PROGSTAGE_IDX_ZIP_CONV_SCS_DirsReconstruct := fEntriesConvertingProgNode.Add(50);
  DART_PROGSTAGE_IDX_ZIP_CONV_SCS_ProgressReset   := fEntriesConvertingProgNode.Add(25);
  // assign obtained indices to shorter-named variables  
  PSIDX_Z_EntriesCopy     := DART_PROGSTAGE_IDX_ZIP_CONV_SCS_EntriesCopy;
  PSIDX_Z_DirsReconstruct := DART_PROGSTAGE_IDX_ZIP_CONV_SCS_DirsReconstruct;
  PSIDX_Z_ProgressReset   := DART_PROGSTAGE_IDX_ZIP_CONV_SCS_ProgressReset;
finally
  fEntriesConvertingProgNode.EndUpdate;
end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Convert_SCS.ConvertArchiveStructure;
var
  i:  Integer;
begin
DoProgress(fProcessingProgNode,PSIDX_Z_EntriesConverting,0.0);
ZIP_Conv_SCS_InitializeConvertedArchive;
// prepare entries
SetLength(fSCSArchiveStructure.Entries.Arr,fArchiveStructure.Entries.Count);
fSCSArchiveStructure.Entries.Count := 0;
DoProgress(fEntriesConvertingProgNode,DART_PROGSTAGE_IDX_ZIP_CONV_SCS_EntriesCopy,0.0);
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  begin
    with fArchiveStructure.Entries.Arr[i],fSCSArchiveStructure.Entries.Arr[fSCSArchiveStructure.Entries.Count] do
      If (CentralDirectoryHeader.BinPart.ExternalFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
        begin
          // copy only file entries, no directory entries (they will be reconstructed from file names)
          FileName := AnsiReplaceText(DART_ExcludeLeadingPathDelim(CentralDirectoryHeader.FileName,DART_ZIP_PathDelim),
                                      DART_ZIP_PathDelim,DART_SCS_PathDelim);
          UtilityData.Erroneous := False;                            
          UtilityData.DataOffset := UInt64(fArchiveStructure.Entries.Arr[i].UtilityData.DataOffset);
          UtilityData.Index := i;                                 // binds SCS# item to original ZIP item
          BinPart.Hash := ZIP_Conv_SCS_EntryFileNameHash(FileName);
          BinPart.DataOffset := 0;                                // will be obtained on saving
          BinPart.Flags := DART_SCS_FLAG_Unknown;                 // compression flag will be discerned on entry processing
          BinPart.CRC32 := CentralDirectoryHeader.BinPart.CRC32;  // might be changed on saving
          BinPart.UncompressedSize := 0;                          // will be obtained on saving
          BinPart.CompressedSize := 0;                            // will be obtained on saving
          Inc(fSCSArchiveStructure.Entries.Count);
        end;
    DoProgress(fEntriesConvertingProgNode,DART_PROGSTAGE_IDX_ZIP_CONV_SCS_EntriesCopy,(i + 1) / fArchiveStructure.Entries.Count);
  end;
DoProgress(fEntriesConvertingProgNode,DART_PROGSTAGE_IDX_ZIP_CONV_SCS_EntriesCopy,1.0);
// set-up header
with fSCSArchiveStructure.ArchiveHeader do
  begin
    Signature := DART_SCS_ArchiveSignature;
    Unknown := 1;
    HashType := DART_SCS_HASH_City;
    EntryCount := 0;  // will be filled later
    EntryTableOffset := DART_SCS_DefaultEntryTableOffset;
    UnknownOffset := DART_SCS_DefaultUnknownOffset;
  end;
// reconstruct directories
ZIP_Conv_SCS_ReconstructDirectories;
// finalize
ZIP_Conv_SCS_SortConvertedEntries;
// reset progress
ZIP_Conv_SCS_ResetProgress;
DoProgress(fProcessingProgNode,PSIDX_Z_EntriesConverting,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Convert_SCS.ConvertArchive;
var
  i:  Integer;
begin
DoProgress(fProcessingProgNode,PSIDX_Z_EntriesProcessing,0.0);
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
  // reserve place for entry table
  ZIP_Conv_SCS_WriteHeaderAndEntryTable(True);
  // save individual entries
  For i := Low(fSCSArchiveStructure.Entries.Arr) to Pred(fSCSArchiveStructure.Entries.Count) do
    ZIP_Conv_SCS_WriteEntry(i);
  fConvertedArchiveStream.Size := fConvertedArchiveStream.Position;
  // write header and entry table
  fConvertedArchiveStream.Seek(0,soBeginning);
  ZIP_Conv_SCS_WriteHeaderAndEntryTable(False);
  // finalize
  DoProgress(fProcessingProgNode,PSIDX_Z_EntriesProcessing,1.0);
  If fArchiveProcessingSettings.Common.InMemoryProcessing then
    ProgressedSaveFile(fArchiveProcessingSettings.Common.TargetPath,fConvertedArchiveStream,
                       ProgressStageInfo(fProgressTracker,DART_PROGSTAGE_IDX_Saving));
finally
  fConvertedArchiveStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Convert_SCS.ZIP_Conv_SCS_InitializeConvertedArchive;
begin
fSCSArchiveStructure.ArchiveHeader.HashType := DART_SCS_HASH_City;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_ZIP_Convert_SCS.ZIP_Conv_SCS_EntryFileNameHash(const EntryFileName: AnsiString): UInt64;
begin
Result := 0;
case fSCSArchiveStructure.ArchiveHeader.HashType of
  DART_SCS_HASH_City: Result := CityHash64(PAnsiChar(EntryFileName),Length(EntryFileName) * SizeOf(AnsiChar));
else
  DoError(DART_METHOD_ID_ZIP_CONV_SCS_ENFNHSH,'Unknown hashing algorithm (0x%.8x).',[fSCSArchiveStructure.ArchiveHeader.HashType]);
end;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_ZIP_Convert_SCS.ZIP_Conv_SCS_HashCompare(A,B: UInt64): Integer;
begin
If AuxTypes.NativeUInt64 then
  begin
    If A < B then Result := 1
      else If A > B then Result := -1
        else Result := 0;
  end
else
  begin
    If Int64Rec(A).Hi <> Int64Rec(B).Hi then
      begin
        If Int64Rec(A).Hi < Int64Rec(B).Hi then Result := 2
          else Result := -2
      end
    else
      begin
        If Int64Rec(A).Lo < Int64Rec(B).Lo then Result := 1
          else If Int64Rec(A).Lo > Int64Rec(B).Lo then Result := -1
            else Result := 0;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Convert_SCS.ZIP_Conv_SCS_SortConvertedEntries;

  procedure QuickSort(LeftIdx,RightIdx: Integer);
  var
    Pivot:  UInt64;
    Idx,i:  Integer;

    procedure ExchangeEntries(Idx1,Idx2: Integer);
    var
      TempEntry:  TDART_SCS_Entry;
    begin
      If (Idx1 <> Idx2) then
        begin
          If (Idx1 < Low(fSCSArchiveStructure.Entries.Arr)) or (Idx1 >= fSCSArchiveStructure.Entries.Count) then
            DoError(DART_METHOD_ID_ZIP_CONV_SCS_SCEQSEE,'Index 1 (%d) out of bounds.',[Idx1]);
          If (Idx2 < Low(fSCSArchiveStructure.Entries.Arr)) or (Idx2 >= fSCSArchiveStructure.Entries.Count) then
            DoError(DART_METHOD_ID_ZIP_CONV_SCS_SCEQSEE,'Index 2 (%d) out of bounds.',[Idx1]);
          TempEntry := fSCSArchiveStructure.Entries.Arr[Idx1];
          fSCSArchiveStructure.Entries.Arr[Idx1] := fSCSArchiveStructure.Entries.Arr[Idx2];
          fSCSArchiveStructure.Entries.Arr[Idx2] := TempEntry;
        end;
    end;
    
  begin
    DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0); // must be called at the start of this routine
    If LeftIdx < RightIdx then
      begin
        ExchangeEntries((LeftIdx + RightIdx) shr 1,RightIdx);
        Pivot := fSCSArchiveStructure.Entries.Arr[RightIdx].BinPart.Hash;
        Idx := LeftIdx;
        For i := LeftIdx to Pred(RightIdx) do
          If ZIP_Conv_SCS_HashCompare(Pivot,fSCSArchiveStructure.Entries.Arr[i].BinPart.Hash) < 0 then
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
If fSCSArchiveStructure.Entries.Count > 1 then
  QuickSort(Low(fSCSArchiveStructure.Entries.Arr),Pred(fSCSArchiveStructure.Entries.Count));
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Convert_SCS.ZIP_Conv_SCS_ReconstructDirectories;
var
  PathDeconstructor:  TDARTPathDeconstructor;
  i,j:                Integer;
begin
DoProgress(fEntriesConvertingProgNode,DART_PROGSTAGE_IDX_ZIP_CONV_SCS_DirsReconstruct,0.0);
PathDeconstructor := TDARTPathDeconstructor.Create(DART_SCS_PathDelim);
try
  // deconstruct path of all entries
  For i := Low(fSCSArchiveStructure.Entries.Arr) to Pred(fSCSArchiveStructure.Entries.Count) do
    begin
      PathDeconstructor.DeconstructPath(fSCSArchiveStructure.Entries.Arr[i].FileName);
      // this is the slowest part of path reconstruction, so progress is done here
      DoProgress(fEntriesConvertingProgNode,DART_PROGSTAGE_IDX_ZIP_CONV_SCS_DirsReconstruct,(i + 1) / fSCSArchiveStructure.Entries.Count);
    end;
  PathDeconstructor.Sort;
  // store deconstructed paths into entries
  If (fSCSArchiveStructure.Entries.Count + PathDeconstructor.Count) > Length(fSCSArchiveStructure.Entries.Arr) then
    SetLength(fSCSArchiveStructure.Entries.Arr,fSCSArchiveStructure.Entries.Count + PathDeconstructor.Count);
  For i := 0 to Pred(PathDeconstructor.Count) do
    with fSCSArchiveStructure.Entries.Arr[fSCSArchiveStructure.Entries.Count + i] do
      begin
        FileName := PathDeconstructor[i].FullPath;
        BinPart.Hash := ZIP_Conv_SCS_EntryFileNameHash(FileName);
        BinPart.Flags := DART_SCS_FLAG_Unknown or DART_SCS_FLAG_Directory;
        // other bin fields (data offset, CRC32, sizes) will be filled when the item is written
        UtilityData.Erroneous := False;
        SetLength(UtilityData.DirContent,PathDeconstructor[i].SubNodeCount + PathDeconstructor[i].FileCount);
        For j := 0 to Pred(PathDeconstructor[i].SubNodeCount) do
          UtilityData.DirContent[j] := '*' + PathDeconstructor[i].SubNodes[j].Name;
        For j := 0 to Pred(PathDeconstructor[i].FileCount) do
          UtilityData.DirContent[PathDeconstructor[i].SubNodeCount + j] := PathDeconstructor[i].Files[j];
        DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0);
      end;
  fSCSArchiveStructure.Entries.Count := fSCSArchiveStructure.Entries.Count + PathDeconstructor.Count;
finally
  PathDeconstructor.Free;
end;
DoProgress(fEntriesConvertingProgNode,DART_PROGSTAGE_IDX_ZIP_CONV_SCS_DirsReconstruct,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Convert_SCS.ZIP_Conv_SCS_ResetProgress;
var
  i,Index:  Integer;
  CurrNode: TProgressTracker;
begin
DoProgress(fEntriesConvertingProgNode,DART_PROGSTAGE_IDX_ZIP_CONV_SCS_ProgressReset,0.0);
fEntriesProcessingProgNode.BeginUpdate;
try
  fEntriesProcessingProgNode.Clear;
  For i := Low(fSCSArchiveStructure.Entries.Arr) to Pred(fSCSArchiveStructure.Entries.Count) do
    with fSCSArchiveStructure.Entries.Arr[i] do
      begin
        with fArchiveStructure.Entries.Arr[fSCSArchiveStructure.Entries.Arr[i].UtilityData.Index] do
          If CentralDirectoryHeader.BinPart.CompressedSize <> 0 then
            Index := fEntriesProcessingProgNode.Add(CentralDirectoryHeader.BinPart.CompressedSize)
          else
            Index := fEntriesProcessingProgNode.Add(1);
        CurrNode := fEntriesProcessingProgNode.StageObjects[Index];
        CurrNode.BeginUpdate;
        try
          with fArchiveStructure.Entries.Arr[fSCSArchiveStructure.Entries.Arr[i].UtilityData.Index] do
            If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
              begin
                // entry is a directory
                DART_PROGSTAGE_IDX_ZIP_EntryLoading       := CurrNode.Add(0);
                DART_PROGSTAGE_IDX_ZIP_EntryDecompression := CurrNode.Add(60);
                DART_PROGSTAGE_IDX_ZIP_EntrySaving        := CurrNode.Add(40);
              end
            else
              begin
                // entry is a file
                DART_PROGSTAGE_IDX_ZIP_EntryLoading       := CurrNode.Add(40);
                If (LocalHeader.BinPart.CompressionMethod <> DART_ZCM_Store) and
                  (LocalHeader.BinPart.CompressedSize > 0) then
                  DART_PROGSTAGE_IDX_ZIP_EntryDecompression := CurrNode.Add(20)
                else
                  DART_PROGSTAGE_IDX_ZIP_EntryDecompression := CurrNode.Add(0);
                DART_PROGSTAGE_IDX_ZIP_EntrySaving        := CurrNode.Add(40);
              end;
           // assign obtained indices to shorter named-variables
          PSIDX_Z_EntryProcessing    := DART_PROGSTAGE_IDX_ZIP_EntryProcessing;
          PSIDX_Z_EntryLoading       := DART_PROGSTAGE_IDX_ZIP_EntryLoading;
          PSIDX_Z_EntryDecompression := DART_PROGSTAGE_IDX_ZIP_EntryDecompression;
          PSIDX_Z_EntrySaving        := DART_PROGSTAGE_IDX_ZIP_EntrySaving;
        finally
          CurrNode.EndUpdate;
        end;
        DoProgress(fEntriesConvertingProgNode,DART_PROGSTAGE_IDX_ZIP_CONV_SCS_ProgressReset,(i + 1) / fSCSArchiveStructure.Entries.Count);
      end;
finally
  fEntriesProcessingProgNode.EndUpdate;
end;
DoProgress(fEntriesConvertingProgNode,DART_PROGSTAGE_IDX_ZIP_CONV_SCS_ProgressReset,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Convert_SCS.ZIP_Conv_SCS_WriteHeaderAndEntryTable(Placeholder: Boolean);
var
  EntryTableSize: TMemSize;
  i:              Integer;
begin
If Placeholder then
  begin
    // header and entry table is not actually written, instead write appropriate number of zeroes
    EntryTableSize := TMemSize(fSCSArchiveStructure.Entries.Count * SizeOf(TDART_SCS_EntryRecord));     
    ReallocBufferKeep(fBuffer_Entry,DART_SCS_DefaultEntryTableOffset + EntryTableSize);
    FillChar(fBuffer_Entry.Memory^,DART_SCS_DefaultEntryTableOffset + EntryTableSize,0);
    fConvertedArchiveStream.WriteBuffer(fBuffer_Entry.Memory^,DART_SCS_DefaultEntryTableOffset + EntryTableSize);
  end
else
  begin
    // write actual header and entry table
    fSCSArchiveStructure.ArchiveHeader.EntryCount := fSCSArchiveStructure.Entries.Count;
    fConvertedArchiveStream.Seek(fSCSArchiveStructure.ArchiveHeader.EntryTableOffset,soBeginning);
    For i := Low(fSCSArchiveStructure.Entries.Arr) to Pred(fSCSArchiveStructure.Entries.Count) do
      If not fSCSArchiveStructure.Entries.Arr[i].UtilityData.Erroneous then
        fConvertedArchiveStream.WriteBuffer(fSCSArchiveStructure.Entries.Arr[i].BinPart,SizeOf(TDART_SCS_EntryRecord))
      else
        Dec(fSCSArchiveStructure.ArchiveHeader.EntryCount);
    fConvertedArchiveStream.Seek(0,soBeginning);
    fConvertedArchiveStream.WriteBuffer(fSCSArchiveStructure.ArchiveHeader,SizeOf(TDART_SCS_ArchiveHeader));
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Convert_SCS.ZIP_Conv_SCS_WriteEntry(Index: Integer);
var
  ZIP_Index:        Integer;
  DirBuffer:        TMemoryStream;
  CompressedBuff:   Pointer;
  CompressedSize:   TMemSize;  
  TempOffset:       Int64;
  DecompressedBuff: Pointer;
  DecompressedSize: TMemSize;
begin
with fSCSArchiveStructure.Entries.Arr[Index] do
try
  // prepare progress
  fEntryProcessingProgNode := fEntriesProcessingProgNode.StageObjects[Index];
  DoProgress(fEntriesProcessingProgNode,Index,0.0);
  // prepare index of corresponding entry in input SCS# archive
  ZIP_Index := UtilityData.Index;

  If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
    begin
      // entry is a directory
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
        BinPart.DataOffset := UInt64(fConvertedArchiveStream.Position);
        BinPart.CRC32 := StreamCRC32(DirBuffer);
        BinPart.UncompressedSize := UInt32(DirBuffer.Size);
        If BinPart.UncompressedSize > DART_SCS_MaxUncompDirEntrySize then
          begin
            ProgressedCompressBuffer(DirBuffer.Memory,DirBuffer.Size,CompressedBuff,CompressedSize,WBITS_ZLIB,
              ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_ZIP_EntryDecompression));
            try
              BinPart.CompressedSize := UInt32(CompressedSize) ;
              SetFlagValue(BinPart.Flags,DART_SCS_FLAG_Compressed);
              ProgressedStreamWrite(fConvertedArchiveStream,CompressedBuff,CompressedSize,
                ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_ZIP_EntrySaving));
            finally
              FreeMem(CompressedBuff,CompressedSize);
            end;
          end
        else
          begin
            BinPart.CompressedSize := BinPart.UncompressedSize;
            ResetFlagValue(BinPart.Flags,DART_SCS_FLAG_Compressed);
            ProgressedStreamWrite(fConvertedArchiveStream,DirBuffer.Memory,DirBuffer.Size,
              ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_ZIP_EntrySaving));
          end;
      finally
        DirBuffer.Free;
      end;        
    end
  else
    begin
      // entry is a file
      // current position in converted archive is used as data offset
      BinPart.DataOffset := UInt64(fConvertedArchiveStream.Position);

      with fArchiveStructure.Entries.Arr[ZIP_Index] do
        begin
          // obtain original compressed size of the entry if required
          If UtilityData.NeedsSizes then
            begin
              // need to obtain sizes of entry - getting compressed size here
              If ZIP_Index < Pred(fArchiveStructure.Entries.Count) then
                // inner entry (ie. not a last one), compressed size is calculated as a difference
                // between data start (obtained earlier) and start of local header of next entry
                LocalHeader.BinPart.CompressedSize :=
                  UInt32(Int64(fArchiveStructure.Entries.Arr[ZIP_Index + 1].CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader) -
                  fArchiveStructure.Entries.Arr[ZIP_Index].UtilityData.DataOffset)
              else
                begin
                  // last entry, find CD or EOCD
                  TempOffset := FindSignature(DART_ZIP_CentralDirectoryFileHeaderSignature,DART_PROGSTAGE_INFO_NoProgress);
                  If TempOffset < 0 then
                    TempOffset := FindSignature(DART_ZIP_EndOfCentralDirectorySignature,DART_PROGSTAGE_INFO_NoProgress);
                  If TempOffset >= fArchiveStructure.Entries.Arr[ZIP_Index].UtilityData.DataOffset then
                    // CD or EOCD found, let's assume it marks an end of entry data
                    LocalHeader.BinPart.CompressedSize := UInt32(TempOffset - fArchiveStructure.Entries.Arr[ZIP_Index].UtilityData.DataOffset)
                  else
                    // CD or EOCD not found, assuming entry data are continuing to EOF
                    LocalHeader.BinPart.CompressedSize := UInt32(fInputArchiveStream.Size - fArchiveStructure.Entries.Arr[ZIP_Index].UtilityData.DataOffset);
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

          // prepare buffer that will hold entry data
          ReallocBufferKeep(fBuffer_Entry,LocalHeader.BinPart.CompressedSize);
          // load potentially compressed data
          fInputArchiveStream.Seek(UtilityData.DataOffset,soBeginning);
          ProgressedStreamRead(fInputArchiveStream,fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,
            ProgressStageInfo(fEntryProcessingProgNode,PSIDX_Z_EntryLoading));

          If CentralDirectoryHeader.BinPart.CompressionMethod <> DART_ZCM_Store then
            begin
              // data are compressed, decompress them (no progress, it is saved for recompression since that takes more time)
              ProgressedDecompressBuffer(fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,
                DecompressedBuff,DecompressedSize,WBITS_RAW,DART_PROGSTAGE_INFO_NoProgress);
              try
                // recompress data
              {
                This can be optimized - instead of recompression, do:
                  - construct zlib stream header
                  - calculate adler32 of decompressed stream
                  - construct zlib stream footer
                  - set compressed size to orig. size with footer and header

                I don't have adler implementation atm., so later...    
              }
                ProgressedCompressBuffer(DecompressedBuff,DecompressedSize,CompressedBuff,CompressedSize,WBITS_ZLIB,
                  ProgressStageInfo(fEntryProcessingProgNode,PSIDX_Z_EntryDecompression));
                try
                  If UtilityData.NeedsCRC32 then
                    BinPart.CRC32 := BufferCRC32(DecompressedBuff^,DecompressedSize);
                  BinPart.UncompressedSize := UInt32(DecompressedSize);
                  BinPart.CompressedSize := UInt32(CompressedSize);
                  SetFlagValue(BinPart.Flags,DART_SCS_FLAG_Compressed);
                  // store recompressed data
                  ProgressedStreamWrite(fConvertedArchiveStream,CompressedBuff,CompressedSize,
                    ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_ZIP_EntrySaving));
                finally
                  FreeMem(CompressedBuff,CompressedSize);
                end;  
              finally
                FreeMem(DecompressedBuff,DecompressedSize);
              end;
            end
          else
            begin
              // data are not compressed
              If UtilityData.NeedsCRC32 then
                BinPart.CRC32 := BufferCRC32(fBuffer_Entry.Memory^,LocalHeader.BinPart.CompressedSize);
              BinPart.UncompressedSize := UInt32(LocalHeader.BinPart.CompressedSize);
              BinPart.CompressedSize := UInt32(LocalHeader.BinPart.CompressedSize);
              ResetFlagValue(BinPart.Flags,DART_SCS_FLAG_Compressed);
              ProgressedStreamWrite(fConvertedArchiveStream,fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,
                ProgressStageInfo(fEntryProcessingProgNode,DART_PROGSTAGE_IDX_ZIP_EntrySaving));
            end;
        end;
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

//==============================================================================

class Function TDARTRepairer_ZIP_Convert_SCS.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  DART_METHOD_ID_ZIP_CONV_SCS_ENFNHSH:  Result := 'ZIP_Conv_SCS_EntryFileNameHash';
  DART_METHOD_ID_ZIP_CONV_SCS_SCEQSEE:  Result := 'ZIP_Conv_SCS_SortConvertedEntries.QuickSort.ExchangeEntries';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

end.
