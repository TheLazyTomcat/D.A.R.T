{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_SCS;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes, ProgressTracker,
  DART_Common, DART_ProcessingSettings, DART_Format_SCS, DART_Repairer,
  DART_Resolver;

{===============================================================================
--------------------------------------------------------------------------------
                                TDARTRepairer_SCS
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_SCS - progress stages indexing variables
===============================================================================}

var
  DART_PROGSTAGE_IDX_SCS_ArchiveHeaderLoading:    Integer = -1;
  DART_PROGSTAGE_IDX_SCS_EntriesLoading:          Integer = -1;
  DART_PROGSTAGE_IDX_SCS_PathsResolving:          Integer = -1;
  DART_PROGSTAGE_IDX_SCS_EntriesProgressPrep:     Integer = -1;
  DART_PROGSTAGE_IDX_SCS_EntriesConverting:       Integer = -1;
  DART_PROGSTAGE_IDX_SCS_EntriesProcessing:       Integer = -1;

  DART_PROGSTAGE_IDX_SCS_PathsRes_Local:          Integer = -1;
  DART_PROGSTAGE_IDX_SCS_PathsRes_HelpArchives:   Integer = -1;
  DART_PROGSTAGE_IDX_SCS_PathsRes_ContentParsing: Integer = -1;
  DART_PROGSTAGE_IDX_SCS_PathsRes_LocalSecond:    Integer = -1;
  DART_PROGSTAGE_IDX_SCS_PathsRes_BruteForce:     Integer = -1;
  DART_PROGSTAGE_IDX_SCS_PathsRes_Reconstruct:    Integer = -1;

  DART_PROGSTAGE_IDX_SCS_EntryProcessing:         Integer = -1;
  DART_PROGSTAGE_IDX_SCS_EntryLoading:            Integer = -1;
  DART_PROGSTAGE_IDX_SCS_EntryDecompression:      Integer = -1;
  DART_PROGSTAGE_IDX_SCS_EntrySaving:             Integer = -1;

  PSIDX_C_ArchiveHeaderLoading:    Integer = -1;
  PSIDX_C_EntriesLoading:          Integer = -1;
  PSIDX_C_PathsResolving:          Integer = -1;
  PSIDX_C_EntriesProgressPrep:     Integer = -1;
  PSIDX_C_EntriesConverting:       Integer = -1;
  PSIDX_C_EntriesProcessing:       Integer = -1;

  PSIDX_C_PathsRes_Local:          Integer = -1;
  PSIDX_C_PathsRes_HelpArchives:   Integer = -1;
  PSIDX_C_PathsRes_ContentParsing: Integer = -1;
  PSIDX_C_PathsRes_LocalSecond:    Integer = -1;
  PSIDX_C_PathsRes_BruteForce:     Integer = -1;
  PSIDX_C_PathsRes_Reconstruct:    Integer = -1;

  PSIDX_C_EntryProcessing:         Integer = -1;
  PSIDX_C_EntryLoading:            Integer = -1;
  PSIDX_C_EntryDecompression:      Integer = -1;
  PSIDX_C_EntrySaving:             Integer = -1;

{===============================================================================
    TDARTRepairer_SCS - class declaration
===============================================================================}
type
  TDARTRepairer_SCS = class(TDARTRepairer)
  protected
    fProcessingSettings:    TDART_PS_SCS;
    fArchiveStructure:      TDART_SCS_ArchiveStructure;
    fEntriesSorted:         Boolean;
    // progress nodes
    fPathsResolveProcNode:  TProgressTracker;
    // path resolving
    fResolver:              TDARTResolver;
    // initialization methods
    procedure InitializeProcessingSettings; override;
    procedure InitializeData; override;
    procedure InitializeProgress; override;
    // flow control methods;
    procedure DoTerminate; override;
    // methods for content parsing
    Function LowEntryIndex: Integer; override;
    Function HighEntryIndex: Integer; override;
    Function IndexOfEntry(const EntryFileName: AnsiString): Integer; override;
    Function GetEntryData(EntryIndex: Integer; out Data: Pointer; out Size: TMemSize): Boolean; override;
    // methods working with known paths
    Function LowKnownPathIndex: Integer; override;
    Function HighKnownPathIndex: Integer; override;
    Function GetKnownPath(Index: Integer): TDARTKnownPath; override;
    Function IndexOfKnownPath(const Path: AnsiString): Integer; override;
    Function AddKnownPath(const Path: AnsiString; Directory: Boolean): Integer; override;
    // processing methods
    procedure ArchiveProcessing; override;
    // scs specific routines
    Function SCS_EntryFileNameHash(const EntryFileName: AnsiString): TDARTHash64; virtual;
    Function SCS_HashName: String; virtual;
    Function SCS_IndexOfEntry(Hash: TDARTHash64): Integer; virtual;
    procedure SCS_SortEntries; virtual;
    procedure SCS_LoadArchiveHeader; virtual;
    procedure SCS_LoadEntries; virtual;
    procedure SCS_AssignPaths(OnlyCountUnresolved: Boolean = False); virtual;
    procedure SCS_DiscardDirectories; virtual;
    procedure SCS_ReconstructDirectories; virtual;
    procedure SCS_ResolvePaths; virtual; 
    procedure SCS_ResolvePaths_Local(SecondRound: Boolean = False); virtual;
    procedure SCS_ResolvePaths_HelpArchives; virtual;
    procedure SCS_ResolvePaths_ContentParsing; virtual;
    procedure SCS_ResolvePaths_BruteForce; virtual;
    procedure SCS_ResolvePaths_Reconstruct; virtual;
    // progress handlers
    procedure SCS_ResolvePaths_ContentParsing_ProgressHandler(Sender: TObject; Progress: Double); virtual;
    procedure SCS_ResolvePaths_BruteForce_ProgressHandler(Sender: TObject; Progress: Double); virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
    constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings; CatchExceptions: Boolean);
    Function GetAllKnownPaths(var KnownPaths: TDARTKnownPaths): Integer; override;
    property ArchiveStructure: TDART_SCS_ArchiveStructure read fArchiveStructure;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                        TDARTRepairer_SCS_ProcessingBase
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_SCS_ProcessingBase - class declaration
===============================================================================}

  TDARTRepairer_SCS_ProcessingBase = class(TDARTRepairer_SCS)
  protected
    procedure SCS_SaveEntryAsUnresolved(EntryIdx: Integer; Data: Pointer; Size: TMemSize; ProgressInfo: TDART_PSI); virtual;
    procedure SCS_PrepareEntriesProgress; virtual;
    procedure ArchiveProcessing; override;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;    
  end;
  
implementation

uses
  SysUtils, Classes,
  City, BitOps, CRC32, StrRect, MemoryBuffer, ExplicitStringLists, ZLibCommon,
  StaticMemoryStream,
  DART_Auxiliary, DART_PathDeconstructor, DART_Repairer_ZIP,
  DART_Resolver_BruteForce, DART_Resolver_ContentParsing;


{===============================================================================
--------------------------------------------------------------------------------
                                TDARTRepairer_SCS
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_SCS - method indexing constants
===============================================================================}

const
  DART_METHOD_ID_SCS_ARCHPROC   = $00000200;
  DART_METHOD_ID_SCS_GETKNPTH   = $00000201;
  DART_METHOD_ID_SCS_GETENTRY   = $00000202;
  DART_METHOD_ID_SCS_SENTRFNHS  = $00000210;
  DART_METHOD_ID_SCS_SSRTENQSEX = $00000211;
  DART_METHOD_ID_SCS_SLDARHEAD  = $00000212;
  DART_METHOD_ID_SCS_SLDPLOCLP  = $00000213;

{===============================================================================
    TDARTRepairer_SCS - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTRepairer_SCS - protected methods
-------------------------------------------------------------------------------}

procedure TDARTRepairer_SCS.InitializeProcessingSettings;
begin
inherited;
fProcessingSettings := fArchiveProcessingSettings.SCS;
RectifySCSProcessingSettings(fProcessingSettings);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.InitializeData;
begin
FillChar(fArchiveStructure.ArchiveHeader,SizeOf(TDART_SCS_ArchiveHeader),0);
SetLength(fArchiveStructure.Entries.Arr,0);
fArchiveStructure.Entries.Count := 0;
SetLength(fArchiveStructure.KnownPaths.Arr,0);
fArchiveStructure.KnownPaths.Count := 0;
fArchiveStructure.UtilityData.UnresolvedCount := 0;
fResolver := nil;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.InitializeProgress;
begin
inherited;
fProcessingProgNode.BeginUpdate;
try
  // set progress info for loading of archive header
  DART_PROGSTAGE_IDX_SCS_ArchiveHeaderLoading := fProcessingProgNode.Add(10);
  // loading of entries
  DART_PROGSTAGE_IDX_SCS_EntriesLoading := fProcessingProgNode.Add(40);
  // paths resolving
  DART_PROGSTAGE_IDX_SCS_PathsResolving := fProcessingProgNode.Add(50);
  // preparing progress of entries processing
  DART_PROGSTAGE_IDX_SCS_EntriesProgressPrep := fProcessingProgNode.Add(20);
  // entries conversion
  If fArchiveProcessingSettings.Common.RepairMethod = rmConvert then
    DART_PROGSTAGE_IDX_SCS_EntriesConverting := fProcessingProgNode.Add(20);
  // entries processing
  DART_PROGSTAGE_IDX_SCS_EntriesProcessing := fProcessingProgNode.Add(900);
  // assign obtained indices to shorter-named variables
  PSIDX_C_ArchiveHeaderLoading := DART_PROGSTAGE_IDX_SCS_ArchiveHeaderLoading;
  PSIDX_C_EntriesLoading       := DART_PROGSTAGE_IDX_SCS_EntriesLoading;
  PSIDX_C_PathsResolving       := DART_PROGSTAGE_IDX_SCS_PathsResolving;
  PSIDX_C_EntriesProgressPrep  := DART_PROGSTAGE_IDX_SCS_EntriesProgressPrep;
  PSIDX_C_EntriesConverting    := DART_PROGSTAGE_IDX_SCS_EntriesConverting;
  PSIDX_C_EntriesProcessing    := DART_PROGSTAGE_IDX_SCS_EntriesProcessing;
  fPathsResolveProcNode := fProcessingProgNode.StageObjects[PSIDX_C_PathsResolving];
  fEntriesProcessingProgNode := fProcessingProgNode.StageObjects[PSIDX_C_EntriesProcessing];
finally
  fProcessingProgNode.EndUpdate;
end;
fPathsResolveProcNode.BeginUpdate;
try
  // individual stages of paths resolving
  // local
  DART_PROGSTAGE_IDX_SCS_PathsRes_Local := fPathsResolveProcNode.Add(20);
  // help archives
  If Length(fProcessingSettings.PathResolve.HelpArchives) > 0 then
    DART_PROGSTAGE_IDX_SCS_PathsRes_HelpArchives := fPathsResolveProcNode.Add(20);
  // content parsing
  If fProcessingSettings.PathResolve.ContentParsing.ParseContent then
    DART_PROGSTAGE_IDX_SCS_PathsRes_ContentParsing := fPathsResolveProcNode.Add(20);
  // second round of local resolve
  If (Length(fProcessingSettings.PathResolve.HelpArchives) > 0) or
    fProcessingSettings.PathResolve.ContentParsing.ParseContent then
    DART_PROGSTAGE_IDX_SCS_PathsRes_LocalSecond := fPathsResolveProcNode.Add(20);
  // brute force
  If fProcessingSettings.PathResolve.BruteForce.ActivateBruteForce then
    DART_PROGSTAGE_IDX_SCS_PathsRes_BruteForce := fPathsResolveProcNode.Add(30);
  // reconstruct dirs
  DART_PROGSTAGE_IDX_SCS_PathsRes_Reconstruct := fPathsResolveProcNode.Add(10);
  // assign obtained indices to shorter-named variables
  PSIDX_C_PathsRes_Local          := DART_PROGSTAGE_IDX_SCS_PathsRes_Local;
  PSIDX_C_PathsRes_HelpArchives   := DART_PROGSTAGE_IDX_SCS_PathsRes_HelpArchives;
  PSIDX_C_PathsRes_ContentParsing := DART_PROGSTAGE_IDX_SCS_PathsRes_ContentParsing;
  PSIDX_C_PathsRes_LocalSecond    := DART_PROGSTAGE_IDX_SCS_PathsRes_LocalSecond;
  PSIDX_C_PathsRes_BruteForce     := DART_PROGSTAGE_IDX_SCS_PathsRes_BruteForce;
  PSIDX_C_PathsRes_Reconstruct    := DART_PROGSTAGE_IDX_SCS_PathsRes_Reconstruct;
finally
  fPathsResolveProcNode.EndUpdate;
end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.DoTerminate;
begin
If Assigned(fResolver) then
  fResolver.Stop;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.LowEntryIndex: Integer;
begin
Result := Low(fArchiveStructure.Entries.Arr);
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.HighEntryIndex: Integer;
begin
Result := Pred(fArchiveStructure.Entries.Count);
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.IndexOfEntry(const EntryFileName: AnsiString): Integer;
begin
Result := SCS_IndexOfEntry(SCS_EntryFileNameHash(EntryFileName));
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.GetEntryData(EntryIndex: Integer; out Data: Pointer; out Size: TMemSize): Boolean;
var
  DecompressedBuff: Pointer;
  DecompressedSize: TMemSize;
begin
Result := False;
If (EntryIndex >= Low(fArchiveStructure.Entries.Arr)) and (EntryIndex < fArchiveStructure.Entries.Count) then
  begin
    with fArchiveStructure.Entries.Arr[EntryIndex] do
      If not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
        begin
          // entry is a file
          // prepare buffer that will hold entry data
          ReallocBufferKeep(fBuffer_Entry,BinPart.CompressedSize);
          // load data from input archive
          fInputArchiveStream.Seek(BinPart.DataOffset,soBeginning);
          ProgressedStreamRead(fInputArchiveStream,fBuffer_Entry.Memory,BinPart.CompressedSize,DART_PROGSTAGE_INFO_NoProgress);
          If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed) then
            begin
              // data are compressed, decompress them
              ProgressedDecompressBuffer(fBuffer_Entry.Memory,BinPart.CompressedSize,
                DecompressedBuff,DecompressedSize,WBITS_ZLIB,DART_PROGSTAGE_INFO_NoProgress);
              Data := DecompressedBuff;
              Size := TMemSize(DecompressedSize);
            end
          else
            begin
              // data are not compressed, copy them out of buffer
              GetMem(Data,BinPart.CompressedSize);
              Size := TMemSize(BinPart.CompressedSize);
              Move(fBuffer_Entry.Memory^,Data^,Size);
            end;
          // if we are here, everything should be fine
          Result := True;
        end
      // entry is a directory
      else Result := False;
  end
else DoError(DART_METHOD_ID_SCS_GETENTRY,'Entry index (%d) out of bounds.',[EntryIndex]);
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.LowKnownPathIndex: Integer;
begin
Result := Low(fArchiveStructure.KnownPaths.Arr);
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.HighKnownPathIndex: Integer;
begin
Result := Pred(fArchiveStructure.KnownPaths.Count);
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.GetKnownPath(Index: Integer): TDARTKnownPath;
begin
If (Index >= Low(fArchiveStructure.KnownPaths.Arr)) and (Index < fArchiveStructure.KnownPaths.Count) then
  Result := fArchiveStructure.KnownPaths.Arr[Index]
else
  DoError(DART_METHOD_ID_SCS_GETKNPTH,'Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.IndexOfKnownPath(const Path: AnsiString): Integer;
var
  i:        Integer;
  PathHash: TDARTHash64;
begin
Result := -1;
PathHash := SCS_EntryFileNameHash(Path);
For i := Low(fArchiveStructure.KnownPaths.Arr) to Pred(fArchiveStructure.KnownPaths.Count) do
  If HashCompare(PathHash,fArchiveStructure.KnownPaths.Arr[i].Hash64) = 0 then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.AddKnownPath(const Path: AnsiString; Directory: Boolean): Integer;
begin
Result := IndexOfKnownPath(Path);
If Result < 0 then
  begin
    If fArchiveStructure.KnownPaths.Count >= Length(fArchiveStructure.KnownPaths.Arr) then
      SetLength(fArchiveStructure.KnownPaths.Arr,Length(fArchiveStructure.KnownPaths.Arr) + 1024);
    fArchiveStructure.KnownPaths.Arr[fArchiveStructure.KnownPaths.Count].Path := Path;
    fArchiveStructure.KnownPaths.Arr[fArchiveStructure.KnownPaths.Count].Directory := Directory;
    fArchiveStructure.KnownPaths.Arr[fArchiveStructure.KnownPaths.Count].Hash := StringCRC32(AnsiLowerCase(AnsiToStr(Path)));
    fArchiveStructure.KnownPaths.Arr[fArchiveStructure.KnownPaths.Count].Hash64 := SCS_EntryFileNameHash(Path);
    Inc(fArchiveStructure.KnownPaths.Count);
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.ArchiveProcessing;
begin
SCS_LoadArchiveHeader;
SCS_LoadEntries;
If fArchiveStructure.Entries.Count <= 0 then
  DoError(DART_METHOD_ID_SCS_ARCHPROC,'Input archive does not contain any valid entries.');
// following step is optional at this point, but provides better performance
SCS_SortEntries;
SCS_ResolvePaths;
// parse content for paths if used as help archive
If fArchiveProcessingSettings.Auxiliary.HelpArchive and
  fArchiveProcessingSettings.SCS.PathResolve.ContentParsing.ParseHelpArchives then
  ParseContentForPaths;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.SCS_EntryFileNameHash(const EntryFileName: AnsiString): TDARTHash64;
begin
Result := 0;
case fArchiveStructure.ArchiveHeader.HashType of
  DART_SCS_HASH_City: Result := TDARTHash64(CityHash64(PAnsiChar(EntryFileName),Length(EntryFileName) * SizeOf(AnsiChar)));
else
  DoError(DART_METHOD_ID_SCS_SENTRFNHS,'Unknown hashing algorithm (0x%.8x).',[fArchiveStructure.ArchiveHeader.HashType]);
end;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.SCS_HashName: String;
begin
case fArchiveStructure.ArchiveHeader.HashType of
  DART_SCS_HASH_City: Result := 'CITY';
else
  Result := 'UNKN';
end;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.SCS_IndexOfEntry(Hash: TDARTHash64): Integer;
var
  i,Min,Max:  Integer;
begin
Result := -1;
If fEntriesSorted then
  begin
    Min := 0;
    Max := Pred(fArchiveStructure.Entries.Count);
    while Max >= min do
      begin
        i := ((max - Min) shr 1) + Min;
        // i-th entry has lower hash than is requested
        If HashCompare(fArchiveStructure.Entries.Arr[i].BinPart.Hash,Hash) > 0 then Min := i + 1
          // i-th entry has higher hash than is requested
          else If HashCompare(fArchiveStructure.Entries.Arr[i].BinPart.Hash,Hash) < 0 then Max := i - 1
            else begin
              // i-th entry has the requested hash
              Result := i;
              Break{while};
            end;
      end;
  end
else
  begin
    For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
      If HashCompare(fArchiveStructure.Entries.Arr[i].BinPart.Hash,Hash) = 0 then
        begin
          Result := i;
          Break{For i};
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_SortEntries;

  procedure QuickSort(LeftIdx,RightIdx: Integer);
  var
    Pivot:  TDARTHash64;
    Idx,i:  Integer;

    procedure ExchangeEntries(Idx1,Idx2: Integer);
    var
      TempEntry:  TDART_SCS_Entry;
    begin
      If (Idx1 <> Idx2) then
        begin
          If (Idx1 < Low(fArchiveStructure.Entries.Arr)) or (Idx1 >= fArchiveStructure.Entries.Count) then
            DoError(DART_METHOD_ID_SCS_SSRTENQSEX,'Index 1 (%d) out of bounds.',[Idx1]);
          If (Idx2 < Low(fArchiveStructure.Entries.Arr)) or (Idx2 >= fArchiveStructure.Entries.Count) then
            DoError(DART_METHOD_ID_SCS_SSRTENQSEX,'Index 2 (%d) out of bounds.',[Idx1]);
          TempEntry := fArchiveStructure.Entries.Arr[Idx1];
          fArchiveStructure.Entries.Arr[Idx1] := fArchiveStructure.Entries.Arr[Idx2];
          fArchiveStructure.Entries.Arr[Idx2] := TempEntry;
        end;
    end;
    
  begin
    DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0); // must be called at the start of this routine
    If LeftIdx < RightIdx then
      begin
        ExchangeEntries((LeftIdx + RightIdx) shr 1,RightIdx);
        Pivot := fArchiveStructure.Entries.Arr[RightIdx].BinPart.Hash;
        Idx := LeftIdx;
        For i := LeftIdx to Pred(RightIdx) do
          If HashCompare(Pivot,fArchiveStructure.Entries.Arr[i].BinPart.Hash) < 0 then
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
If fArchiveStructure.Entries.Count > 1 then
  QuickSort(Low(fArchiveStructure.Entries.Arr),Pred(fArchiveStructure.Entries.Count));
fEntriesSorted := True;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_LoadArchiveHeader;
begin
DoProgress(fProcessingProgNode,PSIDX_C_ArchiveHeaderLoading,0.0);
If fInputArchiveStream.Size >= SizeOf(TDART_SCS_ArchiveHeader) then
  begin
    fInputArchiveStream.Seek(0,soBeginning);
    fInputArchiveStream.ReadBuffer(fArchiveStructure.ArchiveHeader,SizeOf(TDART_SCS_ArchiveHeader));
    If fArchiveProcessingSettings.Common.IgnoreArchiveSignature then
      fArchiveStructure.ArchiveHeader.Signature := DART_SCS_ArchiveSignature;
    If fProcessingSettings.PathResolve.AssumeCityHash then
      fArchiveStructure.ArchiveHeader.HashType := DART_SCS_HASH_City
    else
      If fArchiveStructure.ArchiveHeader.HashType <> DART_SCS_HASH_City then
        DoError(DART_METHOD_ID_SCS_SLDARHEAD,'Unsupported hash algorithm (0x%.8x).',[fArchiveStructure.ArchiveHeader.HashType]);
  end
else DoError(DART_METHOD_ID_SCS_SLDARHEAD,'Archive is too small (%d bytes) to contain a valid archive header.',[fInputArchiveStream.Size]);
DoProgress(fProcessingProgNode,PSIDX_C_ArchiveHeaderLoading,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_LoadEntries;
var
  EntryTable: TStaticMemoryStream;

  procedure LoadEntriesFromStream(Stream: TStream);
  var
    i:  Integer;
  begin
    For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
      with fArchiveStructure.Entries.Arr[i] do
        begin
          Stream.ReadBuffer(BinPart,SizeOf(TDART_SCS_EntryRecord));
          FileName := '';
          UtilityData.Resolved := False;
          If fProcessingSettings.Entry.IgnoreCRC32 then
            BinPart.CRC32 := 0;
          If fProcessingSettings.Entry.IgnoreCompressionFlag then
            SetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed,BinPart.UncompressedSize <> BinPart.CompressedSize);
          DoProgress(fProcessingProgNode,PSIDX_C_EntriesLoading,(i + 1) / fArchiveStructure.Entries.Count);
        end;
  end;

begin
DoProgress(fProcessingProgNode,PSIDX_C_EntriesLoading,0.0);
SetLength(fArchiveStructure.Entries.Arr,fArchiveStructure.ArchiveHeader.EntryCount);
fArchiveStructure.Entries.Count := Length(fArchiveStructure.Entries.Arr);
fInputArchiveStream.Seek(fArchiveStructure.ArchiveHeader.EntryTableOffset,soBeginning);
If fProcessingSettings.EntryTabInMem and not fArchiveProcessingSettings.Common.InMemoryProcessing then
  begin
    // load entire entry table to memory to speed things up
    ReallocBufferKeep(fBuffer_Entry,fArchiveStructure.ArchiveHeader.EntryCount * SizeOf(TDART_SCS_EntryRecord));
    fInputArchiveStream.ReadBuffer(fBuffer_Entry.Memory^,fArchiveStructure.ArchiveHeader.EntryCount * SizeOf(TDART_SCS_EntryRecord));
    EntryTable := TStaticMemoryStream.Create(fBuffer_Entry.Memory,fArchiveStructure.ArchiveHeader.EntryCount * SizeOf(TDART_SCS_EntryRecord));
    try
      EntryTable.Seek(0,soBeginning);
      LoadEntriesFromStream(EntryTable);
    finally
      EntryTable.Free;
    end;
  end
else LoadEntriesFromStream(fInputArchiveStream); 
DoProgress(fProcessingProgNode,PSIDX_C_EntriesLoading,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_AssignPaths(OnlyCountUnresolved: Boolean = False);
var
  i,Index:  Integer;
begin
If not OnlyCountUnresolved then
  For i := Low(fArchiveStructure.KnownPaths.Arr) to Pred(fArchiveStructure.KnownPaths.Count) do
    begin
      Index := SCS_IndexOfEntry(fArchiveStructure.KnownPaths.Arr[i].Hash64);
      If Index >= 0 then
        If not fArchiveStructure.Entries.Arr[Index].UtilityData.Resolved then
          begin
            fArchiveStructure.Entries.Arr[Index].FileName := fArchiveStructure.KnownPaths.Arr[i].Path;
            fArchiveStructure.Entries.Arr[Index].UtilityData.Resolved := True;
          end;
      DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0);
    end;
// count unresolved entries
fArchiveStructure.UtilityData.UnresolvedCount := 0;
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  If not fArchiveStructure.Entries.Arr[i].UtilityData.Resolved then
    Inc(fArchiveStructure.UtilityData.UnresolvedCount);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_DiscardDirectories;
var
  EntryCount: Integer;
  i:          Integer;
begin
// removes all resolved directory entries - they will be reconstructed from file names
EntryCount := Low(fArchiveStructure.Entries.Arr);
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  with fArchiveStructure.Entries.Arr[i] do
    begin
      If not(GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) and UtilityData.Resolved) then
        begin
          If EntryCount <> i then
            fArchiveStructure.Entries.Arr[EntryCount] := fArchiveStructure.Entries.Arr[i];
          Inc(EntryCount);
        end;
      DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0);
    end;
fArchiveStructure.Entries.Count := EntryCount;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ReconstructDirectories;
var
  PathDeconstructor:  TDARTPathDeconstructor;
  i,j:                Integer;
begin
DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_Reconstruct,0.0);
PathDeconstructor := TDARTPathDeconstructor.Create(DART_SCS_PathDelim);
try
  // deconstruct path of all resolved entries
  For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
    begin
      If fArchiveStructure.Entries.Arr[i].UtilityData.Resolved then
        PathDeconstructor.DeconstructPath(fArchiveStructure.Entries.Arr[i].FileName);
      // this is the slowest part of path reconstruction, so progress is done here
      DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_Reconstruct,(i + 1) / fArchiveStructure.Entries.Count);
    end;
  PathDeconstructor.Sort;
  // store deconstructed paths into entries
  If (fArchiveStructure.Entries.Count + PathDeconstructor.Count) > Length(fArchiveStructure.Entries.Arr) then
    SetLength(fArchiveStructure.Entries.Arr,fArchiveStructure.Entries.Count + PathDeconstructor.Count);
  For i := 0 to Pred(PathDeconstructor.Count) do
    with fArchiveStructure.Entries.Arr[fArchiveStructure.Entries.Count + i] do
      begin
        FileName := PathDeconstructor[i].FullPath;
        BinPart.Hash := SCS_EntryFileNameHash(FileName);
        BinPart.Flags := DART_SCS_FLAG_Unknown or DART_SCS_FLAG_Directory;
        // other bin fields (data offset, CRC32, sizes) will be filled when the item is written
        UtilityData.Resolved := True;
        UtilityData.Erroneous := False;
        SetLength(UtilityData.DirContent,PathDeconstructor[i].SubNodeCount + PathDeconstructor[i].FileCount);
        For j := 0 to Pred(PathDeconstructor[i].SubNodeCount) do
          UtilityData.DirContent[j] := '*' + PathDeconstructor[i].SubNodes[j].Name;
        For j := 0 to Pred(PathDeconstructor[i].FileCount) do
          UtilityData.DirContent[PathDeconstructor[i].SubNodeCount + j] := PathDeconstructor[i].Files[j];
        DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0);
      end;
  fArchiveStructure.Entries.Count := fArchiveStructure.Entries.Count + PathDeconstructor.Count;
finally
  PathDeconstructor.Free;
end;
DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_Reconstruct,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths;
var
  i:  Integer;
begin
DoProgress(fProcessingProgNode,PSIDX_C_PathsResolving,0.0);
fArchiveStructure.KnownPaths.Count := 0;
// add root
AddKnownPath(DART_SCS_PATHS_Root,True);
// add predefined paths
If fProcessingSettings.PathResolve.UsePredefinedPaths then
  begin
    For i := Low(DART_SCS_PATHS_PredefinedDirs) to High(DART_SCS_PATHS_PredefinedDirs) do
      AddKnownPath(DART_SCS_PATHS_PredefinedDirs[i],True);
    For i := Low(DART_SCS_PATHS_PredefinedFiles) to High(DART_SCS_PATHS_PredefinedFiles) do
      AddKnownPath(DART_SCS_PATHS_PredefinedFiles[i],False);
  end;
// add custom paths
with fProcessingSettings.PathResolve do
  For i := Low(CustomPaths) to High(CustomPaths) do
    AddKnownPath(CustomPaths[i],Length(ExtractFileExt(CustomPaths[i])) <= 0);
// assign any predefined or custom paths    
SCS_AssignPaths;    
// load all paths stored in the archive
SCS_ResolvePaths_Local;
// load paths from help archives
If Length(fProcessingSettings.PathResolve.HelpArchives) > 0 then
  SCS_ResolvePaths_HelpArchives;
// parse content of processed archive
If fProcessingSettings.PathResolve.ContentParsing.ParseContent then
  SCS_ResolvePaths_ContentParsing;
// repeat local resolve (in case some paths were obtained from help archives or content parsing)
If (Length(fProcessingSettings.PathResolve.HelpArchives) > 0) or
  fProcessingSettings.PathResolve.ContentParsing.ParseContent then
  SCS_ResolvePaths_Local(True);
// bruteforce resolve
If fProcessingSettings.PathResolve.BruteForce.ActivateBruteForce then
  SCS_ResolvePaths_BruteForce;
SCS_ResolvePaths_Reconstruct;
DoProgress(fProcessingProgNode,PSIDX_C_PathsResolving,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_Local(SecondRound: Boolean = False);
var
  i:                  Integer;
  DirectoryList:      TAnsiStringList;
  CurrentLevel:       TAnsiStringList;
  EntryLines:         TAnsiStringList;  // used inside of nested function LoadPath
  DirCount:           Integer;
  ProcessedDirCount:  Integer;
  ProgressIndex:      Integer;

  procedure LoadPath(const Path: AnsiString; Directories: TAnsiStringList);
  var
    Index:        Integer;
    EntryString:  AnsiString;
    OutBuff:      Pointer;
    OutSize:      TMemSize;
    ii:           Integer;
  begin
    Index := SCS_IndexOfEntry(SCS_EntryFileNameHash(Path));
    If Index >= 0 then
      with fArchiveStructure.Entries.Arr[Index] do
        If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
          begin
            // directory entry, load it...
            ReallocBufferKeep(fBuffer_Entry,BinPart.CompressedSize);
            fInputArchiveStream.Seek(BinPart.DataOffset,soBeginning);
            fInputArchiveStream.ReadBuffer(fBuffer_Entry.Memory^,BinPart.CompressedSize);
            SetLength(EntryString,BinPart.UncompressedSize);
            If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed) then
              begin
                // data needs to be decompressed first
                ProgressedDecompressBuffer(fBuffer_Entry.Memory,BinPart.CompressedSize,OutBuff,OutSize,WBITS_ZLIB,DART_PROGSTAGE_INFO_NoProgress);
                try
                  If UInt32(OutSize) <> BinPart.UncompressedSize then
                    DoError(DART_METHOD_ID_SCS_SLDPLOCLP,'Decompressed size does not match for entry #%d ("%s").',[Index,AnsiToStr(Path)]);
                  Move(OutBuff^,PAnsiChar(EntryString)^,OutSize);
                finally
                  FreeMem(OutBuff,OutSize);
                end;
              end
            else Move(fBuffer_Entry.Memory^,PAnsiChar(EntryString)^,BinPart.CompressedSize);
            // ...and parse its content (new level of directories and files)
            EntryLines.Clear;
            EntryLines.Text := EntryString;
            For ii := 0 to Pred(EntryLines.Count) do
              If Length(EntryLines[ii]) > 0 then
                begin
                  If EntryLines[ii][1] = DART_SCS_DirMark then
                    begin
                      // directory
                      If Length(Path) > 0 then
                        Directories.Add(Path + DART_SCS_PathDelim + Copy(EntryLines[ii],2,Length(EntryLines[ii])))
                      else
                        Directories.Add(Copy(EntryLines[ii],2,Length(EntryLines[ii])));
                      AddKnownPath(Directories[Pred(Directories.Count)],True);
                    end
                  else
                    begin
                      // file
                      If Length(Path) > 0 then
                        AddKnownPath(Path + DART_SCS_PathDelim + EntryLines[ii],False)
                      else
                        AddKnownPath(EntryLines[ii],False)
                    end;
                end;
            Inc(ProcessedDirCount);
            If DirCount > 0 then
              DoProgress(fPathsResolveProcNode,ProgressIndex,ProcessedDirCount / DirCount);
          end;
  end;

begin
If SecondRound then
  ProgressIndex := PSIDX_C_PathsRes_LocalSecond
else
  ProgressIndex := PSIDX_C_PathsRes_Local;
DoProgress(fPathsResolveProcNode,ProgressIndex,0.0);
If fArchiveStructure.UtilityData.UnresolvedCount > 0 then
  begin
    DirectoryList := TAnsiStringList.Create;
    try
      CurrentLevel := TAnsiStringList.Create;
      try
        DirectoryList.Capacity := fArchiveStructure.KnownPaths.Count;
        For i := Low(fArchiveStructure.KnownPaths.Arr) to Pred(fArchiveStructure.KnownPaths.Count) do
          DirectoryList.Add(fArchiveStructure.KnownPaths.Arr[i].Path);
        EntryLines := TAnsiStringList.Create;
        try
          // count entries marked as directory (progress is using this number)
          DirCount := 0;
          ProcessedDirCount := 0;
          For i := Low(fArchiveStructure.Entries.Arr) to PRed(fArchiveStructure.Entries.Count) do
            If GetFlagState(fArchiveStructure.Entries.Arr[i].BinPart.Flags,DART_SCS_FLAG_Directory) then Inc(DirCount);
          repeat
            CurrentLevel.Assign(DirectoryList);
            DirectoryList.Clear;
            For i := 0 to Pred(CurrentLevel.Count) do
              LoadPath(CurrentLevel[i],DirectoryList);
            // remove duplicities
            For i := Pred(DirectoryList.Count) downto 0 do
              If CurrentLevel.IndexOf(DirectoryList[i]) >= 0 then
                DirectoryList.Delete(i);
          until DirectoryList.Count <= 0;
        finally
          EntryLines.Free;
        end;
      finally
        CurrentLevel.Free;
      end;
    finally
      DirectoryList.Free;
    end;
    SCS_AssignPaths;
  end;
DoProgress(fPathsResolveProcNode,ProgressIndex,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_HelpArchives;
var
  i:  Integer;

  procedure LoadHelpArchive(const FileName: String);
  var
    HelpArchiveProcSettings:  TDARTArchiveProcessingSettings;
    HelpArchiveRepairer:      TDARTRepairer;
    ii:                       Integer;
    TempKnownPaths:           TDARTKnownPaths;
  begin
  {
    Creates local basic repairer that will load all possible paths from a help
    archive without doing anything with it.
    Also, for SCS# archives, all paths found to this moment are passed as custom
    paths to the repairer.
    What repairer should be used is discerned by an archive signature, when it
    matches signature of SCS# archive, then SCS repairer is used, otherwise ZIP
    repairer is used.
  }
    // init processing settings for local repairer
    HelpArchiveProcSettings := DART_DefaultArchiveProcessingSettings;
    HelpArchiveProcSettings.Common.ArchivePath := FileName;
    // explicitly turn off in-memory processing, as we do not know size of the archive
    HelpArchiveProcSettings.Common.InMemoryProcessing := False;
    // indicate help archive
    HelpArchiveProcSettings.Auxiliary.HelpArchive := True;
    // set-up content parsing settings
    HelpArchiveProcSettings.SCS.PathResolve.ContentParsing := fProcessingSettings.PathResolve.ContentParsing;
    HelpArchiveProcSettings.SCS.PathResolve.ContentParsing.ParseContent := False;
    // do archive-type-specific processing
    case DART_GetFileSignature(FileName) of
      DART_SCS_ArchiveSignature:  // - - - - - - - - - - - - - - - - - - - - - -
        begin
          // SCS# archive
          // archive signature must be checked because we assume it is SCS# format
          HelpArchiveProcSettings.Common.IgnoreArchiveSignature := False;
          // prepare all already known paths
          SetLength(HelpArchiveProcSettings.SCS.PathResolve.CustomPaths,fArchiveStructure.KnownPaths.Count);
          For ii := Low(fArchiveStructure.KnownPaths.Arr) to Pred(fArchiveStructure.KnownPaths.Count) do
            HelpArchiveProcSettings.SCS.PathResolve.CustomPaths[ii] := fArchiveStructure.KnownPaths.Arr[ii].Path;
          HelpArchiveRepairer := TDARTRepairer_SCS.Create(fPauseControlObject,HelpArchiveProcSettings,False);
          try
            HelpArchiveRepairer.OnProgress := ForwardedProgressHandler;
            HelpArchiveRepairer.Heartbeat := fHeartbeat;
            HelpArchiveRepairer.Run;
            TempKnownPaths.Count := 0;
            If HelpArchiveRepairer.GetAllKnownPaths(TempKnownPaths) > 0 then
              For ii := Low(TempKnownPaths.Arr) to Pred(TempKnownPaths.Count) do
                AddKnownPath(TempKnownPaths.Arr[ii].Path,TempKnownPaths.Arr[ii].Directory);
          finally
            HelpArchiveRepairer.Free;
          end;
        end;
    else
     {DART_ZIP_FileSignature:}  // - - - - - - - - - - - - - - - - - - - - - - -
      // other archive types (ZIP)
      HelpArchiveRepairer := TDARTRepairer_ZIP.Create(fPauseControlObject,HelpArchiveProcSettings,False);
      try
        HelpArchiveRepairer.OnProgress := ForwardedProgressHandler;
        HelpArchiveRepairer.Heartbeat := fHeartbeat;
        HelpArchiveRepairer.Run;
        TempKnownPaths.Count := 0;
        If HelpArchiveRepairer.GetAllKnownPaths(TempKnownPaths) > 0 then
          For ii := Low(TempKnownPaths.Arr) to Pred(TempKnownPaths.Count) do
            AddKnownPath(TempKnownPaths.Arr[ii].Path,TempKnownPaths.Arr[ii].Directory);
      finally
        HelpArchiveRepairer.Free;
      end;
    end;
  end;

begin
DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_HelpArchives,0.0);
If fArchiveStructure.UtilityData.UnresolvedCount > 0 then
  begin
    For i := Low(fProcessingSettings.PathResolve.HelpArchives) to High(fProcessingSettings.PathResolve.HelpArchives) do
      begin
        If Length(fProcessingSettings.PathResolve.HelpArchives[i]) > 0 then
          LoadHelpArchive(fProcessingSettings.PathResolve.HelpArchives[i]);
        DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_HelpArchives,(i + 1) / Length(fProcessingSettings.PathResolve.HelpArchives));
        SCS_AssignPaths;
        If fArchiveStructure.UtilityData.UnresolvedCount <= 0 then
          Break{For i}; // all entries are resolved, no need to continue
      end;
  end;
DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_HelpArchives,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_ContentParsing;
var
  i:          Integer;
  EntryData:  Pointer;
  EntrySize:  TMemSize;
begin
DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_ContentParsing,0.0);
If fArchiveStructure.UtilityData.UnresolvedCount > 0 then
  begin
    fResolver := TDARTResolver_ContentParsing.Create(fPauseControlObject,fArchiveProcessingSettings);
    try
      fResolver.OnProgress := SCS_ResolvePaths_ContentParsing_ProgressHandler;
      fResolver.Initialize(fArchiveStructure);
      If fResolver.UnresolvedCount > 0 then
        begin
          For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
            begin
              If GetEntryData(i,EntryData,EntrySize) then
                try
                  TDARTResolver_ContentParsing(fResolver).Run(EntryData,EntrySize);
                finally
                  FreeMem(EntryData,EntrySize);
                end;
            {
              it is highly unlikely that processing of any single entry will take much time,
              therefore there is no progress in the resolver and it is instead done here per entry
            }
              DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_ContentParsing,(i + 1) / fArchiveStructure.Entries.Count);
              If fResolver.UnresolvedCount <= 0 then
                Break{For i};
            end;
          // store resolved paths to known
          For i := 0 to Pred(fResolver.ResolvedCount) do
            AddKnownPath(fResolver.Resolved[i].Path,Length(ExtractFileExt(AnsiToStr(fResolver.Resolved[i].Path))) <= 0);
        end;
      // assign any newly found paths
      SCS_AssignPaths;        
      // second round if needed, this time only already known paths are processed
      If fResolver.UnresolvedCount > 0 then
        begin
          For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
            begin
              If fArchiveStructure.Entries.Arr[i].UtilityData.Resolved and
                (Length(fArchiveStructure.Entries.Arr[i].FileName) > 0) then
                TDARTResolver_ContentParsing(fResolver).Run(fArchiveStructure.Entries.Arr[i].FileName);
              If fResolver.UnresolvedCount <= 0 then
                Break{For i};                
            end;
          // store resolved paths to known
          For i := 0 to Pred(fResolver.ResolvedCount) do
            AddKnownPath(fResolver.Resolved[i].Path,Length(ExtractFileExt(AnsiToStr(fResolver.Resolved[i].Path))) <= 0);
        end;
      // assign any newly found paths
      SCS_AssignPaths;
    finally
      FreeAndNil(fResolver);
    end;
  end;
DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_ContentParsing,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_BruteForce;
var
  i:  Integer;
begin
DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_BruteForce,0.0);
If fArchiveStructure.UtilityData.UnresolvedCount > 0 then
  begin
    fResolver := TDARTResolver_BruteForce.Create(fPauseControlObject,fArchiveProcessingSettings);
    try
      fResolver.OnProgress := SCS_ResolvePaths_BruteForce_ProgressHandler;
      fResolver.Initialize(fArchiveStructure);
      If fResolver.UnresolvedCount > 0 then
        fResolver.Run;
      // get resolved
      For i := 0 to Pred(fResolver.ResolvedCount) do
        AddKnownPath(fResolver.Resolved[i].Path,Length(ExtractFileExt(AnsiToStr(fResolver.Resolved[i].Path))) <= 0);
      // assign any newly found paths
      SCS_AssignPaths;
    finally
      FreeAndNil(fResolver);
    end;
  end;
DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_BruteForce,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_Reconstruct;
begin
DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_Reconstruct,0.0);
// reconstruct all directory entries from file names
fEntriesSorted := False;
SCS_DiscardDirectories;
SCS_ReconstructDirectories;
SCS_SortEntries;
// count unresolved
SCS_AssignPaths(True);
DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_Reconstruct,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_ContentParsing_ProgressHandler(Sender: TObject; Progress: Double);
begin
DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_ContentParsing,Progress);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_BruteForce_ProgressHandler(Sender: TObject; Progress: Double);
begin
DoProgress(fPathsResolveProcNode,PSIDX_C_PathsRes_BruteForce,Progress);
end;

{-------------------------------------------------------------------------------
    TDARTRepairer_SCS - public methods
-------------------------------------------------------------------------------}

class Function TDARTRepairer_SCS.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  DART_METHOD_ID_SCS_ARCHPROC:   Result := 'ArchiveProcessing';
  DART_METHOD_ID_SCS_GETENTRY:   Result := 'GetEntryData(Index)';
  DART_METHOD_ID_SCS_GETKNPTH:   Result := 'GetKnownPath';
  DART_METHOD_ID_SCS_SENTRFNHS:  Result := 'SCS_EntryFileNameHash';
  DART_METHOD_ID_SCS_SSRTENQSEX: Result := 'SCS_SortEntries.QuickSort.ExchangeEntries';
  DART_METHOD_ID_SCS_SLDARHEAD:  Result := 'SCS_LoadArchiveHeader';
  DART_METHOD_ID_SCS_SLDPLOCLP:  Result := 'SCS_LoadPaths_Local.LoadPath';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

//------------------------------------------------------------------------------

constructor TDARTRepairer_SCS.Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings; CatchExceptions: Boolean);
begin
inherited Create(PauseControlObject,ArchiveProcessingSettings,CatchExceptions);
fExpectedSignature := DART_SCS_ArchiveSignature;
fEntriesSorted := False;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.GetAllKnownPaths(var KnownPaths: TDARTKnownPaths): Integer;
var
  i:  Integer;
begin
If (KnownPaths.Count + fArchiveStructure.KnownPaths.Count) > Length(KnownPaths.Arr) then
  SetLength(KnownPaths.Arr,KnownPaths.Count + fArchiveStructure.KnownPaths.Count);
Result := 0;
For i := Low(fArchiveStructure.KnownPaths.Arr) to Pred(fArchiveStructure.KnownPaths.Count) do
  If IndexOfKnownPath(fArchiveStructure.KnownPaths.Arr[i].Path,KnownPaths) < 0 then
    begin
      KnownPaths.Arr[KnownPaths.Count] := fArchiveStructure.KnownPaths.Arr[i];
      Inc(Result);
      Inc(KnownPaths.Count);
    end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                        TDARTRepairer_SCS_ProcessingBase
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_SCS_ProcessingBase - method indexing constants
===============================================================================}

const
  DART_METHOD_ID_SCS_PROC_ARCHPROC = $01000200;

{===============================================================================
    TDARTRepairer_SCS_ProcessingBase - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTRepairer_SCS_ProcessingBase - protected methods
-------------------------------------------------------------------------------}

procedure TDARTRepairer_SCS_ProcessingBase.SCS_SaveEntryAsUnresolved(EntryIdx: Integer; Data: Pointer; Size: TMemSize; ProgressInfo: TDART_PSI);
var
  EntryFileName:    String;
  EntryFileStream:  TFileStream;
begin
If (EntryIdx >= Low(fArchiveStructure.Entries.Arr)) and (EntryIdx < fArchiveStructure.Entries.Count) then
  with fArchiveStructure.Entries.Arr[EntryIdx] do
    begin
      If fProcessingSettings.PathResolve.ExtractedUnresolvedEntries then  // save only when required
        begin
          // raw data will be saved to a file
          DoWarning(Format('File name of entry #%d (0x%.16x) could not be resolved, extracting entry data.',
                           [EntryIdx,BinPart.Hash,BinPart.UncompressedSize]));
          If Length(ExtractFileName(fArchiveProcessingSettings.Common.TargetPath)) > 0 then
            // target is file
            EntryFileName := IncludeTrailingPathDelimiter(ExtractFilePath(fArchiveProcessingSettings.Common.TargetPath) + 'unresolved_' +
              ChangeFileExt(ExtractFileName(fArchiveProcessingSettings.Common.TargetPath),'')) + Format('%s(%.16x)',[SCS_HashName,BinPart.Hash])
          else
            // target is directory
            EntryFileName := IncludeTrailingPathDelimiter(ExtractFilePath(fArchiveProcessingSettings.Common.TargetPath) + '_unresolved_') +
              Format('%s(%.16x)',[SCS_HashName,BinPart.Hash]);
          If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
            EntryFileName := EntryFileName + 'D'
          else
            EntryFileName := EntryFileName + 'F';
          DART_ForceDirectories(ExtractFileDir(EntryFileName));
          EntryFileStream := TFileStream.Create(StrToRTL(EntryFileName),fmCreate or fmShareDenyWrite);
          try
            ProgressedStreamWrite(EntryFileStream,Data,Size,ProgressInfo);
            // finalize
            EntryFileStream.Size := EntryFileStream.Position;
          finally
            EntryFileStream.Free;
          end;
        end
      // data will not be extracted...
      else DoWarning(Format('File name of entry #%d (0x%.16x) could not be resolved.',[EntryIdx,BinPart.Hash]));
    end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS_ProcessingBase.SCS_PrepareEntriesProgress;
var
  i,Index:  Integer;
  CurrNode: TProgressTracker;
begin
DoProgress(fProcessingProgNode,PSIDX_C_EntriesProgressPrep,0.0);
fEntriesProcessingProgNode.BeginUpdate;
try
  For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
    with fArchiveStructure.Entries.Arr[i] do
      begin
        Index := fEntriesProcessingProgNode.Add(BinPart.CompressedSize,DART_PROGSTAGE_IDX_SCS_EntryProcessing);
        CurrNode := fEntriesProcessingProgNode.StageObjects[Index];
        CurrNode.BeginUpdate;
        try
          case fArchiveProcessingSettings.Common.RepairMethod of
            rmRebuild:  begin
                          If not(GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) and UtilityData.Resolved) then
                            DART_PROGSTAGE_IDX_SCS_EntryLoading := CurrNode.Add(30)
                          else
                            DART_PROGSTAGE_IDX_SCS_EntryLoading := CurrNode.Add(0);

                          {
                            decompression progres is required when:
                              - entry is a resolved directory (it might need compression, decomp. prog. is used there)
                              - entry is compressed and original CRC32 is ignored or the entry is not resolved
                          }
                          If (GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) and UtilityData.Resolved) or
                             ((fProcessingSettings.Entry.IgnoreCRC32 or not UtilityData.Resolved) and
                             GetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed)) then
                            DART_PROGSTAGE_IDX_SCS_EntryDecompression := CurrNode.Add(30)
                          else
                            DART_PROGSTAGE_IDX_SCS_EntryDecompression := CurrNode.Add(0);

                          DART_PROGSTAGE_IDX_SCS_EntrySaving := CurrNode.Add(30);
                        end;
            rmExtract:  begin
                          If not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) or not UtilityData.Resolved then
                            DART_PROGSTAGE_IDX_SCS_EntryLoading := CurrNode.Add(30)
                          else
                            DART_PROGSTAGE_IDX_SCS_EntryLoading := CurrNode.Add(0);

                          {
                            decompression progres is required when:
                              - entry is compressed, not resolved and extraction of unresolved entries is active
                              - entry is compressed and it is a file (not a directory)
                          }
                          If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed) and
                             (not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) or
                             (not UtilityData.Resolved and fProcessingSettings.PathResolve.ExtractedUnresolvedEntries)) then
                            DART_PROGSTAGE_IDX_SCS_EntryDecompression := CurrNode.Add(30)
                          else
                            DART_PROGSTAGE_IDX_SCS_EntryDecompression := CurrNode.Add(0);

                          If not UtilityData.Resolved or not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
                            DART_PROGSTAGE_IDX_SCS_EntrySaving := CurrNode.Add(30)
                          else
                            DART_PROGSTAGE_IDX_SCS_EntrySaving := CurrNode.Add(0);    
                        end;
            rmConvert:  begin
                          If not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
                            DART_PROGSTAGE_IDX_SCS_EntryLoading := CurrNode.Add(30)
                          else
                            DART_PROGSTAGE_IDX_SCS_EntryLoading := CurrNode.Add(0);

                          {
                            decompression progres is required when:
                              - entry is compressed, resolved but original CRC32 was ignored
                              - entry id compressed, not resolved and extraction of unresolved entries is active
                          }
                          If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed) and
                             ((fProcessingSettings.PathResolve.ExtractedUnresolvedEntries and not UtilityData.Resolved) or
                             fProcessingSettings.Entry.IgnoreCRC32) then
                            DART_PROGSTAGE_IDX_SCS_EntryDecompression := CurrNode.Add(30)
                          else
                            DART_PROGSTAGE_IDX_SCS_EntryDecompression := CurrNode.Add(0);

                          If (not UtilityData.Resolved and fProcessingSettings.PathResolve.ExtractedUnresolvedEntries) or
                            not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
                            DART_PROGSTAGE_IDX_SCS_EntrySaving := CurrNode.Add(30)
                          else
                            DART_PROGSTAGE_IDX_SCS_EntrySaving := CurrNode.Add(1);  // only header is saved
                        end;
          end;
          // assign obtained indices to shorter named-variables
          PSIDX_C_EntryProcessing    := DART_PROGSTAGE_IDX_SCS_EntryProcessing;
          PSIDX_C_EntryLoading       := DART_PROGSTAGE_IDX_SCS_EntryLoading;
          PSIDX_C_EntryDecompression := DART_PROGSTAGE_IDX_SCS_EntryDecompression;
          PSIDX_C_EntrySaving        := DART_PROGSTAGE_IDX_SCS_EntrySaving;
        finally
          CurrNode.EndUpdate;
        end;
        DoProgress(fProcessingProgNode,PSIDX_C_EntriesProgressPrep,(i + 1) / fArchiveStructure.Entries.Count);
      end;
finally
  fEntriesProcessingProgNode.EndUpdate;
end;
DoProgress(fProcessingProgNode,PSIDX_C_EntriesProgressPrep,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS_ProcessingBase.ArchiveProcessing;
begin
// check if target <> source
If AnsiSameText(fArchiveProcessingSettings.Common.ArchivePath,fArchiveProcessingSettings.Common.TargetPath) then
  DoError(DART_METHOD_ID_SCS_PROC_ARCHPROC,'Output is directed into an input archive, cannot proceed.');
inherited;
SCS_PrepareEntriesProgress;
end;

{-------------------------------------------------------------------------------
    TDARTRepairer_SCS_ProcessingBase - public methods
-------------------------------------------------------------------------------}

class Function TDARTRepairer_SCS_ProcessingBase.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  DART_METHOD_ID_SCS_PROC_ARCHPROC: Result := 'ArchiveProcessing';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

end.
