unit DART_Repairer_SCS;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes,
  DART_Common, DART_ProcessingSettings, DART_Format_SCS, DART_Repairer;

const
  // progress stages
  DART_PROGSTAGE_ID_SCS_ArchiveHeaderLoading  = DART_PROGSTAGE_ID_MAX + 1;
  DART_PROGSTAGE_ID_SCS_EntriesLoading        = DART_PROGSTAGE_ID_MAX + 2;
  DART_PROGSTAGE_ID_SCS_PathsResolving        = DART_PROGSTAGE_ID_MAX + 3;
  DART_PROGSTAGE_ID_SCS_EntriesProgressPrep   = DART_PROGSTAGE_ID_MAX + 4;
  DART_PROGSTAGE_ID_SCS_EntriesProcessing     = DART_PROGSTAGE_ID_MAX + 5;

  DART_PROGSTAGE_ID_SCS_PathsRes_Local        = DART_PROGSTAGE_ID_MAX + 10;
  DART_PROGSTAGE_ID_SCS_PathsRes_HelpFiles    = DART_PROGSTAGE_ID_MAX + 11;
  DART_PROGSTAGE_ID_SCS_PathsRes_ParseContent = DART_PROGSTAGE_ID_MAX + 12;
  DART_PROGSTAGE_ID_SCS_PathsRes_BruteForce   = DART_PROGSTAGE_ID_MAX + 13;
  DART_PROGSTAGE_ID_SCS_PathsRes_Reconstruct  = DART_PROGSTAGE_ID_MAX + 14;

  DART_PROGSTAGE_ID_SCS_EntryProcessing       = DART_PROGSTAGE_ID_MAX + 20;
  DART_PROGSTAGE_ID_SCS_EntryLoading          = DART_PROGSTAGE_ID_MAX + 21;
  DART_PROGSTAGE_ID_SCS_EntryDecompression    = DART_PROGSTAGE_ID_MAX + 22;
  DART_PROGSTAGE_ID_SCS_EntrySaving           = DART_PROGSTAGE_ID_MAX + 23;

  DART_PROGSTAGE_ID_SCS_Max                   = DART_PROGSTAGE_ID_MAX + 100;

  PSID_C_ArchiveHeaderLoading  = DART_PROGSTAGE_ID_SCS_ArchiveHeaderLoading;
  PSID_C_EntriesLoading        = DART_PROGSTAGE_ID_SCS_EntriesLoading;
  PSID_C_PathsResolving        = DART_PROGSTAGE_ID_SCS_PathsResolving;
  PSID_C_EntriesProgressPrep   = DART_PROGSTAGE_ID_SCS_EntriesProgressPrep;
  PSID_C_EntriesProcessing     = DART_PROGSTAGE_ID_SCS_EntriesProcessing;

  PSID_C_PathsRes_Local        = DART_PROGSTAGE_ID_SCS_PathsRes_Local;
  PSID_C_PathsRes_HelpFiles    = DART_PROGSTAGE_ID_SCS_PathsRes_HelpFiles;
  PSID_C_PathsRes_ParseContent = DART_PROGSTAGE_ID_SCS_PathsRes_ParseContent;
  PSID_C_PathsRes_BruteForce   = DART_PROGSTAGE_ID_SCS_PathsRes_BruteForce;
  PSID_C_PathsRes_Reconstruct  = DART_PROGSTAGE_ID_SCS_PathsRes_Reconstruct;

  PSID_C_EntryProcessing       = DART_PROGSTAGE_ID_SCS_EntryProcessing;
  PSID_C_EntryLoading          = DART_PROGSTAGE_ID_SCS_EntryLoading;
  PSID_C_EntryDecompression    = DART_PROGSTAGE_ID_SCS_EntryDecompression;
  PSID_C_EntrySaving           = DART_PROGSTAGE_ID_SCS_EntrySaving;


type
  TDARTRepairer_SCS = class(TDARTRepairer)
  protected
    fProcessingSettings:  TDART_PS_SCS;
    fArchiveStructure:    TDART_SCS_ArchiveStructure;
    fEntriesSorted:       Boolean;
    // initialization methods
    procedure InitializeProcessingSettings; override;
    procedure InitializeData; override;
    procedure InitializeProgress; override;
    // methods for content parsing
    Function IndexOfEntry(const EntryFileName: AnsiString): Integer; override;
    Function GetEntryData(EntryIndex: Integer; out Data: Pointer; out Size: TMemSize): Boolean; override;
    // processing methods
    procedure ArchiveProcessing; override;
    // scs specific routines
    Function SCS_EntryFileNameHash(const EntryFileName: AnsiString): UInt64; virtual;
    Function SCS_HashName: String; virtual;
    Function SCS_HashCompare(A,B: UInt64): Integer; virtual;
    Function SCS_IndexOfEntry(Hash: UInt64): Integer; virtual;
    procedure SCS_SortEntries; virtual;
    Function SCS_KnownPaths_IndexOf(const Path: AnsiString): Integer; virtual;
    Function SCS_KnownPaths_Add(const Path: AnsiString): Integer; virtual;
    procedure SCS_LoadArchiveHeader; virtual;
    procedure SCS_LoadEntries; virtual;
    procedure SCS_AssignPaths; virtual;
    procedure SCS_DiscardDirectories; virtual;
    procedure SCS_ReconstructDirectories; virtual;
    procedure SCS_ResolvePaths; virtual; 
    procedure SCS_ResolvePaths_Local; virtual;
    procedure SCS_ResolvePaths_HelpFiles; virtual;
    procedure SCS_ResolvePaths_ParseContent; virtual;
    procedure SCS_ResolvePaths_BruteForce; virtual;
    procedure SCS_ResolvePaths_Reconstruct; virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
    constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings; CatchExceptions: Boolean);
    Function GetAllKnownPaths(var KnownPaths: TDART_KnownPaths): Integer; override;
    property ArchiveStructure: TDART_SCS_ArchiveStructure read fArchiveStructure;
  end;

  TDARTRepairer_SCS_ProcessingBase = class(TDARTRepairer_SCS)
  protected
    procedure SCS_SaveUnresolvedEntry(EntryIdx: Integer; Data: Pointer; Size: TMemSize); virtual;
    procedure SCS_PrepareEntriesProgress; virtual;
    procedure ArchiveProcessing; override;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;    
  end;
  
implementation

uses
  SysUtils, Classes,
  City, BitOps, CRC32, StrRect, MemoryBuffer, StaticMemoryStream,
  ExplicitStringLists, ProgressTracker, ZLibCommon,
  DART_Auxiliary, DART_PathDeconstructor, DART_Repairer_ZIP;

const
  DART_METHOD_ID_SCS_ARCHPROC   = 0200;
  DART_METHOD_ID_SCS_SGETENTRY  = 0201;
  DART_METHOD_ID_SCS_SENTRFNHS  = 0202;
  DART_METHOD_ID_SCS_SSRTENQSEX = 0203;
  DART_METHOD_ID_SCS_SLDARHEAD  = 0204;
  DART_METHOD_ID_SCS_SLDPLOCLP  = 0205;

procedure TDARTRepairer_SCS.InitializeProcessingSettings;
begin
inherited;
fProcessingSettings := fArchiveProcessingSettings.SCS;
RectifySCSProcessingSettings(fProcessingSettings);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.InitializeData;
begin
inherited;
FillChar(fArchiveStructure.ArchiveHeader,SizeOf(TDART_SCS_ArchiveHeader),0);
SetLength(fArchiveStructure.Entries.Arr,0);
fArchiveStructure.Entries.Count := 0;
SetLength(fArchiveStructure.KnownPaths.Arr,0);
fArchiveStructure.KnownPaths.Count := 0;
fArchiveStructure.UtilityData.UnresolvedCount := 0;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.InitializeProgress;
var
  Index:          Integer;
  ProcessingNode: TProgressTracker;
  PathsResNode:   TProgressTracker;
begin
inherited;
Index := fProgressTracker.IndexOf(DART_PROGSTAGE_ID_Processing);
If Index >= 0 then
  begin
    ProcessingNode := fProgressTracker.StageObjects[Index];
    // set progress info for loading of archive header
    ProcessingNode.Add(10,DART_PROGSTAGE_ID_SCS_ArchiveHeaderLoading);
    // loading of entries
    ProcessingNode.Add(40,DART_PROGSTAGE_ID_SCS_EntriesLoading);
    // paths resolving
    Index := ProcessingNode.Add(50,DART_PROGSTAGE_ID_SCS_PathsResolving);
    PathsResNode := ProcessingNode.StageObjects[Index];
    // preparing progress of entries processing
    ProcessingNode.Add(20,DART_PROGSTAGE_ID_SCS_EntriesProgressPrep);    
    // entries processing
    Index := ProcessingNode.Add(900,DART_PROGSTAGE_ID_SCS_EntriesProcessing);
    fEntriesProcProgNode := ProcessingNode.StageObjects[Index];
    // individual stages of paths resolving
    // local
    PathsResNode.Add(20,DART_PROGSTAGE_ID_SCS_PathsRes_Local);
    // help files
    PathsResNode.Add(20,DART_PROGSTAGE_ID_SCS_PathsRes_HelpFiles);
    // parse content
    PathsResNode.Add(20,DART_PROGSTAGE_ID_SCS_PathsRes_ParseContent);
    // brute force
    PathsResNode.Add(30,DART_PROGSTAGE_ID_SCS_PathsRes_BruteForce);
    // reconstruct dirs
    PathsResNode.Add(10,DART_PROGSTAGE_ID_SCS_PathsRes_Reconstruct);
  end
else raise Exception.Create('TDARTRepairer_SCS.InitializeProgress: Processing progress node not found.');
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.IndexOfEntry(const EntryFileName: AnsiString): Integer;
begin
inherited;
Result := SCS_IndexOfEntry(SCS_EntryFileNameHash(EntryFileName));
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.GetEntryData(EntryIndex: Integer; out Data: Pointer; out Size: TMemSize): Boolean;
var
  DecompressedBuff: Pointer;
  DecompressedSize: TMemSize;
begin
inherited;
Result := False;
If (EntryIndex >= Low(fArchiveStructure.Entries.Arr)) and (EntryIndex < fArchiveStructure.Entries.Count) then
  begin
    with fArchiveStructure.Entries.Arr[EntryIndex] do
      If not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
        begin
          // entry is a file
          // prepare buffer that will hold entry data
          ReallocBufferKeep(fBuffer_Entry,BinPart.CompressedSize);
          // load data from input file
          fInputArchiveStream.Seek(BinPart.DataOffset,soBeginning);
          ProgressedStreamRead(fInputArchiveStream,fBuffer_Entry.Memory,BinPart.CompressedSize,[DART_PROGSTAGE_ID_NoProgress]);
          If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed) then
            begin
              // data are compressed, decompress them
              ProgressedDecompressBuffer(fBuffer_Entry.Memory,BinPart.CompressedSize,
                DecompressedBuff,DecompressedSize,WBITS_ZLIB,[DART_PROGSTAGE_ID_NoProgress]);
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
else DoError(DART_METHOD_ID_SCS_SGETENTRY,'Entry index (%d) out of bounds.',[EntryIndex]);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.ArchiveProcessing;
begin
inherited;
SCS_LoadArchiveHeader;
SCS_LoadEntries;
If fArchiveStructure.Entries.Count <= 0 then
  DoError(DART_METHOD_ID_SCS_ARCHPROC,'Input file does not contain any valid entries.');
// following step is optional at this point, but provides better performance
SCS_SortEntries;
SCS_ResolvePaths;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.SCS_EntryFileNameHash(const EntryFileName: AnsiString): UInt64;
begin
Result := 0;
case fArchiveStructure.ArchiveHeader.HashType of
  DART_SCS_HASH_City: Result := CityHash64(PAnsiChar(EntryFileName),Length(EntryFileName) * SizeOf(AnsiChar));
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

Function TDARTRepairer_SCS.SCS_HashCompare(A,B: UInt64): Integer;
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

Function TDARTRepairer_SCS.SCS_IndexOfEntry(Hash: UInt64): Integer;
var
  i,Min,Max:  Integer;
begin
Result := -1;
If fEntriesSorted then
  begin
    Min := 0;
    max := Pred(fArchiveStructure.Entries.Count);
    while Max >= min do
      begin
        i := ((max - Min) shr 1) + Min;
        // i-th entry has lower hash than is requested
        If SCS_HashCompare(fArchiveStructure.Entries.Arr[i].BinPart.Hash,Hash) > 0 then Min := i + 1
          // i-th entry has higher hash than is requested
          else If SCS_HashCompare(fArchiveStructure.Entries.Arr[i].BinPart.Hash,Hash) < 0 then Max := i - 1
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
      If SCS_HashCompare(fArchiveStructure.Entries.Arr[i].BinPart.Hash,Hash) = 0 then
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
    Pivot:  UInt64;
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
    DoProgress([DART_PROGSTAGE_ID_NoProgress],0.0); // must be called at the start of this routine
    If LeftIdx < RightIdx then
      begin
        ExchangeEntries((LeftIdx + RightIdx) shr 1,RightIdx);
        Pivot := fArchiveStructure.Entries.Arr[RightIdx].BinPart.Hash;
        Idx := LeftIdx;
        For i := LeftIdx to Pred(RightIdx) do
          If SCS_HashCompare(Pivot,fArchiveStructure.Entries.Arr[i].BinPart.Hash) < 0 then
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

Function TDARTRepairer_SCS.SCS_KnownPaths_IndexOf(const Path: AnsiString): Integer;
var
  i:        Integer;
  PathHash: UInt64;
begin
Result := -1;
PathHash := SCS_EntryFileNameHash(Path);
For i := Low(fArchiveStructure.KnownPaths.Arr) to Pred(fArchiveStructure.KnownPaths.Count) do
  If SCS_HashCompare(PathHash,fArchiveStructure.KnownPaths.Arr[i].Hash64) = 0 then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.SCS_KnownPaths_Add(const Path: AnsiString): Integer;
begin
Result := SCS_KnownPaths_IndexOf(Path);
If Result < 0 then
  begin
    If fArchiveStructure.KnownPaths.Count >= Length(fArchiveStructure.KnownPaths.Arr) then
      SetLength(fArchiveStructure.KnownPaths.Arr,Length(fArchiveStructure.KnownPaths.Arr) + 1024);
    fArchiveStructure.KnownPaths.Arr[fArchiveStructure.KnownPaths.Count].Path := Path;
    fArchiveStructure.KnownPaths.Arr[fArchiveStructure.KnownPaths.Count].Directory := False;
    fArchiveStructure.KnownPaths.Arr[fArchiveStructure.KnownPaths.Count].Hash := AnsiStringCRC32(AnsiLowerCase(Path));
    fArchiveStructure.KnownPaths.Arr[fArchiveStructure.KnownPaths.Count].Hash64 := SCS_EntryFileNameHash(Path);
    Inc(fArchiveStructure.KnownPaths.Count);
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_LoadArchiveHeader;
begin
DoProgress([PSID_Processing,PSID_C_ArchiveHeaderLoading],0.0);
If fInputArchiveStream.Size >= SizeOf(TDART_SCS_ArchiveHeader) then
  begin
    fInputArchiveStream.Seek(0,soBeginning);
    fInputArchiveStream.ReadBuffer(fArchiveStructure.ArchiveHeader,SizeOf(TDART_SCS_ArchiveHeader));
    If fArchiveProcessingSettings.Common.IgnoreFileSignature then
      fArchiveStructure.ArchiveHeader.Signature := DART_SCS_FileSignature;
    If fProcessingSettings.PathResolve.AssumeCityHash then
      fArchiveStructure.ArchiveHeader.HashType := DART_SCS_HASH_City
    else
      If fArchiveStructure.ArchiveHeader.HashType <> DART_SCS_HASH_City then
        DoError(DART_METHOD_ID_SCS_SLDARHEAD,'Unsupported hash algorithm (0x%.8x).',[fArchiveStructure.ArchiveHeader.HashType]);
  end
else DoError(DART_METHOD_ID_SCS_SLDARHEAD,'File is too small (%d bytes) to contain a valid archive header.',[fInputArchiveStream.Size]);
DoProgress([PSID_Processing,PSID_C_ArchiveHeaderLoading],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_LoadEntries;
var
  Entries:  TStaticMemoryStream;

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
          DoProgress([PSID_Processing,PSID_C_EntriesLoading],(i + 1) / fArchiveStructure.Entries.Count);
        end;
  end;

begin
DoProgress([PSID_Processing,PSID_C_EntriesLoading],1.0);
SetLength(fArchiveStructure.Entries.Arr,fArchiveStructure.ArchiveHeader.EntryCount);
fArchiveStructure.Entries.Count := Length(fArchiveStructure.Entries.Arr);
fInputArchiveStream.Seek(fArchiveStructure.ArchiveHeader.EntriesOffset,soBeginning);
If fProcessingSettings.EntryTabToMem then
  begin
    // load entire entry table to memory to speed things up
    ReallocBufferKeep(fBuffer_Entry,fArchiveStructure.ArchiveHeader.EntryCount * SizeOf(TDART_SCS_EntryRecord));
    fInputArchiveStream.ReadBuffer(fBuffer_Entry.Memory^,fArchiveStructure.ArchiveHeader.EntryCount * SizeOf(TDART_SCS_EntryRecord));
    Entries := TStaticMemoryStream.Create(fBuffer_Entry.Memory,fArchiveStructure.ArchiveHeader.EntryCount * SizeOf(TDART_SCS_EntryRecord));
    try
      Entries.Seek(0,soBeginning);
      LoadEntriesFromStream(Entries);
    finally
      Entries.Free;
    end;
  end
else LoadEntriesFromStream(fInputArchiveStream); 
DoProgress([PSID_Processing,PSID_C_EntriesLoading],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_AssignPaths;
var
  i,Index:  Integer;
begin
For i := Low(fArchiveStructure.KnownPaths.Arr) to Pred(fArchiveStructure.KnownPaths.Count) do
  begin
    Index := SCS_IndexOfEntry(fArchiveStructure.KnownPaths.Arr[i].Hash64);
    If Index >= 0 then
      If not fArchiveStructure.Entries.Arr[Index].UtilityData.Resolved then
        begin
          fArchiveStructure.Entries.Arr[Index].FileName := fArchiveStructure.KnownPaths.Arr[i].Path;
          fArchiveStructure.Entries.Arr[Index].UtilityData.Resolved := True;
        end;
    DoProgress([DART_PROGSTAGE_ID_NoProgress],0.0);  
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
      DoProgress([DART_PROGSTAGE_ID_NoProgress],0.0);
    end;
fArchiveStructure.Entries.Count := EntryCount;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ReconstructDirectories;
var
  PathDeconstructor:  TDARTPathDeconstructor;
  i,j:                Integer;
begin
PathDeconstructor := TDARTPathDeconstructor.Create(DART_SCS_PathDelim);
try
  // deconstruct path of all resolved entries
  For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
    begin
      If fArchiveStructure.Entries.Arr[i].UtilityData.Resolved then
        PathDeconstructor.DeconstructPath(fArchiveStructure.Entries.Arr[i].FileName);
      // this is the slowest part of path reconstruction, so progress is done here
      DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_Reconstruct],(i + 1) / fArchiveStructure.Entries.Count);
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
        DoProgress([DART_PROGSTAGE_ID_NoProgress],0.0);
      end;
  fArchiveStructure.Entries.Count := fArchiveStructure.Entries.Count + PathDeconstructor.Count;
finally
  PathDeconstructor.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths;
var
  i:  Integer;
begin
DoProgress([PSID_Processing,PSID_C_PathsResolving],0.0);
fArchiveStructure.KnownPaths.Count := 0;
// add root
SCS_KnownPaths_Add(DART_SCS_PATHS_Root);
// add predefined paths
If fProcessingSettings.PathResolve.UsePredefinedPaths then
  For i := Low(DART_SCS_PATHS_Predefined) to High(DART_SCS_PATHS_Predefined) do
    SCS_KnownPaths_Add(DART_SCS_PATHS_Predefined[i]);
// add custom paths
with fProcessingSettings.PathResolve do
  For i := Low(CustomPaths) to High(CustomPaths) do
    SCS_KnownPaths_Add(CustomPaths[i]);
// load all paths stored in the archive
SCS_ResolvePaths_Local;
// load paths from help files
If Length(fProcessingSettings.PathResolve.HelpFiles) > 0 then
  SCS_ResolvePaths_HelpFiles;
// parse content of processed archive
If fProcessingSettings.PathResolve.ParseContent then
  SCS_ResolvePaths_ParseContent;
// bruteforce resolve  
If fProcessingSettings.PathResolve.BruteForce then
  SCS_ResolvePaths_BruteForce;
SCS_ResolvePaths_Reconstruct;
DoProgress([PSID_Processing,PSID_C_PathsResolving],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_Local;
var
  i:                  Integer;
  DirectoryList:      TAnsiStringList;
  CurrentLevel:       TAnsiStringList;
  EntryLines:         TAnsiStringList;  // used inside of nested function LoadPath
  DirCount:           Integer;
  ProcessedDirCount:  Integer;

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
                ProgressedDecompressBuffer(fBuffer_Entry.Memory,BinPart.CompressedSize,OutBuff,OutSize,WBITS_ZLIB,[DART_PROGSTAGE_ID_NoProgress]);
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
                      If Path <> '' then
                        Directories.Add(Path + DART_SCS_PathDelim + Copy(EntryLines[ii],2,Length(EntryLines[ii])))
                      else
                        Directories.Add(Copy(EntryLines[ii],2,Length(EntryLines[ii])));
                      SCS_KnownPaths_Add(Directories[Pred(Directories.Count)]);
                    end
                  else SCS_KnownPaths_Add(Path + DART_SCS_PathDelim + EntryLines[ii]); // file
                end;
            Inc(ProcessedDirCount);
            If DirCount > 0 then
              DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_Local],ProcessedDirCount / DirCount);
          end;
  end;

begin
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_Local],0.0);
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
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_Local],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_HelpFiles;
var
  i:  Integer;

  procedure LoadHelpFile(const FileName: String);
  var
    HelpFileProcSettings: TDARTArchiveProcessingSettings;
    HelpFileRepairer:     TDARTRepairer;
    ii:                   Integer;
    TempKnownPaths:       TDART_KnownPaths;
  begin
  {
    Creates local basic repairer that will load all possible paths from a help
    file without doing anything with it.
    Also, for SCS# archives, all paths found to this moment are passed as custom
    paths to the repairer.
    What repairer should be used is discerned by a file signature, when it
    matches signature of SCS# archive, then SCS repairer is used, otherwise ZIP
    repairer is used.
  }
    // init processing settings for local repairer
    HelpFileProcSettings := DART_DefaultArchiveProcessingSettings;
    HelpFileProcSettings.Common.ArchivePath := FileName;
    // explicitly turn off in-memory processing, as we do not know size of the file
    HelpFileProcSettings.Common.InMemoryProcessing := False;
    // do archive-type-specific processing
    case DART_GetFileSignature(FileName) of
      DART_SCS_FileSignature:   // - - - - - - - - - - - - - - - - - - - - - - -
        begin
          // SCS# archive
          // file signature must be checked because we assume it is SCS# format
          HelpFileProcSettings.Common.IgnoreFileSignature := False;
          // prepare all already known paths
          SetLength(HelpFileProcSettings.SCS.PathResolve.CustomPaths,fArchiveStructure.KnownPaths.Count);
          For ii := Low(fArchiveStructure.KnownPaths.Arr) to Pred(fArchiveStructure.KnownPaths.Count) do
            HelpFileProcSettings.SCS.PathResolve.CustomPaths[ii] := fArchiveStructure.KnownPaths.Arr[ii].Path;            
          HelpFileRepairer := TDARTRepairer_SCS.Create(fPauseControlObject,HelpFileProcSettings,False);
          try
            HelpFileRepairer.OnProgress := ForwardedProgressHandler;
            HelpFileRepairer.Heartbeat := fHeartbeat;
            HelpFileRepairer.Run;
            TempKnownPaths.Count := 0;
            If HelpFileRepairer.GetAllKnownPaths(TempKnownPaths) > 0 then
              For ii := Low(TempKnownPaths.Arr) to Pred(TempKnownPaths.Count) do
                SCS_KnownPaths_Add(TempKnownPaths.Arr[ii].Path);
          finally
            HelpFileRepairer.Free;
          end;
        end;
    else
     {DART_ZIP_FileSignature:}  // - - - - - - - - - - - - - - - - - - - - - - -
      // other archive types (ZIP)
      HelpFileRepairer := TDARTRepairer_ZIP.Create(fPauseControlObject,HelpFileProcSettings,False);
      try
        HelpFileRepairer.OnProgress := ForwardedProgressHandler;
        HelpFileRepairer.Heartbeat := fHeartbeat;
        HelpFileRepairer.Run;
        TempKnownPaths.Count := 0;
        If HelpFileRepairer.GetAllKnownPaths(TempKnownPaths) > 0 then
          For ii := Low(TempKnownPaths.Arr) to Pred(TempKnownPaths.Count) do
            SCS_KnownPaths_Add(TempKnownPaths.Arr[ii].Path);
      finally
        HelpFileRepairer.Free;
      end;
    end;
  end;

begin
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_HelpFiles],0.0);
If fArchiveStructure.UtilityData.UnresolvedCount > 0 then
  begin
    For i := Low(fProcessingSettings.PathResolve.HelpFiles) to High(fProcessingSettings.PathResolve.HelpFiles) do
      begin
        If Length(fProcessingSettings.PathResolve.HelpFiles[i]) > 0 then
          LoadHelpFile(fProcessingSettings.PathResolve.HelpFiles[i]);
        DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_HelpFiles],
                   (i + 1) / Length(fProcessingSettings.PathResolve.HelpFiles));
        SCS_AssignPaths;
        If fArchiveStructure.UtilityData.UnresolvedCount <= 0 then
          Break{For i}; // all entries are resolved, no need to continue
      end;
  end;
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_HelpFiles],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_ParseContent;
begin
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_ParseContent],0.0);
If fArchiveStructure.UtilityData.UnresolvedCount > 0 then
  begin
    DoError(-1,'Content parsing is not implemented in this build.');
    SCS_AssignPaths;
  end;
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_ParseContent],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_BruteForce;
begin
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_BruteForce],0.0);
If fArchiveStructure.UtilityData.UnresolvedCount > 0 then
  begin
    DoError(-1,'Brute-force resolve is not implemented in this build.');
  end;
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_BruteForce],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_Reconstruct;
begin
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_Reconstruct],0.0);
If fArchiveStructure.UtilityData.UnresolvedCount > 0 then
  begin
    // reconstruct all directory entries from file names
    fEntriesSorted := False;
    SCS_DiscardDirectories;
    SCS_ReconstructDirectories;
    SCS_SortEntries;
  end;
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_Reconstruct],1.0);
end;

//==============================================================================

class Function TDARTRepairer_SCS.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  DART_METHOD_ID_SCS_ARCHPROC:   Result := 'ArchiveProcessing';
  DART_METHOD_ID_SCS_SGETENTRY:  Result := 'GetEntryData(Index)';
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
fExpectedSignature := DART_SCS_FileSignature;
fEntriesSorted := False;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.GetAllKnownPaths(var KnownPaths: TDART_KnownPaths): Integer;
var
  i:  Integer;
begin
inherited;
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

//******************************************************************************

const
  DART_METHOD_ID_SCS_PROC_ARCHPROC = 0;

procedure TDARTRepairer_SCS_ProcessingBase.SCS_SaveUnresolvedEntry(EntryIdx: Integer; Data: Pointer; Size: TMemSize);
var
  EntryFileName:    String;
  EntryFileStream:  TFileStream;
begin
If (EntryIdx >= Low(fArchiveStructure.Entries.Arr)) and (EntryIdx < fArchiveStructure.Entries.Count) then
  with fArchiveStructure.Entries.Arr[EntryIdx] do
    If not UtilityData.Resolved then  // save only unresolved entries...
      begin
        If fProcessingSettings.PathResolve.ExtractedUnresolvedEntries then  // ...and only when required
          begin
            // raw data will be saved to a file
            DoWarning(Format('File name of entry #%d (0x%.16x) could not be resolved, extracting entry data.',
                             [EntryIdx,BinPart.Hash,BinPart.UncompressedSize]));
            If ExtractFileName(fArchiveProcessingSettings.Common.TargetPath) <> '' then
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
              EntryFileStream.WriteBuffer(Data^,Size);
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
DoProgress([PSID_Processing,PSID_C_EntriesProgressPrep],0.0);
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  with fArchiveStructure.Entries.Arr[i] do
    begin
      Index := fEntriesProcProgNode.Add(BinPart.CompressedSize,DART_PROGSTAGE_ID_SCS_EntryProcessing);
      CurrNode := fEntriesProcProgNode.StageObjects[Index];
      case fArchiveProcessingSettings.Common.RepairMethod of
        rmRebuild:  begin
                      If not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
                        CurrNode.Add(30,DART_PROGSTAGE_ID_SCS_EntryLoading);
                      {$message 'check when implementing'}  
                      // need decompressed data for crc32 calculation, unresolved entry saving or directory entry compression/crc32
                      If (fProcessingSettings.Entry.IgnoreCRC32 and GetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed)) or
                         (not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) and not UtilityData.Resolved) or
                         GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
                        CurrNode.Add(30,DART_PROGSTAGE_ID_SCS_EntryDecompression);
                      CurrNode.Add(30,DART_PROGSTAGE_ID_SCS_EntrySaving);
                    end;
        rmExtract:  begin
                      If not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
                        CurrNode.Add(30,DART_PROGSTAGE_ID_SCS_EntryLoading);
                      // need decompression for compressed file or unresolved entry
                      If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed) and
                         (not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) or not UtilityData.Resolved) then
                        CurrNode.Add(30,DART_PROGSTAGE_ID_SCS_EntryDecompression);
                      CurrNode.Add(30,DART_PROGSTAGE_ID_SCS_EntrySaving);
                    end;
        rmConvert:  begin
                      If not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
                        CurrNode.Add(30,DART_PROGSTAGE_ID_SCS_EntryLoading);
                      // need decompression for compressed files and unresolved entries  
                      If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed) and
                         ((not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) and fProcessingSettings.Entry.IgnoreCRC32) or
                         not UtilityData.Resolved) then
                        CurrNode.Add(30,DART_PROGSTAGE_ID_SCS_EntryDecompression);
                      CurrNode.Add(30,DART_PROGSTAGE_ID_SCS_EntrySaving);  
                    end;
      end;
      DoProgress([PSID_Processing,PSID_C_EntriesProgressPrep],(i + 1) / fArchiveStructure.Entries.Count);    
    end;
DoProgress([PSID_Processing,PSID_C_EntriesProgressPrep],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS_ProcessingBase.ArchiveProcessing;
begin
// check if target <> source
If AnsiSameText(fArchiveProcessingSettings.Common.ArchivePath,fArchiveProcessingSettings.Common.TargetPath) then
  DoError(DART_METHOD_ID_SCS_PROC_ARCHPROC,'Output is directed into an input file, cannot proceed.');
inherited;
SCS_PrepareEntriesProgress;
end;

//==============================================================================

class Function TDARTRepairer_SCS_ProcessingBase.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  DART_METHOD_ID_SCS_PROC_ARCHPROC: Result := 'ArchiveProcessing';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

end.