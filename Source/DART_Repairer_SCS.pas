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
  DART_PROGSTAGE_ID_SCS_EntriesProcessing     = DART_PROGSTAGE_ID_MAX + 4;

  DART_PROGSTAGE_ID_SCS_PathsRes_Local        = DART_PROGSTAGE_ID_MAX + 10;
  DART_PROGSTAGE_ID_SCS_PathsRes_HelpFiles    = DART_PROGSTAGE_ID_MAX + 11;
  DART_PROGSTAGE_ID_SCS_PathsRes_ParseContent = DART_PROGSTAGE_ID_MAX + 12;
  DART_PROGSTAGE_ID_SCS_PathsRes_BruteForce   = DART_PROGSTAGE_ID_MAX + 13;
  DART_PROGSTAGE_ID_SCS_PathsRes_Reconstruct  = DART_PROGSTAGE_ID_MAX + 14;

  DART_PROGSTAGE_ID_SCS_EntryProcessing       = DART_PROGSTAGE_ID_MAX + 20;
  DART_PROGSTAGE_ID_SCS_EntryLoading          = DART_PROGSTAGE_ID_MAX + 21;
  DART_PROGSTAGE_ID_SCS_EntryDecompressing    = DART_PROGSTAGE_ID_MAX + 22;
  DART_PROGSTAGE_ID_SCS_EntrySaving           = DART_PROGSTAGE_ID_MAX + 23;

  DART_PROGSTAGE_ID_SCS_Max                   = DART_PROGSTAGE_ID_MAX + 100;

  PSID_C_ArchiveHeaderLoading  = DART_PROGSTAGE_ID_SCS_ArchiveHeaderLoading;
  PSID_C_EntriesLoading        = DART_PROGSTAGE_ID_SCS_EntriesLoading;
  PSID_C_PathsResolving        = DART_PROGSTAGE_ID_SCS_PathsResolving;
  PSID_C_EntriesProcessing     = DART_PROGSTAGE_ID_SCS_EntriesProcessing;

  PSID_C_PathsRes_Local        = DART_PROGSTAGE_ID_SCS_PathsRes_Local;
  PSID_C_PathsRes_HelpFiles    = DART_PROGSTAGE_ID_SCS_PathsRes_HelpFiles;
  PSID_C_PathsRes_ParseContent = DART_PROGSTAGE_ID_SCS_PathsRes_ParseContent;
  PSID_C_PathsRes_BruteForce   = DART_PROGSTAGE_ID_SCS_PathsRes_BruteForce;
  PSID_C_PathsRes_Reconstruct  = DART_PROGSTAGE_ID_SCS_PathsRes_Reconstruct;

  PSID_C_EntryProcessing       = DART_PROGSTAGE_ID_SCS_EntryProcessing;
  PSID_C_EntryLoading          = DART_PROGSTAGE_ID_SCS_EntryLoading;
  PSID_C_EntryDecompressing    = DART_PROGSTAGE_ID_SCS_EntryDecompressing;
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
    //procedure InitializeProgress; override;
    // methods for content parsing
    Function IndexOfEntry(const EntryFileName: AnsiString): Integer; override;
    //Function GetEntryData(EntryIndex: Integer; out Data: Pointer; out Size: TMemSize): Boolean; override;
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
    procedure SCS_ResolvePaths; virtual; 
    procedure SCS_ResolvePaths_Local; virtual;
    procedure SCS_ResolvePaths_HelpFiles; virtual;
    procedure SCS_ResolvePaths_ParseContent; virtual;
    procedure SCS_ResolvePaths_BruteForce; virtual;
    procedure SCS_ResolvePaths_Reconstruct; virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
    constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
    //Function GetAllKnownPaths(var KnownPaths: TDART_KnownPaths): Integer; override;
    property ArchiveStructure: TDART_SCS_ArchiveStructure read fArchiveStructure;
  end;

  TDARTRepairer_SCS_ProcessingBase = class(TDARTRepairer_SCS)
  end;
  
implementation

uses
  SysUtils, Classes,
  City, BitOps, CRC32;

const
  DART_METHOD_ID_SCS_ARCHPROC   = 0200;
  DART_METHOD_ID_SCS_SGETENTRY  = 0201;
  DART_METHOD_ID_SCS_SENTRFNHS  = 0202;
  DART_METHOD_ID_SCS_SSRTENQSEX = 0203;
  DART_METHOD_ID_SCS_SLDARHEAD  = 0204;

procedure TDARTRepairer_SCS.InitializeProcessingSettings;
begin
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
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_SCS.IndexOfEntry(const EntryFileName: AnsiString): Integer;
begin
Result := SCS_IndexOfEntry(SCS_EntryFileNameHash(EntryFileName));
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.ArchiveProcessing;
begin
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
  i,min,max:  Integer;
begin
Result := -1;
If fEntriesSorted then
  begin
    min := 0;
    max := Pred(fArchiveStructure.Entries.Count);
    while max >= min do
      begin
        i := ((max - min) shr 1) + min;
        // i-th entry has lower hash than is requested
        If SCS_HashCompare(fArchiveStructure.Entries.Arr[i].Bin.Hash,Hash) > 0 then Min := i + 1
          // i-th entry has higher hash than is requested
          else If SCS_HashCompare(fArchiveStructure.Entries.Arr[i].Bin.Hash,Hash) < 0 then Max := i - 1
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
      If fArchiveStructure.Entries.Arr[i].Bin.Hash = Hash then
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
        Pivot := fArchiveStructure.Entries.Arr[RightIdx].Bin.Hash;
        Idx := LeftIdx;
        For i := LeftIdx to Pred(RightIdx) do
          If SCS_HashCompare(Pivot,fArchiveStructure.Entries.Arr[i].Bin.Hash) < 0 then
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
  i:  Integer;
begin
DoProgress([PSID_Processing,PSID_C_EntriesLoading],1.0);
SetLength(fArchiveStructure.Entries.Arr,fArchiveStructure.ArchiveHeader.EntryCount);
fArchiveStructure.Entries.Count := Length(fArchiveStructure.Entries.Arr);
fInputArchiveStream.Seek(fArchiveStructure.ArchiveHeader.EntriesOffset,soBeginning);
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  with fArchiveStructure.Entries.Arr[i] do
    begin
      fInputArchiveStream.ReadBuffer(Bin,SizeOf(TDART_SCS_EntryRecord));
      FileName := '';
      UtilityData.Resolved := False;
      If fProcessingSettings.Entry.IgnoreCRC32 then
        Bin.CRC32 := 0;
      If fProcessingSettings.Entry.IgnoreCompressionFlag then
        SetFlagState(Bin.Flags,DART_SCS_FLAG_Compressed,Bin.UncompressedSize <> Bin.CompressedSize);
      DoProgress([PSID_Processing,PSID_C_EntriesLoading],(i + 1) / fArchiveStructure.Entries.Count);
    end;
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
SCS_AssignPaths;
// load paths from help files
SCS_ResolvePaths_HelpFiles;
SCS_AssignPaths;
// parse content of processed archive
//If fProcessingSettings.PathResolve.ParseContent then
//  SCS_ResolvePaths_ParseContent;
SCS_AssignPaths;
//If fProcessingSettings.PathResolve.BruteForceResolve then
//  SCS_ResolvePaths_BruteForce;
SCS_ResolvePaths_Reconstruct;
DoProgress([PSID_Processing,PSID_C_PathsResolving],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_Local;
begin
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_Local],0.0);
{$messsage 'implement'}
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_Local],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_HelpFiles;
begin
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_HelpFiles],0.0);
If fArchiveStructure.UtilityData.UnresolvedCount > 0 then
  begin
  end;
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_HelpFiles],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_ParseContent;
begin
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_ParseContent],0.0);
If fArchiveStructure.UtilityData.UnresolvedCount > 0 then
  begin
  end;
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_ParseContent],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_BruteForce;
begin
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_BruteForce],0.0);
If fArchiveStructure.UtilityData.UnresolvedCount > 0 then
  begin
  end;
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_BruteForce],1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS.SCS_ResolvePaths_Reconstruct;
begin
DoProgress([PSID_Processing,PSID_C_PathsResolving,PSID_C_PathsRes_Reconstruct],0.0);
If fArchiveStructure.UtilityData.UnresolvedCount > 0 then
  begin
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
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

//------------------------------------------------------------------------------

constructor TDARTRepairer_SCS.Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
inherited Create(PauseControlObject,ArchiveProcessingSettings);
fExpectedSignature := DART_SCS_FileSignature;
fEntriesSorted := False;
end;

end.
