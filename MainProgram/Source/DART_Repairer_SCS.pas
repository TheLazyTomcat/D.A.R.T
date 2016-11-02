{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_SCS;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes,
  CRC32, WinSyncObjs,
  DART_Auxiliary, DART_ProcessingSettings, DART_Repairer;

{==============================================================================}
{------------------------------------------------------------------------------}
{                         SCS# structures and constants                        }
{------------------------------------------------------------------------------}
{==============================================================================}

const
  FileSignature_SCS = UInt32($23534353);  // SCS#

  SCS_HASH_City = UInt32($59544943);  // CITY

  SCS_FLAG_Directory  = $00000001;
  SCS_FLAG_Compressed = $00000002;
  SCS_FLAG_Unknown    = $00000004;  // seems to be always set

type
  TSCS_ArchiveHeader = packed record
    Signature:      UInt32;
    Unknown:        UInt32;
    Hash:           UInt32;
    Entries:        UInt32;
    EntriesOffset:  UInt64;
    UnknownOffset:  UInt64;
  end;

  TSCS_EntryRecord = packed record
    Hash:             UInt64;
    DataOffset:       UInt64;
    Flags:            UInt32;
    CRC32:            TCRC32;
    UncompressedSize: UInt32;
    CompressedSize:   UInt32;
  end;

  TSCS_Entry = record
    Bin:      TSCS_EntryRecord;
    FileName: AnsiString;
    Resolved: Boolean;
  end;

  TSCS_FileStructure = record
    ArchiveHeader:  TSCS_ArchiveHeader;
    Entries:        array of TSCS_Entry;
    KnownPaths:     TAoStr;
  end;

const
  SCS_RootPath = '';

  SCS_PredefinedPaths: array[0..0] of String = ('');

type
  TRepairer_SCS = class(TRepairer)
  protected
    fProcessingSettings:  TSCS_Settings;
    fArchiveStructure:    TSCS_FileStructure;
    fEntriesSorted:       Boolean;
    Function SCS_EntryFileNameHash(const EntryFileName: AnsiString): UInt64; virtual;
    Function SCS_HashCompare(A,B: UInt64): Integer; virtual;
    Function SCS_IndexOfEntry(Hash: UInt64): Integer; virtual;    
    procedure SCS_LoadArchiveHeader; virtual;
    procedure SCS_LoadEntries; virtual;
    procedure SCS_SortEntries; virtual;
    procedure SCS_ResolvePaths; virtual;
    procedure RectifyFileProcessingSettings; override;
    procedure InitializeData; override;
    procedure InitializeProgress; override;
    procedure ArchiveProcessing; override;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
    constructor Create(FlowControlObject: TEvent; FileProcessingSettings: TFileProcessingSettings);
  published
    property ArchiveStructure: TSCS_FileStructure read fArchiveStructure;
  end;

  TRepairer_SCS_Rebuild = class(TRepairer_SCS);
  TRepairer_SCS_Extract = class(TRepairer_SCS);

implementation

uses
  SysUtils, Classes,
  BitOps, CITY;

Function TRepairer_SCS.SCS_HashCompare(A,B: UInt64): Integer;
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

//------------------------------------------------------------------------------

Function TRepairer_SCS.SCS_IndexOfEntry(Hash: UInt64): Integer;
var
  i,min,max:  Integer;
begin
Result := -1;
If fEntriesSorted then
  begin
    min := 0;
    max := High(fArchiveStructure.Entries);
    while max >= min do
      begin
        i := ((max - min) shr 1) + min;
        // i-th entry has lower hash than is requested
        If SCS_HashCompare(fArchiveStructure.Entries[i].Bin.Hash,Hash) > 0 then Min := i + 1
          // i-th entry has higher hash than is requested
          else If SCS_HashCompare(fArchiveStructure.Entries[i].Bin.Hash,Hash) < 0 then Max := i - 1
            else begin
              // i-th entry has the requested hash
              Result := i;
              Break{while};
            end;
      end;
  end
else
  begin
    For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
      If fArchiveStructure.Entries[i].Bin.Hash = Hash then
        begin
          Result := i;
          Break{For i};
        end;
  end;
end;

//------------------------------------------------------------------------------

Function TRepairer_SCS.SCS_EntryFileNameHash(const EntryFileName: AnsiString): UInt64;
begin
Result := 0;
case fArchiveStructure.ArchiveHeader.Hash of
  SCS_HASH_City: Result := CityHash64(PAnsiChar(EntryFileName),Length(EntryFileName) * SizeOf(AnsiChar));
else
  DoError(200,'Unknown hashing algorithm (0x%.8x).',[fArchiveStructure.ArchiveHeader.Hash]);
end;
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS.SCS_LoadArchiveHeader;
begin
If fArchiveStream.Size >= SizeOf(TSCS_ArchiveHeader) then
  begin
    fArchiveStream.Seek(0,soFromBeginning);
    fArchiveStream.ReadBuffer(fArchiveStructure.ArchiveHeader,SizeOf(TSCS_ArchiveHeader));
    If fFileProcessingSettings.Common.IgnoreFileSignature then
      fArchiveStructure.ArchiveHeader.Signature := FileSignature_SCS;
    If fFileProcessingSettings.SCSSettings.PathResolve.AssumeCityHash then
      fArchiveStructure.ArchiveHeader.Hash := SCS_HASH_City
    else
      If fArchiveStructure.ArchiveHeader.Hash <> SCS_HASH_City then
        DoError(201,'Unsupported hash algorithm (0x%.8x).',[fArchiveStructure.ArchiveHeader.Hash]);
  end
else DoError(201,'File is too small (%d bytes) to contain a valid archive header.',[fArchiveStream.Size]);
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS.SCS_LoadEntries;
var
  i:  Integer;
begin
SetLength(fArchiveStructure.Entries,fArchiveStructure.ArchiveHeader.Entries);
fArchiveStream.Seek(fArchiveStructure.ArchiveHeader.EntriesOffset,soFromBeginning);
For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
  with fArchiveStructure.Entries[i] do
    begin
      fArchiveStream.ReadBuffer(Bin,SizeOf(TSCS_EntryRecord));
      FileName := '';
      Resolved := False;
      If fProcessingSettings.Entry.IgnoreCRC32 then
        Bin.CRC32 := 0;
      If fProcessingSettings.Entry.IgnoreCompressionFlag then
        SetFlagState(Bin.Flags,SCS_FLAG_Compressed,Bin.UncompressedSize <> Bin.CompressedSize);
    end;
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS.SCS_SortEntries;

  procedure ExchangeEntries(Idx1,Idx2: Integer);
  var
    TempEntry:  TSCS_Entry;
  begin
    If (Idx1 <> Idx2) then
      begin
        If (Idx1 < Low(fArchiveStructure.Entries)) or (Idx1 > High(fArchiveStructure.Entries)) then
          DoError(203,'Index 1 (%d) out of bounds.'[Idx1]);
        If (Idx2 < Low(fArchiveStructure.Entries)) or (Idx2 > High(fArchiveStructure.Entries)) then
          DoError(203,'Index 2 (%d) out of bounds.'[Idx1]);
        TempEntry := fArchiveStructure.Entries[Idx1];
        fArchiveStructure.Entries[Idx1] := fArchiveStructure.Entries[Idx2];
        fArchiveStructure.Entries[Idx2] := TempEntry;
      end;
  end;

  procedure QuickSort(LeftIdx,RightIdx: Integer);
  var
    Pivot:  UInt64;
    Idx,i:  Integer;
  begin
    If LeftIdx < RightIdx then
      begin
        ExchangeEntries((LeftIdx + RightIdx) shr 1,RightIdx);
        Pivot := fArchiveStructure.Entries[RightIdx].Bin.Hash;
        Idx := LeftIdx;
        For i := LeftIdx to Pred(RightIdx) do
          If SCS_HashCompare(Pivot,fArchiveStructure.Entries[i].Bin.Hash) < 0 then
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
If Length(fArchiveStructure.Entries) > 1 then
  QuickSort(0,High(fArchiveStructure.Entries));
fEntriesSorted := True;
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS.SCS_ResolvePaths;
var
  i,Idx:  Integer;

  procedure LoadHelpFile(const FileName: String);
  var
    HelpFileProcSettings: TFileProcessingSettings;
    HelpFileRepairer:     TRepairer_SCS;
    ii:                   Integer;
  begin
    HelpFileProcSettings := DefaultFileProcessingSettings;
    HelpFileProcSettings.Common.FilePath := FileName;
    HelpFileProcSettings.SCSSettings.PathResolve.CustomPaths := fArchiveStructure.KnownPaths;
    HelpFileRepairer := TRepairer_SCS.Create(fFlowControlObject,HelpFileProcSettings);
    try
      HelpFileRepairer.Run;
      If Length(HelpFileRepairer.ArchiveStructure.KnownPaths) > 0 then
        begin
          Idx := Length(fArchiveStructure.KnownPaths);
          SetLength(fArchiveStructure.KnownPaths,Idx + Length(HelpFileRepairer.ArchiveStructure.KnownPaths));
          For ii := Low(HelpFileRepairer.ArchiveStructure.KnownPaths) to High(HelpFileRepairer.ArchiveStructure.KnownPaths) do
            fArchiveStructure.KnownPaths[Idx + ii] := HelpFileRepairer.ArchiveStructure.KnownPaths[ii];
        end;
    finally
      HelpFileRepairer.Free;
    end;
  end;

begin
// load predefined paths
If fProcessingSettings.PathResolve.UsePredefinedPaths then
  begin
    SetLength(fArchiveStructure.KnownPaths,Length(SCS_PredefinedPaths));
    For i := Low(SCS_PredefinedPaths) to High(SCS_PredefinedPaths) do
      fArchiveStructure.KnownPaths[i] := SCS_PredefinedPaths[i];
  end;
// load user paths
If Length(fProcessingSettings.PathResolve.CustomPaths) > 0 then
  begin
    Idx := Length(fArchiveStructure.KnownPaths);
    SetLength(fArchiveStructure.KnownPaths,Idx + Length(fProcessingSettings.PathResolve.CustomPaths));
    For i := Low(fProcessingSettings.PathResolve.CustomPaths) to High(fProcessingSettings.PathResolve.CustomPaths) do
      fArchiveStructure.KnownPaths[Idx + i] := fProcessingSettings.PathResolve.CustomPaths[i];
  end;
// load paths from help files
For i := Low(fProcessingSettings.PathResolve.HelpFiles) to High(fProcessingSettings.PathResolve.HelpFiles) do
  LoadHelpFile(fProcessingSettings.PathResolve.HelpFiles[i]);
{$message 'implement'}
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS.RectifyFileProcessingSettings;
begin
inherited;
fProcessingSettings := fFileProcessingSettings.SCSSettings;
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS.InitializeData;
begin
inherited;
FillChar(fArchiveStructure.ArchiveHeader,SizeOf(TSCS_ArchiveHeader),0);
SetLength(fArchiveStructure.Entries,0);
SetLength(fArchiveStructure.KnownPaths,0);
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS.InitializeProgress;
begin
inherited;
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS.ArchiveProcessing;
begin
inherited;
SCS_LoadArchiveHeader;
SCS_LoadEntries;
SCS_SortEntries;
SCS_ResolvePaths;
If Length(fArchiveStructure.Entries) <= 0 then
  DoError(202,'Input file does not contain any valid entries.');
end;

//==============================================================================

class Function TRepairer_SCS.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  200:  Result := 'SCS_EntryPathHash';
  201:  Result := 'SCS_LoadArchiveHeader';
  202:  Result := 'ArchiveProcessing';
  203:  Result := 'SCS_SortEntries.ExchangeEntries';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

//------------------------------------------------------------------------------

constructor TRepairer_SCS.Create(FlowControlObject: TEvent; FileProcessingSettings: TFileProcessingSettings);
begin
inherited Create(FlowControlObject,FileProcessingSettings);
fExpectedSignature := FileSignature_SCS;
fEntriesSorted := False;
end;

end.
