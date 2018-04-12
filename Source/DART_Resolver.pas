{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Resolver;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes,
  DART_ProcessingSettings, DART_Format_SCS, DART_Common, DART_Repairer;

{===============================================================================
--------------------------------------------------------------------------------
                                 TDARTResolver
--------------------------------------------------------------------------------
===============================================================================}

type
  TDARTResolvedEntry = record
    Hash: TDARTHash64;
    Path: AnsiString;
  end;

  TDARTResolvedEntries = record
    Arr:    array of TDARTResolvedEntry;
    Count:  Integer;
  end;

  TDARTUnresolvedEntries = record
    Arr:    array of TDARTHash64;
    Count:  Integer;
  end;  

{===============================================================================
    TDARTResolver - class declaration
===============================================================================}
type
  TDARTResolver = class(TObject)
  private
    fOnProgress:  TDARTProgressEvent;
  protected
    fPauseControlObject:        TDARTPauseObject;
    fArchiveProcessingSettings: TDARTArchiveProcessingSettings;
    fUnresSorted:               Boolean;
    fUnresolved:                TDARTUnresolvedEntries;
    fResolved:                  TDARTResolvedEntries;
    fHashType:                  UInt32;
    // getters
    Function GetUnresolved(Index: Integer): TDARTHash64;
    Function GetResolved(Index: Integer): TDARTResolvedEntry;
    // progress
    procedure DoProgress(Progress: Double); virtual;
    // operation with unresolved entries
    Function Unresolved_IndexOf(Hash: TDARTHash64): Integer; virtual;
    procedure Unresolved_MoveToResolved(Index: Integer; const Path: AnsiString); virtual;
    procedure Unresolved_SortEntries; virtual;
  public
    constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
    procedure Initialize(const ArchiveStructure: TDART_SCS_ArchiveStructure); virtual;
    procedure Run; virtual; abstract;
    procedure Stop; virtual; abstract;
    property Unresolved[Index: Integer]: TDARTHash64 read GetUnresolved;
    property Resolved[Index: Integer]: TDARTResolvedEntry read GetResolved;
    property UnresolvedCount: Integer read fUnresolved.Count;
    property ResolvedCount: Integer read fResolved.Count;
    property OnProgress: TDARTProgressEvent read fOnProgress write fOnProgress;
  end;

implementation

uses
  SysUtils;

{===============================================================================
--------------------------------------------------------------------------------
                                 TDARTResolver
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTResolver - class implmentation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTResolver - protected methods
-------------------------------------------------------------------------------}

Function TDARTResolver.GetUnresolved(Index: Integer): TDARTHash64;
begin
If (Index >= Low(fUnresolved.Arr)) and (Index < fUnresolved.Count) then
  Result := fUnresolved.Arr[Index]
else
  raise Exception.CreateFmt('TDARTResolver.GetUnresolved: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TDARTResolver.GetResolved(Index: Integer): TDARTResolvedEntry;
begin
If (Index >= Low(fResolved.Arr)) and (Index < fResolved.Count) then
  Result := fResolved.Arr[Index]
else
  raise Exception.CreateFmt('TDARTResolver.GetResolved: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TDARTResolver.DoProgress(Progress: Double);
begin
If Assigned(fOnProgress) then
  fOnProgress(Self,Progress);
end;

//------------------------------------------------------------------------------

Function TDARTResolver.Unresolved_IndexOf(Hash: TDARTHash64): Integer;
var
  i,Min,Max:  Integer;
begin
Result := -1;
Min := 0;
Max := Pred(fUnresolved.Count);
If (Max < 10) or not fUnresSorted then
  begin
    // for short lists use normal linear search
    For i := 0 to Max do
      If HashCompare(fUnresolved.Arr[i],Hash) = 0 then
        begin
          Result := i;
          Break{For i};
        end;
  end
else
  // for longer lists, use binary search
  while Max >= min do
    begin
      i := ((max - Min) shr 1) + Min;
      // i-th entry has lower hash than is requested
      If HashCompare(fUnresolved.Arr[i],Hash) > 0 then Min := i + 1
        // i-th entry has higher hash than is requested
        else If HashCompare(fUnresolved.Arr[i],Hash) < 0 then Max := i - 1
          else begin
            // i-th entry has the requested hash
            Result := i;
            Break{while};
          end;
    end;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver.Unresolved_MoveToResolved(Index: Integer; const Path: AnsiString);
var
  i:  Integer;
begin
If fResolved.Count >= Length(fResolved.Arr) then
  SetLength(fResolved.Arr,Length(fResolved.Arr) + 1024);
fResolved.Arr[fResolved.Count].Hash := fUnresolved.Arr[Index];
fResolved.Arr[fResolved.Count].Path := Path;
Inc(fResolved.Count);
For i := Index to (fUnresolved.Count - 2) do
  fUnresolved.Arr[i] := fUnresolved.Arr[i + 1];
Dec(fUnresolved.Count);
DoProgress(fResolved.Count / (fUnresolved.Count + fResolved.Count));
end;

//------------------------------------------------------------------------------

procedure TDARTResolver.Unresolved_SortEntries;

  procedure QuickSort(LeftIdx,RightIdx: Integer);
  var
    Pivot:  TDARTHash64;
    Idx,i:  Integer;

    procedure ExchangeEntries(Idx1,Idx2: Integer);
    var
      TempEntry:  TDARTHash64;
    begin
      If (Idx1 <> Idx2) then
        begin
          If (Idx1 < Low(fUnresolved.Arr)) or (Idx1 >= fUnresolved.Count) then
            raise Exception.CreateFmt('TDARTResolver_BruteForce.Unresolved_SortEntries: Index1 (%d) out of bounds.',[Idx1]);
          If (Idx2 < Low(fUnresolved.Arr)) or (Idx2 >= fUnresolved.Count) then
            raise Exception.CreateFmt('TDARTResolver_BruteForce.Unresolved_SortEntries: Index2 (%d) out of bounds.',[Idx2]);
          TempEntry := fUnresolved.Arr[Idx1];
          fUnresolved.Arr[Idx1] := fUnresolved.Arr[Idx2];
          fUnresolved.Arr[Idx2] := TempEntry;
        end;
    end;
    
  begin
    If LeftIdx < RightIdx then
      begin
        ExchangeEntries((LeftIdx + RightIdx) shr 1,RightIdx);
        Pivot := fUnresolved.Arr[RightIdx];
        Idx := LeftIdx;
        For i := LeftIdx to Pred(RightIdx) do
          If HashCompare(Pivot,fUnresolved.Arr[i]) < 0 then
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
If (fUnresolved.Count > 1) and not fUnresSorted then
  QuickSort(Low(fUnresolved.Arr),Pred(fUnresolved.Count));
fUnresSorted := True;
end;

{-------------------------------------------------------------------------------
    TDARTResolver - public methods
-------------------------------------------------------------------------------}

constructor TDARTResolver.Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
inherited Create;
fOnProgress := nil;
fPauseControlObject := PauseControlObject;
fArchiveProcessingSettings := ArchiveProcessingSettings;
fUnresSorted := False;
fUnresolved.Count := 0;
fResolved.Count := 0;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver.Initialize(const ArchiveStructure: TDART_SCS_ArchiveStructure);
var
  i:  Integer;
begin
// prepare list of unresolved
For i := Low(ArchiveStructure.Entries.Arr) to Pred(ArchiveStructure.Entries.Count) do
  If not ArchiveStructure.Entries.Arr[i].UtilityData.Resolved then
    begin
      If fUnresolved.Count >= Length(fUnresolved.Arr) then
        SetLength(fUnresolved.Arr,Length(fUnresolved.Arr) + 1024);
      fUnresolved.Arr[fUnresolved.Count] := ArchiveStructure.Entries.Arr[i].BinPart.Hash;
      Inc(fUnresolved.Count);
    end;
Unresolved_SortEntries;
// prepare hashing
fHashType := ArchiveStructure.ArchiveHeader.HashType;
end;

end.
