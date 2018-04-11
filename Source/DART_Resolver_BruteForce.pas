unit DART_Resolver_BruteForce;

{$INCLUDE DART_defs.inc}

interface

uses
  StrUtils,
  AuxTypes, ConcurrentTasks,
  DART_Format_SCS, DART_Common, DART_ProcessingSettings, DART_Repairer;

type
  TDARTUsedKnownPaths = record
    Arr:    array of AnsiString;
    Count:  Integer;
  end;

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

  TDARTAlphabet = record
    Letters:  array[Byte] of AnsiChar;
    Count:    Integer;
  end;

const
  DART_RES_BF_LimitedAlphabet: AnsiString = '0123456789abcdefghijklmnopqrstuvwxyz_.-/';

type
  TDARTResolver_BruteForce = class(TObject)
  private
    fPauseControlObject:  TDARTPauseObject;
    fBruteForceSettings:  TDART_PS_SCS_PathResolve_BruteForce;
    fUsedKnownPaths:      TDARTUsedKnownPaths;
    fUnresolved:          TDARTUnresolvedEntries;
    fResolved:            TDARTResolvedEntries;
    fOnProgress:          TDARTProgressEvent;
    fHashType:            UInt32;
    fAlphabet:            TDARTAlphabet;
  protected
    procedure DoProgress(Progress: Double); virtual;
    Function PathHash(const Path: AnsiString): TDARTHash64; virtual;
    Function Unresolved_IndexOf(Hash: TDARTHash64): Integer; virtual;
    procedure Unresolved_MoveToResolved(Index: Integer; const Path: AnsiString); virtual;
    procedure Unresolved_SortEntries; virtual;
    // single thread (local) processing
    procedure MainProcessing_SingleThreaded; virtual;
  public
    constructor Create(PauseControlObject: TDARTPauseObject; BruteForceSettings: TDART_PS_SCS_PathResolve_BruteForce);
    destructor Destroy; override;
    procedure Initialize(const ArchiveStructure: TDART_SCS_ArchiveStructure); virtual;
    procedure Run; virtual;
    property UnresolvedCount: Integer read fUnresolved.Count;
    property ResolvedCount: Integer read fResolved.Count;
    property OnProgress: TDARTProgressEvent read fOnProgress write fOnProgress;
  end;

implementation

uses
  SysUtils,
  CITY;

procedure TDARTResolver_BruteForce.DoProgress(Progress: Double);
begin
If Assigned(fOnProgress) then
  fOnProgress(Self,Progress);
end;

//------------------------------------------------------------------------------

Function TDARTResolver_BruteForce.PathHash(const Path: AnsiString): TDARTHash64;
begin
case fHashType of
  DART_SCS_HASH_City: Result := TDARTHash64(CityHash64(PAnsiChar(Path),Length(Path) * SizeOf(AnsiChar)));
else
  Result := 0;
end;
end;

//------------------------------------------------------------------------------

Function TDARTResolver_BruteForce.Unresolved_IndexOf(Hash: TDARTHash64): Integer;
var
  i,Min,Max:  Integer;
begin
Result := -1;
Min := 0;
Max := Pred(fUnresolved.Count);
If Max < 10 then
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

procedure TDARTResolver_BruteForce.Unresolved_MoveToResolved(Index: Integer; const Path: AnsiString);
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

procedure TDARTResolver_BruteForce.Unresolved_SortEntries;

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
If fUnresolved.Count > 1 then
  QuickSort(Low(fUnresolved.Arr),Pred(fUnresolved.Count));
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_BruteForce.MainProcessing_SingleThreaded;
var
  Shadow: AnsiString;
  Buffer: AnsiString;
  i,j:    Integer;
  Index:  Integer;
begin
DoProgress(0.0);
// prepare buffers
SetLength(Shadow,fBruteForceSettings.PathLengthLimit);
FillChar(PAnsiChar(Shadow)^,Length(Shadow),0);
SetLength(Buffer,1);
// main cycle
while (Length(Buffer) <= fBruteForceSettings.PathLengthLimit) and (fUnresolved.Count > 0) do
  begin
    // alphabet cycle
    For i := 0 to Pred(fAlphabet.Count) do
      begin
        Shadow[1] := AnsiChar(i);
        Buffer[1] := fAlphabet.Letters[i];
        // check hash of the buffer itself
        Index := Unresolved_IndexOf(PathHash(Buffer));
        If Index < 0 then
          begin
            // bare hash not found, search in combination with used known paths
            For j := Low(fUsedKnownPaths.Arr) to Pred(fUsedKnownPaths.Count) do
              begin
              Index := Unresolved_IndexOf(PathHash(fUsedKnownPaths.Arr[j].Path + Buffer));
                If Index >= 0 then
                  Unresolved_MoveToResolved(Index,fUsedKnownPaths.Arr[j].Path + Buffer);
              end;
          end
        else Unresolved_MoveToResolved(Index,Buffer);
      end;
    // propagate cycle
    For i := 1 to Length(Shadow) do
      begin
        If Ord(Shadow[i]) < Pred(fAlphabet.Count) then
          begin
            Shadow[i] := AnsiChar(Ord(Shadow[i]) + 1);
            Buffer[i] := fAlphabet.Letters[Ord(Shadow[i])];
            Break{For i};
          end
        else
          begin
            Shadow[i] := AnsiChar(0);
            Buffer[i] := fAlphabet.Letters[Ord(Shadow[i])];
            If (Length(Buffer) <= i) then
              begin
                SetLength(Buffer,i + 1);
                Buffer[i + 1] := fAlphabet.Letters[0];
                Break{For i};
              end;
          end;
      end;
    // not really needed, but should be here to ensure responsiveness
    DoProgress(fResolved.Count / (fUnresolved.Count + fResolved.Count));
  end;
DoProgress(1.0);
end;

//==============================================================================

constructor TDARTResolver_BruteForce.Create(PauseControlObject: TDARTPauseObject; BruteForceSettings: TDART_PS_SCS_PathResolve_BruteForce);
begin
inherited Create;
fPauseControlObject := PauseControlObject;
fBruteForceSettings := BruteForceSettings;
end;

//------------------------------------------------------------------------------

destructor TDARTResolver_BruteForce.Destroy;
begin
inherited;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_BruteForce.Initialize(const ArchiveStructure: TDART_SCS_ArchiveStructure);
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
// prepare list of used known paths
If fBruteForceSettings.UseKnownPaths then
  For i := Low(ArchiveStructure.KnownPaths.Arr) to Pred(ArchiveStructure.KnownPaths.Count) do
    If (Length(ArchiveStructure.KnownPaths.Arr[i].Path) > 0) and ArchiveStructure.KnownPaths.Arr[i].Directory then
      begin
        If fUsedKnownPaths.Count >= Length(fUsedKnownPaths.Arr) then
          SetLength(fUsedKnownPaths.Arr,Length(fUsedKnownPaths.Arr) + 1024);
        fUsedKnownPaths.Arr[fUsedKnownPaths.Count].Path := ArchiveStructure.KnownPaths.Arr[i].Path + DART_SCS_PathDelim;
        Inc(fUsedKnownPaths.Count);
      end;
// prepare hashing function
fHashType := ArchiveStructure.ArchiveHeader.HashType;
// prepare alphabet
If fBruteForceSettings.PrintableASCIIOnly then
  begin
    If fBruteForceSettings.LimitedAlphabet then
      begin
        // '0'..'9', 'a'..'z', '_', '.', '-', '/'
        For i := Low(fAlphabet.Letters) to Pred(Length(DART_RES_BF_LimitedAlphabet)) do
          fAlphabet.Letters[i] := DART_RES_BF_LimitedAlphabet[i + 1];
        fAlphabet.Count := Length(DART_RES_BF_LimitedAlphabet);
      end
    else
      begin
        // #32 .. #127
        For i := Low(fAlphabet.Letters) to 95 do
          fAlphabet.Letters[i] := AnsiChar(i + 32);
        fAlphabet.Count := 96;
      end;
  end
else
  begin
    For i := Low(fAlphabet.Letters) to High(fAlphabet.Letters) do
      fAlphabet.Letters[i] := AnsiChar(i);
    fAlphabet.Count := Length(fAlphabet.Letters);
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_BruteForce.Run;
begin
Unresolved_SortEntries;
WriteLn('known paths: ',fUsedKnownPaths.count);
WriteLn(' unresolved: ',fUnresolved.count);
WriteLn('   resolved: ',fResolved.count);
Readln;
If fBruteForceSettings.PathLengthLimit > 0 then
  MainProcessing_SingleThreaded;
end;


end.
