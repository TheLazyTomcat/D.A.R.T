unit DART_Resolver_BruteForce;

{$INCLUDE DART_defs.inc}

interface

uses
  StrUtils,
  AuxTypes, ConcurrentTasks,
  DART_Format_SCS, DART_Common, DART_ProcessingSettings, DART_Repairer;

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
    fUsedKnownPaths:      TDARTKnownPaths;
    fUnresolved:          TDARTUnresolvedEntries;
    fResolved:            TDARTResolvedEntries;
    fOnProgress:          TDARTProgressEvent;
    // processing fields
    fHashType:            UInt32;
    fAlphabet:            TDARTAlphabet;
  protected
    procedure DoProgress(Progress: Double); virtual;
    Function PathHash(const Path: AnsiString): TDARTHash64; virtual;
    Function Unresolved_IndexOf(Hash: TDARTHash64): Integer; virtual;
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
  i:  Integer;
begin
Result := -1;
For i := Low(fUnresolved.Arr) to Pred(fUnresolved.Count) do
  If HashCompare(Hash,fUnresolved.Arr[i]) = 0 then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_BruteForce.MainProcessing_SingleThreaded;
begin
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
        SetLength(fUnresolved.Arr,Length(fUnresolved.Arr) + 128);
      fUnresolved.Arr[fUnresolved.Count] := ArchiveStructure.Entries.Arr[i].BinPart.Hash;
      Inc(fUnresolved.Count);
    end;
// prepare list of used known paths
If fBruteForceSettings.UseKnownPaths then
  For i := Low(ArchiveStructure.KnownPaths.Arr) to Pred(ArchiveStructure.KnownPaths.Count) do
    If Length(ArchiveStructure.KnownPaths.Arr[i].Path) > 0 then
      begin
        If fUsedKnownPaths.Count >= Length(fUsedKnownPaths.Arr) then
          SetLength(fUsedKnownPaths.Arr,Length(fUsedKnownPaths.Arr) + 128);
        fUsedKnownPaths.Arr[fUsedKnownPaths.Count] := ArchiveStructure.KnownPaths.Arr[i];
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
MainProcessing_SingleThreaded;
end;


end.
