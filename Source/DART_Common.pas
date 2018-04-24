{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Common;

{$INCLUDE DART_defs.inc}

interface

uses
  SysUtils,
  AuxTypes, WinSyncObjs, CRC32, ProgressTracker,
  DART_Format_SCS;

{===============================================================================
    Hash types and functions
===============================================================================}

Function HashCompare(A,B: TCRC32): Integer; {$IFDEF CanInline}inline;{$ENDIF}
Function PathHash(const Path: AnsiString): TCRC32; {$IFDEF CanInline}inline;{$ENDIF}

{===============================================================================
    Progress stage information types and functions
===============================================================================}

type
  TDARTProgressStageInfo = record
    ParentStage:  TProgressTracker;
    StageIndex:   Integer;
  end;

  TDART_PSI = TDARTProgressStageInfo;

Function ProgressStageInfo(ParentStage: TProgressTracker; StageIndex: Integer): TDARTProgressStageInfo; {$IFDEF CanInline}inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                TDARTKnownPaths
--------------------------------------------------------------------------------
===============================================================================}

type
  TDARTKnownPath = record
    Path:       AnsiString;
    Directory:  Boolean;
    Hash:       TCRC32;
    Hash64:     TDARTHash64;
  end;

{===============================================================================
    TDARTKnownPaths - class declaration
===============================================================================}

  TDARTKnownPaths = class(TObject)
  private
    fList:        array of TDARTKnownPath;
    fCount:       Integer;
    fSorted:      Boolean;
    fHash64Type:  UInt32;
    Function GetCapacity: Integer;
    Function GetItem(Index: Integer): TDARTKnownPath;
  protected
    procedure Grow; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    Function LowIndex: Integer; virtual;
    Function HighIndex: Integer; virtual;
    Function IndexOf(const Path: AnsiString): Integer; virtual;
    Function IndexOfHash64(Hash: TDARTHash64): Integer; virtual;
    Function Add(const Path: AnsiString; Directory: Boolean): Integer; overload; virtual;
    Function Add(const Path: AnsiString): Integer; overload; virtual;
    Function Add(Paths: TDARTKnownPaths): Integer; overload; virtual;
    procedure Clear; virtual;
    procedure Sort; virtual;
    procedure Assign(Paths: TDARTKnownPaths; DirectoriesOnly: Boolean = False); virtual;
    property Items[Index: Integer]: TDARTKnownPath read GetItem; default;
    property Capacity: Integer read GetCapacity;
    property Count: Integer read fCount;
    property Sorted: Boolean read fSorted;
    property Hash64Type: UInt32 read fHash64Type write fHash64Type;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                            EDARTProcessingException
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    EDARTProcessingException - class declaration
===============================================================================}
type
  EDARTProcessingException = class(Exception)
  private
    fFaultObjectRef:    TObject;
    fFaultObjectClass:  String;
    fFaultFunctionIdx:  Integer;
    fFaultFunctionName: String;
  public
    constructor Create(const Text: String; ObjectRef: TObject; FunctionIdx: Integer; const FunctionName: String);
    property FaultObjectRef: TObject read fFaultObjectRef;
    property FaultObjectClass: String read fFaultObjectClass;
    property FaultFunctionIdx: Integer read fFaultFunctionIdx;
    property FaultFunctionName: String read fFaultFunctionName;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TDARTPauseObject
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTPauseObject - class declaration
===============================================================================}

  TDARTPauseObject = WinSyncObjs.TEvent;

implementation

uses
  CITY, StrRect,
  DART_Auxiliary;


{===============================================================================
    Hash functions implementation
===============================================================================}

Function HashCompare(A,B: TCRC32): Integer;
begin
If UInt32(A) < UInt32(B) then
  Result := 1
else If UInt32(A) > UInt32(B) then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function PathHash(const Path: AnsiString): TCRC32;
begin
Result := StringCRC32(AnsiLowerCase(AnsiToStr(Path)));
end;

{===============================================================================
    Progress stage information functions imlementation
===============================================================================}

Function ProgressStageInfo(ParentStage: TProgressTracker; StageIndex: Integer): TDARTProgressStageInfo;
begin
Result.ParentStage := ParentStage;
Result.StageIndex := StageIndex;
end;

{===============================================================================
--------------------------------------------------------------------------------
                                TDARTKnownPaths
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTKnownPaths - class declaration
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTKnownPaths - private methods
-------------------------------------------------------------------------------}

Function TDARTKnownPaths.GetCapacity: Integer;
begin
Result := Length(fList);
end;

//------------------------------------------------------------------------------

Function TDARTKnownPaths.GetItem(Index: Integer): TDARTKnownPath;
begin
If (Index >= Low(fList)) and (Index < fCount) then
  Result := fList[Index]
else
  raise Exception.CreateFmt('TDARTKnownPaths.GetItem: Index (%d) out of bounds.',[Index]);
end;

{-------------------------------------------------------------------------------
    TDARTKnownPaths - protected methods
-------------------------------------------------------------------------------}

procedure TDARTKnownPaths.Grow;
begin
If fCount >= Length(fList) then
  SetLength(fList,Length(fList) + 4096);
end;

{-------------------------------------------------------------------------------
    TDARTKnownPaths - public methods
-------------------------------------------------------------------------------}

constructor TDARTKnownPaths.Create;
begin
inherited Create;
SetLength(fList,0);
fCount := 0;
fSorted := False;
fHash64Type := DART_SCS_HASH_City;
end;

//------------------------------------------------------------------------------

destructor TDARTKnownPaths.Destroy;
begin
Clear;
inherited;
end;

//------------------------------------------------------------------------------

Function TDARTKnownPaths.LowIndex: Integer;
begin
Result := Low(fList);
end;

//------------------------------------------------------------------------------

Function TDARTKnownPaths.HighIndex: Integer;
begin
Result := Pred(fCount);
end;

//------------------------------------------------------------------------------

Function TDARTKnownPaths.IndexOf(const Path: AnsiString): Integer;
var
  i,Min,Max:  Integer;
  Hash:       TCRC32;
begin
Result := -1;
Hash := PathHash(Path);
If not fSorted or (fCount <= 5) then
  begin
    For i := LowIndex to HighIndex do
      If Hashcompare(fList[i].Hash,Hash) = 0 then
        If AnsiSameText(AnsiToStr(fList[i].Path),AnsiToStr(Path)) then
          begin
            Result := i;
            Break{For i};
          end;
  end
else
  begin
    Min := LowIndex;
    Max := HighIndex;
    while Max >= Min do
      begin
        i := ((Max - Min) shr 1) + Min;
        // i-th entry has lower hash than is requested
        If Hashcompare(fList[i].Hash,Hash) > 0 then Min := i + 1
          // i-th entry has higher hash than is requested
          else If Hashcompare(fList[i].Hash,Hash) < 0 then Max := i - 1
            else begin
              // i-th entry has the requested hash
              Result := i;
              Break{while};
            end;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TDARTKnownPaths.IndexOfHash64(Hash: TDARTHash64): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If Hash64Compare(fList[i].Hash64,Hash) = 0 then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TDARTKnownPaths.Add(const Path: AnsiString; Directory: Boolean): Integer;
begin
Result := IndexOf(Path);
If Result < 0 then
  begin
    Grow;
    fList[fCount].Path := DART_ExcludeOuterPathDelim(Path,DART_SCS_PathDelim);
    fList[fCount].Directory := Directory;
    fList[fCount].Hash := PathHash(Path);
    fList[fCount].Hash64 := PathHash64(Path,fHash64Type);
    Result := fCount;
    Inc(fCount);
    fSorted := False;
  end;
end;

//------------------------------------------------------------------------------

Function TDARTKnownPaths.Add(const Path: AnsiString): Integer;
begin
Result := Add(Path,not DART_PathIsFile(Path,DART_SCS_PathDelim));
end;

//------------------------------------------------------------------------------

Function TDARTKnownPaths.Add(Paths: TDARTKnownPaths): Integer;
var
  i:  Integer;
begin
Result := 0;
For i := Paths.LowIndex to Paths.HighIndex do
  If IndexOf(Paths[i].Path) < 0 then
    begin
      Add(Paths[i].Path,Paths[i].Directory);
      Inc(Result);
    end;
end;

//------------------------------------------------------------------------------

procedure TDARTKnownPaths.Clear;
begin
fCount := 0;
SetLength(fList,0);
fSorted := False;
end;

//------------------------------------------------------------------------------

procedure TDARTKnownPaths.Sort;

  procedure QuickSort(LeftIdx,RightIdx: Integer);
  var
    Pivot:  TCRC32;
    Idx,i:  Integer;

    procedure ExchangeEntries(Idx1,Idx2: Integer);
    var
      TempEntry:  TDARTKnownPath;
    begin
      If (Idx1 <> Idx2) then
        begin
          If (Idx1 < LowIndex) or (Idx1 > HighIndex) then
            raise Exception.CreateFmt('Index 1 (%d) out of bounds.',[Idx1]);
          If (Idx2 < LowIndex) or (Idx2 > HighIndex) then
            raise Exception.CreateFmt('Index 2 (%d) out of bounds.',[Idx1]);
          TempEntry := fList[Idx1];
          fList[Idx1] := fList[Idx2];
          fList[Idx2] := TempEntry;
        end;
    end;
    
  begin
    If LeftIdx < RightIdx then
      begin
        ExchangeEntries((LeftIdx + RightIdx) shr 1,RightIdx);
        Pivot := fList[RightIdx].Hash;
        Idx := LeftIdx;
        For i := LeftIdx to Pred(RightIdx) do
          If HashCompare(Pivot,fList[i].Hash) < 0 then
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
If not fSorted and (fCount > 1) then
  QuickSort(LowIndex,HighIndex);
fSorted := True;
end;

//------------------------------------------------------------------------------

procedure TDARTKnownPaths.Assign(Paths: TDARTKnownPaths; DirectoriesOnly: Boolean = False);
var
  i:  Integer;
begin
Clear;
SetLength(fList,Paths.Count);
For i := Paths.LowIndex to Paths.HighIndex do
  If Paths[i].Directory or not DirectoriesOnly then
    fList[i] := Paths[i];
fCount := Paths.Count;
end;

{===============================================================================
--------------------------------------------------------------------------------
                            EDARTProcessingException
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    EDARTProcessingException - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    EDARTProcessingException - public methods
-------------------------------------------------------------------------------}

constructor EDARTProcessingException.Create(const Text: String; ObjectRef: TObject; FunctionIdx: Integer; const FunctionName: String);
begin
inherited Create(Text);
fFaultObjectRef := ObjectRef;
fFaultObjectClass := ObjectRef.ClassName;
fFaultFunctionIdx := FunctionIdx;
fFaultFunctionName := FunctionName;
end;

end.
