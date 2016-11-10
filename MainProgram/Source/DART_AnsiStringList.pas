unit DART_AnsiStringList;

interface

type
  TAnsiStringList = class(TObject)
  private
    fStringArray: array of AnsiString;
    fCount:       Integer;
    Function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    Function GetString(Index: Integer): AnsiString;
    procedure SetString(Index: Integer; Value: AnsiString);
    procedure SetText(const Str: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    Function IndexOf(const Str: AnsiString): Integer; virtual;
    Function Add(const Str: AnsiString): Integer; virtual;
    procedure Exchange(Index1,Index2: Integer); virtual;
    procedure Assign(List: TAnsiStringList); virtual;
    procedure Sort; virtual;
    property Strings[Index: Integer]: AnsiString read GetString write SetString; default;
  published
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read fCount;
    property Text: AnsiString write SetText;
  end;

implementation

uses
  SysUtils,
  AuxTypes;

Function TAnsiStringList.GetCapacity: Integer;
begin
Result := Length(fStringArray);
end;

//------------------------------------------------------------------------------

procedure TAnsiStringList.SetCapacity(Value: Integer);
begin
If Value > Length(fStringArray) then
  SetLength(fStringArray,Value);
end;
 
//------------------------------------------------------------------------------

Function TAnsiStringList.GetString(Index: Integer): AnsiString;
begin
If (Index >= Low(fStringArray)) and (Index < fCount) then
  Result := fStringArray[Index]
else
  raise Exception.CreateFmt('TAnsiStringList.GetString: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TAnsiStringList.SetString(Index: Integer; Value: AnsiString);
begin
If (Index >= Low(fStringArray)) and (Index < fCount) then
  fStringArray[Index] := Value
else
  raise Exception.CreateFmt('TAnsiStringList.SetString: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TAnsiStringList.SetText(const Str: AnsiString);
var
  P:    PAnsiChar;
  Len:  Integer;
  i:    Integer;
  Buff: AnsiString;
begin
Clear;
If Length(Str) > 0 then
  begin
    P := PAnsiChar(Str);
    Len := 0;
    For i := 1 to Length(Str) do
      begin
        If P^ in [#0,#10,#13] then
          begin
            If Len > 0 then
              begin
                SetLength(Buff,Len);
                Move({%H-}PAnsiChar({%H-}PtrUInt(P) - PtrUInt(Len))^,PAnsiChar(Buff)^,Len);
                Add(Buff);
                Len := 0;
              end;
          end
        else Inc(Len);
        Inc(P);
      end;
    If Len > 0 then
      begin
        SetLength(Buff,Len);
        Move({%H-}PAnsiChar({%H-}PtrUInt(P) - PtrUInt(Len))^,PAnsiChar(Buff)^,Len);
        Add(Buff);
      end;
  end;
end;

//==============================================================================

constructor TAnsiStringList.Create;
begin
inherited;
SetLength(fStringArray,0);
fCount := 0;
end;

//------------------------------------------------------------------------------

destructor TAnsiStringList.Destroy;
begin
SetLength(fStringArray,0);
inherited;
end;

//------------------------------------------------------------------------------

procedure TAnsiStringList.Clear;
var
  i:  Integer;
begin
For i := Low(fStringArray) to High(fStringArray) do
  fStringArray[i] := '';
fCount := 0;
end;

//------------------------------------------------------------------------------

Function TAnsiStringList.IndexOf(const Str: AnsiString): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := Low(fStringArray) to Pred(fCount) do
  If AnsiSameText(fStringArray[i],Str) then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TAnsiStringList.Add(const Str: AnsiString): Integer;
begin
If Length(fStringArray) <= fCount then
  SetLength(fStringArray,Length(fStringArray) + 128);
fStringArray[fCount] := Str;
Result := fCount;
Inc(fCount);
end;

//------------------------------------------------------------------------------

procedure TAnsiStringList.Exchange(Index1,Index2: Integer);
var
  Temp: AnsiString;
begin
If Index1 <> Index2 then
  begin
    If (Index1 < Low(fStringArray)) or (Index1 >= fCount) then
      raise Exception.CreateFmt('TAnsiStringList.Exchange: Index1 (%d) out of bounds.',[Index1]);
    If (Index2 < Low(fStringArray)) or (Index2 >= fCount) then
      raise Exception.CreateFmt('TAnsiStringList.Exchange: Index2 (%d) out of bounds.',[Index2]);
    Temp := fStringArray[Index1];
    fStringArray[Index1] := fStringArray[Index2];
    fStringArray[Index2] := Temp;
  end;
end;

//------------------------------------------------------------------------------

procedure TAnsiStringList.Assign(List: TAnsiStringList);
var
  i:  Integer;
begin
Clear;
If Length(fStringArray) < List.Count then
  SetLength(fStringArray,List.Count);
For i := 0 to Pred(List.Count) do
  fStringArray[i] := List[i];
fCount := List.Count;
end;

//------------------------------------------------------------------------------

procedure TAnsiStringList.Sort;

  procedure QuickSort(LeftIdx,RightIdx: Integer);
  var
    Pivot:  AnsiString;
    Idx,i:  Integer;
  begin
    If LeftIdx < RightIdx then
      begin
        Exchange((LeftIdx + RightIdx) shr 1,RightIdx);
        Pivot := fStringArray[RightIdx];
        Idx := LeftIdx;
        For i := LeftIdx to Pred(RightIdx) do
          If AnsiCompareStr(Pivot,fStringArray[i]) > 0 then
            begin
              Exchange(i,idx);
              Inc(Idx);
            end;
        Exchange(Idx,RightIdx);
        QuickSort(LeftIdx,Idx - 1);
        QuickSort(Idx + 1,RightIdx);
      end;
  end;
  
begin
If fCount > 1 then
  QuickSort(Low(fStringArray),Pred(fCount));
end;

end.
