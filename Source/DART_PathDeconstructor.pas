{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_PathDeconstructor;

{$INCLUDE DART_defs.inc}

interface

uses
  ExplicitStringLists;

{===============================================================================
--------------------------------------------------------------------------------
                             TDARTPathDeconstructor
--------------------------------------------------------------------------------
===============================================================================}

type
  TDARTPathNode = class; // forward declaration

  TDARTPathNodes = record
    Arr:    array of TDARTPathNode;
    Count:  Integer
  end;

{===============================================================================
    TDARTPathDeconstructor - class declaration
===============================================================================}

  TDARTPathDeconstructor = class(TObject)
  private
    fPathDelimiter: AnsiChar;
    fRootNode:      TDARTPathNode;
    fNodes:         TDARTPathNodes;
    Function GetNode(Index: Integer): TDARTPathNode;
  protected
    Function AddNode(Node: TDARTPathNode): Integer; virtual;
  public
    constructor Create(PathDelimiter: AnsiChar);
    destructor Destroy; override;
    procedure DeconstructPath(const FilePath: AnsiString); virtual;
    procedure Clear; virtual;
    procedure Sort; virtual;
    property Nodes[Index: Integer]: TDARTPathNode read GetNode; default;
    property PathDelimiter: AnsiChar read fPathDelimiter;
    property Count: Integer read fNodes.Count;
    property RootNode: TDARTPathNode read fRootNode;
  end;

{===============================================================================
    TDARTPathNode - class declaration
===============================================================================}

  TDARTPathNode = class(TObject)
  private
    fName:          AnsiString;
    fFullPath:      AnsiString;
    fDeconstructor: TDARTPathDeconstructor;
    fPathDelimiter: AnsiChar;
    fOwner:         TDARTPathNode;
    fFiles:         TAnsiStringList;
    fSubNodes:      TDARTPathNodes;
    Function GetSubNode(Index: Integer): TDARTPathNode;
    Function GetFileCount: Integer;
    Function GetFile(Index: Integer): AnsiString;
  protected
    Function IndexOfSubNode(const SubNodeName: AnsiString): Integer; virtual;
    procedure ExchangeSubNodes(Idx1,Idx2: Integer); virtual;
  public
    class Function CharPos(Chr: AnsiChar; const Str: AnsiString): Integer; virtual;
    constructor Create(const Name: AnsiString; Deconstructor: TDARTPathDeconstructor; PathDelimiter: AnsiChar; Owner: TDARTPathNode = nil);
    destructor Destroy; override;
    procedure DeconstructPath(FilePath: AnsiString); virtual;
    procedure Clear; virtual;
    procedure Sort; virtual;
    property SubNodes[Index: Integer]: TDARTPathNode read GetSubNode;
    property Files[Index: Integer]: AnsiString read GetFile; default;
    property Name: AnsiString read fName;
    property FullPath: AnsiString read fFullPath;
    property Deconstructor: TDARTPathDeconstructor read fDeconstructor;
    property PathDelimiter: AnsiChar read fPathDelimiter;
    property Owner: TDARTPathNode read fOwner;
    property SubNodeCount: Integer read fSubNodes.Count;
    property FileCount: Integer read GetFileCount;
  end;

implementation

uses
  SysUtils,
  StrRect,
  DART_Auxiliary;

{===============================================================================
--------------------------------------------------------------------------------
                             TDARTPathDeconstructor
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTPathDeconstructor - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTPathDeconstructor - private methods
-------------------------------------------------------------------------------}

Function TDARTPathDeconstructor.GetNode(Index: Integer): TDARTPathNode;
begin
If (Index >= Low(fNodes.Arr)) and (Index < fNodes.Count) then
  Result := fNodes.Arr[Index]
else
  raise Exception.CreateFmt('TDARTPathDeconstructor.GetNode: Index (%d) out of bounds.',[Index]);
end;

{-------------------------------------------------------------------------------
    TDARTPathDeconstructor - protected methods
-------------------------------------------------------------------------------}

Function TDARTPathDeconstructor.AddNode(Node: TDARTPathNode): Integer;
begin
If fNodes.Count >= Length(fNodes.Arr) then
  SetLength(fNodes.Arr,Length(fNodes.Arr) + 32);
Result := fNodes.Count;
fNodes.Arr[Result] := Node;
Inc(fNodes.Count);
end;

{-------------------------------------------------------------------------------
    TDARTPathDeconstructor - public methods
-------------------------------------------------------------------------------}

constructor TDARTPathDeconstructor.Create(PathDelimiter: AnsiChar);
begin
inherited Create;
fPathDelimiter := PathDelimiter;
SetLength(fNodes.Arr,0);
fNodes.Count := 0;
fRootNode := TDARTPathNode.Create('',Self,PathDelimiter); // must be AFTER SetLength...
end;

//------------------------------------------------------------------------------

destructor TDARTPathDeconstructor.Destroy;
begin
SetLength(fNodes.Arr,0);
fNodes.Count := 0;
fRootNode.Free; // also frees all stored nodes
inherited;
end;

//------------------------------------------------------------------------------

procedure TDARTPathDeconstructor.DeconstructPath(const FilePath: AnsiString);
begin
fRootNode.DeconstructPath(FilePath);
end;

//------------------------------------------------------------------------------

procedure TDARTPathDeconstructor.Clear;
var
  i:  Integer;
begin
fRootNode.Clear;
For i := Low(fNodes.Arr) to Pred(fNodes.Count) do
  fNodes.Arr[i] := nil;
fNodes.Count := 0;
end;

//------------------------------------------------------------------------------

procedure TDARTPathDeconstructor.Sort;
var
  i:  Integer;
begin
For i := Low(fNodes.Arr) to Pred(fNodes.Count) do
  fNodes.Arr[i].Sort;
end;

{===============================================================================
    TDARTPathNode - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTPathNode - private methods
-------------------------------------------------------------------------------}

Function TDARTPathNode.GetSubNode(Index: Integer): TDARTPathNode;
begin
If (Index >= Low(fSubNodes.Arr)) and (Index < fSubNodes.Count) then
  Result := fSubNodes.Arr[Index]
else
  raise Exception.CreateFmt('TDARTPathNode.GetSubNode: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TDARTPathNode.GetFileCount: Integer;
begin
Result := fFiles.Count;
end;

//------------------------------------------------------------------------------

Function TDARTPathNode.GetFile(Index: Integer): AnsiString;
begin
If (Index >= 0) and (Index < fFiles.Count) then
  Result := fFiles[Index]
else
  raise Exception.CreateFmt('TDARTPathNode.GetFile: Index (%d) out of bounds.',[Index]);
end;

{-------------------------------------------------------------------------------
    TDARTPathNode - protected methods
-------------------------------------------------------------------------------}

Function TDARTPathNode.IndexOfSubNode(const SubNodeName: AnsiString): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := Low(fSubNodes.Arr) to Pred(fSubNodes.Count) do
  If AnsiSameText(AnsiToStr(fSubNodes.Arr[i].Name),AnsiToStr(SubNodeName)) then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

procedure TDARTPathNode.ExchangeSubNodes(Idx1,Idx2: Integer);
var
  Temp: TDARTPathNode;
begin
If Idx1 <> Idx2 then
  begin
    If (Idx1 < Low(fSubNodes.Arr)) or (Idx1 >= fSubNodes.Count) then
      raise Exception.CreateFmt('TDARTPathNode.ExchangeSubNodes: Index1 (%d) out of bounds.',[Idx1]);
    If (Idx2 < Low(fSubNodes.Arr)) or (Idx2 > fSubNodes.Count) then
      raise Exception.CreateFmt('TDARTPathNode.ExchangeSubNodes: Index2 (%d) out of bounds.',[Idx2]);
    Temp := fSubNodes.Arr[Idx1];
    fSubNodes.Arr[Idx1] := fSubNodes.Arr[Idx2];
    fSubNodes.Arr[Idx2] := Temp;
  end;
end;

{-------------------------------------------------------------------------------
    TDARTPathNode - public methods
-------------------------------------------------------------------------------}

class Function TDARTPathNode.CharPos(Chr: AnsiChar; const Str: AnsiString): Integer;
var
  i:  Integer;
begin
Result := 0;
If Length(Str) > 0 then
  For i := 1 to Length(Str) do
    If Str[i] = Chr then
      begin
        Result := i;
        Break{For i};
      end;
end;

//------------------------------------------------------------------------------

constructor TDARTPathNode.Create(const Name: AnsiString; Deconstructor: TDARTPathDeconstructor; PathDelimiter: AnsiChar; Owner: TDARTPathNode = nil);
begin
inherited Create;
fName := Name;
If Assigned(Owner) then
  begin
    If Length(Owner.FullPath) > 0 then
      fFullPath := Owner.FullPath + PathDelimiter + fName
    else
      fFullPath := fName;
  end
else fFullPath := fName;
fDeconstructor := Deconstructor;
fPathDelimiter := PathDelimiter;
fOwner := Owner;
fFiles := TAnsiStringList.Create;
SetLength(fSubNodes.Arr,0);
fSubNodes.Count := 0;
// add this node to a list of nodes
fDeconstructor.AddNode(Self);
end;

//------------------------------------------------------------------------------

destructor TDARTPathNode.Destroy;
begin
Clear;
fFiles.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TDARTPathNode.DeconstructPath(FilePath: AnsiString);
var
  DelimPos: Integer;
  Idx:      Integer;
begin
FilePath := DART_ExcludeOuterPathDelim(FilePath,fPathDelimiter);
// process only paths that are not empty
If Length(FilePath) > 0 then
  begin
    // find position of path delimiter
    DelimPos := CharPos(fPathDelimiter,FilePath);
    If DelimPos > 0 then
      begin
        // path delimiter found, path contains at least one directory
        Idx := IndexOfSubNode(Copy(FilePath,1,DelimPos - 1));
        If Idx < 0 then
          begin
            // first directory in path is not yet listed, create its node
            If fSubNodes.Count >= Length(fSubNodes.Arr) then
              SetLength(fSubNodes.Arr,Length(fSubNodes.Arr) + 32);
            Idx := fSubNodes.Count;
            fSubNodes.Arr[Idx] := TDARTPathNode.Create(Copy(FilePath,1,DelimPos - 1),fDeconstructor,fPathDelimiter,Self);
            Inc(fSubNodes.Count);
          end;
        fSubNodes.Arr[Idx].DeconstructPath(Copy(FilePath,DelimPos + 1,Length(FilePath)));
      end
    else
      begin
        // there is no path delimiter in the path, meaning it is a file name
        Idx := fFiles.IndexOf(FilePath);
        If Idx < 0 then
          fFiles.Add(FilePath);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTPathNode.Clear;
var
  i:  Integer;
begin
For i := Low(fSubNodes.Arr) to Pred(fSubNodes.Count) do
  fSubNodes.Arr[i].Free;
SetLength(fSubNodes.Arr,0);
fSubNodes.Count := 0;
fFiles.Clear;
end;

//------------------------------------------------------------------------------

procedure TDARTPathNode.Sort;

  procedure QuickSort(LeftIdx,RightIdx: Integer);
  var
    Pivot:  TDARTPathNode;
    Idx,i:  Integer;
  begin
    If LeftIdx < RightIdx then
      begin
        ExchangeSubNodes((LeftIdx + RightIdx) shr 1,RightIdx);
        Pivot := fSubNodes.Arr[RightIdx];
        Idx := LeftIdx;
        For i := LeftIdx to Pred(RightIdx) do
          If AnsiCompareStr(AnsiToStr(Pivot.Name),AnsiToStr(fSubNodes.Arr[i].Name)) > 0 then
            begin
              ExchangeSubNodes(i,idx);
              Inc(Idx);
            end;
        ExchangeSubNodes(Idx,RightIdx);
        QuickSort(LeftIdx,Idx - 1);
        QuickSort(Idx + 1,RightIdx);
      end;
  end;

begin
fFiles.Sort;
If fSubNodes.Count > 1 then
  QuickSort(Low(fSubNodes.Arr),Pred(fSubNodes.Count));
end;

end.
