{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_PathDeconstructor;

{$INCLUDE DART_defs.inc}

interface

uses
  DART_AnsiStringList;

type
  TPathNode = class; // forward declaration

  TPathDeconstructor = class(TObject)
  private
    fRootNode:  TPathNode;
    fNodes:     array of TPathNode;
    Function GetCount: Integer;
    Function GetNode(Index: Integer): TPathNode;
  protected
    Function AddNode(Node: TPathNode): Integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DeconstructFilePath(const FilePath: AnsiString); virtual;
    procedure Sort; virtual;
    property Nodes[Index: Integer]: TPathNode read GetNode; default;
  published
    property Count: Integer read GetCount;
    property RootNode: TPathNode read fRootNode;
  end;

  TPathNode = class(TObject)
  private
    fName:          AnsiString;
    fFullPath:      AnsiString;
    fDeconstructor: TPathDeconstructor;
    fOwner:         TPathNode;
    fFiles:         TAnsiStringList;
    fSubNodes:      array of TPathNode;
    Function GetSubNodeCount: Integer;
    Function GetSubNode(Index: Integer): TPathNode;
    Function GetFileCount: Integer;
    Function GetFile(Index: Integer): AnsiString;
  protected
    Function IndexOfSubNode(const SubNodeName: AnsiString): Integer; virtual;
    procedure ExchangeSubNodes(Idx1,Idx2: Integer); virtual;
  public
    class Function CharPos(Chr: AnsiChar; const Str: AnsiString): Integer; virtual;
    constructor Create(const Name: AnsiString; Deconstructor: TPathDeconstructor; Owner: TPathNode = nil);
    destructor Destroy; override;
    procedure DeconstructFilePath(const FilePath: AnsiString); virtual;
    procedure Sort; virtual;
    property SubNodes[Index: Integer]: TPathNode read GetSubNode;
    property Files[Index: Integer]: AnsiString read GetFile; default;
  published
    property Name: AnsiString read fName;
    property FullPath: AnsiString read fFullPath;
    property Deconstructor: TPathDeconstructor read fDeconstructor;
    property Owner: TPathNode read fOwner;
    property SubNodeCount: Integer read GetSubNodeCount;
    property FileCount: Integer read GetFileCount;
  end;

implementation

uses
  SysUtils;

const
  PD_PathDelimiter = '/';

Function TPathNode.GetSubNodeCount: Integer;
begin
Result := Length(fSubNodes);
end;

//------------------------------------------------------------------------------

Function TPathNode.GetSubNode(Index: Integer): TPathNode;
begin
If (Index >= Low(fSubNodes)) and (Index <= High(fSubNodes)) then
  Result := fSubNodes[Index]
else
  raise Exception.CreateFmt('TPathNode.GetSubNode: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TPathNode.GetFileCount: Integer;
begin
Result := fFiles.Count;
end;

//------------------------------------------------------------------------------

Function TPathNode.GetFile(Index: Integer): AnsiString;
begin
If (Index >= 0) and (Index < fFiles.Count) then
  Result := fFiles[Index]
else
  raise Exception.CreateFmt('TPathNode.GetFile: Index (%d) out of bounds.',[Index]);
end;

//==============================================================================

Function TPathNode.IndexOfSubNode(const SubNodeName: AnsiString): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := Low(fSubNodes) to High(fSubNodes) do
  If AnsiSameText(fSubNodes[i].Name,SubNodeName) then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

procedure TPathNode.ExchangeSubNodes(Idx1,Idx2: Integer);
var
  Temp: TPathNode;
begin
If Idx1 <> Idx2 then
  begin
    If (Idx1 < Low(fSubNodes)) or (Idx1 > High(fSubNodes)) then
      raise Exception.CreateFmt('TPathNode.ExchangeSubNodes: Index1 (%d) out of bounds.',[Idx1]);
    If (Idx2 < Low(fSubNodes)) or (Idx2 > High(fSubNodes)) then
      raise Exception.CreateFmt('TPathNode.ExchangeSubNodes: Index2 (%d) out of bounds.',[Idx2]);
    Temp := fSubNodes[Idx1];
    fSubNodes[Idx1] := fSubNodes[Idx2];
    fSubNodes[Idx2] := Temp;
  end;
end;

//==============================================================================

class Function TPathNode.CharPos(Chr: AnsiChar; const Str: AnsiString): Integer;
var
  i:  Integer;
begin
Result := 0;
If Length(Str) > 0 then
  For i := 1 to Length(Str) do
    If Str[i] = Chr then
      begin
        Result := i;
        Break;
      end;
end;

//------------------------------------------------------------------------------

constructor TPathNode.Create(const Name: AnsiString; Deconstructor: TPathDeconstructor; Owner: TPathNode = nil);
begin
inherited Create;
fName := Name;
If Assigned(Owner) then
  begin
    If Owner.FullPath <> '' then
      fFullPath := Owner.FullPath + PD_PathDelimiter + fName
    else
      fFullPath := fName;
  end
else fFullPath := fName;
fDeconstructor := Deconstructor;
fOwner := Owner;
fFiles := TAnsiStringList.Create;
SetLength(fSubNodes,0);
// add this node to a list of nodes
fDeconstructor.AddNode(Self);
end;

//------------------------------------------------------------------------------

destructor TPathNode.Destroy;
var
  i:  Integer;
begin
For i := Low(fSubNodes) to High(fSubNodes) do
  fSubNodes[i].Free;
SetLength(fSubNodes,0);
fFiles.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TPathNode.DeconstructFilePath(const FilePath: AnsiString);
var
  DelimPos: Integer;
  Idx:      Integer;
begin
// process only paths that are not empty
If Length(FilePath) > 0 then
  begin
    // find position of path delimiter
    DelimPos := CharPos(PD_PathDelimiter,FilePath);
    If DelimPos > 0 then
      begin
        // path delimiter found, path contains at least one directory
        Idx := IndexOfSubNode(Copy(FilePath,1,DelimPos - 1));
        If Idx < 0 then
          begin
            // first directory in path is not yet listed, create its node
            SetLength(fSubNodes,Length(fSubNodes) + 1);
            Idx := High(fSubNodes);
            fSubNodes[Idx] := TPathNode.Create(Copy(FilePath,1,DelimPos - 1),fDeconstructor,Self);
          end;
        fSubNodes[Idx].DeconstructFilePath(Copy(FilePath,DelimPos + 1,Length(FilePath)));
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

procedure TPathNode.Sort;

  procedure QuickSort(LeftIdx,RightIdx: Integer);
  var
    Pivot:  TPathNode;
    Idx,i:  Integer;
  begin
    If LeftIdx < RightIdx then
      begin
        ExchangeSubNodes((LeftIdx + RightIdx) shr 1,RightIdx);
        Pivot := fSubNodes[RightIdx];
        Idx := LeftIdx;
        For i := LeftIdx to Pred(RightIdx) do
          If AnsiCompareStr(Pivot.Name,fSubNodes[i].Name) > 0 then
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
If Length(fSubNodes) > 1 then
  QuickSort(Low(fSubNodes),High(fSubNodes));
end;

//******************************************************************************
//******************************************************************************
//******************************************************************************

Function TPathDeconstructor.GetCount: Integer;
begin
Result := Length(fNodes);
end;

//------------------------------------------------------------------------------

Function TPathDeconstructor.GetNode(Index: Integer): TPathNode;
begin
If (Index >= Low(fNodes)) and (Index <= High(fNodes)) then
  Result := fNodes[Index]
else
  raise Exception.CreateFmt('TPathDeconstructor.GetNode: Index (%d) out of bounds.',[Index]);
end;

//==============================================================================

Function TPathDeconstructor.AddNode(Node: TPathNode): Integer;
begin
SetLength(fNodes,Length(fNodes) + 1);
Result := High(fNodes);
fNodes[Result] := Node;
end;

//==============================================================================

constructor TPathDeconstructor.Create;
begin
inherited Create;
SetLength(fNodes,0);
fRootNode := TPathNode.Create('',Self); // must be AFTER SetLength...
end;

//------------------------------------------------------------------------------

destructor TPathDeconstructor.Destroy;
begin
SetLength(fNodes,0);
fRootNode.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TPathDeconstructor.DeconstructFilePath(const FilePath: AnsiString);
begin
fRootNode.DeconstructFilePath(FilePath);
end;

//------------------------------------------------------------------------------

procedure TPathDeconstructor.Sort;
var
  i:  Integer;
begin
For i := Low(fNodes) to High(fNodes) do
  fNodes[i].Sort;
end;

end.
