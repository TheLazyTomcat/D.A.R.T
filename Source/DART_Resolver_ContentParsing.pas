{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Resolver_ContentParsing;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes, StaticMemoryStream, ExplicitStringLists,
  DART_ProcessingSettings, DART_Common, DART_Resolver;

{===============================================================================
--------------------------------------------------------------------------------
                          TDARTResolver_ContentParsing
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTResolver_ContentParsing - class declaration
===============================================================================}
type
  TDARTResolver_ContentParsing = class(TDARTResolver)
  private
    fContentPasingSettings: TDART_PS_SCS_PathResolve_ContentParsing;
    // processing
    fData:                  Pointer;
    fSize:                  TMemSize;
    fDataStream:            TStaticMemoryStream;
  protected
    Function CP_GetSignature: UInt32; virtual;
    procedure CP_SplitLineToBlocks(const Line: AnsiString; Blocks: TAnsiStringList); virtual;
    procedure CP_Parsing_Unknown; virtual;
    procedure CP_Parsing_Unknown_Text; virtual;
    procedure CP_Parsing_Unknown_Binary; virtual;
  public
    constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
    procedure Run(const Data: Pointer; Size: TMemSize); reintroduce;
    procedure Stop; override;
  end;

implementation

uses
  Classes,
  DART_Auxiliary;

{===============================================================================
--------------------------------------------------------------------------------
                          TDARTResolver_ContentParsing
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTResolver_ContentParsing - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTResolver_ContentParsing - protected methods
-------------------------------------------------------------------------------}

Function TDARTResolver_ContentParsing.CP_GetSignature: UInt32;
begin
fDataStream.Seek(0,soBeginning);
If fDataStream.Read(Result,SizeOf(UInt32)) < SizeOf(UInt32) then
  Result := 0;
end;

//------------------------------------------------------------------------------

// splits string on white spaces and quoted blocks (double quote)
procedure TDARTResolver_ContentParsing.CP_SplitLineToBlocks(const Line: AnsiString; Blocks: TAnsiStringList);
var
  i:          Integer;
  Start,Len:  Integer;
  Quoted:     Boolean;
begin
Blocks.Clear;
Start := -1;
Len := 0;
Quoted := False;
For i := 1 to Length(Line) do
  begin
    If ((Ord(Line[i]) <= 32) or ((Ord(Line[i]) > 127) and
        fContentPasingSettings.LimitedCharacterSet)) and not Quoted then
      begin
        If Len > 0 then
          Blocks.Add(Copy(Line,Start,Len));
        Start := -1;
        Len := 0;
      end
    else
      begin
        If Line[i] = AnsiChar('"') then
          begin
            If Quoted then
              begin
                Blocks.Add(Copy(Line,Start + 1,Len));
                Start := -1;
                Len := 0;
              end
            else
              begin
                If Len > 0 then
                  Blocks.Add(Copy(Line,Start,Len));
                Start := i;
                Len := 0;
              end;
            Quoted := not Quoted;
          end
        else
          begin
            If Start < 0 then
              Start := i;
            Inc(Len);
          end;
      end;
  end;
If Len > 0 then
  begin
    If Quoted then
      Blocks.Add(Copy(Line,Start + 1,Len))
    else
      Blocks.Add(Copy(Line,Start,Len));
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_ContentParsing.CP_Parsing_Unknown;
var
  Counter:  TMemSize;
  i:        TMemSize;
begin
//select if data are binary or text (atm. it simply counts zeroed bytes)
Counter := 0;
For i := 0 to Pred(fSize) do
  If PByte(PtrUInt(fData) + PtrUInt(i))^ = 0 then
    Inc(Counter);
If (Counter / fSize) > fContentPasingSettings.BinaryThreshold then
  // data will be treated as an unknown binary
  CP_Parsing_Unknown_Binary
else
  // data will be treated as andunknown text
  CP_Parsing_Unknown_Text;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_ContentParsing.CP_Parsing_Unknown_Text;
var
  Text:   TAnsiStringList;
  Line:   TAnsiStringList;
  i,j:    Integer;
  Index:  Integer;
begin
Text := TAnsiStringList.Create;
try
  Text.LoadFromStream(fDataStream);
  Line := TAnsiStringList.Create;
  try
    For i := 0 to Pred(Text.Count) do
      begin
        CP_SplitLineToBlocks(Text[i],Line);
        For j := 0 to Pred(Line.Count) do
          If Length(Line[j]) >= fContentPasingSettings.MinPathLength then
            begin
              Index := Unresolved_IndexOf(PathHash(Line[j],fHashType));
              If Index >= 0 then
                Unresolved_MoveToResolved(Index,Line[j]);
            end;
      end;
  finally
    Line.Free;
  end;
finally
  Text.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_ContentParsing.CP_Parsing_Unknown_Binary;
var
  i:          TMemSize;
  Start,Len:  TMemSize;
  CurrByte:   Byte;
  TempStr:    AnsiString;
  Index:      Integer;
begin
Start := 0;
Len := 0;
For i := 0 to Pred(fSize) do
  begin
    CurrByte := PByte(PtrUInt(fData) + PtrUInt(i))^;
    If (CurrByte < 32) or ((CurrByte > 127) and fContentPasingSettings.LimitedCharacterSet) then
      begin
        If (Len > 0) and (Len >= TMemSize(fContentPasingSettings.MinPathLength)) then
          begin
            SetLength(TempStr,Len);
            Move(Pointer(PtrUInt(fData) + PtrUInt(Start))^,PAnsiChar(TempStr)^,Len);
            Index := Unresolved_IndexOf(PathHash(TempStr,fHashType));
            If Index >= 0 then
              Unresolved_MoveToResolved(Index,TempStr);
          end;
        Start := i;
        Len := 0;
      end
    else
      begin
        If Len = 0 then
          Start := i;
        Inc(Len);
      end;
  end;
end;

{-------------------------------------------------------------------------------
    TDARTResolver_ContentParsing - public methods
-------------------------------------------------------------------------------}

constructor TDARTResolver_ContentParsing.Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
inherited Create(PauseControlObject,ArchiveProcessingSettings);
fContentPasingSettings := fArchiveProcessingSettings.SCS.PathResolve.ParseContent;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_ContentParsing.Run(const Data: Pointer; Size: TMemSize);
begin
If Assigned(Data) and (Size > 0) then
  begin
    fData := Data;
    fSize := Size;
    fDataStream := TStaticMemoryStream.Create(Data,Size);
    try
      // later implement parsers for known types (SII, SII/3nK, MAT, TOBJ, ....) 
      //case CP_GetSignature of

      //else
        //If fContentPasingSettings.ParseEverything then
          CP_Parsing_Unknown;
      //end;
    finally
      fDataStream.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_ContentParsing.Stop;
begin
// nothing to do here
end;

end.
