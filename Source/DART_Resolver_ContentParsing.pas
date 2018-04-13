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

type
  TDARTOrdinalExtension = (oeUnk,oeMAT,oeTOBJ,oeDDS,oePMA,oePMC,oePMD,oePMG);

const
  DART_OrdinalExtensionStrings: array[TDARTOrdinalExtension] of String =
    ('','.mat','.tobj','.dds','.pma','.pmc','.pmd','.pmg');

{===============================================================================
    TDARTResolver_ContentParsing - class declaration
===============================================================================}
type
  TDARTResolver_ContentParsing = class(TDARTResolver)
  private
    fContentParsingSettings:  TDART_PS_SCS_PathResolve_ContentParsing;
    // processing
    fData:                    Pointer;
    fSize:                    TMemSize;
    fDataStream:              TStaticMemoryStream;
  protected
    Function CP_GetSignature: UInt32; virtual;
    Function CP_ValidPathCharacter(Character: AnsiChar): Boolean; virtual;
    Function CP_OrdinalExtension(const Ext: AnsiString): TDARTOrdinalExtension; virtual;
    procedure CP_SplitLineToBlocks(const Line: AnsiString; Blocks: TAnsiStringList); virtual;
    Function CP_TryResolvePath(const Path: AnsiString): Integer; virtual;
    procedure CP_Parsing_Unknown; virtual;
    procedure CP_Parsing_Unknown_Text; virtual;
    procedure CP_Parsing_Unknown_Binary; virtual;
  public
    constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
    procedure Run(const Data: Pointer; Size: TMemSize); reintroduce; overload;
    procedure Run(const Path: AnsiString); reintroduce; overload;
    procedure Stop; override;
  end;

implementation

uses
  SysUtils, Classes,
  StrRect,
  DART_Auxiliary, DART_Format_SCS;

{===============================================================================
--------------------------------------------------------------------------------
                          TDARTResolver_ContentParsing
--------------------------------------------------------------------------------
===============================================================================}

const
  DART_RES_CP_LimitedCharSet = ['0'..'9','A'..'Z','a'..'z','_','.','-','/'];

  // known file type signatures
  DART_REP_CP_KTS_DDS = UInt32($20534444);  // DDS texture
  DART_REP_CP_KTS_OGG = UInt32($5367674F);  // OGG sound

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

Function TDARTResolver_ContentParsing.CP_ValidPathCharacter(Character: AnsiChar): Boolean;
begin
If fContentParsingSettings.PrintableASCIIOnly then
  begin
    If fContentParsingSettings.LimitedCharacterSet then
      Result := Character in DART_RES_CP_LimitedCharSet
    else
      Result := (Ord(Character) > 32) and (Ord(Character) < 128);
  end
else Result := Ord(Character) > 32;
end;

//------------------------------------------------------------------------------

Function TDARTResolver_ContentParsing.CP_OrdinalExtension(const Ext: AnsiString): TDARTOrdinalExtension;
var
  i:  TDARTOrdinalExtension;
begin
Result := oeUnk;
For i := Low(TDARTOrdinalExtension) to High(TDARTOrdinalExtension) do
  If AnsiSameText(DART_OrdinalExtensionStrings[i],AnsiToStr(Ext)) then
    begin
      Result := i;
      Break{For i};
    end;
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
    If (CP_ValidPathCharacter(Line[i]) or (Line[i] = AnsiChar('"'))) or Quoted then
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
      end
    else
      begin
        If Len > 0 then
          Blocks.Add(Copy(Line,Start,Len));
        Start := -1;
        Len := 0;
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

Function TDARTResolver_ContentParsing.CP_TryResolvePath(const Path: AnsiString): Integer;
begin
Result := Unresolved_IndexOf(PathHash(Path,fHashType));
If Result >= 0 then
  Unresolved_MoveToResolved(Result,Path);
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
If (Counter / fSize) > fContentParsingSettings.BinaryThreshold then
  // data will be treated as an unknown binary
  CP_Parsing_Unknown_Binary
else
  // data will be treated as andunknown text
  CP_Parsing_Unknown_Text;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_ContentParsing.CP_Parsing_Unknown_Text;
var
  Text:     TAnsiStringList;
  Line:     TAnsiStringList;
  i,j:      Integer;
  TempStr:  AnsiString;
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
          begin
            TempStr := DART_ExcludeOuterPathDelim(Line[j],DART_SCS_PathDelim);
            If Length(TempStr) >= fContentParsingSettings.MinPathLength then
              begin
                If CP_TryResolvePath(TempStr) >= 0 then
                  If fUnresolved.Count <= 0 then
                    Break{For j};
              end;
          end;
        If fUnresolved.Count <= 0 then
          Break{For i};
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
  CurrChar:   AnsiChar;
  TempStr:    AnsiString;
begin
Start := 0;
Len := 0;
For i := 0 to Pred(fSize) do
  begin
    CurrChar := PAnsiChar(PtrUInt(fData) + PtrUInt(i))^;
    If not CP_ValidPathCharacter(CurrChar) then
      begin
        If (Len > 0) and (Len >= TMemSize(fContentParsingSettings.MinPathLength)) then
          begin
            SetLength(TempStr,Len);
            Move(Pointer(PtrUInt(fData) + PtrUInt(Start))^,PAnsiChar(TempStr)^,Len);
            TempStr := DART_ExcludeOuterPathDelim(TempStr,DART_SCS_PathDelim);
            If CP_TryResolvePath(TempStr) >= 0 then
              If fUnresolved.Count <= 0 then
                Break{For i}; 
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
fContentParsingSettings := fArchiveProcessingSettings.SCS.PathResolve.ContentParsing;
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
      case CP_GetSignature of
        DART_REP_CP_KTS_DDS,
        DART_REP_CP_KTS_OGG:  
          If fContentParsingSettings.ParseEverything then
             CP_Parsing_Unknown;
      else
        If fContentParsingSettings.ParseEverything then
          CP_Parsing_Unknown;
      end;
    finally
      fDataStream.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_ContentParsing.Run(const Path: AnsiString);
begin
// process known file series
If Length(Path) > 0 then
  case CP_OrdinalExtension(ExtractFileExt(Path)) of
    oeMAT,oeTOBJ,oeDDS:
      begin
        CP_TryResolvePath(ChangeFileExt(AnsiToStr(Path),DART_OrdinalExtensionStrings[oeMAT]));
        CP_TryResolvePath(ChangeFileExt(AnsiToStr(Path),DART_OrdinalExtensionStrings[oeTOBJ]));
        CP_TryResolvePath(ChangeFileExt(AnsiToStr(Path),DART_OrdinalExtensionStrings[oeDDS]));
      end;
    oePMA,oePMC,oePMD,oePMG:
      begin
        CP_TryResolvePath(ChangeFileExt(AnsiToStr(Path),DART_OrdinalExtensionStrings[oePMA]));
        CP_TryResolvePath(ChangeFileExt(AnsiToStr(Path),DART_OrdinalExtensionStrings[oePMC]));
        CP_TryResolvePath(ChangeFileExt(AnsiToStr(Path),DART_OrdinalExtensionStrings[oePMD]));
        CP_TryResolvePath(ChangeFileExt(AnsiToStr(Path),DART_OrdinalExtensionStrings[oePMG]));
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_ContentParsing.Stop;
begin
// nothing to do here
end;

end.
