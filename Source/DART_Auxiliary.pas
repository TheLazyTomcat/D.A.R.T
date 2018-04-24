{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Auxiliary;

{$INCLUDE DART_defs.inc}

interface

uses
  Windows, SysUtils,
  AuxTypes;

//--- Files --------------------------------------------------------------------

Function DART_GetFileSize(const FilePath: String): Int64;
Function DART_GetFileSignature(const FilePath: String): UInt32;
Function DART_FileExists(const FilePath: String): Boolean; {$IFDEF CanInline}inline;{$ENDIF}
Function DART_ExpandFileName(const FilePath: String): String; {$IFDEF CanInline}inline;{$ENDIF}

Function DART_FindFirst(const Path: String; Attr: Integer; var F: TSearchRec): Integer; {$IFDEF CanInline}inline;{$ENDIF}
Function DART_FindNext(var F: TSearchRec): Integer; {$IFDEF CanInline}inline;{$ENDIF}
procedure DART_FindClose(var F: TSearchRec); {$IFDEF CanInline}inline;{$ENDIF}

//--- Directoris ---------------------------------------------------------------

Function DART_ForceDirectories(const Path: String): Boolean; {$IFDEF CanInline}inline;{$ENDIF}
Function DART_DirectoryExists(const Path: String): Boolean; {$IFDEF CanInline}inline;{$ENDIF}

//--- Path rectification -------------------------------------------------------

Function DART_ExcludeTralingPathDelim(const Path: AnsiString; Delim: AnsiChar): AnsiString;
Function DART_ExcludeLeadingPathDelim(const Path: AnsiString; Delim: AnsiChar): AnsiString;
Function DART_ExcludeOuterPathDelim(const Path: AnsiString; Delim: AnsiChar): AnsiString;

Function DART_IncludeTralingPathDelim(const Path: AnsiString; Delim: AnsiChar): AnsiString;

Function DART_ReplaceChars(const Path: AnsiString; FromChar, ToChar: AnsiChar): AnsiString;
Function DART_PathIsFile(const Path: AnsiString; PathDelim: AnsiChar; ExtDelim: AnsiChar = '.'; DefaultsToFile: Boolean = True): Boolean;

//--- System information -------------------------------------------------------

Function DART_GetAvailableMemory: UInt64;

//--- Public auxilary constants ------------------------------------------------

{$IF not Declared(FILE_WRITE_ATTRIBUTES)}
const
  FILE_WRITE_ATTRIBUTES = 256;
{$IFEND}

implementation

uses
  Classes, StrRect {$IFDEF FPC_NonUnicode_NoUTF8RTL}, LazFileUtils{$ENDIF};

//==============================================================================

Function DART_GetFileSize(const FilePath: String): Int64;
var
  SearchResult: TSearchRec;
begin
If DART_FindFirst(FilePath,faAnyFile,{%H-}SearchResult) = 0 then
  try
  {$WARN SYMBOL_PLATFORM OFF}
    Int64Rec(Result).Hi := SearchResult.FindData.nFileSizeHigh;
    Int64Rec(Result).Lo := SearchResult.FindData.nFileSizeLow;
  {$WARN SYMBOL_PLATFORM ON}
  finally
    DART_FindClose(SearchResult);
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

Function DART_GetFileSignature(const FilePath: String): UInt32;
begin
Result := 0;
with TFileStream.Create(StrToRTL(FilePath),fmOpenRead or fmShareDenyWrite) do
try
  If Read(Result,SizeOf(Result)) < SizeOf(Result) then
    Result := 0;
finally
  Free;
end;
end;

//------------------------------------------------------------------------------

Function DART_FileExists(const FilePath: String): Boolean;
begin
{$IFDEF FPC_NonUnicode_NoUTF8RTL}
Result := FileExistsUTF8(FilePath);
{$ELSE}
Result := FileExists(FilePath);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function DART_ExpandFileName(const FilePath: String): String;
begin
{$IFDEF FPC_NonUnicode_NoUTF8RTL}
Result := ExpandFileNameUTF8(FilePath);
{$ELSE}
Result := ExpandFileName(FilePath);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function DART_FindFirst(const Path: String; Attr: Integer; var F: TSearchRec): Integer;
begin
{$IFDEF FPC_NonUnicode_NoUTF8RTL}
Result := FindFirstUTF8(Path,Attr,F);
{$ELSE}
Result := FindFirst(Path,Attr,F);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function DART_FindNext(var F: TSearchRec): Integer;
begin
{$IFDEF FPC_NonUnicode_NoUTF8RTL}
Result := FindNextUTF8(F);
{$ELSE}
Result := FindNext(F);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure DART_FindClose(var F: TSearchRec);
begin
{$IFDEF FPC_NonUnicode_NoUTF8RTL}
FindCloseUTF8(F);
{$ELSE}
FindClose(F);
{$ENDIF}
end;

//==============================================================================

Function DART_ForceDirectories(const Path: String): Boolean;
begin
{$IFDEF FPC_NonUnicode_NoUTF8RTL}
Result := ForceDirectoriesUTF8(Path);
{$ELSE}
Result := ForceDirectories(Path);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function DART_DirectoryExists(const Path: String): Boolean;
begin
{$IFDEF FPC_NonUnicode_NoUTF8RTL}
Result := DirectoryExistsUTF8(Path);
{$ELSE}
Result := DirectoryExists(Path)
{$ENDIF}
end;

//==============================================================================

Function DART_ExcludeTralingPathDelim(const Path: AnsiString; Delim: AnsiChar): AnsiString;
begin
If Length(Path) > 0 then
  begin
    If Path[Length(Path)] = Delim then
      Result := Copy(Path,1,Length(Path) - 1)
    else
      Result := Path;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function DART_ExcludeLeadingPathDelim(const Path: AnsiString; Delim: AnsiChar): AnsiString;
begin
If Length(Path) > 0 then
  begin
    If Path[1] = Delim then
      Result := Copy(Path,2,Length(Path) - 1)
    else
      Result := Path;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function DART_ExcludeOuterPathDelim(const Path: AnsiString; Delim: AnsiChar): AnsiString;
var
  Start, Len: Integer;
begin
Start := 1;
Len := Length(Path);
If Length(Path) > 0 then
  begin
    If Path[1] = Delim then
      begin
        Inc(Start);
        Dec(Len);
      end;
    If (Length(Path) > 1) and (Path[Length(Path)] = Delim) then
      Dec(Len);
    If (Start <> 1) or (Len <> Length(Path)) then
      Result := Copy(Path,Start,Len)
    else
      Result := Path;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function DART_IncludeTralingPathDelim(const Path: AnsiString; Delim: AnsiChar): AnsiString;
begin
If Length(Path) > 0 then
  begin
    Result := Path;
    If Result[Length(Result)] <> Delim then
      Result := Result + Delim;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function DART_ReplaceChars(const Path: AnsiString; FromChar, ToChar: AnsiChar): AnsiString;
var
  i:  Integer;
begin
Result := Path;
For i := 1 to Length(Result) do
  If Result[i] = FromChar then
    Result[i] := ToChar;
end;

//------------------------------------------------------------------------------

Function DART_PathIsFile(const Path: AnsiString; PathDelim: AnsiChar; ExtDelim: AnsiChar = '.'; DefaultsToFile: Boolean = True): Boolean;
var
  i:  Integer;
begin
Result := DefaultsToFile;
For i := Length(Path) downto 1 do
  If Path[i] = ExtDelim then
    begin
      Result := True;
      Break{For i};
    end
  else If Path[i] = PathDelim then
    begin
      Result := False;
      Break{For i};
    end;             
end;

//==============================================================================

type
  TMemoryStatusEx = record
    dwLength:                 DWORD;
    dwMemoryLoad:             DWORD;
    ullTotalPhys:             UInt64;
    ullAvailPhys:             UInt64;
    ullTotalPageFile:         UInt64;
    ullAvailPageFile:         UInt64;
    ullTotalVirtual:          UInt64;
    ullAvailVirtual:          UInt64;
    ullAvailExtendedVirtual:  UInt64;
  end;
  PMemoryStatusEx = ^TMemoryStatusEx;

Function GlobalMemoryStatusEx(lpBuffer: PMemoryStatusEx): BOOL; stdcall; external kernel32;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function DART_GetAvailableMemory: UInt64;
var
  MemStat:  TMemoryStatusEx;
begin
FillChar({%H-}MemStat,SizeOf(TMemoryStatusEx),0);
MemStat.dwLength := SizeOf(TMemoryStatusEx);
If not GlobalMemoryStatusEx(@MemStat) then
  raise Exception.CreateFmt('GlobalMemoryStatusEx has failed with error 0x%.8x.',[GetLastError]);
{$IFNDEF 64bit}
If MemStat.ullTotalPhys > $0000000080000000 then
  Result := $0000000080000000
else
{$ENDIF}
  Result := MemStat.ullTotalPhys;
end;

end.
