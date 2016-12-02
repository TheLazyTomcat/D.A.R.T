{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Note that this file contains only things necessary for D.A.R.T project.
  It is in no way complete header for zlib library.

===============================================================================}
unit laz_zlib;

{.$DEFINE dll_mode}

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE 64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL 'Unsupported CPU.'}
{$ELSE}
  {$DEFINE 32bit}
{$IFEND}

interface

const
{$IFDEF dll_mode}
  zlib_dll = True;
{$ELSE}
  zlib_dll = False;
{$ENDIF}

  zlib_version = AnsiString('1.2.8');

  Z_OK                  = 0;
  Z_STREAM_END          = 1;
  Z_BUF_ERROR           = -5;
  Z_NO_FLUSH            = 0;
  Z_FINISH              = 4;
  Z_DEFAULT_COMPRESSION = -1;
  Z_DEFLATED            = 8;
  Z_DEFAULT_STRATEGY    = 0;

  z_errmsg: array[0..9] of PAnsiChar = (
    'Need dictionary',      // Z_NEED_DICT      (2)
    'Stream end',           // Z_STREAM_END     (1)
    'OK',                   // Z_OK             (0)
    'File error',           // Z_ERRNO          (-1)
    'Stream error',         // Z_STREAM_ERROR   (-2)
    'Data error',           // Z_DATA_ERROR     (-3)
    'Insufficient memory',  // Z_MEM_ERROR      (-4)
    'Buffer error',         // Z_BUF_ERROR      (-5)
    'Incompatible version', // Z_VERSION_ERROR  (-6)
    'Unknown error'
  ); cvar; // tells compiler to produce unmangled symbol name

type
  TZStream = record
    next_in:        Pointer;
    avail_in:       LongWord;
    total_in:       LongWord;
    next_out:       Pointer;
    avail_out:      LongWord;
    total_out:      LongWord;
    msg:            PAnsiChar;
    internal_state: Pointer;
    zalloc:         Pointer;
    zfree:          Pointer;
    opaque:         Pointer;
    data_type:      Integer;
    adler:          LongWord;
    reserved:       LongWord;
  end;
  PZStream = ^TZStream;

{$IFDEF dll_mode}

const
  zlib_file = 'zlib1.dll';

type
  TZInflateInit2 = Function(strm: PZStream; windowBits: Integer; version: PAnsiChar; stream_size: Integer): Integer; cdecl;
  TZInflate = Function(strm: PZStream; flush: Integer): Integer; cdecl;
  TZInflateEnd = Function(strm: PZStream): Integer; cdecl;

  TZDeflateInit2 = Function(strm: PZStream; level, method, windowBits, memLevel, Strategy: Integer; version: PAnsiChar; stream_size: Integer): Integer; cdecl;
  TZDeflate = Function(strm: PZStream; flush: Integer): Integer; cdecl;
  TZDeflateEnd = Function(strm: PZStream): Integer; cdecl;

var
  InflateInit2_:  TZInflateInit2;
  Inflate_:       TZInflate;
  InflateEnd_:    TZInflateEnd;
  DeflateInit2_:  TZDeflateInit2;
  Deflate_:       TZDeflate;
  DeflateEnd_:    TZDeflateEnd;

{$ELSE}

Function InflateInit2_(strm: PZStream; windowBits: Integer; version: PAnsiChar; stream_size: Integer): Integer; cdecl; external name 'inflateInit2_';
Function Inflate_(strm: PZStream; flush: Integer): Integer; cdecl; external name 'inflate';
Function InflateEnd_(strm: PZStream): Integer; cdecl; external name 'inflateEnd';

//Function DeflateInit2_(strm: PZStream; level, method, windowBits, memLevel, Strategy: Integer; version: PAnsiChar; stream_size: Integer): Integer; cdecl; external name 'deflateInit2_';
//Function Deflate_(strm: PZStream; flush: Integer): Integer; cdecl; external name 'deflate';
//Function DeflateEnd_(strm: PZStream): Integer; cdecl; external name 'deflateEnd';

{$ENDIF}

Function InflateInit2(var Stream: TZStream; WindowBits: Integer): Integer;
Function Inflate(var Stream: TZStream; Flush: Integer): Integer;
Function InflateEnd(var Stream: TZStream): Integer;

Function DeflateInit2(var {%H-}Stream: TZStream; {%H-}Level, {%H-}Method, {%H-}WindowBits, {%H-}MemLevel, {%H-}Strategy: Integer): Integer;
Function Deflate(var {%H-}Stream: TZStream; {%H-}Flush: Integer): Integer;
Function DeflateEnd(var {%H-}Stream: TZStream): Integer;

procedure Initialize;
procedure Finalize;

implementation

//==============================================================================

{$IFDEF dll_mode}

uses
  Windows, SysUtils, Classes;

{$IFDEF 64bit}
  {$R '..\..\..\Resources\zlib64.res'}
{$ELSE}
  {$R '..\..\..\Resources\zlib32.res'}
{$ENDIF}

var
  LibModule:  HModule = 0;

{$ELSE} //======================================================================

uses
  SysUtils;

const
{$IFDEF 64bit}
  //{$L 'win64\deflate.o'}
  {$L 'win64\inflate.o'}
  {$L 'win64\inftrees.o'}
  {$L 'win64\inffast.o'}
  //{$L 'win64\trees.o'}
  {$L 'win64\adler32.o'}
  {$L 'win64\crc32.o'}
  PublicNamePrefix = '';
{$ELSE}
  //{$L 'win32\deflate.o'}
  {$L 'win32\inflate.o'}
  {$L 'win32\inftrees.o'}
  {$L 'win32\inffast.o'}
  //{$L 'win32\trees.o'}
  {$L 'win32\adler32.o'}
  {$L 'win32\crc32.o'}
  PublicNamePrefix = '_';
{$ENDIF}

//------------------------------------------------------------------------------

function zcalloc({%H-}Opaque: Pointer; Items, Size: Integer): Pointer; cdecl; public name PublicNamePrefix + 'zcalloc';
begin
GetMem(Result,Items * Size);
end;

procedure zcfree({%H-}Opaque, Block: Pointer); cdecl; public name PublicNamePrefix + 'zcfree';
begin
FreeMem(Block);
end;

procedure memcpy(Dest, Src: Pointer; Count: LongWord); cdecl; public name PublicNamePrefix + 'memcpy';
begin
Move(Src^,Dest^,Count);
end;

Function memset(Ptr: Pointer; Value: Byte; Count: LongWord): Pointer; cdecl; public name PublicNamePrefix + 'memset';
begin
FillChar(Ptr^,Count,Value);
Result := Ptr;
end;

{$ENDIF}

//==============================================================================

Function InflateInit2(var Stream: TZStream; WindowBits: Integer): Integer;
begin
Result := InflateInit2_(@Stream,WindowBits,PAnsiChar(zlib_version),SizeOf(TZStream));
end;

//------------------------------------------------------------------------------

Function Inflate(var Stream: TZStream; Flush: Integer): Integer;
begin
Result := Inflate_(@Stream,Flush);
end;

//------------------------------------------------------------------------------

Function InflateEnd(var Stream: TZStream): Integer;
begin
Result := InflateEnd_(@Stream);
end;

//------------------------------------------------------------------------------

Function DeflateInit2(var Stream: TZStream; Level, Method, WindowBits, MemLevel, Strategy: Integer): Integer;
begin
{$IFDEF dll_mode}
Result := DeflateInit2_(@Stream,Level,Method,WindowBits,MemLevel,Strategy,PAnsiChar(zlib_version),SizeOf(TZStream));
{$ELSE}
Result := Z_OK;
raise Exception.Create('DeflateInit2: Not implemented.');
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function Deflate(var Stream: TZStream; Flush: Integer): Integer;
begin
{$IFDEF dll_mode}
Result := Deflate_(@Stream,Flush);
{$ELSE}
Result := Z_OK;
raise Exception.Create('Deflate: Not implemented.');
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function DeflateEnd(var Stream: TZStream): Integer;
begin
{$IFDEF dll_mode}
Result := DeflateEnd_(@Stream);
{$ELSE}
Result := Z_OK;
raise Exception.Create('DeflateEnd: Not implemented.');
{$ENDIF}
end;

//==============================================================================

{$IFDEF dll_mode}
procedure ExtractLibrary;
begin
If not FileExists(ExtractFilePath(ParamStr(0)) + zlib_file) then
  with TResourceStream.Create(hInstance,'zlibdll',RT_RCDATA) do
    begin
      SaveToFile(ExtractFilePath(ParamStr(0)) + zlib_file);
      Free;
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure Initialize;
begin
{$IFDEF dll_mode}
ExtractLibrary;
LibModule := LoadLibrary(zlib_file);
If LibModule <> 0 then
  begin
    InflateInit2_ := TZInflateInit2(GetProcAddress(LibModule,'inflateInit2_'));
    Inflate_ := TZInflate(GetProcAddress(LibModule,'inflate'));
    InflateEnd_ := TZInflateEnd(GetProcAddress(LibModule,'inflateEnd'));
    DeflateInit2_ := TZDeflateInit2(GetProcAddress(LibModule,'deflateInit2_'));;
    Deflate_ := TZDeflate(GetProcAddress(LibModule,'deflate'));;
    DeflateEnd_ := TZDeflateEnd(GetProcAddress(LibModule,'deflateEnd'));;
  end
else raise Exception.CreateFmt('Module %s could not be loaded (%d).',[zlib_file,GetLastError]);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure Finalize;
begin
{$IFDEF dll_mode}
If LibModule <> 0 then
  FreeLibrary(LibModule);
{$ENDIF}
end;

end.

