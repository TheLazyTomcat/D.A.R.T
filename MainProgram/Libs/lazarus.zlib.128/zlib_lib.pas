{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Note that this file contains only things necessary for SCS Unlocker.
  It is in no way complete header for zlib library.

===============================================================================}

const
  zlib_version = '1.2.8';

  Z_SYNC_FLUSH =  2;
  Z_STREAM_END =  1;

  z_errmsg: Array [0..9] of String = (
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
  );

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

//==============================================================================

{$IFDEF zlib_lib_dll}

const
  zlib_file    = 'zlib1.dll';

{$IFDEF x64}
  {$R 'Resources\zlib64.res'}
{$ELSE}
  {$R 'Resources\zlib32.res'}
{$ENDIF}

type
  TZInflateInit2 = Function(strm: PZStream; windowBits: Integer; version: PAnsiChar; stream_size: Integer): Integer; cdecl;
  TZInflate = Function(strm: PZStream; flush: Integer): Integer; cdecl;
  TZInflateEnd = Function(strm: PZStream): Integer; cdecl;

var
  InflateInit2_:  TZInflateInit2;
  Inflate_:       TZInflate;
  InflateEnd_:    TZInflateEnd;

const
  ZLibDLLFile = 'zlib1.dll';

procedure ExtractLibrary;
begin
If not FileExists(ExtractFilePath(ParamStr(0)) + ZLibDLLFile) then
  with TResourceStream.Create(hInstance,'zlibdll',RT_RCDATA) do
    begin
      SaveToFile(ExtractFilePath(ParamStr(0)) + ZLibDLLFile);
      Free;
    end;
end;

//==============================================================================
// library (un)loading

var
  LibModule:  HModule = 0;

procedure Initialize;
begin
LibModule := LoadLibrary(zlib_file);
If LibModule <> 0 then
  begin
    InflateInit2_ := TZInflateInit2(GetProcAddress(LibModule,'inflateInit2_'));
    Inflate_ := TZInflate(GetProcAddress(LibModule,'inflate'));
    InflateEnd_ := TZInflateEnd(GetProcAddress(LibModule,'inflateEnd'));
  end
else raise Exception.CreateFmt('Module %s could not be loaded (%d).',[zlib_file,GetLastError]);
end;

//------------------------------------------------------------------------------

procedure Finalize;
begin
If LibModule <> 0 then
  FreeLibrary(LibModule);
end;

{$ELSE} //**********************************************************************

const
{$IFDEF x64}
  {$L 'libs\lazarus.zlib.128\win64\inflate.o'}
  {$L 'libs\lazarus.zlib.128\win64\inftrees.o'}
  {$L 'libs\lazarus.zlib.128\win64\inffast.o'}
  {$L 'libs\lazarus.zlib.128\win64\adler32.o'}
  {$L 'libs\lazarus.zlib.128\win64\crc32.o'}
  zcalloc_name = 'zcalloc';
  zcfree_name  = 'zcfree';
  memcpy_name  = 'memcpy';
{$ELSE}
  {$L 'libs\lazarus.zlib.128\win32\inflate.o'}
  {$L 'libs\lazarus.zlib.128\win32\inftrees.o'}
  {$L 'libs\lazarus.zlib.128\win32\inffast.o'}
  {$L 'libs\lazarus.zlib.128\win32\adler32.o'}
  {$L 'libs\lazarus.zlib.128\win32\crc32.o'}
  zcalloc_name = '_zcalloc';
  zcfree_name  = '_zcfree';
  memcpy_name  = '_memcpy';
{$ENDIF}

Function zcalloc({%H-}Opaque: Pointer; Items, Size: LongWord): Pointer; cdecl; public; alias: zcalloc_name;
begin
GetMem(Result,Items * Size);
end;

procedure zcfree({%H-}Opaque, Ptr: Pointer); cdecl; public; alias: zcfree_name;
begin
FreeMem(Ptr);
end;

procedure memcpy(Dest, Src: Pointer; Count: LongWord); cdecl; public; alias: memcpy_name;
begin
Move(Src^,Dest^,Count);
end;

//------------------------------------------------------------------------------

Function InflateInit2_(strm: PZStream; windowBits: Integer; version: PAnsiChar; stream_size: Integer): Integer; cdecl; external name 'inflateInit2_';
Function Inflate_(strm: PZStream; flush: Integer): Integer; cdecl; external name 'inflate';
Function InflateEnd_(strm: PZStream): Integer; cdecl; external name 'inflateEnd';

{$ENDIF}

//==============================================================================
// macro functions

Function InflateInit2(var Stream: TZStream; WindowBits: Integer): Integer;
begin
Result := InflateInit2_(@Stream,WindowBits,zlib_version,SizeOf(TZStream));
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
