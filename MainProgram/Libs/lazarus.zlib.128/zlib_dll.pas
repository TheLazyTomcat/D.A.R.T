{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Note that this file contains only things necessary for SCS Unlocker.
  It is in no way complete header for zlib library.

===============================================================================}
unit zlib_dll;

{$mode objfpc}{$H+}

interface

const
  zlib_file    = 'zlib1.dll';
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

//------------------------------------------------------------------------------

type
  TZInflateInit2 = Function(strm: PZStream; windowBits: Integer; version: PAnsiChar; stream_size: Integer): Integer; cdecl;
  TZInflate = Function(strm: PZStream; flush: Integer): Integer; cdecl;
  TZInflateEnd = Function(strm: PZStream): Integer; cdecl;

var
  InflateInit2_:  TZInflateInit2;
  Inflate_:       TZInflate;
  InflateEnd_:    TZInflateEnd;

//------------------------------------------------------------------------------

// macro functions
Function InflateInit2(var Stream: TZStream; WindowBits: Integer): Integer;
Function Inflate(var Stream: TZStream; Flush: Integer): Integer;
Function InflateEnd(var Stream: TZStream): Integer;

// library (un)loading
procedure Initialize;
procedure Finalize;

implementation

uses
  Windows, SysUtils;

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

//==============================================================================

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

end.

