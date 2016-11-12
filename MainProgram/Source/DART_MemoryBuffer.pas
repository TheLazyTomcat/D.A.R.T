{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_MemoryBuffer;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes;

type
  TMemoryBuffer = record
    Memory: Pointer;
    Size:   TMemSize;
  end;
  PMemoryBuffer = TMemoryBuffer;

procedure AllocateMemoryBuffer(var Buffer: TMemoryBuffer; Size: TMemSize);
procedure FreeMemoryBuffer(var Buffer: TMemoryBuffer);
procedure ReallocateMemoryBuffer(var Buffer: TMemoryBuffer; NewSize: TMemSize; AllowShrink: Boolean = False);

implementation

procedure AllocateMemoryBuffer(var Buffer: TMemoryBuffer; Size: TMemSize);
begin
GetMem(Buffer.Memory,Size);
Buffer.Size := Size;
end;

//------------------------------------------------------------------------------

procedure FreeMemoryBuffer(var Buffer: TMemoryBuffer);
begin
FreeMem(Buffer.Memory,Buffer.Size);
Buffer.Memory := nil;
Buffer.Size := 0;
end;

//------------------------------------------------------------------------------

procedure ReallocateMemoryBuffer(var Buffer: TMemoryBuffer; NewSize: TMemSize; AllowShrink: Boolean = False);
begin
If (NewSize > Buffer.Size) or AllowShrink then
  begin
    ReallocMem(Buffer.Memory,NewSize);
    Buffer.Size := NewSize;
  end;
end;

end.
