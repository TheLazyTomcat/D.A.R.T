{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  CITY hash calculation

  ©František Milt 2017-07-19

  Version 1.0.1

  This is only naive reimplementation of reference code that can be found in
  this repository:

    https://github.com/google/cityhash

  Version 1.1.1 of the hash is implemented, but you can switch to version 1.0.3
  by activating VER_1_0_3 define in the CITY_defs.inc file.

  Dependencies:
    AuxTypes    - github.com/ncs-sniper/Lib.AuxTypes
    BitOps      - github.com/ncs-sniper/Lib.BitOps
  * SimpleCPUID - github.com/ncs-sniper/Lib.SimpleCPUID

  SimpleCPUID is required only when both AllowCRCExtension and CRC_Functions
  symbols are defined and PurePascal symbol is not defined.
  Also, it might be needed by BitOps library, depending whether ASM extensions
  are allowed there.

===============================================================================}
unit CITY;

{$INCLUDE 'CITY_defs.inc'}

interface

uses
  AuxTypes;

type
  UInt128 = record
    case Integer of
      0:(QWords:  array[0..1] of UInt64);
      1:(First:   UInt64;
         Second:  UInt64);
      2:(Low:     UInt64;
         High:    UInt64);
  end;
  PUInt128 = ^UInt128;

Function UInt128Make(Low,High: UInt64): UInt128;

Function UInt128Low64(x: UInt128): UInt64;
Function UInt128High64(x: UInt128): UInt64;

//------------------------------------------------------------------------------

Function Hash128to64(x: UInt128): UInt64;

{$IFNDEF VER_1_0_3}
Function CityHash32(s: Pointer; len: TMemSize): UInt32;
{$ENDIF}

Function CityHash64(s: Pointer; len: TMemSize): UInt64;

Function CityHash64WithSeed(s: Pointer; len: TMemSize; seed: UInt64): UInt64;
Function CityHash64WithSeeds(s: Pointer; len: TMemSize; seed0,seed1: UInt64): UInt64;

Function CityHash128(s: Pointer; len: TMemSize): UInt128;
Function CityHash128WithSeed(s: Pointer; len: TMemSize; seed: UInt128): UInt128;

//------------------------------------------------------------------------------

{$IFDEF CRC_Functions}

type
  UInt256 = array[0..3] of UInt64;
  PUInt256 = ^UInt256;

procedure CityHashCrc256(s: Pointer; len: TMemSize; out result: UInt256);

Function CityHashCrc128WithSeed(s: Pointer; len: TMemSize; seed: UInt128): UInt128;
Function CityHashCrc128(s: Pointer; len: TMemSize): UInt128;

{$ENDIF}

const
{$IFNDEF VER_1_0_3}
  CITY_version = '1.0.3';
{$ELSE}
  CITY_version = '1.1.1';
{$ENDIF}

implementation

uses
  BitOps
{$IF Defined(AllowCRCExtension) and Defined(CRC_Functions) and not Defined(PurePascal)}
  , SimpleCPUID
{$IFEND};

Function UInt128Make(Low,High: UInt64): UInt128;
begin
Result.Low := Low;
Result.High := High;
end;

//------------------------------------------------------------------------------

Function UInt128Low64(x: UInt128): UInt64;
begin
Result := x.Low;
end;

//------------------------------------------------------------------------------

Function UInt128High64(x: UInt128): UInt64;
begin
Result := x.High;
end;

//==============================================================================

// Hash 128 input bits down to 64 bits of output.
// This is intended to be a reasonably good hash function.
Function Hash128to64(x: UInt128): UInt64;
const
  kMul: UInt64 = UInt64($9ddfea08eb382d69);
var
  a,b:  UInt64;
begin
a := (UInt128Low64(x) xor UInt128High64(x)) * kMul;
a := a xor (a shr 47);
b := (UInt128High64(x) xor a) * kMul;
b := b xor (b shr 47);
b := b * kMul;
Result := b;
end;

//==============================================================================

procedure SWAP(var a,b: UInt64);
var
  Temp: UInt64;
begin
Temp := a;
a := b;
b := Temp;
end;

//------------------------------------------------------------------------------

Function UNALIGNED_LOAD32(Ptr: Pointer): UInt32;
begin
Move(Ptr^,Addr(Result)^,SizeOf(Result));
end;

//------------------------------------------------------------------------------

Function UNALIGNED_LOAD64(Ptr: Pointer): UInt64;
begin
Move(Ptr^,Addr(Result)^,SizeOf(Result));
end;

//------------------------------------------------------------------------------

Function uint32_in_expected_order(x: UInt32): UInt32;
begin
{$IFDEF ENDIAN_BIG}
Result := EndianSwap(x);
{$ELSE}
Result := x;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function uint64_in_expected_order(x: UInt64): UInt64;
begin
{$IFDEF ENDIAN_BIG}
Result := EndianSwap(x);
{$ELSE}
Result := x;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function Fetch32(p: Pointer): UInt32; overload;
begin
Result := uint32_in_expected_order(UNALIGNED_LOAD32(p));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Fetch32(Addr: PtrUInt): UInt32; overload;
begin
Result := uint32_in_expected_order(UNALIGNED_LOAD32({%H-}Pointer(Addr)));
end;

//------------------------------------------------------------------------------

Function Fetch64(p: Pointer): UInt64; overload;
begin
Result := uint64_in_expected_order(UNALIGNED_LOAD64(p));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Fetch64(Addr: PtrUInt): UInt64; overload;
begin
Result := uint64_in_expected_order(UNALIGNED_LOAD64({%H-}Pointer(Addr)));
end;

//==============================================================================

const
  // Some primes between 2^63 and 2^64 for various uses.
  _k0 = UInt64($c3a5c85c97cb3127);
  _k1 = UInt64($b492b66fbe98f273);
  _k2 = UInt64($9ae16a3b2f90404f);

  // to prevent internal error 200706094 in fpc during compilation...
  k0: UInt64 = _k0;
  k1: UInt64 = _k1;
  k2: UInt64 = _k2;

{$IFDEF VER_1_0_3}
  _k3 = UInt64($c949d7c7509e6557);

  k3: UInt64 = _k3;
{$ENDIF}

{$IFNDEF VER_1_0_3}
  // Magic numbers for 32-bit hashing.  Copied from Murmur3.
  c1 = UInt32($cc9e2d51);
  c2 = UInt32($1b873593);
{$ENDIF}

//-----------------------------------------------------------------------------

{$IFNDEF VER_1_0_3}
// A 32-bit to 32-bit integer hash copied from Murmur3.
Function fmix(h: UInt32): UInt32;
begin
h := h xor (h shr 16);
h := h * $85ebca6b;
h := h xor (h shr 13);
h := h * $c2b2ae35;
Result := h xor (h shr 16);
end;

//------------------------------------------------------------------------------

Function Rotate32(val: UInt32; Shift: Integer): UInt32;
begin
// Avoid shifting by 32: doing so yields an undefined result.
If shift = 0 then Result := val
  else Result := ROR(Val,Byte(Shift));
end;

//------------------------------------------------------------------------------

procedure PERMUTE3(var a,b,c: UInt32); overload;
var
  Temp: UInt32;
begin
Temp := c;
c := b;
b := a;
a := Temp;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure PERMUTE3(var a,b,c: UInt64); overload;
var
  Temp: UInt64;
begin
Temp := c;
c := b;
b := a;
a := Temp;
end;

//------------------------------------------------------------------------------

Function Mur(a,h: UInt32): UInt32;
begin
// Helper from Murmur3 for combining two 32-bit values.
a := a * c1;
a := Rotate32(a,17);
a := a * c2;
h := h xor a;
h := Rotate32(h,19);
Result := h * 5 + $e6546b64;
end;

//------------------------------------------------------------------------------

Function Hash32Len13to24(s: Pointer; len: TMemSize): UInt32;
var
  a,b,c,d,e,f,h:  UInt32;
begin
a := Fetch32({%H-}PtrUInt(s) - 4 + (len shr 1));
b := Fetch32({%H-}PtrUInt(s) + 4);
c := Fetch32({%H-}PtrUInt(s) + len {%H-}- 8);
d := Fetch32({%H-}PtrUInt(s) + (len shr 1));
e := Fetch32(s);
f := Fetch32({%H-}PtrUInt(s) + len {%H-}- 4);
h := len;
result := fmix(Mur(f,Mur(e,Mur(d,Mur(c,Mur(b,Mur(a,h)))))));
end;

//------------------------------------------------------------------------------

Function Hash32Len0to4(s: Pointer; len: TMemSize): UInt32;
var
  b,c:  UInt32;
  i:    TMemSize;
  v:    Int8;
begin
b := 0;
c := 9;
If Len > 0 then
  For i := 0 to Pred(Len) do
    begin
      v := {%H-}PInt8({%H-}PtrUInt(s) + i)^;
      b := Int64(b * c1) {%H-}+ v;
      c := c xor b;
    end;
Result := fmix(Mur(b,Mur(len,c)));    
end;

//------------------------------------------------------------------------------

Function Hash32Len5to12(s: Pointer; len: TMemSize): UInt32;
var
  a,b,c,d:  UInt32;
begin
a := len; b := len * 5; c := 9; d := b;
a := a + Fetch32(s);
b := b + Fetch32({%H-}PtrUInt(s) + len {%H-}- 4);
c := c + Fetch32({%H-}PtrUInt(s) + ((len shr 1) and 4));
Result := fmix(Mur(c,Mur(b,Mur(a,d))));
end;

//------------------------------------------------------------------------------

Function CityHash32(s: Pointer; len: TMemSize): UInt32;
var
  h,g,f:          UInt32;
  a0,a1,a2,a3,a4: UInt32;
  iters:          TMemSize;
begin
If len <= 24 then
  begin
    case len of
      0..4:   Result := Hash32Len0to4(s,len);
      5..12:  Result := Hash32Len5to12(s,len);
    else
      Result := Hash32Len13to24(s,len);
    end;
    Exit;
  end;
// len > 24
h := len; g := c1 * len; f := g;
a0 := Rotate32(Fetch32({%H-}PtrUInt(s) + len {%H-}- 4) * c1,17) * c2;
a1 := Rotate32(Fetch32({%H-}PtrUInt(s) + len {%H-}- 8) * c1,17) * c2;
a2 := Rotate32(Fetch32({%H-}PtrUInt(s) + len {%H-}- 16) * c1,17) * c2;
a3 := Rotate32(Fetch32({%H-}PtrUInt(s) + len {%H-}- 12) * c1,17) * c2;
a4 := Rotate32(Fetch32({%H-}PtrUInt(s) + len {%H-}- 20) * c1,17) * c2;
h := h xor a0;
h := Rotate32(h,19);
h := h * 5 + $e6546b64;
h := h xor a2;
h := Rotate32(h,19);
h := h * 5 + $e6546b64;
g := g xor a1;
g := Rotate32(g,19);
g := g * 5 + $e6546b64;
g := g xor a3;
g := Rotate32(g,19);
g := g * 5 + $e6546b64;
f := f + a4;
f := Rotate32(f,19);
f := f * 5 + $e6546b64;
iters := (len - 1) div 20;
repeat
  a0 := Rotate32(Fetch32(s) * c1,17) * c2;
  a1 := Fetch32({%H-}PtrUInt(s) + 4);
  a2 := Rotate32(Fetch32({%H-}PtrUInt(s) + 8) * c1,17) * c2;
  a3 := Rotate32(Fetch32({%H-}PtrUInt(s) + 12) * c1,17) * c2;
  a4 := Fetch32({%H-}PtrUInt(s) + 16);
  h := h xor a0;
  h := Rotate32(h,18);
  h := h * 5 + $e6546b64;
  f := f + a1;
  f := Rotate32(f,19);
  f := f * c1;
  g := g + a2;
  g := Rotate32(g,18);
  g := g * 5 + $e6546b64;
  h := h xor (a3 + a1);
  h := Rotate32(h,19);
  h := h * 5 + $e6546b64;
  g := g xor a4;
  g := EndianSwap(g) * 5;
  h := h + (a4 * 5);
  h := EndianSwap(h);
  f := f + a0;
  PERMUTE3(f,h,g);
  s := {%H-}Pointer({%H-}PtrUInt(s) + 20);
  Dec(iters);
until iters <= 0;
g := Rotate32(g,11) * c1;
g := Rotate32(g,17) * c1;
f := Rotate32(f,11) * c1;
f := Rotate32(f,17) * c1;
h := Rotate32(h + g, 19);
h := h * 5 + $e6546b64;
h := Rotate32(h,17) * c1;
h := Rotate32(h + f, 19);
h := h * 5 + $e6546b64;
h := Rotate32(h,17) * c1;
Result := h;
end;
{$ENDIF}

//------------------------------------------------------------------------------

// Bitwise right rotate.  Normally this will compile to a single
// instruction, especially if the shift is a manifest constant.
Function Rotate(val: UInt64; shift: Integer): UInt64;
begin
// Avoid shifting by 64: doing so yields an undefined result.
If shift = 0 then Result := val
  else Result := ROR(val,Byte(Shift));
end;

//------------------------------------------------------------------------------

{$IFDEF VER_1_0_3}
// Equivalent to Rotate(), but requires the second arg to be non-zero.
// On x86-64, and probably others, it's possible for this to compile
// to a single instruction if both args are already in registers.
Function RotateByAtLeast1(val: UInt64; shift: Integer): UInt64;
begin
Result := ROR(val,Byte(Shift));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ShiftMix(val: UInt64): UInt64;
begin
Result := val xor (val shr 47);
end;

//------------------------------------------------------------------------------

Function HashLen16(u,v: UInt64): UInt64; overload;
begin
Result := Hash128to64(UInt128Make(u,v));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFNDEF VER_1_0_3}
Function HashLen16(u,v,mul: UInt64): UInt64; overload;
var
  a,b:  UInt64;
begin
// Murmur-inspired hashing.
a := (u xor v) * mul;
a := a xor (a shr 47);
b := (v xor a) * mul;
b := b xor (b shr 47);
b := b * mul;
Result := b;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function HashLen0to16(s: Pointer; len: TMemSize): UInt64;
var
  a,b:      UInt64;
{$IFNDEF VER_1_0_3}
  mul,c,d:  UInt64;
{$ENDIF}
  a8,b8,c8: UInt8;
  y,z:      UInt32;
begin
{$IFDEF VER_1_0_3}
case len of
  1..3:
    begin
      a8 := PUInt8(s)^;
      b8 := {%H-}PUInt8({%H-}PtrUInt(s) + (len shr 1))^;
      c8 := {%H-}PUInt8({%H-}PtrUInt(s) + (len - 1))^;
      y := UInt32(a8) + UInt32(UInt32(b8) shl 8);
      z := len + UInt32(UInt32(c8) shl 2);
      Result := ShiftMix((y * k2) xor (z * k3)) * k2;
    end;
  4..8:
    begin
      a := Fetch32(s);
      Result := HashLen16(len + UInt64(a shl 3),Fetch32({%H-}PtrUInt(s) + len {%H-}- 4));
    end;
  9..High(len):
    begin
      a := Fetch64(s);
      b := Fetch64({%H-}PtrUInt(s) + len {%H-}- 8);
      Result := HashLen16(a,RotateByAtLeast1(b + len,len)) xor b;
    end;
else
  Result := k2;
end;
{$ELSE}
case len of
  1..3:
    begin
      a8 := PUInt8(s)^;
      b8 := {%H-}PUInt8({%H-}PtrUInt(s) + (len shr 1))^;
      c8 := {%H-}PUInt8({%H-}PtrUInt(s) + (len - 1))^;
      y := UInt32(a8) + UInt32(UInt32(b8) shl 8);
      z := len + UInt32(UInt32(c8) shl 2);
      Result := ShiftMix((y * k2) xor (z * k0)) * k2;
    end;
  4..7:
    begin
      mul := k2 + UInt64(len) * 2;
      a := Fetch32(s);
      Result := HashLen16(len + UInt64(a shl 3),Fetch32({%H-}PtrUInt(s) + len {%H-}- 4),mul);
    end;
  8..High(len):
    begin
      mul := k2 + UInt64(len) * 2;
      a := Fetch64(s) + k2;
      b := Fetch64({%H-}PtrUInt(s) + len {%H-}- 8);
      c := Rotate(b,37) * mul + a;
      d := (Rotate(a,25) + b) * mul;
      Result := HashLen16(c,d,mul);
    end;
else
  Result := k2;
end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

// This probably works well for 16-byte strings as well, but it may be overkill
// in that case.
Function HashLen17to32(s: Pointer; len: TMemSize): UInt64;
var
  a,b,c,d:  UInt64;
{$IFNDEF VER_1_0_3}
  mul:      UInt64;
{$ENDIF}
begin
{$IFNDEF VER_1_0_3}
mul := k2 + UInt64(len) * 2;
{$ENDIF}
a := Fetch64(s) * k1;
b := Fetch64({%H-}PtrUInt(s) + 8);
{$IFDEF VER_1_0_3}
c := Fetch64({%H-}PtrUInt(s) + len {%H-}- 8) * k2;
d := Fetch64({%H-}PtrUInt(s) + len {%H-}- 16) * k0;
Result := HashLen16(Rotate(a - b,43) + Rotate(c,30) + d,a + Rotate(b xor k3,20) - c + len);
{$ELSE}
c := Fetch64({%H-}PtrUInt(s) + len {%H-}- 8) * mul;
d := Fetch64({%H-}PtrUInt(s) + len {%H-}- 16) * k2;
Result := HashLen16(Rotate(a + b,43) + Rotate(c,30) + d,a + Rotate(b + k2,18) + c,mul);
{$ENDIF}
end;

//------------------------------------------------------------------------------

// Return a 16-byte hash for 48 bytes.  Quick and dirty.
// Callers do best to use "random-looking" values for a and b.
Function WeakHashLen32WithSeeds(w,x,y,z,a,b: UInt64): UInt128; overload;
var
  c:  UInt64;
begin
a := a + w;
b := Rotate(b + a + z,21);
c := a;
a := a + x;
a := a + y;
b := b + Rotate(a,44);
Result.First := a + z;
Result.Second := b + c;
end;

//------------------------------------------------------------------------------

// Return a 16-byte hash for s[0] ... s[31], a, and b.  Quick and dirty.
Function WeakHashLen32WithSeeds(s: Pointer; a,b: UInt64): UInt128; overload;
begin
Result := WeakHashLen32WithSeeds(Fetch64(s),Fetch64({%H-}PtrUInt(s) + 8),
            Fetch64({%H-}PtrUInt(s) + 16),Fetch64({%H-}PtrUInt(s) + 24),a,b);
end;

//------------------------------------------------------------------------------

// Return an 8-byte hash for 33 to 64 bytes.
Function HashLen33to64(s: Pointer; len: TMemSize): UInt64;
{$IFDEF VER_1_0_3}
var
  a,b,c,z,vf,vs,wf,ws,r:  UInt64;
begin
z := Fetch64({%H-}PtrUInt(s) + 24);
a := Fetch64(s) + (len + Fetch64({%H-}PtrUInt(s) + len {%H-}- 16)) * k0;
b := Rotate(a + z,52);
c := Rotate(a,37);
a := a + Fetch64({%H-}PtrUInt(s) + 8);
c := c + Rotate(a,7);
a := a + Fetch64({%H-}PtrUInt(s) + 16);
vf := a + z;
vs := b + Rotate(a,31) + c;
a := Fetch64({%H-}PtrUInt(s) + 16) + Fetch64({%H-}PtrUInt(s) + len {%H-}- 32);
z := Fetch64({%H-}PtrUInt(s) + len {%H-}- 8);
b := Rotate(a + z,52);
c := Rotate(a,37);
a := a + Fetch64({%H-}PtrUInt(s) + len {%H-}- 24);
c := c + Rotate(a,7);
a := a + Fetch64({%H-}PtrUInt(s) + len {%H-}- 16);
wf := a + z;
ws := b + Rotate(a,31) + c;
r := ShiftMix((vf + ws) * k2 + (wf + vs) * k0);
Result := ShiftMix(r * k0 + vs) * k2;
end;
{$ELSE}
var
  mul,a,b,c,d,e,f,g,h,u,v,w,x,y,z:  UInt64;
begin
mul := k2 + UInt64(len) * 2;
a := Fetch64(s) * k2;
b := Fetch64({%H-}PtrUInt(s) + 8);
c := Fetch64({%H-}PtrUInt(s) + len {%H-}- 24);
d := Fetch64({%H-}PtrUInt(s) + len {%H-}- 32);
e := Fetch64({%H-}PtrUInt(s) + 16) * k2;
f := Fetch64({%H-}PtrUInt(s) + 24) * 9;
g := Fetch64({%H-}PtrUInt(s) + len {%H-}- 8);
h := Fetch64({%H-}PtrUInt(s) + len {%H-}- 16) * mul;
u := Rotate(a + g,43) + (Rotate(b,30) + c) * 9;
v := ((a + g) xor d) + f + 1;
w := EndianSwap((u + v) * mul) + h;
x := Rotate(e + f, 42) + c;
y := (EndianSwap((v + w) * mul) + g) * mul;
z := e + f + c;
a := EndianSwap((x + z) * mul + y) + b;
b := ShiftMix((z + a) * mul + d + h) * mul;
Result := b + x;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function CityHash64(s: Pointer; len: TMemSize): UInt64;
var
  x,y,z:  UInt64;
  v,w:    UInt128;
begin
If Len <= 64 then
  begin
    case len of
       0..16: Result := HashLen0to16(s,len);
      17..32: Result := HashLen17to32(s,len);
    else
      Result := HashLen33to64(s,len);
    end;
    Exit;
  end;
// For strings over 64 bytes we hash the end first, and then as we
// loop we keep 56 bytes of state: v, w, x, y, and z.
x := Fetch64({%H-}PtrUInt(s) + len {%H-}- 40);
y := Fetch64({%H-}PtrUInt(s) + len {%H-}- 16) + Fetch64({%H-}PtrUInt(s) + len {%H-}- 56);
z := HashLen16(Fetch64({%H-}PtrUInt(s) + len {%H-}- 48) + len,Fetch64({%H-}PtrUInt(s) + len {%H-}- 24));
v := WeakHashLen32WithSeeds({%H-}Pointer({%H-}PtrUInt(s) + len {%H-}- 64),len,z);
w := WeakHashLen32WithSeeds({%H-}Pointer({%H-}PtrUInt(s) + len {%H-}- 32),y + k1,x);
x := x * k1 + Fetch64(s);
// Decrease len to the nearest multiple of 64, and operate on 64-byte chunks.
len := (len - 1) and not TMemSize(63);
repeat
  x := Rotate(x + y + v.First + Fetch64({%H-}PtrUInt(s) + 8),37) * k1;
  y := Rotate(y + v.Second + Fetch64({%H-}PtrUInt(s) + 48), 42) * k1;
  x := x xor w.Second;
  y := y + (v.First + Fetch64({%H-}PtrUInt(s) + 40));
  z := Rotate(z + w.First,33) * k1;
  v := WeakHashLen32WithSeeds(s,v.Second * k1,x + w.First);
  w := WeakHashLen32WithSeeds({%H-}Pointer({%H-}PtrUInt(s) + 32),z + w.Second,y + Fetch64({%H-}PtrUInt(s) + 16));
  SWAP(x,z);
  s := {%H-}Pointer({%H-}PtrUInt(s) + 64);
  len := len - 64;
until len <= 0;
Result := HashLen16(HashLen16(v.First,w.First) + ShiftMix(y) * k1 + z,
                    HashLen16(v.Second,w.Second) + x);
end;

//------------------------------------------------------------------------------

Function CityHash64WithSeeds(s: Pointer; len: TMemSize; seed0,seed1: UInt64): UInt64;
begin
Result := HashLen16(CityHash64(s,len) - seed0, seed1);
end;

//------------------------------------------------------------------------------

Function CityHash64WithSeed(s: Pointer; len: TMemSize; seed: UInt64): UInt64;
begin
Result := CityHash64WithSeeds(s,len,k2,seed);
end;

//------------------------------------------------------------------------------

// A subroutine for CityHash128().  Returns a decent 128-bit hash for strings
// of any length representable in signed long.  Based on City and Murmur.
Function CityMurmur(s: Pointer; len: TMemSize; seed: UInt128): UInt128;
var
  a,b,c,d:  UInt64;
  l:        PtrInt;
begin
a := UInt128Low64(seed);
b := UInt128High64(seed);
l := PtrInt(len) - 16;
If l <= 0 then  // len <= 16
  begin
    a := ShiftMix(a * k1) * k1;
    c := b * k1 + HashLen0to16(s,len);
    If len >= 8 then
      d := ShiftMix(a + Fetch64(s))
    else
      d := ShiftMix(a + c);
  end
else  // len > 16
  begin
    c := HashLen16(Fetch64({%H-}PtrUInt(s) + len {%H-}- 8) + k1,a);
    d := HashLen16(b + len,c + Fetch64({%H-}PtrUInt(s) + len {%H-}- 16));
    a := a + d;
    repeat
      a := a xor ShiftMix(Fetch64(s) * k1) * k1;
      a := a * k1;
      b := b xor a;
      c := c xor (ShiftMix(Fetch64({%H-}PtrUInt(s) + 8) * k1) * k1);
      c := c * k1;
      d := d xor c;
      s := {%H-}Pointer({%H-}PtrUInt(s) + 16);
      l := l - 16;
    until l <= 0;
  end;
a := HashLen16(a,c);
b := HashLen16(d,b);
Result := UInt128Make(a xor b,HashLen16(b,a));
end;

//------------------------------------------------------------------------------

Function CityHash128WithSeed(s: Pointer; len: TMemSize; seed: UInt128): UInt128;
var
  v,w:        UInt128;
  x,y,z:      UInt64;
  tail_done:  TMemSize;
begin
If len < 128 then
  begin
    Result := CityMurmur(s,len,seed);
    Exit;
  end;
// We expect len >= 128 to be the common case.  Keep 56 bytes of state:
// v, w, x, y, and z.
x := UInt128Low64(seed);
y := UInt128High64(seed);
z := len * k1;
v.First := Rotate(y xor k1,49) * k1 + Fetch64(s);
v.Second := Rotate(v.First,42) * k1 + Fetch64({%H-}PtrUInt(s) + 8);
w.First := Rotate(y + z,35) * k1 + x;
w.Second := Rotate(x + Fetch64({%H-}PtrUInt(s) + 88),53) * k1;
// This is the same inner loop as CityHash64(), manually unrolled.
repeat
  x := Rotate(x + y + v.First + Fetch64({%H-}PtrUInt(s) + 8),37) * k1;
  y := Rotate(y + v.Second + Fetch64({%H-}PtrUInt(s) + 48),42) * k1;
  x := x xor w.Second;
  y := y + (v.First + Fetch64({%H-}PtrUInt(s) + 40));
  z := Rotate(z + w.First,33) * k1;
  v := WeakHashLen32WithSeeds(s,v.Second * k1,x + w.First);
  w := WeakHashLen32WithSeeds({%H-}Pointer({%H-}PtrUInt(s) + 32),z + w.Second,y + Fetch64({%H-}PtrUInt(s) + 16));
  SWAP(x,z);
  s := {%H-}Pointer({%H-}PtrUInt(s) + 64);
  x := Rotate(x + y + v.First + Fetch64({%H-}PtrUInt(s) + 8),37) * k1;
  y := Rotate(y + v.Second + Fetch64({%H-}PtrUInt(s) + 48),42) * k1;
  x := x xor w.Second;
  y := y + (v.First + Fetch64({%H-}PtrUInt(s) + 40));
  z := Rotate(z + w.First,33) * k1;
  v := WeakHashLen32WithSeeds(s,v.Second * k1,x + w.First);
  w := WeakHashLen32WithSeeds({%H-}Pointer({%H-}PtrUInt(s) + 32),z + w.Second,y + Fetch64({%H-}PtrUInt(s) + 16));
  SWAP(x,z);
  s := {%H-}Pointer({%H-}PtrUInt(s) + 64);
  len := len - 128;
until len < 128;
x := x + (Rotate(v.First + z,49) * k0);
{$IFDEF VER_1_0_3}
z := z + Rotate(w.First,37) * k0;
{$ELSE}
y := y * k0 + Rotate(w.Second,37);
z := z * k0 + Rotate(w.First,27);
w.First := w.First * 9;
v.First := v.First * k0;
{$ENDIF}
// If 0 < len < 128, hash up to 4 chunks of 32 bytes each from the end of s.
tail_done := 0;
while tail_done < len do
  begin
    tail_done := tail_done + 32;
    y := Rotate(x + y,42) * k0 + v.Second;
    w.First := w.First + Fetch64({%H-}PtrUInt(s) + len {%H-}- tail_done + 16);
    x := x * k0 + w.First;
    z := z + (w.Second + Fetch64({%H-}PtrUInt(s) + len {%H-}- tail_done));
    w.Second := w.Second + v.First;
    v := WeakHashLen32WithSeeds({%H-}Pointer({%H-}PtrUInt(s) + len {%H-}- tail_done),v.First + z,v.Second);
  {$IFNDEF VER_1_0_3}
    v.First := v.First * k0;
  {$ENDIF}  
  end;
// At this point our 56 bytes of state should contain more than
// enough information for a strong 128-bit hash.  We use two
// different 56-byte-to-8-byte hashes to get a 16-byte final result.
x := HashLen16(x,v.First);
y := HashLen16(y + z,w.First);
Result := UInt128Make(HashLen16(x + v.Second,w.Second) + y,
                      HashLen16(x + w.Second,y + v.Second));
end;

//------------------------------------------------------------------------------

Function CityHash128(s: Pointer; len: TMemSize): UInt128;
begin
{$IFDEF VER_1_0_3}
If len >= 16 then
  Result := CityHash128WithSeed({%H-}Pointer({%H-}PtrUInt(s) + 16),len - 16,UInt128Make(Fetch64(s) xor k3,Fetch64({%H-}PtrUInt(s) + 8)))
else If len >= 8 then
  Result := CityHash128WithSeed(nil,0,UInt128Make(Fetch64(s) xor (len * k0),Fetch64({%H-}PtrUInt(s) + len {%H-}- 8) xor k1))
else
  Result := CityHash128WithSeed(s,len,UInt128Make(k0,k1));
{$ELSE}
If len >= 16 then
  Result := CityHash128WithSeed({%H-}Pointer({%H-}PtrUInt(s) + 16),len - 16,UInt128Make(Fetch64(s),Fetch64({%H-}PtrUInt(s) + 8) + k0))
else
  Result := CityHash128WithSeed(s,len,UInt128Make(k0,k1));
{$ENDIF}
end;

//==============================================================================

{$IFDEF CRC_Functions}

// Software implemntation of the SSE4.2 intrinsic
Function _mm_crc32_u64_PAS(crc,v: UInt64): UInt64;
const
  CRCTable: array[Byte] of UInt32 = (
    $00000000, $F26B8303, $E13B70F7, $1350F3F4, $C79A971F, $35F1141C, $26A1E7E8, $D4CA64EB,
    $8AD958CF, $78B2DBCC, $6BE22838, $9989AB3B, $4D43CFD0, $BF284CD3, $AC78BF27, $5E133C24,
    $105EC76F, $E235446C, $F165B798, $030E349B, $D7C45070, $25AFD373, $36FF2087, $C494A384,
    $9A879FA0, $68EC1CA3, $7BBCEF57, $89D76C54, $5D1D08BF, $AF768BBC, $BC267848, $4E4DFB4B,
    $20BD8EDE, $D2D60DDD, $C186FE29, $33ED7D2A, $E72719C1, $154C9AC2, $061C6936, $F477EA35,
    $AA64D611, $580F5512, $4B5FA6E6, $B93425E5, $6DFE410E, $9F95C20D, $8CC531F9, $7EAEB2FA,
    $30E349B1, $C288CAB2, $D1D83946, $23B3BA45, $F779DEAE, $05125DAD, $1642AE59, $E4292D5A,
    $BA3A117E, $4851927D, $5B016189, $A96AE28A, $7DA08661, $8FCB0562, $9C9BF696, $6EF07595,
    $417B1DBC, $B3109EBF, $A0406D4B, $522BEE48, $86E18AA3, $748A09A0, $67DAFA54, $95B17957,
    $CBA24573, $39C9C670, $2A993584, $D8F2B687, $0C38D26C, $FE53516F, $ED03A29B, $1F682198,
    $5125DAD3, $A34E59D0, $B01EAA24, $42752927, $96BF4DCC, $64D4CECF, $77843D3B, $85EFBE38,
    $DBFC821C, $2997011F, $3AC7F2EB, $C8AC71E8, $1C661503, $EE0D9600, $FD5D65F4, $0F36E6F7,
    $61C69362, $93AD1061, $80FDE395, $72966096, $A65C047D, $5437877E, $4767748A, $B50CF789,
    $EB1FCBAD, $197448AE, $0A24BB5A, $F84F3859, $2C855CB2, $DEEEDFB1, $CDBE2C45, $3FD5AF46,
    $7198540D, $83F3D70E, $90A324FA, $62C8A7F9, $B602C312, $44694011, $5739B3E5, $A55230E6,
    $FB410CC2, $092A8FC1, $1A7A7C35, $E811FF36, $3CDB9BDD, $CEB018DE, $DDE0EB2A, $2F8B6829,
    $82F63B78, $709DB87B, $63CD4B8F, $91A6C88C, $456CAC67, $B7072F64, $A457DC90, $563C5F93,
    $082F63B7, $FA44E0B4, $E9141340, $1B7F9043, $CFB5F4A8, $3DDE77AB, $2E8E845F, $DCE5075C,
    $92A8FC17, $60C37F14, $73938CE0, $81F80FE3, $55326B08, $A759E80B, $B4091BFF, $466298FC,
    $1871A4D8, $EA1A27DB, $F94AD42F, $0B21572C, $DFEB33C7, $2D80B0C4, $3ED04330, $CCBBC033,
    $A24BB5A6, $502036A5, $4370C551, $B11B4652, $65D122B9, $97BAA1BA, $84EA524E, $7681D14D,
    $2892ED69, $DAF96E6A, $C9A99D9E, $3BC21E9D, $EF087A76, $1D63F975, $0E330A81, $FC588982,
    $B21572C9, $407EF1CA, $532E023E, $A145813D, $758FE5D6, $87E466D5, $94B49521, $66DF1622,
    $38CC2A06, $CAA7A905, $D9F75AF1, $2B9CD9F2, $FF56BD19, $0D3D3E1A, $1E6DCDEE, $EC064EED,
    $C38D26C4, $31E6A5C7, $22B65633, $D0DDD530, $0417B1DB, $F67C32D8, $E52CC12C, $1747422F,
    $49547E0B, $BB3FFD08, $A86F0EFC, $5A048DFF, $8ECEE914, $7CA56A17, $6FF599E3, $9D9E1AE0,
    $D3D3E1AB, $21B862A8, $32E8915C, $C083125F, $144976B4, $E622F5B7, $F5720643, $07198540,
    $590AB964, $AB613A67, $B831C993, $4A5A4A90, $9E902E7B, $6CFBAD78, $7FAB5E8C, $8DC0DD8F,
    $E330A81A, $115B2B19, $020BD8ED, $F0605BEE, $24AA3F05, $D6C1BC06, $C5914FF2, $37FACCF1,
    $69E9F0D5, $9B8273D6, $88D28022, $7AB90321, $AE7367CA, $5C18E4C9, $4F48173D, $BD23943E,
    $F36E6F75, $0105EC76, $12551F82, $E03E9C81, $34F4F86A, $C69F7B69, $D5CF889D, $27A40B9E,
    $79B737BA, $8BDCB4B9, $988C474D, $6AE7C44E, $BE2DA0A5, $4C4623A6, $5F16D052, $AD7D5351);
var
  Input:  array[0..7] of Byte absolute v;
  Temp:   UInt32;
  i:      Integer;
begin
Temp := UInt32(crc);
For i := Low(Input) to High(Input) do
  Temp := CRCTable[Byte(Temp xor UInt32(Input[i]))] xor (Temp shr 8);
Result := UInt64(Temp);
end;

//------------------------------------------------------------------------------

{$IFNDEF PurePascal}

Function _mm_crc32_u64_ASM(crc,v: UInt64): UInt64; register; assembler;
asm
{$IFDEF x64}
    CRC32   crc,  v
    MOV     RAX,  crc
{$ELSE}
    MOV     EAX,  dword ptr [CRC]

  {$IFDEF ASM_MachineCode}
    DB  $F2, $0F, $38, $F1, $45, $08    // CRC32   EAX,  dword ptr [EBP + 8]
    DB  $F2, $0F, $38, $F1, $45, $0C    // CRC32   EAX,  dword ptr [EPB + 12]
  {$ELSE}
    CRC32   EAX,  dword ptr [v]
    CRC32   EAX,  dword ptr [v + 4]
  {$ENDIF}

    XOR     EDX,  EDX
{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

var
  _mm_crc32_u64:  Function(crc,v: UInt64): UInt64;

//==============================================================================

// Requires len >= 240.
procedure CityHashCrc256Long(s: Pointer; len: TMemSize; seed: UInt32; out result: UInt256);
{$IFDEF VER_1_0_3}
var
  a,b,c,d,e,f,g,h,i,j,t:  UInt64;
  iters:                  TMemSize;

  procedure CHUNK(multiplier,z: UInt64);
  var
    old_a:  Uint64;
  begin
    old_a := a;
    a := Rotate(b,41 xor z) * multiplier + Fetch64(s);
    b := Rotate(c,27 xor z) * multiplier + Fetch64({%H-}PtrUInt(s) + 8);
    c := Rotate(d,41 xor z) * multiplier + Fetch64({%H-}PtrUInt(s) + 16);
    d := Rotate(e,33 xor z) * multiplier + Fetch64({%H-}PtrUInt(s) + 24);
    e := Rotate(t,25 xor z) * multiplier + Fetch64({%H-}PtrUInt(s) + 32);
    t := old_a;
    f := _mm_crc32_u64(f,a);
    g := _mm_crc32_u64(g,b);
    h := _mm_crc32_u64(h,c);
    i := _mm_crc32_u64(i,d);
    j := _mm_crc32_u64(j,e);
    s := {%H-}Pointer({%H-}PtrUInt(s) + 40);
  end;

begin
a := Fetch64({%H-}PtrUInt(s) + 56) + k0;
b := Fetch64({%H-}PtrUInt(s) + 96) + k0;
result[0] := HashLen16(b,len);
c := result[0];
result[1] := Fetch64({%H-}PtrUInt(s) + 120) * k0 + len;
d := result[1];
e := Fetch64({%H-}PtrUInt(s) + 184) + seed;
f := seed;
g := 0;
h := 0;
i := 0;
j := 0;
t := c + d;
// 240 bytes of input per iter.
iters := len div 240;
len := len {%H-}- (iters * 240);
repeat
  CHUNK(1,1); CHUNK(k0,0);
  CHUNK(1,1); CHUNK(k0,0);
  CHUNK(1,1); CHUNK(k0,0);
  Dec(iters);
until iters <= 0;
while len >= 40 do
  begin
    CHUNK(k0,0);
    len := len - 40;
  end;
If len > 0 then
  begin
    s := {%H-}Pointer({%H-}PtrUInt(s) + len {%H-}- 40);
    CHUNK(k0,0);
  end;
j := j + UInt64(i shl 32);
a := HashLen16(a,j);
h := h + UInt64(g shl 32);
b := b + h;
c := HashLen16(c,f) + i;
d := HashLen16(d,e + result[0]);
j := j + e;
i := i + HashLen16(h,t);
e := HashLen16(a,d) + j;
f := HashLen16(b,c) + a;
g := HashLen16(j,i) + c;
result[0] := e + f + g + h;
a := ShiftMix((a + g) * k0) * k0 + b;
result[1] := result[1] + a + result[0];
a := ShiftMix(a * k0) * k0 + c;
result[2] := a + result[1];
a := ShiftMix((a + e) * k0) * k0;
result[3] := a + result[2];
end;
{$ELSE}
var
  a,b,c,d,e,f,g,h,x,y,z:  UInt64;
  iters:                  TMemSize;

  procedure CHUNK(r: Integer);
  begin
    PERMUTE3(x,z,y);
    b := b + Fetch64(s);
    c := c + Fetch64({%H-}PtrUInt(s) + 8);
    d := d + Fetch64({%H-}PtrUInt(s) + 16);
    e := e + Fetch64({%H-}PtrUInt(s) + 24);
    f := f + Fetch64({%H-}PtrUInt(s) + 32);
    a := a + b;
    h := h + f;
    b := b + c;
    f := f + d;
    g := g + e;
    e := e + z;
    g := g + x;
    z := _mm_crc32_u64(z,b + g);
    y := _mm_crc32_u64(y,e + h);
    x := _mm_crc32_u64(x,f + a);
    e := Rotate(e,r);
    c := c + e;
    s := {%H-}Pointer({%H-}PtrUInt(s) + 40);
  end;

begin
a := Fetch64({%H-}PtrUInt(s) + 56) + k0;
b := Fetch64({%H-}PtrUInt(s) + 96) + k0;
result[0] := HashLen16(b,len);
c := result[0];
result[1] := Fetch64({%H-}PtrUInt(s) + 120) * k0 + len;
d := result[1];
e := Fetch64({%H-}PtrUInt(s) + 184) + seed;
f := 0;
g := 0;
h := c + d;
x := seed;
y := 0;
z := 0;
// 240 bytes of input per iter.
iters := len div 240;
len := len {%H-}- (iters * 240);
repeat
  CHUNK(0);  PERMUTE3(a,h,c);
  CHUNK(33); PERMUTE3(a,h,f);
  CHUNK(0);  PERMUTE3(b,h,f);
  CHUNK(42); PERMUTE3(b,h,d);
  CHUNK(0);  PERMUTE3(b,h,e);
  CHUNK(33); PERMUTE3(a,h,e);
  Dec(iters);
until iters <= 0;
while len >= 40 do
  begin
    CHUNK(29);
    e := e xor Rotate(a,20);
    h := h + Rotate(b,30);
    g := g xor Rotate(c,40);
    f := f + Rotate(d,34);
    PERMUTE3(c,h,g);
    len := len - 40;
  end;
If len > 0 then
  begin
    s := {%H-}Pointer({%H-}PtrUInt(s) + len {%H-}- 40);
    CHUNK(33);
    e := e xor Rotate(a,43);
    h := h + Rotate(b,42);
    g := g xor Rotate(c,41);
    f := f + Rotate(d,40);
  end;
result[0] := result[0] xor h;
result[1] := result[1] xor g;
g := g + h;
a := HashLen16(a,g + z);
x := x + UInt64(y shl 32);
b := b + x;
c := HashLen16(c,z) + h;
d := HashLen16(d,e + result[0]);
g := g + e;
h := h + HashLen16(x,f);
e := HashLen16(a,d) + g;
z := HashLen16(b,c) + a;
y := HashLen16(g,h) + c;
result[0] := e + z + y + x;
a := ShiftMix((a + y) * k0) * k0 + b;
result[1] := result[1] + a + result[0];
a := ShiftMix(a * k0) * k0 + c;
result[2] := a + result[1];
a := ShiftMix((a + e) * k0) * k0;
result[3] := a + result[2];
end;
{$ENDIF}

//------------------------------------------------------------------------------

// Requires len < 240.
procedure CityHashCrc256Short(s: Pointer; len: TMemSize; out result: UInt256);
var
  buf:  array[0..239] of Byte;
begin
Move(s^,{%H-}buf,len);
FillChar(buf[len],240 - len,0);
CityHashCrc256Long(@buf,240,not UInt32(len),result)
end;

//------------------------------------------------------------------------------

procedure CityHashCrc256(s: Pointer; len: TMemSize; out result: UInt256);
begin
If len >= 240 then
  CityHashCrc256Long(s,len,0,result)
else
  CityHashCrc256Short(s,len,result);
end;

//------------------------------------------------------------------------------

Function CityHashCrc128WithSeed(s: Pointer; len: TMemSize; seed: UInt128): UInt128;
var
  res:  UInt256;
  u,v:  UInt64;
begin
If len <= 900 then
  Result := CityHash128WithSeed(s,len,seed)
else
  begin
    CityHashCrc256(s,len,res);
    u := UInt128High64(seed) + res[0];
    v := UInt128Low64(seed) + res[1];
    Result := UInt128Make(HashLen16(u,v + res[2]),HashLen16(Rotate(v,32),u * k0 + res[3]));
  end;
end;

//------------------------------------------------------------------------------

Function CityHashCrc128(s: Pointer; len: TMemSize): UInt128;
var
  res:  UInt256;
begin
If len <= 900 then
  Result := CityHash128(s,len)
else
  begin
    CityHashCrc256(s,len,res);
    Result := UInt128Make(res[2],res[3]);
  end;
end;

//==============================================================================

procedure Initialize;
begin
_mm_crc32_u64 := _mm_crc32_u64_PAS;
{$IF Defined(AllowCRCExtension) and not Defined(PurePascal)}
with TSimpleCPUID.Create do
try
  If Info.SupportedExtensions.CRC32 then
    _mm_crc32_u64 := _mm_crc32_u64_ASM;
finally
  Free;
end;
{$IFEND}
end;

//------------------------------------------------------------------------------

initialization
  Initialize;

{$ENDIF CRC_Functions}

end.
