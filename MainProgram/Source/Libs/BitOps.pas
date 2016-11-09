{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  BitOps - Binary operations

  ©František Milt 2016-08-11

  Version 1.4

===============================================================================}
unit BitOps;

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE 64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL '16bit CPU not supported'}
{$ELSE}
  {$DEFINE 32bit}
{$IFEND}

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
  {$IF not(defined(WINDOWS) or defined(MSWINDOWS))}
    {$DEFINE PurePascal}
  {$IFEND}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF defined(FPC) and not defined(PurePascal)}
  {$ASMMODE Intel}
{$IFEND}

{
  When defined, PopCount functions will use lookup table instead of testing each
  bit in a passed value.
}
{$DEFINE UseLookupTable}

interface

uses
  AuxTypes;

{------------------------------------------------------------------------------}
{==============================================================================}
{                  Integer number <-> Bit string conversions                   }
{==============================================================================}
{------------------------------------------------------------------------------}

Function NumberToBits(Number: UInt8): String; overload;
Function NumberToBits(Number: UInt16): String; overload;
Function NumberToBits(Number: UInt32): String; overload;
Function NumberToBits(Number: UInt64): String; overload;

Function BitsToNumber(const BitString: String): UInt64;

{------------------------------------------------------------------------------}
{==============================================================================}
{                               Rotate left (ROL)                              }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ROL(Value: UInt8; Shift: Byte): UInt8; overload;
Function ROL(Value: UInt16; Shift: Byte): UInt16; overload;
Function ROL(Value: UInt32; Shift: Byte): UInt32; overload;
Function ROL(Value: UInt64; Shift: Byte): UInt64; overload;

procedure ROLValue(var Value: UInt8; Shift: Byte); overload;
procedure ROLValue(var Value: UInt16; Shift: Byte); overload;
procedure ROLValue(var Value: UInt32; Shift: Byte); overload;
procedure ROLValue(var Value: UInt64; Shift: Byte); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Rotate right (ROR)                              }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ROR(Value: UInt8; Shift: Byte): UInt8; overload;
Function ROR(Value: UInt16; Shift: Byte): UInt16; overload;
Function ROR(Value: UInt32; Shift: Byte): UInt32; overload;
Function ROR(Value: UInt64; Shift: Byte): UInt64; overload;

procedure RORValue(var Value: UInt8; Shift: Byte); overload;
procedure RORValue(var Value: UInt16; Shift: Byte); overload;
procedure RORValue(var Value: UInt32; Shift: Byte); overload;
procedure RORValue(var Value: UInt64; Shift: Byte); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Rotate left with carry (RCL)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function RCLCarry(Value: UInt8; Shift: Byte; var CF: Boolean): UInt8; overload;
Function RCLCarry(Value: UInt16; Shift: Byte; var CF: Boolean): UInt16; overload;
Function RCLCarry(Value: UInt32; Shift: Byte; var CF: Boolean): UInt32; overload;
Function RCLCarry(Value: UInt64; Shift: Byte; var CF: Boolean): UInt64; overload;

Function RCL(Value: UInt8; Shift: Byte; CF: Boolean = False): UInt8; overload;
Function RCL(Value: UInt16; Shift: Byte; CF: Boolean = False): UInt16; overload;
Function RCL(Value: UInt32; Shift: Byte; CF: Boolean = False): UInt32; overload;
Function RCL(Value: UInt64; Shift: Byte; CF: Boolean = False): UInt64; overload;

procedure RCLValueCarry(var Value: UInt8; Shift: Byte; var CF: Boolean); overload;
procedure RCLValueCarry(var Value: UInt16; Shift: Byte; var CF: Boolean); overload;
procedure RCLValueCarry(var Value: UInt32; Shift: Byte; var CF: Boolean); overload;
procedure RCLValueCarry(var Value: UInt64; Shift: Byte; var CF: Boolean); overload;

procedure RCLValue(var Value: UInt8; Shift: Byte; CF: Boolean = False); overload;
procedure RCLValue(var Value: UInt16; Shift: Byte; CF: Boolean = False); overload;
procedure RCLValue(var Value: UInt32; Shift: Byte; CF: Boolean = False); overload;
procedure RCLValue(var Value: UInt64; Shift: Byte; CF: Boolean = False); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                        Rotate right with carry (RCR)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function RCRCarry(Value: UInt8; Shift: Byte; var CF: Boolean): UInt8; overload;
Function RCRCarry(Value: UInt16; Shift: Byte; var CF: Boolean): UInt16; overload;
Function RCRCarry(Value: UInt32; Shift: Byte; var CF: Boolean): UInt32; overload;
Function RCRCarry(Value: UInt64; Shift: Byte; var CF: Boolean): UInt64; overload;

Function RCR(Value: UInt8; Shift: Byte; CF: Boolean = False): UInt8; overload;
Function RCR(Value: UInt16; Shift: Byte; CF: Boolean = False): UInt16; overload;
Function RCR(Value: UInt32; Shift: Byte; CF: Boolean = False): UInt32; overload;
Function RCR(Value: UInt64; Shift: Byte; CF: Boolean = False): UInt64; overload;

procedure RCRValueCarry(var Value: UInt8; Shift: Byte; var CF: Boolean); overload;
procedure RCRValueCarry(var Value: UInt16; Shift: Byte; var CF: Boolean); overload;
procedure RCRValueCarry(var Value: UInt32; Shift: Byte; var CF: Boolean); overload;
procedure RCRValueCarry(var Value: UInt64; Shift: Byte; var CF: Boolean); overload;

procedure RCRValue(var Value: UInt8; Shift: Byte; CF: Boolean = False); overload;
procedure RCRValue(var Value: UInt16; Shift: Byte; CF: Boolean = False); overload;
procedure RCRValue(var Value: UInt32; Shift: Byte; CF: Boolean = False); overload;
procedure RCRValue(var Value: UInt64; Shift: Byte; CF: Boolean = False); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Arithmetic left shift (SAL)                          }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SAL(Value: UInt8; Shift: Byte): UInt8; overload;
Function SAL(Value: UInt16; Shift: Byte): UInt16; overload;
Function SAL(Value: UInt32; Shift: Byte): UInt32; overload;
Function SAL(Value: UInt64; Shift: Byte): UInt64; overload;

procedure SALValue(var Value: UInt8; Shift: Byte); overload;
procedure SALValue(var Value: UInt16; Shift: Byte); overload;
procedure SALValue(var Value: UInt32; Shift: Byte); overload;
procedure SALValue(var Value: UInt64; Shift: Byte); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Arithmetic right shift (SAR)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SAR(Value: UInt8; Shift: Byte): UInt8; overload;
Function SAR(Value: UInt16; Shift: Byte): UInt16; overload;
Function SAR(Value: UInt32; Shift: Byte): UInt32; overload;
Function SAR(Value: UInt64; Shift: Byte): UInt64; overload;

procedure SARValue(var Value: UInt8; Shift: Byte); overload;
procedure SARValue(var Value: UInt16; Shift: Byte); overload;
procedure SARValue(var Value: UInt32; Shift: Byte); overload;
procedure SARValue(var Value: UInt64; Shift: Byte); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Endianity swap                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function EndianSwap(Value: UInt16): UInt16; overload;
Function EndianSwap(Value: UInt32): UInt32; overload;
Function EndianSwap(Value: UInt64): UInt64; overload;

procedure EndianSwapValue(var Value: UInt16); overload;
procedure EndianSwapValue(var Value: UInt32); overload;
procedure EndianSwapValue(var Value: UInt64); overload;

procedure EndianSwap(var Buffer; Size: TMemSize); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                 Bit test (BT)                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BT(Value: UInt8; Bit: Byte): Boolean; overload;
Function BT(Value: UInt16; Bit: Byte): Boolean; overload;
Function BT(Value: UInt32; Bit: Byte): Boolean; overload;
Function BT(Value: UInt64; Bit: Byte): Boolean; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit test and set (BTS)                            }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTS(var Value: UInt8; Bit: Byte): Boolean; overload;
Function BTS(var Value: UInt16; Bit: Byte): Boolean; overload;
Function BTS(var Value: UInt32; Bit: Byte): Boolean; overload;
Function BTS(var Value: UInt64; Bit: Byte): Boolean; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                           Bit test and reset (BTR)                           }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTR(var Value: UInt8; Bit: Byte): Boolean; overload;
Function BTR(var Value: UInt16; Bit: Byte): Boolean; overload;
Function BTR(var Value: UInt32; Bit: Byte): Boolean; overload;
Function BTR(var Value: UInt64; Bit: Byte): Boolean; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Bit test and complement (BTC)                        }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTC(var Value: UInt8; Bit: Byte): Boolean; overload;
Function BTC(var Value: UInt16; Bit: Byte): Boolean; overload;
Function BTC(var Value: UInt32; Bit: Byte): Boolean; overload;
Function BTC(var Value: UInt64; Bit: Byte): Boolean; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                      Bit test and set to a given value                       }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BitSetTo(var Value: UInt8; Bit: Byte; NewValue: Boolean): Boolean; overload;
Function BitSetTo(var Value: UInt16; Bit: Byte; NewValue: Boolean): Boolean; overload;
Function BitSetTo(var Value: UInt32; Bit: Byte; NewValue: Boolean): Boolean; overload;
Function BitSetTo(var Value: UInt64; Bit: Byte; NewValue: Boolean): Boolean; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit scan forward (BSF)                            }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BSF(Value: UInt8): Integer; overload;
Function BSF(Value: UInt16): Integer; overload;
Function BSF(Value: UInt32): Integer; overload;
Function BSF(Value: UInt64): Integer; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit scan reversed (BSR)                           }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BSR(Value: UInt8): Integer; overload;
Function BSR(Value: UInt16): Integer; overload;
Function BSR(Value: UInt32): Integer; overload;
Function BSR(Value: UInt64): Integer; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                               Population count                               }
{==============================================================================}
{------------------------------------------------------------------------------}
{
  There is a posibility of using POPCNT instruction on x86, but it was added
  very late and therefore is not supported in many still used CPUs.
  I have decided to not use it.
}
Function PopCount(Value: UInt8): Integer; overload;
Function PopCount(Value: UInt16): Integer; overload;
Function PopCount(Value: UInt32): Integer; overload;
Function PopCount(Value: UInt64): Integer; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Nibble manipulation                             }
{==============================================================================}
{------------------------------------------------------------------------------}

Function GetHighNibble(Value: UInt8): TNibble;
Function GetLowNibble(Value: UInt8): TNibble;

Function SetHighNibble(Value: UInt8; SetTo: TNibble): UInt8;
Function SetLowNibble(Value: UInt8; SetTo: TNibble): UInt8;

procedure SetHighNibbleValue(var Value: UInt8; SetTo: TNibble);
procedure SetLowNibbleValue(var Value: UInt8; SetTo: TNibble);

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Get flag state                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function GetFlagState(Value,FlagBitmask: UInt8; ExactMatch: Boolean = False): Boolean; overload;
Function GetFlagState(Value,FlagBitmask: UInt16; ExactMatch: Boolean = False): Boolean; overload;
Function GetFlagState(Value,FlagBitmask: UInt32; ExactMatch: Boolean = False): Boolean; overload;
Function GetFlagState(Value,FlagBitmask: UInt64; ExactMatch: Boolean = False): Boolean; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                   Set flag                                   }
{==============================================================================}
{------------------------------------------------------------------------------}
{
  Functions with bits noted in name (*_8, *_16, ...) are there mainly for older
  versions of Delphi (up to Delphi 2007), because they are not able distinguish
  what overloaded function to call (problem with open array parameter parsing).
}

Function SetFlag(Value,FlagBitmask: UInt8): UInt8; overload;
Function SetFlag(Value,FlagBitmask: UInt16): UInt16; overload;
Function SetFlag(Value,FlagBitmask: UInt32): UInt32; overload;
Function SetFlag(Value,FlagBitmask: UInt64): UInt64; overload;

procedure SetFlagValue(var Value: UInt8; FlagBitmask: UInt8); overload;
procedure SetFlagValue(var Value: UInt16; FlagBitmask: UInt16); overload;
procedure SetFlagValue(var Value: UInt32; FlagBitmask: UInt32); overload;
procedure SetFlagValue(var Value: UInt64; FlagBitmask: UInt64); overload;

Function SetFlags_8(Value: UInt8; Flags: array of UInt8): UInt8;
Function SetFlags_16(Value: UInt16; Flags: array of UInt16): UInt16;
Function SetFlags_32(Value: UInt32; Flags: array of UInt32): UInt32;
Function SetFlags_64(Value: UInt64; Flags: array of UInt64): UInt64;

Function SetFlags(Value: UInt8; Flags: array of UInt8): UInt8; overload;
Function SetFlags(Value: UInt16; Flags: array of UInt16): UInt16; overload;
Function SetFlags(Value: UInt32; Flags: array of UInt32): UInt32; overload;
Function SetFlags(Value: UInt64; Flags: array of UInt64): UInt64; overload;

procedure SetFlagsValue_8(var Value: UInt8; Flags: array of UInt8);
procedure SetFlagsValue_16(var Value: UInt16; Flags: array of UInt16);
procedure SetFlagsValue_32(var Value: UInt32; Flags: array of UInt32);
procedure SetFlagsValue_64(var Value: UInt64; Flags: array of UInt64);

procedure SetFlagsValue(var Value: UInt8; Flags: array of UInt8); overload;
procedure SetFlagsValue(var Value: UInt16; Flags: array of UInt16); overload;
procedure SetFlagsValue(var Value: UInt32; Flags: array of UInt32); overload;
procedure SetFlagsValue(var Value: UInt64; Flags: array of UInt64); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                  Reset flag                                  }
{==============================================================================}
{------------------------------------------------------------------------------}
{
  Functions with bits noted in name (*_8, *_16, ...) are there mainly for older
  versions of Delphi (up to Delphi 2007), because they are not able distinguish
  what overloaded function to call (problem with open array parameter parsing).
}

Function ResetFlag(Value,FlagBitmask: UInt8): UInt8; overload;
Function ResetFlag(Value,FlagBitmask: UInt16): UInt16; overload;
Function ResetFlag(Value,FlagBitmask: UInt32): UInt32; overload;
Function ResetFlag(Value,FlagBitmask: UInt64): UInt64; overload;

procedure ResetFlagValue(var Value: UInt8; FlagBitmask: UInt8); overload;
procedure ResetFlagValue(var Value: UInt16; FlagBitmask: UInt16); overload;
procedure ResetFlagValue(var Value: UInt32; FlagBitmask: UInt32); overload;
procedure ResetFlagValue(var Value: UInt64; FlagBitmask: UInt64); overload;

Function ResetFlags_8(Value: UInt8; Flags: array of UInt8): UInt8;
Function ResetFlags_16(Value: UInt16; Flags: array of UInt16): UInt16;
Function ResetFlags_32(Value: UInt32; Flags: array of UInt32): UInt32;
Function ResetFlags_64(Value: UInt64; Flags: array of UInt64): UInt64;

Function ResetFlags(Value: UInt8; Flags: array of UInt8): UInt8; overload;
Function ResetFlags(Value: UInt16; Flags: array of UInt16): UInt16; overload;
Function ResetFlags(Value: UInt32; Flags: array of UInt32): UInt32; overload;
Function ResetFlags(Value: UInt64; Flags: array of UInt64): UInt64; overload;

procedure ResetFlagsValue_8(var Value: UInt8; Flags: array of UInt8);
procedure ResetFlagsValue_16(var Value: UInt16; Flags: array of UInt16);
procedure ResetFlagsValue_32(var Value: UInt32; Flags: array of UInt32);
procedure ResetFlagsValue_64(var Value: UInt64; Flags: array of UInt64);

procedure ResetFlagsValue(var Value: UInt8; Flags: array of UInt8); overload;
procedure ResetFlagsValue(var Value: UInt16; Flags: array of UInt16); overload;
procedure ResetFlagsValue(var Value: UInt32; Flags: array of UInt32); overload;
procedure ResetFlagsValue(var Value: UInt64; Flags: array of UInt64); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Set flag state                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SetFlagState(Value,FlagBitmask: UInt8; NewState: Boolean): UInt8; overload;
Function SetFlagState(Value,FlagBitmask: UInt16; NewState: Boolean): UInt16; overload;
Function SetFlagState(Value,FlagBitmask: UInt32; NewState: Boolean): UInt32; overload;
Function SetFlagState(Value,FlagBitmask: UInt64; NewState: Boolean): UInt64; overload;

procedure SetFlagStateValue(var Value: UInt8; FlagBitmask: UInt8; NewState: Boolean); overload;
procedure SetFlagStateValue(var Value: UInt16; FlagBitmask: UInt16; NewState: Boolean); overload;
procedure SetFlagStateValue(var Value: UInt32; FlagBitmask: UInt32; NewState: Boolean); overload;
procedure SetFlagStateValue(var Value: UInt64; FlagBitmask: UInt64; NewState: Boolean); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                   Set bits                                   }
{==============================================================================}
{------------------------------------------------------------------------------}
{
  Replaces contiguous segment of bits in Value by corresponding bits from
  NewBits.
}
Function SetBits(Value,NewBits: UInt8; FromBit,ToBit: Integer): UInt8; overload;
Function SetBits(Value,NewBits: UInt16; FromBit,ToBit: Integer): UInt16; overload;
Function SetBits(Value,NewBits: UInt32; FromBit,ToBit: Integer): UInt32; overload;
Function SetBits(Value,NewBits: UInt64; FromBit,ToBit: Integer): UInt64; overload;

procedure SetBitsValue(var Value: UInt8; NewBits: UInt8; FromBit,ToBit: Integer); overload;
procedure SetBitsValue(var Value: UInt16; NewBits: UInt16; FromBit,ToBit: Integer); overload;
procedure SetBitsValue(var Value: UInt32; NewBits: UInt32; FromBit,ToBit: Integer); overload;
procedure SetBitsValue(var Value: UInt64; NewBits: UInt64; FromBit,ToBit: Integer); overload;

implementation

uses
  SysUtils, Math;

{------------------------------------------------------------------------------}
{==============================================================================}
{                  Integer number <-> Bit string conversions                   }
{==============================================================================}
{------------------------------------------------------------------------------}

Function _NumberToBits(Number: UInt64; Bits: Byte): String;
var
  i:  Integer;
begin
Result := StringOfChar('0',Bits);
For i := Bits downto 1 do
  begin
    If (Number and 1) <> 0 then Result[i] := '1';
    Number := Number shr 1;
  end;
end;

//------------------------------------------------------------------------------

Function NumberToBits(Number: UInt8): String;
begin
Result := _NumberToBits(Number,8);
end;

//------------------------------------------------------------------------------

Function NumberToBits(Number: UInt16): String;
begin
Result := _NumberToBits(Number,16);
end;

//------------------------------------------------------------------------------

Function NumberToBits(Number: UInt32): String;
begin
Result := _NumberToBits(Number,32);
end;

//------------------------------------------------------------------------------

Function NumberToBits(Number: UInt64): String;
begin
Result := _NumberToBits(Number,64);
end;

//==============================================================================

Function BitsToNumber(const BitString: String): UInt64;
var
  i:  Integer;
begin
Result := 0;
For i := 1 to Min(Length(BitString),64) do
  begin
    Result := Result shl 1;
    If BitString[i] <> '0' then Result := Result or 1;
  end;
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Rotate left (ROL)                               }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ROL(Value: UInt8; Shift: Byte): UInt8;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   AL,   CL
{$ENDIF}
    MOV   CL,   DL
    ROL   AL,   CL
end;
{$ELSE}
begin
Shift := Shift and $07;
Result := UInt8((Value shl Shift) or (Value shr (8 - Shift)));
end; 
{$ENDIF}

//------------------------------------------------------------------------------

Function ROL(Value: UInt16; Shift: Byte): UInt16;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   AX,   CX
{$ENDIF}
    MOV   CL,   DL
    ROL   AX,   CL
end;
{$ELSE}
begin
Shift := Shift and $0F;
Result := UInt16((Value shl Shift) or (Value shr (16 - Shift)));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROL(Value: UInt32; Shift: Byte): UInt32;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   EAX,  ECX
{$ENDIF}
    MOV   CL,   DL
    ROL   EAX,  CL
end;
{$ELSE}
begin
Shift := Shift and $1F;
Result := UInt32((Value shl Shift) or (Value shr (32 - Shift)));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROL(Value: UInt64; Shift: Byte): UInt64;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
    MOV   CL,   DL
    ROL   RAX,  CL
{$ELSE}
    MOV   ECX,  EAX
    AND   ECX,  $3F
    CMP   ECX,  32

    JAE   @Above31

  @Below32:
    MOV   EAX,  dword ptr [Value]
    MOV   EDX,  dword ptr [Value + 4]
    CMP   ECX,  0
    JE    @FuncEnd

    MOV   dword ptr [Value],  EDX
    JMP   @Rotate

  @Above31:
    MOV   EDX,  dword ptr [Value]
    MOV   EAX,  dword ptr [Value + 4]
    JE    @FuncEnd

    AND   ECX,  $1F

  @Rotate:
    SHLD  EDX,  EAX, CL
    SHL   EAX,  CL
    PUSH  EAX
    MOV   EAX,  dword ptr [Value]
    XOR   CL,   31
    INC   CL
    SHR   EAX,  CL
    POP   ECX
    OR    EAX,  ECX

  @FuncEnd:
{$ENDIF}
end;
{$ELSE}
begin
Shift := Shift and $3F;
Result := UInt64((Value shl Shift) or (Value shr (64 - Shift)));
end;
{$ENDIF}

//==============================================================================

procedure ROLValue(var Value: UInt8; Shift: Byte);
begin
Value := ROL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure ROLValue(var Value: UInt16; Shift: Byte);
begin
Value := ROL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure ROLValue(var Value: UInt32; Shift: Byte);
begin
Value := ROL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure ROLValue(var Value: UInt64; Shift: Byte);
begin
Value := ROL(Value,Shift);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Rotate right (ROR)                              }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ROR(Value: UInt8; Shift: Byte): UInt8;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   AL,   CL
{$ENDIF}
    MOV   CL,   DL
    ROR   AL,   CL
end;
{$ELSE}
begin
Shift := Shift and $07;
Result := UInt8((Value shr Shift) or (Value shl (8 - Shift)));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROR(Value: UInt16; Shift: Byte): UInt16;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   AX,   CX
{$ENDIF}
    MOV   CL,   DL
    ROR   AX,   CL
end;
{$ELSE}
begin
Shift := Shift and $0F;
Result := UInt16((Value shr Shift) or (Value shl (16 - Shift)));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROR(Value: UInt32; Shift: Byte): UInt32;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   EAX,  ECX
{$ENDIF}
    MOV   CL,   DL
    ROR   EAX,  CL
end;
{$ELSE}
begin
Shift := Shift and $1F;
Result := UInt32((Value shr Shift) or (Value shl (32 - Shift)));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROR(Value: UInt64; Shift: Byte): UInt64;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
    MOV   CL,   DL
    ROR   RAX,  CL
{$ELSE}
    MOV   ECX,  EAX
    AND   ECX,  $3F
    CMP   ECX,  32

    JAE   @Above31

  @Below32:
    MOV   EAX,  dword ptr [Value]
    MOV   EDX,  dword ptr [Value + 4]
    CMP   ECX,  0
    JE    @FuncEnd

    MOV   dword ptr [Value],  EDX
    JMP   @Rotate

  @Above31:
    MOV   EDX,  dword ptr [Value]
    MOV   EAX,  dword ptr [Value + 4]
    JE    @FuncEnd

    AND   ECX,  $1F

  @Rotate:
    SHRD  EDX,  EAX, CL
    SHR   EAX,  CL
    PUSH  EAX
    MOV   EAX,  dword ptr [Value]
    XOR   CL,   31
    INC   CL
    SHL   EAX,  CL
    POP   ECX
    OR    EAX,  ECX

  @FuncEnd:
{$ENDIF}
end;
{$ELSE}
begin
Shift := Shift and $3F;
Result := UInt64((Value shr Shift) or (Value shl (64 - Shift)));
end;
{$ENDIF}

//==============================================================================

procedure RORValue(var Value: UInt8; Shift: Byte);
begin
Value := ROR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure RORValue(var Value: UInt16; Shift: Byte);
begin
Value := ROR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure RORValue(var Value: UInt32; Shift: Byte);
begin
Value := ROR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure RORValue(var Value: UInt64; Shift: Byte);
begin
Value := ROR(Value,Shift);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Rotate left with carry (RCL)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function RCLCarry(Value: UInt8; Shift: Byte; var CF: Boolean): UInt8;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
    MOV   RCX,  RDX
    SHR   byte ptr [R8],  1
    RCL   AL,   CL
    SETC  CL
    MOV   byte ptr [R8],  CL
{$ELSE}
    XCHG  EDX,  ECX
    SHR   byte ptr [EDX], 1
    RCL   AL,   CL
    SETC  CL
    MOV   byte ptr [EDX], CL
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $07;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result shr 7) <> 0;
    Result := UInt8((Result shl 1) or (UInt8(Carry) and UInt8(1)));
    Carry := CF;
  end;
end;
{$ENDIF}
      
//------------------------------------------------------------------------------

Function RCLCarry(Value: UInt16; Shift: Byte; var CF: Boolean): UInt16;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
    MOV   RCX,  RDX
    SHR   byte ptr [R8],  1
    RCL   AX,   CL
    SETC  CL
    MOV   byte ptr [R8],  CL
{$ELSE}
    XCHG  EDX,  ECX
    SHR   byte ptr [EDX], 1
    RCL   AX,   CL
    SETC  CL
    MOV   byte ptr [EDX], CL
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $0F;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result shr 15) <> 0;
    Result := UInt16((Result shl 1) or (UInt16(Carry) and UInt16(1)));
    Carry := CF;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCLCarry(Value: UInt32; Shift: Byte; var CF: Boolean): UInt32;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
    MOV   RCX,  RDX
    SHR   byte ptr [R8],  1
    RCL   EAX,  CL
    SETC  CL
    MOV   byte ptr [R8],  CL
{$ELSE}
    XCHG  EDX,  ECX
    SHR   byte ptr [EDX], 1
    RCL   EAX,  CL
    SETC  CL
    MOV   byte ptr [EDX], CL
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $1F;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result shr 31) <> 0;
    Result := UInt32((Result shl 1) or (UInt32(Carry) and 1));
    Carry := CF;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCLCarry(Value: UInt64; Shift: Byte; var CF: Boolean): UInt64;{$IFNDEF PurePascal}assembler;
{$IFDEF x64}
asm
    MOV   RAX,  RCX
    MOV   RCX,  RDX
    SHR   byte ptr [R8],  1
    RCL   RAX,  CL
    SETC  CL
    MOV   byte ptr [R8],  CL
{$ELSE}
var
  TempShift:  Integer;
asm
    PUSH  EBX
    
    AND   EAX,  $3F
    MOV   dword ptr [TempShift],  EAX
    MOV   ECX,  EAX    
    MOV   EBX,  EDX

    MOV   EAX,  dword ptr [Value]
    MOV   EDX,  dword ptr [Value + 4]
    CMP   ECX,  32

    JE    @Exactly32
    JA    @Above32

{- Shift is below 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    TEST  ECX,  ECX
    JZ    @FuncEnd

    SHLD  EDX,  EAX, CL
    JMP   @Shift

{- Shift is above 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Above32:
    AND   ECX,  $1F

    DEC   CL
    SHLD  EDX,  EAX, CL
    INC   CL

{- Main shifting  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Shift:
    SHL   EAX,  CL
    PUSH  ECX
    PUSH  EAX
    MOV   EAX,  dword ptr [Value + 4]
    SHR   EAX,  2
    XOR   CL,   31
    SHR   EAX,  CL
    POP   ECX
    OR    EAX,  ECX
    POP   ECX
    JMP   @SetCarry

{- Shift is equal to 32, no shifting required, only swap Hi and Lo dwords - - -}
  @Exactly32:
    SHR   EDX,  1
    XCHG  EAX,  EDX

{- Write passed carry bit to the result - - - - - - - - - - - - - - - - - - - -}
  @SetCarry:
    DEC   ECX
    CMP   byte ptr [EBX], 0
    JE    @ResetBit

    BTS   EAX,  ECX
    JMP   @Swap

  @ResetBit:
    BTR   EAX,  ECX

{- Swap Hi and Lo dwords for shift > 32 - - - - - - - - - - - - - - - - - - - -}
  @Swap:
    CMP   byte ptr [TempShift],  32
    JBE   @GetCarry
    XCHG  EAX,  EDX

{- Get carry bit that will be output in CF parameter  - - - - - - - - - - - - -}
  @GetCarry:
    MOV   CL,   byte ptr [TempShift]
    AND   ECX,  $3F
    CMP   CL,   32
    JBE   @FromHigh

    AND   CL,   $1F
    DEC   CL
    XOR   CL,   31
    BT    dword ptr [Value],  ECX
    JMP   @StoreCarry

  @FromHigh:
    DEC   CL
    XOR   CL,   31
    BT    dword ptr [Value + 4], ECX

  @StoreCarry:
    SETC  CL
    MOV   byte ptr [EBX], CL

{- Restore EBX register - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @FuncEnd:
    POP   EBX
{$ENDIF}  
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $3F;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result shr 63) <> 0;
    Result := UInt64((Result shl 1) or (UInt64(Carry) and 1));
    Carry := CF;
  end;
end;
{$ENDIF}


//==============================================================================

Function RCL(Value: UInt8; Shift: Byte; CF: Boolean = False): UInt8;
begin
Result := RCLCarry(Value,Shift,{%H-}CF);
end;
 
//------------------------------------------------------------------------------

Function RCL(Value: UInt16; Shift: Byte; CF: Boolean = False): UInt16;
begin
Result := RCLCarry(Value,Shift,{%H-}CF);
end;

//------------------------------------------------------------------------------

Function RCL(Value: UInt32; Shift: Byte; CF: Boolean = False): UInt32;
begin
Result := RCLCarry(Value,Shift,{%H-}CF);
end;

//------------------------------------------------------------------------------

Function RCL(Value: UInt64; Shift: Byte; CF: Boolean = False): UInt64;
begin
Result := RCLCarry(Value,Shift,{%H-}CF);
end;

//==============================================================================

procedure RCLValueCarry(var Value: UInt8; Shift: Byte; var CF: Boolean);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValueCarry(var Value: UInt16; Shift: Byte; var CF: Boolean);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValueCarry(var Value: UInt32; Shift: Byte; var CF: Boolean);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValueCarry(var Value: UInt64; Shift: Byte; var CF: Boolean);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//==============================================================================

procedure RCLValue(var Value: UInt8; Shift: Byte; CF: Boolean = False);
begin
Value := RCL(Value,Shift,CF);
end;
 
//------------------------------------------------------------------------------

procedure RCLValue(var Value: UInt16; Shift: Byte; CF: Boolean = False);
begin
Value := RCL(Value,Shift,CF);
end;
 
//------------------------------------------------------------------------------

procedure RCLValue(var Value: UInt32; Shift: Byte; CF: Boolean = False);
begin
Value := RCL(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValue(var Value: UInt64; Shift: Byte; CF: Boolean = False);
begin
Value := RCL(Value,Shift,CF);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                        Rotate right with carry (RCR)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function RCRCarry(Value: UInt8; Shift: Byte; var CF: Boolean): UInt8;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
    MOV   RCX,  RDX
    SHR   byte ptr [R8],  1
    RCR   AL,   CL
    SETC  CL
    MOV   byte ptr [R8],  CL
{$ELSE}
    XCHG  EDX,  ECX
    SHR   byte ptr [EDX], 1
    RCR   AL,   CL
    SETC  CL
    MOV   byte ptr [EDX], CL
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $07;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    Result := UInt8((Result shr 1) or ((UInt8(Carry) and UInt8(1)) shl 7));
    Carry := CF;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCRCarry(Value: UInt16; Shift: Byte; var CF: Boolean): UInt16;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
    MOV   RCX,  RDX
    SHR   byte ptr [R8],  1
    RCR   AX,   CL
    SETC  CL
    MOV   byte ptr [R8],  CL
{$ELSE}
    XCHG  EDX,  ECX
    SHR   byte ptr [EDX], 1
    RCR   AX,   CL
    SETC  CL
    MOV   byte ptr [EDX], CL
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $0F;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    Result := UInt16((Result shr 1) or ((UInt16(Carry) and UInt16(1)) shl 15));
    Carry := CF;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCRCarry(Value: UInt32; Shift: Byte; var CF: Boolean): UInt32;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
    MOV   RCX,  RDX
    SHR   byte ptr [R8],  1
    RCR   EAX,  CL
    SETC  CL
    MOV   byte ptr [R8],  CL
{$ELSE}
    XCHG  EDX,  ECX
    SHR   byte ptr [EDX], 1
    RCR   EAX,  CL
    SETC  CL
    MOV   byte ptr [EDX], CL
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $1F;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    Result := UInt32((Result shr 1) or ((UInt32(Carry) and 1) shl 31));
    Carry := CF;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCRCarry(Value: UInt64; Shift: Byte; var CF: Boolean): UInt64;{$IFNDEF PurePascal}assembler;
{$IFDEF x64}
asm
    MOV   RAX,  RCX
    MOV   RCX,  RDX
    SHR   byte ptr [R8],  1
    RCR   RAX,  CL
    SETC  CL
    MOV   byte ptr [R8],  CL
{$ELSE}
var
  TempShift:  Integer;
asm
    PUSH  EBX

    AND   EAX,  $3F
    MOV   dword ptr [TempShift],  EAX
    MOV   ECX,  EAX    
    MOV   EBX,  EDX

    MOV   EAX,  dword ptr [Value]
    MOV   EDX,  dword ptr [Value + 4]
    CMP   ECX,  32

    JE    @Exactly32
    JA    @Above32

{- Shift is below 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    TEST  ECX,  ECX
    JZ    @FuncEnd

    SHRD  EAX,  EDX, CL
    JMP   @Shift

{- Shift is above 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Above32:
    AND   ECX,  $1F

    DEC   CL
    SHRD  EAX,  EDX, CL
    INC   CL

{- Main shifting  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Shift:
    SHR   EDX,  CL
    PUSH  ECX
    PUSH  EDX
    MOV   EDX,  dword ptr [Value]
    SHL   EDX,  2
    XOR   CL,   31
    SHL   EDX,  CL
    POP   ECX
    OR    EDX,  ECX
    POP   ECX
    JMP   @SetCarry

{- Shift is equal to 32, no shifting required, only swap Hi and Lo dwords - - -}
  @Exactly32:
    SHL   EAX,  1
    XCHG  EAX,  EDX

{- Write passed carry bit to the result - - - - - - - - - - - - - - - - - - - -}
  @SetCarry:
    DEC   ECX
    XOR   ECX,  31
    CMP   byte ptr [EBX], 0
    JE    @ResetBit

    BTS   EDX,  ECX
    JMP   @Swap

  @ResetBit:
    BTR   EDX,  ECX

{- Swap Hi and Lo dwords for shift > 32 - - - - - - - - - - - - - - - - - - - -}
  @Swap:
    CMP   byte ptr [TempShift],  32
    JBE   @GetCarry
    XCHG  EAX,  EDX

{- Get carry bit that will be output in CF parameter  - - - - - - - - - - - - -}
  @GetCarry:
    MOV   CL,   byte ptr [TempShift]
    AND   ECX,  $3F
    CMP   CL,   32
    JA    @FromHigh

    DEC   CL
    BT    dword ptr [Value],  ECX
    JMP   @StoreCarry

  @FromHigh:
    AND   CL,   $1F  
    DEC   CL
    BT    dword ptr [Value + 4], ECX

  @StoreCarry:
    SETC  CL
    MOV   byte ptr [EBX], CL

{- Restore EBX register - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @FuncEnd:
    POP   EBX
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $3F;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    Result := UInt64((Result shr 1) or ((UInt64(Carry) and 1) shl 63));
    Carry := CF;
  end;
end;
{$ENDIF}

//==============================================================================

Function RCR(Value: UInt8; Shift: Byte; CF: Boolean = False): UInt8;
begin
Result := RCRCarry(Value,Shift,{%H-}CF);
end;

//------------------------------------------------------------------------------

Function RCR(Value: UInt16; Shift: Byte; CF: Boolean = False): UInt16;
begin
Result := RCRCarry(Value,Shift,{%H-}CF);
end;

//------------------------------------------------------------------------------

Function RCR(Value: UInt32; Shift: Byte; CF: Boolean = False): UInt32;
begin
Result := RCRCarry(Value,Shift,{%H-}CF);
end;

//------------------------------------------------------------------------------

Function RCR(Value: UInt64; Shift: Byte; CF: Boolean = False): UInt64;
begin
Result := RCRCarry(Value,Shift,{%H-}CF);
end;

//==============================================================================

procedure RCRValueCarry(var Value: UInt8; Shift: Byte; var CF: Boolean);
begin
Value := RCRCarry(Value,Shift,CF);
end;
 
//------------------------------------------------------------------------------

procedure RCRValueCarry(var Value: UInt16; Shift: Byte; var CF: Boolean);
begin
Value := RCRCarry(Value,Shift,CF);
end;
  
//------------------------------------------------------------------------------

procedure RCRValueCarry(var Value: UInt32; Shift: Byte; var CF: Boolean);
begin
Value := RCRCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCRValueCarry(var Value: UInt64; Shift: Byte; var CF: Boolean);
begin
Value := RCRCarry(Value,Shift,CF);
end;

//==============================================================================

procedure RCRValue(var Value: UInt8; Shift: Byte; CF: Boolean = False);
begin
Value := RCR(Value,Shift,CF);
end;
 
//------------------------------------------------------------------------------

procedure RCRValue(var Value: UInt16; Shift: Byte; CF: Boolean = False);
begin
Value := RCR(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCRValue(var Value: UInt32; Shift: Byte; CF: Boolean = False);
begin
Value := RCR(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCRValue(var Value: UInt64; Shift: Byte; CF: Boolean = False);
begin
Value := RCR(Value,Shift,CF);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Arithmetic left shift (SAL)                          }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SAL(Value: UInt8; Shift: Byte): UInt8;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
{$ENDIF}
    MOV   CL,   DL
    SAL   AL,   CL
end;
{$ELSE}
begin
Result := UInt8(Value shl Shift);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAL(Value: UInt16; Shift: Byte): UInt16;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
{$ENDIF}
    MOV   CL,   DL
    SAL   AX,   CL
end;
{$ELSE}
begin
Result := UInt16(Value shl Shift);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAL(Value: UInt32; Shift: Byte): UInt32;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
{$ENDIF}
    MOV   CL,   DL
    SAL   EAX,  CL
end;
{$ELSE}
begin
Result := UInt32(Value shl Shift);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAL(Value: UInt64; Shift: Byte): UInt64;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
    MOV   CL,   DL
    SAL   RAX,  CL
{$ELSE}
    MOV   ECX,  EAX
    AND   ECX,  $3F

    CMP   ECX,  31
    JA    @Above31

{- Shift is below 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    MOV   EAX,  dword ptr [Value]
    MOV   EDX,  dword ptr [Value + 4]

    TEST  ECX,  ECX
    JZ    @FuncEnd

    SHLD  EDX,  EAX,  CL
    SHL   EAX,  CL
    JMP   @FuncEnd

{- Shift is above 31  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Above31:
    XOR   EAX,  EAX
    MOV   EDX,  dword ptr [Value]
    AND   ECX,  $1F
    SHL   EDX,  CL

{- End of the function  - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @FuncEnd:
{$ENDIF}
end;
{$ELSE}
begin
Result := UInt64(Value shl Shift);
end;
{$ENDIF}

//==============================================================================

procedure SALValue(var Value: UInt8; Shift: Byte);
begin
Value := SAL(Value,Shift);
end;
 
//------------------------------------------------------------------------------

procedure SALValue(var Value: UInt16; Shift: Byte);
begin
Value := SAL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SALValue(var Value: UInt32; Shift: Byte);
begin
Value := SAL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SALValue(var Value: UInt64; Shift: Byte);
begin
Value := SAL(Value,Shift);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Arithmetic right shift (SAR)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SAR(Value: UInt8; Shift: Byte): UInt8;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
{$ENDIF}
    MOV   CL,   DL
    SAR   AL,   CL
end;
{$ELSE}
begin
Shift := Shift and $07;
If (Value shr 7) <> 0 then
  Result := UInt8((Value shr Shift) or (UInt8($FF) shl (8 - Shift)))
else
  Result := UInt8(Value shr Shift);
end;
{$ENDIF}
 
//------------------------------------------------------------------------------

Function SAR(Value: UInt16; Shift: Byte): UInt16;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
{$ENDIF}
    MOV   CL,   DL
    SAR   AX,   CL
end;
{$ELSE}
begin
Shift := Shift and $0F;
If (Value shr 15) <> 0 then
  Result := UInt16((Value shr Shift) or (UInt16($FFFF) shl (16 - Shift)))
else
  Result := UInt16(Value shr Shift);
end;
{$ENDIF}
  
//------------------------------------------------------------------------------

Function SAR(Value: UInt32; Shift: Byte): UInt32;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
{$ENDIF}
    MOV   CL,   DL
    SAR   EAX,  CL
end;
{$ELSE}
begin
Shift := Shift and $1F;
If (Value shr 31) <> 0 then
  Result := UInt32((Value shr Shift) or (UInt32($FFFFFFFF) shl (32 - Shift)))
else
  Result := UInt32(Value shr Shift);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAR(Value: UInt64; Shift: Byte): UInt64;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
    MOV   CL,   DL
    SAR   RAX,  CL
{$ELSE}
    MOV   ECX,  EAX
    AND   ECX,  $3F

    CMP   ECX,  31
    JA    @Above31

{- Shift is below 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    MOV   EAX,  dword ptr [Value]
    MOV   EDX,  dword ptr [Value + 4]

    TEST  ECX,  ECX
    JZ    @FuncEnd

    SHRD  EAX,  EDX,  CL
    SAR   EDX,  CL
    JMP   @FuncEnd

{- Shift is above 31  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Above31:
    MOV   EAX,  dword ptr [Value + 4]
    BT    EAX,  31
    JC    @BitSet

    XOR   EDX,  EDX
    JMP   @DoShift

  @BitSet:
    MOV   EDX,  $FFFFFFFF

  @DoShift:
    AND   ECX,  $1F
    SAR   EAX,  CL

{- End of the function  - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @FuncEnd:
{$ENDIF}
end;
{$ELSE}
begin
Shift := Shift and $3F;
If (Value shr 63) <> 0 then
  Result := UInt64((Value shr Shift) or (UInt64($FFFFFFFFFFFFFFFF) shl (64 - Shift)))
else
  Result := UInt64(Value shr Shift);
end;
{$ENDIF}

//==============================================================================

procedure SARValue(var Value: UInt8; Shift: Byte);
begin
Value := SAL(Value,Shift);
end;
 
//------------------------------------------------------------------------------

procedure SARValue(var Value: UInt16; Shift: Byte);
begin
Value := SAR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SARValue(var Value: UInt32; Shift: Byte);
begin
Value := SAR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SARValue(var Value: UInt64; Shift: Byte);
begin
Value := SAR(Value,Shift);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Endianity swap                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function EndianSwap(Value: UInt16): UInt16;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
{$ENDIF}
    XCHG  AL,   AH
end;
{$ELSE}
begin
Result := UInt16((Value shl 8) or (Value shr 8));
end;
{$ENDIF}
  
//------------------------------------------------------------------------------

Function EndianSwap(Value: UInt32): UInt32;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
{$ENDIF}
    BSWAP EAX
end;
{$ELSE}
begin
Result := UInt32((Value and $000000FF shl 24) or (Value and $0000FF00 shl 8) or
                 (Value and $00FF0000 shr 8) or (Value and $FF000000 shr 24));
end;
{$ENDIF}
      
//------------------------------------------------------------------------------

Function EndianSwap(Value: UInt64): UInt64;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  RCX
    BSWAP RAX
{$ELSE}
    MOV   EAX,  dword ptr [Value + 4]
    MOV   EDX,  dword ptr [Value]
    BSWAP EAX
    BSWAP EDX
{$ENDIF}
end;
{$ELSE}
begin
Int64Rec(Result).Hi := EndianSwap(Int64Rec(Value).Lo);
Int64Rec(Result).Lo := EndianSwap(Int64Rec(Value).Hi);
end;
{$ENDIF}

//==============================================================================

procedure EndianSwapValue(var Value: UInt16);
begin
Value := EndianSwap(Value);
end;
             
//------------------------------------------------------------------------------

procedure EndianSwapValue(var Value: UInt32);
begin
Value := EndianSwap(Value);
end;

//------------------------------------------------------------------------------

procedure EndianSwapValue(var Value: UInt64);
begin
Value := EndianSwap(Value);
end;

//==============================================================================

procedure EndianSwap(var Buffer; Size: TMemSize);{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    XCHG  RCX,  RDX
    CMP   RCX,  1
    JBE   @RoutineEnd

    LEA   RAX,  [RDX + RCX - 1]
    SHR   RCX,  1

  @LoopStart:
    MOV   R8B,  byte ptr [RDX]
    MOV   R9B,  byte ptr [RAX]
    MOV   byte ptr [RAX], R8B
    MOV   byte ptr [RDX], R9B
    INC   RDX
    DEC   RAX
    LOOP  @LoopStart

  @RoutineEnd:
{$ELSE}
    MOV   ECX,  EDX
    CMP   ECX,  1
    JBE   @RoutineEnd

    PUSH  ESI
    PUSH  EDI
    MOV   ESI,  EAX
    LEA   EDI,  [EAX + ECX - 1]
    SHR   ECX,  1

  @LoopStart:
    MOV   AL,   byte ptr [ESI]
    MOV   DL,   byte ptr [EDI]
    MOV   byte ptr [EDI], AL
    MOV   byte ptr [ESI], DL
    INC   ESI
    DEC   EDI
    LOOP  @LoopStart

    POP   EDI
    POP   ESI

  @RoutineEnd:
{$ENDIF}
end;
{$ELSE}
var
  i:        TMemSize;
  ByteBuff: Byte;
begin
case Size of
  Low(Size)..1: Exit;
             2: EndianSwapValue(UInt16(Buffer));
             4: EndianSwapValue(UInt32(Buffer));
             8: EndianSwapValue(UInt64(Buffer));
else
  For i := 0 to Pred(Size div 2) do
    begin
      {%H-}ByteBuff := {%H-}PByte({%H-}PtrUInt(Addr(Buffer)) + i)^;
      {%H-}PByte({%H-}PtrUInt(Addr(Buffer)) + i)^ := {%H-}PByte({%H-}PtrUInt(Addr(Buffer)) + (Size - i - 1))^;
      {%H-}PByte({%H-}PtrUInt(Addr(Buffer)) + (Size - i - 1))^ := ByteBuff;
    end;
end;
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                                 Bit test (BT)                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BT(Value: UInt8; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    BT    CX, DX
{$ELSE}
    BT    AX, DX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$ENDIF}
           
//------------------------------------------------------------------------------

Function BT(Value: UInt16; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    BT    CX, DX
{$ELSE}
    BT    AX, DX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$ENDIF}
           
//------------------------------------------------------------------------------

Function BT(Value: UInt32; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    BT    ECX, EDX
{$ELSE}
    BT    EAX, EDX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BT(Value: UInt64; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    BT    RCX,  RDX
{$ELSE}
    CMP   EAX,  32
    JAE   @TestHigh

    BT    dword ptr [Value],  EAX
    JMP   @SetResult

  @TestHigh:
    AND   EAX,  $1F
    BT    dword ptr [Value + 4],  EAX
{$ENDIF}
  @SetResult:
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit test and set (BTS)                            }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTS(var Value: UInt8; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   AL, byte ptr [RCX]
    BTS   AX, DX
    MOV   byte ptr [RCX], AL
{$ELSE}
    MOV   CL, byte ptr [EAX]
    BTS   CX, DX
    MOV   byte ptr [EAX], CL
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt8(Value or (UInt8(1) shl Bit));
end;
{$ENDIF}
           
//------------------------------------------------------------------------------

Function BTS(var Value: UInt16; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   AX, word ptr [RCX]
    BTS   AX, DX
    MOV   word ptr [RCX], AX
{$ELSE}
    MOV   CX, word ptr [EAX]
    BTS   CX, DX
    MOV   word ptr [EAX], CX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt16(Value or (UInt16(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTS(var Value: UInt32; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   EAX,  dword ptr [RCX]
    BTS   EAX,  EDX
    MOV   dword ptr [RCX],  EAX
{$ELSE}
    MOV   ECX,  dword ptr [EAX]
    BTS   ECX,  EDX
    MOV   dword ptr [EAX],  ECX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt32(Value or (UInt32(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTS(var Value: UInt64; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX,  qword ptr [RCX]
    BTS   RAX,  RDX
    MOV   qword ptr [RCX], RAX
{$ELSE}
    CMP   EDX,  32
    JAE   @TestHigh

    BTS   dword ptr [Value],  EDX
    JMP   @SetResult

  @TestHigh:
    AND   EDX,  $1F
    BTS   dword ptr [Value + 4],  EDX
{$ENDIF}
  @SetResult:
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt64(Value or (UInt64(1) shl Bit));
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                           Bit test and reset (BTR)                           }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTR(var Value: UInt8; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   AL, byte ptr [RCX]
    BTR   AX, DX
    MOV   byte ptr [RCX], AL
{$ELSE}
    MOV   CL, byte ptr [EAX]
    BTR   CX, DX
    MOV   byte ptr [EAX], CL
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt8(Value and not(UInt8(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTR(var Value: UInt16; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   AX, word ptr [RCX]
    BTR   AX, DX
    MOV   word ptr [RCX], AX
{$ELSE}
    MOV   CX, word ptr [EAX]
    BTR   CX, DX
    MOV   word ptr [EAX], CX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt16(Value and not(UInt16(1) shl Bit));
end;
{$ENDIF}
           
//------------------------------------------------------------------------------

Function BTR(var Value: UInt32; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   EAX,  dword ptr [RCX]
    BTR   EAX,  EDX
    MOV   dword ptr [RCX],  EAX
{$ELSE}
    MOV   ECX,  dword ptr [EAX]
    BTR   ECX,  EDX
    MOV   dword ptr [EAX],  ECX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt32(Value and not(UInt32(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTR(var Value: UInt64; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX, qword ptr [RCX]
    BTR   RAX,  RDX
    MOV   qword ptr [RCX], RAX
{$ELSE}
    CMP   EDX,  32
    JAE   @TestHigh

    BTR   dword ptr [Value],  EDX
    JMP   @SetResult

  @TestHigh:
    AND   EDX,  $1F
    BTR   dword ptr [Value + 4],  EDX
{$ENDIF}
  @SetResult:
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt64(Value and not(UInt64(1) shl Bit));
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Bit test and complement (BTC)                        }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTC(var Value: UInt8; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   AL, byte ptr [RCX]
    BTC   AX, DX
    MOV   byte ptr [RCX], AL
{$ELSE}
    MOV   CL, byte ptr [EAX]
    BTC   CX, DX
    MOV   byte ptr [EAX], CL
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
If Result then Value := UInt8(Value and not(UInt8(1) shl Bit))
  else Value := UInt8(Value or (UInt8(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTC(var Value: UInt16; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   AX, word ptr [RCX]
    BTC   AX, DX
    MOV   word ptr [RCX], AX
{$ELSE}
    MOV   CX, word ptr [EAX]
    BTC   CX, DX
    MOV   word ptr [EAX], CX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
If Result then Value := UInt16(Value and not(UInt16(1) shl Bit))
  else Value := UInt16(Value or (UInt16(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTC(var Value: UInt32; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   EAX,  dword ptr [RCX]
    BTC   EAX,  EDX
    MOV   dword ptr [RCX],  EAX
{$ELSE}
    MOV   ECX,  dword ptr [EAX]
    BTC   ECX,  EDX
    MOV   dword ptr [EAX],  ECX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
If Result then Value := UInt32(Value and not(UInt32(1) shl Bit))
  else Value := UInt32(Value or (UInt32(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTC(var Value: UInt64; Bit: Byte): Boolean;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    MOV   RAX, qword ptr [RCX]
    BTC   RAX,  RDX
    MOV   qword ptr [RCX], RAX
{$ELSE}
    CMP   EDX,  32
    JAE   @TestHigh

    BTC   dword ptr [Value],  EDX
    JMP   @SetResult

  @TestHigh:
    AND   EDX,  $1F
    BTC   dword ptr [Value + 4],  EDX
{$ENDIF}
  @SetResult:
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
If Result then Value := UInt64(Value and not(UInt64(1) shl Bit))
  else Value := UInt64(Value or (UInt64(1) shl Bit));
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                      Bit test and set to a given value                       }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BitSetTo(var Value: UInt8; Bit: Byte; NewValue: Boolean): Boolean;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;
             
//------------------------------------------------------------------------------

Function BitSetTo(var Value: UInt16; Bit: Byte; NewValue: Boolean): Boolean;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;
            
//------------------------------------------------------------------------------

Function BitSetTo(var Value: UInt32; Bit: Byte; NewValue: Boolean): Boolean;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;

//------------------------------------------------------------------------------

Function BitSetTo(var Value: UInt64; Bit: Byte; NewValue: Boolean): Boolean;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit scan forward (BSF)                            }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BSF(Value: UInt8): Integer;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    AND   RCX,  $FF
    XOR   RAX,  RAX
    BSF   AX,   CX
{$ELSE}
    AND   EAX,  $FF
    BSF   AX,   AX
{$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX,  -1
  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 0 to 7 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}
              
//------------------------------------------------------------------------------

Function BSF(Value: UInt16): Integer;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    AND   RCX,  $FFFF
    XOR   RAX,  RAX
    BSF   AX,   CX
{$ELSE}
    AND   EAX,  $FFFF
    BSF   AX,   AX
{$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX,  -1
  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 0 to 15 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}
             
//------------------------------------------------------------------------------

Function BSF(Value: UInt32): Integer;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    BSF   EAX,  ECX
{$ELSE}
    BSF   EAX,  EAX
{$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX,  -1
  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 0 to 31 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSF(Value: UInt64): Integer;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    BSF   RAX,  RCX
    JNZ   @RoutineEnd
    MOV   RAX,  -1
  @RoutineEnd:
{$ELSE}
    BSF   EAX,  dword ptr [Value]
    JNZ   @RoutineEnd

    BSF   EAX,  dword ptr [Value + 4]
    JNZ   @Add32

    MOV   EAX,  -1
    JMP   @RoutineEnd

  @Add32:
    ADD   EAX,  32

  @RoutineEnd:
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 0 to 63 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit scan reversed (BSR)                           }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BSR(Value: UInt8): Integer; register;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    AND   RCX,  $FF
    XOR   RAX,  RAX
    BSR   AX,   CX
{$ELSE}
    AND   EAX,  $FF
    BSR   AX,   AX
{$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX,  -1
  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 7 downto 0 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSR(Value: UInt16): Integer;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    AND   RCX,  $FFFF
    XOR   RAX,  RAX
    BSR   AX,   CX
{$ELSE}
    AND   EAX,  $FFFF
    BSR   AX,   AX
{$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX,  -1
  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 15 downto 0 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSR(Value: UInt32): Integer;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    BSR   EAX,  ECX
{$ELSE}
    BSR   EAX,  EAX
{$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX,  -1
  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 31 downto 0 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSR(Value: UInt64): Integer;{$IFNDEF PurePascal}assembler;
asm
{$IFDEF x64}
    BSR   RAX,  RCX
    JNZ   @RoutineEnd
    MOV   RAX,  -1
  @RoutineEnd:
{$ELSE}
    BSR   EAX,  dword ptr [Value + 4]
    JNZ   @Add32

    BSR   EAX,  dword ptr [Value]
    JNZ   @RoutineEnd

    MOV   EAX,  -1
    JMP   @RoutineEnd

  @Add32:
    ADD   EAX,  32

  @RoutineEnd:
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 63 downto 0 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                               Population count                               }
{==============================================================================}
{------------------------------------------------------------------------------}

{$IFDEF UseLookupTable}
const
  PopCountTable: array[Byte] of UInt8 = (
    0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8);
{$ENDIF}

Function PopCount(Value: UInt8): Integer;
{$IFDEF UseLookupTable}
begin
Result := PopCountTable[Value];
end;
{$ELSE}
var
  i:  Integer;
begin
Result := 0;
For i := 1 to 8 do
  begin
    If (Value and 1) <> 0 then Inc(Result);
    Value := UInt8(Value shr 1);
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function PopCount(Value: UInt16): Integer;
{$IFDEF UseLookupTable}
begin
Result := PopCountTable[Byte(Value)] + PopCountTable[Byte(Value shr 8)];
end;
{$ELSE}
var
  i:  Integer;
begin
Result := 0;
For i := 1 to 16 do
  begin
    If (Value and 1) <> 0 then Inc(Result);
    Value := UInt16(Value shr 1);
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function PopCount(Value: UInt32): Integer;
{$IFDEF UseLookupTable}
begin
Result := PopCountTable[Byte(Value)] + PopCountTable[Byte(Value shr 8)] +
  PopCountTable[Byte(Value shr 16)] + PopCountTable[Byte(Value shr 24)];
end;
{$ELSE}
var
  i:  Integer;
begin
Result := 0;
For i := 1 to 32 do
  begin
    If (Value and 1) <> 0 then Inc(Result);
    Value := UInt32(Value shr 1);
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function PopCount(Value: UInt64): Integer;
{$IFDEF UseLookupTable}
begin
{$IFDEF 64bit}
Result := PopCountTable[Byte(Value)] + PopCountTable[Byte(Value shr 8)] +
  PopCountTable[Byte(Value shr 16)] + PopCountTable[Byte(Value shr 24)] +
  PopCountTable[Byte(Value shr 32)] + PopCountTable[Byte(Value shr 40)] +
  PopCountTable[Byte(Value shr 48)] + PopCountTable[Byte(Value shr 56)];
{$ELSE}
Result := PopCount(Int64Rec(Value).Lo) + PopCount(Int64Rec(Value).Hi);
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := 0;
For i := 1 to 64 do
  begin
    If (Value and 1) <> 0 then Inc(Result);
    Value := UInt64(Value shr 1);
  end;
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Nibble manipulation                             }
{==============================================================================}
{------------------------------------------------------------------------------}

Function GetHighNibble(Value: UInt8): TNibble;
begin
Result := (Value shr 4) and $0F;
end;

//------------------------------------------------------------------------------

Function GetLowNibble(Value: UInt8): TNibble;
begin
Result := Value and $0F;
end;

//------------------------------------------------------------------------------

Function SetHighNibble(Value: UInt8; SetTo: TNibble): UInt8;
begin
Result := (Value and $0F) or UInt8((SetTo and $0F) shl 4);
end;

//------------------------------------------------------------------------------

Function SetLowNibble(Value: UInt8; SetTo: TNibble): UInt8;
begin
Result := (Value and $F0) or UInt8(SetTo and $0F);
end;

//------------------------------------------------------------------------------

procedure SetHighNibbleValue(var Value: UInt8; SetTo: TNibble);
begin
Value := SetHighNibble(Value,SetTo);
end;

//------------------------------------------------------------------------------

procedure SetLowNibbleValue(var Value: UInt8; SetTo: TNibble);
begin
Value := SetLowNibble(Value,SetTo);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Get flag state                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function GetFlagState(Value,FlagBitmask: UInt8; ExactMatch: Boolean = False): Boolean;
begin
If ExactMatch then
  Result := (Value and FlagBitmask) = FlagBitmask
else
  Result := (Value and FlagBitmask) <> 0;
end;

//------------------------------------------------------------------------------

Function GetFlagState(Value,FlagBitmask: UInt16; ExactMatch: Boolean = False): Boolean;
begin
If ExactMatch then
  Result := (Value and FlagBitmask) = FlagBitmask
else
  Result := (Value and FlagBitmask) <> 0;
end;

//------------------------------------------------------------------------------

Function GetFlagState(Value,FlagBitmask: UInt32; ExactMatch: Boolean = False): Boolean;
begin
If ExactMatch then
  Result := (Value and FlagBitmask) = FlagBitmask
else
  Result := (Value and FlagBitmask) <> 0;
end;

//------------------------------------------------------------------------------

Function GetFlagState(Value,FlagBitmask: UInt64; ExactMatch: Boolean = False): Boolean;
begin
If ExactMatch then
  Result := (Value and FlagBitmask) = FlagBitmask
else
  Result := (Value and FlagBitmask) <> 0;
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                   Set flag                                   }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SetFlag(Value,FlagBitmask: UInt8): UInt8;
begin
Result := Value or FlagBitmask;
end;

//------------------------------------------------------------------------------

Function SetFlag(Value,FlagBitmask: UInt16): UInt16;
begin
Result := Value or FlagBitmask;
end;

//------------------------------------------------------------------------------

Function SetFlag(Value,FlagBitmask: UInt32): UInt32;
begin
Result := Value or FlagBitmask;
end;

//------------------------------------------------------------------------------

Function SetFlag(Value,FlagBitmask: UInt64): UInt64;
begin
Result := Value or FlagBitmask;
end;

//==============================================================================

procedure SetFlagValue(var Value: UInt8; FlagBitmask: UInt8);
begin
Value := SetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

procedure SetFlagValue(var Value: UInt16; FlagBitmask: UInt16);
begin
Value := SetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

procedure SetFlagValue(var Value: UInt32; FlagBitmask: UInt32);
begin
Value := SetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

procedure SetFlagValue(var Value: UInt64; FlagBitmask: UInt64);
begin
Value := SetFlag(Value,FlagBitmask);
end;

//==============================================================================

Function SetFlags_8(Value: UInt8; Flags: array of UInt8): UInt8;
var
  TempBitmask:  UInt8;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := SetFlag(Value,TempBitmask);
end;

//------------------------------------------------------------------------------

Function SetFlags_16(Value: UInt16; Flags: array of UInt16): UInt16;
var
  TempBitmask:  UInt16;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := SetFlag(Value,TempBitmask);
end;
 
//------------------------------------------------------------------------------

Function SetFlags_32(Value: UInt32; Flags: array of UInt32): UInt32;
var
  TempBitmask:  UInt32;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := SetFlag(Value,TempBitmask);
end;
 
//------------------------------------------------------------------------------

Function SetFlags_64(Value: UInt64; Flags: array of UInt64): UInt64;
var
  TempBitmask:  UInt64;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := SetFlag(Value,TempBitmask);
end;

//==============================================================================

Function SetFlags(Value: UInt8; Flags: array of UInt8): UInt8;
begin
Result := SetFlags_8(Value,Flags);
end;

//------------------------------------------------------------------------------

Function SetFlags(Value: UInt16; Flags: array of UInt16): UInt16;
begin
Result := SetFlags_16(Value,Flags);
end;

//------------------------------------------------------------------------------

Function SetFlags(Value: UInt32; Flags: array of UInt32): UInt32;
begin
Result := SetFlags_32(Value,Flags);
end;

//------------------------------------------------------------------------------

Function SetFlags(Value: UInt64; Flags: array of UInt64): UInt64;
begin
Result := SetFlags_64(Value,Flags);
end;

//==============================================================================

procedure SetFlagsValue_8(var Value: UInt8; Flags: array of UInt8);
begin
Value := SetFlags_8(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure SetFlagsValue_16(var Value: UInt16; Flags: array of UInt16);
begin
Value := SetFlags_16(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure SetFlagsValue_32(var Value: UInt32; Flags: array of UInt32);
begin
Value := SetFlags_32(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure SetFlagsValue_64(var Value: UInt64; Flags: array of UInt64);
begin
Value := SetFlags_64(Value,Flags);
end;

//==============================================================================

procedure SetFlagsValue(var Value: UInt8; Flags: array of UInt8);
begin
SetFlagsValue_8(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure SetFlagsValue(var Value: UInt16; Flags: array of UInt16);
begin
SetFlagsValue_16(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure SetFlagsValue(var Value: UInt32; Flags: array of UInt32);
begin
SetFlagsValue_32(Value,Flags);
end;

//------------------------------------------------------------------------------
 
procedure SetFlagsValue(var Value: UInt64; Flags: array of UInt64);
begin
SetFlagsValue_64(Value,Flags);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                  Reset flag                                  }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ResetFlag(Value,FlagBitmask: UInt8): UInt8;
begin
Result := Value and not FlagBitmask;
end;

//------------------------------------------------------------------------------

Function ResetFlag(Value,FlagBitmask: UInt16): UInt16;
begin
Result := Value and not FlagBitmask;
end;

//------------------------------------------------------------------------------

Function ResetFlag(Value,FlagBitmask: UInt32): UInt32;
begin
Result := Value and not FlagBitmask;
end;

//------------------------------------------------------------------------------

Function ResetFlag(Value,FlagBitmask: UInt64): UInt64;
begin
Result := Value and not FlagBitmask;
end;

//==============================================================================

procedure ResetFlagValue(var Value: UInt8; FlagBitmask: UInt8);
begin
Value := ResetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

procedure ResetFlagValue(var Value: UInt16; FlagBitmask: UInt16);
begin
Value := ResetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

procedure ResetFlagValue(var Value: UInt32; FlagBitmask: UInt32);
begin
Value := ResetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

procedure ResetFlagValue(var Value: UInt64; FlagBitmask: UInt64);
begin
Value := ResetFlag(Value,FlagBitmask);
end;

//==============================================================================

Function ResetFlags_8(Value: UInt8; Flags: array of UInt8): UInt8;
var
  TempBitmask:  UInt8;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := ResetFlag(Value,TempBitmask);
end;

//------------------------------------------------------------------------------

Function ResetFlags_16(Value: UInt16; Flags: array of UInt16): UInt16;
var
  TempBitmask:  UInt16;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := ResetFlag(Value,TempBitmask);
end;

//------------------------------------------------------------------------------

Function ResetFlags_32(Value: UInt32; Flags: array of UInt32): UInt32;
var
  TempBitmask:  UInt32;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := ResetFlag(Value,TempBitmask);
end;

//------------------------------------------------------------------------------

Function ResetFlags_64(Value: UInt64; Flags: array of UInt64): UInt64;
var
  TempBitmask:  UInt64;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := ResetFlag(Value,TempBitmask);
end;

//==============================================================================

Function ResetFlags(Value: UInt8; Flags: array of UInt8): UInt8;
begin
Result := ResetFlags_8(Value,Flags);
end;

//------------------------------------------------------------------------------

Function ResetFlags(Value: UInt16; Flags: array of UInt16): UInt16;
begin
Result := ResetFlags_16(Value,Flags);
end;
//------------------------------------------------------------------------------

Function ResetFlags(Value: UInt32; Flags: array of UInt32): UInt32;
begin
Result := ResetFlags_32(Value,Flags);
end;

//------------------------------------------------------------------------------

Function ResetFlags(Value: UInt64; Flags: array of UInt64): UInt64;
begin
Result := ResetFlags_64(Value,Flags);
end;

//==============================================================================

procedure ResetFlagsValue_8(var Value: UInt8; Flags: array of UInt8);
begin
Value := ResetFlags_8(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure ResetFlagsValue_16(var Value: UInt16; Flags: array of UInt16);
begin
Value := ResetFlags_16(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure ResetFlagsValue_32(var Value: UInt32; Flags: array of UInt32);
begin
Value := ResetFlags_32(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure ResetFlagsValue_64(var Value: UInt64; Flags: array of UInt64);
begin
Value := ResetFlags_64(Value,Flags);
end;

//==============================================================================

procedure ResetFlagsValue(var Value: UInt8; Flags: array of UInt8);
begin
ResetFlagsValue_8(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure ResetFlagsValue(var Value: UInt16; Flags: array of UInt16);
begin
ResetFlagsValue_16(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure ResetFlagsValue(var Value: UInt32; Flags: array of UInt32);
begin
ResetFlagsValue_32(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure ResetFlagsValue(var Value: UInt64; Flags: array of UInt64);
begin
ResetFlagsValue_64(Value,Flags);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Set flag state                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SetFlagState(Value,FlagBitmask: UInt8; NewState: Boolean): UInt8;
begin
If NewState then
  Result := SetFlag(Value,FlagBitmask)
else
  Result := ResetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

Function SetFlagState(Value,FlagBitmask: UInt16; NewState: Boolean): UInt16;
begin
If NewState then
  Result := SetFlag(Value,FlagBitmask)
else
  Result := ResetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

Function SetFlagState(Value,FlagBitmask: UInt32; NewState: Boolean): UInt32;
begin
If NewState then
  Result := SetFlag(Value,FlagBitmask)
else
  Result := ResetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

Function SetFlagState(Value,FlagBitmask: UInt64; NewState: Boolean): UInt64;
begin
If NewState then
  Result := SetFlag(Value,FlagBitmask)
else
  Result := ResetFlag(Value,FlagBitmask);
end;

//==============================================================================

procedure SetFlagStateValue(var Value: UInt8; FlagBitmask: UInt8; NewState: Boolean);
begin
Value := SetFlagState(Value,FlagBitmask,NewState);
end;

//------------------------------------------------------------------------------

procedure SetFlagStateValue(var Value: UInt16; FlagBitmask: UInt16; NewState: Boolean);
begin
Value := SetFlagState(Value,FlagBitmask,NewState);
end;

//------------------------------------------------------------------------------

procedure SetFlagStateValue(var Value: UInt32; FlagBitmask: UInt32; NewState: Boolean);
begin
Value := SetFlagState(Value,FlagBitmask,NewState);
end;

//------------------------------------------------------------------------------

procedure SetFlagStateValue(var Value: UInt64; FlagBitmask: UInt64; NewState: Boolean);
begin
Value := SetFlagState(Value,FlagBitmask,NewState);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                   Set bits                                   }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SetBits(Value,NewBits: UInt8; FromBit,ToBit: Integer): UInt8;
var
  Mask: UInt8;
begin
Mask := ($FF shl (FromBit and 7)) and ($FF shr (7 - (ToBit and 7)));
Result := (Value and not Mask) or (NewBits and Mask);
end;

//------------------------------------------------------------------------------

Function SetBits(Value,NewBits: UInt16; FromBit,ToBit: Integer): UInt16;
var
  Mask: UInt16;
begin
Mask := ($FFFF shl (FromBit and 15)) and ($FFFF shr (15 - (ToBit and 15)));
Result := (Value and not Mask) or (NewBits and Mask);
end;

//------------------------------------------------------------------------------

Function SetBits(Value,NewBits: UInt32; FromBit,ToBit: Integer): UInt32;
var
  Mask: UInt32;
begin
Mask := ($FFFFFFFF shl (FromBit and 31)) and ($FFFFFFFF shr (31 - (ToBit and 31)));
Result := (Value and not Mask) or (NewBits and Mask);
end;

//------------------------------------------------------------------------------

Function SetBits(Value,NewBits: UInt64; FromBit,ToBit: Integer): UInt64;
var
  Mask: UInt64;
begin
Mask := (UInt64($FFFFFFFFFFFFFFFF) shl (FromBit and 63)) and (UInt64($FFFFFFFFFFFFFFFF) shr (63 - (ToBit and 63)));
Result := (Value and not Mask) or (NewBits and Mask);
end;

//==============================================================================

procedure SetBitsValue(var Value: UInt8; NewBits: UInt8; FromBit,ToBit: Integer);
begin
Value := SetBits(Value,NewBits,FromBit,ToBit);
end;

//------------------------------------------------------------------------------

procedure SetBitsValue(var Value: UInt16; NewBits: UInt16; FromBit,ToBit: Integer);
begin
Value := SetBits(Value,NewBits,FromBit,ToBit);
end;

//------------------------------------------------------------------------------

procedure SetBitsValue(var Value: UInt32; NewBits: UInt32; FromBit,ToBit: Integer);
begin
Value := SetBits(Value,NewBits,FromBit,ToBit);
end;

//------------------------------------------------------------------------------

procedure SetBitsValue(var Value: UInt64; NewBits: UInt64; FromBit,ToBit: Integer);
begin
Value := SetBits(Value,NewBits,FromBit,ToBit);
end;

end.
