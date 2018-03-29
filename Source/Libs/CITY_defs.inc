(*
  CRC_Functions

  When active, the CRC functions are available for use, otherwise they are
  removed from the library.

  Default: active
*)
{$DEFINE CRC_Functions}

(*
  VER_1_0_3

  When active, the library uses version 1.0.3 of the CityHash implementation.
  Otherwise the version 1.1.1 is used.

  Default: inactive
*)
{.$DEFINE VER_1_0_3}

{
  AllowCRCExtension

  When defined, allows use of CRC32 instruction (part of SSE 4.2) in ASM.
  Availability of the instruction is tested at unit initialization, and the
  instruction is used only when properly supported. When it is not supported,
  pascal implementation is called instead.
  Has no meaning when PurePascal symbol is defined.

  Default:  active
}
{$DEFINE AllowCRCExtension}

//------------------------------------------------------------------------------

// Do not change following lines
{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IFDEF FPC}
  {$MODE Delphi}
  {$ASMMODE Intel}
{$ENDIF}

{$IF not Defined(FPC) and not Defined(x64)}
  {$DEFINE ASM_MachineCode}
{$IFEND}

{$Q-}
{$R-}
