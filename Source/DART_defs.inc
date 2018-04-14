//--- Architecture -------------------------------------------------------------

{$IF Defined(CPU64) or Defined(CPU64BITS)}
  {$DEFINE 64bit}
{$ELSEIF Defined(CPU16)}
  {$MESSAGE FATAL 'Unsupported CPU.'}
{$ELSE}
  {$DEFINE 32bit}
{$IFEND}

//--- CPU ----------------------------------------------------------------------

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

//--- Operating system ---------------------------------------------------------

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system'}
{$IFEND}

//--- Compiler -----------------------------------------------------------------

{$IFDEF FPC}
  {$MODE ObjFPC}{$H+}
  {$MODESWITCH CLASSICPROCVARS+}
  {$INLINE ON}
  {$DEFINE CanInline}
{$ELSE}
  {$DEFINE Delphi}
  {$IF Declared(CompilerVersion)}
    {$IF CompilerVersion >= 17 then}  // Delphi 2005+
      {$DEFINE CanInline}
    {$IFEND}
  {$IFEND}  
{$ENDIF}

//--- Environment --------------------------------------------------------------

{$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
  {$DEFINE FPC_NonUnicode_NoUTF8RTL}
{$ELSE}
  {$UNDEF FPC_NonUnicode_NoUTF8RTL}
{$IFEND}

{$IF not Defined(FPC) and (CompilerVersion >= 17)}  // not sure about the version
  {$DEFINE Delphi_Inline_Helpers}
{$IFEND}

//--- Others -------------------------------------------------------------------

{.$DEFINE DevelNotes}
