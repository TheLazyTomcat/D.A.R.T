{$IF Defined(CPU64) or Defined(CPU64BITS)}
  {$DEFINE 64bit}
{$ELSEIF Defined(CPU16)}
  {$MESSAGE FATAL 'Unsupported CPU.'}
{$ELSE}
  {$DEFINE 32bit}
{$IFEND}

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}{$H+}
  {$MODESWITCH CLASSICPROCVARS+}
{$ELSE}
  {$DEFINE Delphi}
{$ENDIF}

{$IF Defined(FPC) and not Defined(Unicode)}
  {$DEFINE FPC_NonUnicode}
{$ELSE}
  {$UNDEF FPC_NonUnicode}
{$IFEND}

{$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
  {$DEFINE FPC_NonUnicode_NoUTF8RTL}
{$ELSE}
  {$UNDEF FPC_NonUnicode_NoUTF8RTL}
{$IFEND}