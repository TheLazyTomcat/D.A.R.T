{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$IFDEF Debug}
  {$DEFINE DevMsgs}
{$ENDIF}

{$IF Defined(CPU64) or Defined(CPU64BITS)}
  {$DEFINE 64bit}
{$ELSEIF Defined(CPU16)}
  {$MESSAGE FATAL 'Unsupported CPU.'}
{$ELSE}
  {$DEFINE 32bit}
{$IFEND}

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

{.$DEFINE FPC_Internal_ZLib} // do not activate, it is causing problems (stalls at decompression)

{$IFNDEF FPC}
  {$UNDEF FPC_Internal_ZLib}
{$ENDIF}
