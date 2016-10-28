{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$IFDEF Debug}
  {$DEFINE DevMsgs}
{$ENDIF}

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE 64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL 'Unsupported CPU.'}
{$ELSE}
  {$DEFINE 32bit}
{$IFEND}

