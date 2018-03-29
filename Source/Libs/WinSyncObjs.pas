{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  WinSyncObjs - set of classes encapsulating windows synchronization objects

  ©František Milt 2017-07-17

  Version 1.0.2

  Dependencies:
    StrRect - github.com/ncs-sniper/Lib.StrRect

===============================================================================}
unit WinSyncObjs;

interface

{$IF not(defined(MSWINDOWS) or defined(WINDOWS))}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$IF Declared(CompilerVersion)}
  {$IF CompilerVersion >= 20} // Delphi 2009+
    {$DEFINE DeprecatedCommentDelphi}
  {$IFEND}
{$IFEND}

{$IF Defined(FPC) or Defined(DeprecatedCommentDelphi)}
  {$DEFINE DeprecatedComment}
{$ELSE}
  {$UNDEF DeprecatedComment}
{$IFEND}

uses
  Windows;

const
  SEMAPHORE_MODIFY_STATE = $00000002;
  SEMAPHORE_ALL_ACCESS   = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $3;

  TIMER_MODIFY_STATE = $00000002;
  TIMER_QUERY_STATE  = $00000001;
  TIMER_ALL_ACCESS   = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or
                       TIMER_QUERY_STATE or TIMER_MODIFY_STATE;

type
  TWaitResult = (wrSignaled, wrTimeout, wrAbandoned, wrError);

//==============================================================================
//--  TCriticalSection declaration  --------------------------------------------
//==============================================================================

  TCriticalSection = class(TObject)
  private
    fCriticalSectionObj:  TRTLCriticalSection;
    fSpinCount:           DWORD;
    procedure _SetSpinCount(Value: DWORD);
  public
    constructor Create; overload;
    constructor Create(SpinCount: DWORD); overload;
    destructor Destroy; override;
    Function SetSpinCount(SpinCount: DWORD): DWORD;
    Function TryEnter: Boolean;
    procedure Enter;
    procedure Leave;
    property SpinCount: DWORD read fSpinCount write _SetSpinCount;
  end;

//==============================================================================
//--  TWinSyncObject declaration  ----------------------------------------------
//==============================================================================

  TWinSyncObject = class(TObject)
  private
    fHandle:      THandle;
    fLastError:   Integer;
    fName:        String;
  protected
    Function SetAndRectifyName(const Name: String): Boolean; virtual;
    procedure SetAndCheckHandle(Handle: THandle); virtual;
  public
    destructor Destroy; override;
    Function WaitFor(Timeout: DWORD = INFINITE): TWaitResult; virtual;
    property Handle: THandle read fHandle;
    property LastError: Integer read fLastError;
    property Name: String read fName;
  end;

//==============================================================================
//--  TEvent declaration  ------------------------------------------------------
//==============================================================================

  TEvent = class(TWinSyncObject)
  public
    constructor Create(SecurityAttributes: PSecurityAttributes; ManualReset, InitialState: Boolean; const Name: String); overload;
    constructor Create(const Name: String); overload;
    constructor Create; overload;
    constructor Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String); overload;
    constructor Open(const Name: String); overload;
    Function WaitForAndReset(Timeout: DWORD = INFINITE): TWaitResult;
    Function SetEvent: Boolean;
    Function ResetEvent: Boolean;
  {
    Function PulseEvent is unreliable and should not be used. More info here:
    https://msdn.microsoft.com/en-us/library/windows/desktop/ms684914
  }
    Function PulseEvent: Boolean; deprecated {$IFDEF DeprecatedComment}'Unreliable, do not use.'{$ENDIF};
  end;

//==============================================================================
//--  TMutex declaration  ------------------------------------------------------
//==============================================================================

  TMutex = class(TWinSyncObject)
  public
    constructor Create(SecurityAttributes: PSecurityAttributes; InitialOwner: Boolean; const Name: String); overload;
    constructor Create(const Name: String); overload;
    constructor Create; overload;
    constructor Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String); overload;
    constructor Open(const Name: String); overload;
    Function WaitForAndRelease(TimeOut: DWORD = INFINITE): TWaitResult;
    Function ReleaseMutex: Boolean;
  end;

//==============================================================================
//--  TSemaphore declaration  --------------------------------------------------
//==============================================================================

  TSemaphore = class(TWinSyncObject)
  public
    constructor Create(SecurityAttributes: PSecurityAttributes; InitialCount, MaximumCount: Integer; const Name: String); overload;
    constructor Create(InitialCount, MaximumCount: Integer; const Name: String); overload;
    constructor Create(InitialCount, MaximumCount: Integer); overload;
    constructor Open(DesiredAccess: LongWord; InheritHandle: Boolean; const Name: String); overload;
    constructor Open(const Name: String); overload;
    Function WaitForAndRelease(TimeOut: LongWord = INFINITE): TWaitResult;
    Function ReleaseSemaphore(ReleaseCount: Integer; out PreviousCount: Integer): Boolean; overload;
    Function ReleaseSemaphore: Boolean; overload;
  end;

//==============================================================================
//--  TWaitableTimer declaration  ----------------------------------------------
//==============================================================================

  TTimerAPCRoutine = procedure(ArgToCompletionRoutine: Pointer; TimerLowValue, TimerHighValue: DWORD); stdcall;
  PTimerAPCRoutine = ^TTimerAPCRoutine;

  TWaitableTimer = class(TWinSyncObject)
  public
    constructor Create(SecurityAttributes: PSecurityAttributes; ManualReset: Boolean; const Name: String); overload;
    constructor Create(const Name: String); overload;
    constructor Create; overload;
    constructor Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String); overload;
    constructor Open(const Name: String); overload;
    Function SetWaitableTimer(DueTime: Int64; Period: Integer; CompletionRoutine: TTimerAPCRoutine; ArgToCompletionRoutine: Pointer; Resume: Boolean): Boolean; overload;
    Function SetWaitableTimer(DueTime: Int64; Period: Integer = 0): Boolean; overload;
    Function SetWaitableTimer(DueTime: TDateTime; Period: Integer; CompletionRoutine: TTimerAPCRoutine; ArgToCompletionRoutine: Pointer; Resume: Boolean): Boolean; overload;
    Function SetWaitableTimer(DueTime: TDateTime; Period: Integer = 0): Boolean; overload;
    Function CancelWaitableTimer: Boolean;
  end;

implementation

uses
  SysUtils, StrRect;

//==============================================================================
//--  TCriticalSection implementation  -----------------------------------------
//==============================================================================

//------------------------------------------------------------------------------
//    TCriticalSection - private methods
//------------------------------------------------------------------------------

procedure TCriticalSection._SetSpinCount(Value: DWORD);
begin
fSpinCount := Value;
SetSpinCount(fSpinCount);
end;

//------------------------------------------------------------------------------
//    TCriticalSection - public methods
//------------------------------------------------------------------------------

constructor TCriticalSection.Create;
begin
inherited Create;
fSpinCount := 0;
InitializeCriticalSection(fCriticalSectionObj);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TCriticalSection.Create(SpinCount: DWORD);
begin
inherited Create;
fSpinCount := SpinCount;
InitializeCriticalSectionAndSpinCount(fCriticalSectionObj,SpinCount);
end;

//------------------------------------------------------------------------------

destructor TCriticalSection.Destroy;
begin
DeleteCriticalSection(fCriticalSectionObj);
inherited;
end;

//------------------------------------------------------------------------------

Function TCriticalSection.SetSpinCount(SpinCount: DWORD): DWORD;
begin
fSpinCount := SpinCount;
Result := SetCriticalSectionSpinCount(fCriticalSectionObj,SpinCount);
end;

//------------------------------------------------------------------------------

Function TCriticalSection.TryEnter: Boolean;
begin
Result := TryEnterCriticalSection(fCriticalSectionObj);
end;

//------------------------------------------------------------------------------

procedure TCriticalSection.Enter;
begin
EnterCriticalSection(fCriticalSectionObj);
end;

//------------------------------------------------------------------------------

procedure TCriticalSection.Leave;
begin
LeaveCriticalSection(fCriticalSectionObj);
end;


//==============================================================================
//--  TWinSyncObject implementation  -------------------------------------------
//==============================================================================

//------------------------------------------------------------------------------
//    TWinSyncObject - proteted methods
//------------------------------------------------------------------------------

Function TWinSyncObject.SetAndRectifyName(const Name: String): Boolean;
begin
fName := Name;
If Length(fName) > MAX_PATH then SetLength(fName,MAX_PATH);
Result := Length(fName) > 0;
end;

//------------------------------------------------------------------------------

procedure TWinSyncObject.SetAndCheckHandle(Handle: THandle);
begin
fHandle := Handle;
If fHandle = 0 then
  begin
    fLastError := GetLastError;
    RaiseLastOSError;
  end;
end;

//------------------------------------------------------------------------------
//    TWinSyncObject - public methods
//------------------------------------------------------------------------------

destructor TWinSyncObject.Destroy;
begin
CloseHandle(fHandle);
inherited;
end;

//------------------------------------------------------------------------------

Function TWinSyncObject.WaitFor(Timeout: DWORD = INFINITE): TWaitResult;
begin
case WaitForSingleObject(fHandle,Timeout) of
  WAIT_ABANDONED: Result := wrAbandoned;
  WAIT_OBJECT_0:  Result := wrSignaled;
  WAIT_TIMEOUT:   Result := wrTimeout;
  WAIT_FAILED:    begin
                    Result := wrError;
                    FLastError := GetLastError;
                  end;
else
  Result := wrError;
  FLastError := GetLastError;
end;
end;


//==============================================================================
//--  TEvent implementation  ---------------------------------------------------
//==============================================================================

//------------------------------------------------------------------------------
//    TEvent - public methods
//------------------------------------------------------------------------------

constructor TEvent.Create(SecurityAttributes: PSecurityAttributes; ManualReset, InitialState: Boolean; const Name: String);
begin
inherited Create;
If SetAndRectifyName(Name) then
  SetAndCheckHandle(CreateEvent(SecurityAttributes,ManualReset,InitialState,PChar(StrToWin(fName))))
else
  SetAndCheckHandle(CreateEvent(SecurityAttributes,ManualReset,InitialState,nil));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TEvent.Create(const Name: String);
begin
Create(nil,True,False,Name);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TEvent.Create;
begin
Create(nil,True,False,'');
end;

//------------------------------------------------------------------------------

constructor TEvent.Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String);
begin
inherited Create;
SetAndRectifyName(Name);
SetAndCheckHandle(OpenEvent(DesiredAccess,InheritHandle,PChar(StrToWin(fName))));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TEvent.Open(const Name: String);
begin
Open(SYNCHRONIZE or EVENT_MODIFY_STATE,False,Name);
end;

//------------------------------------------------------------------------------

Function TEvent.WaitForAndReset(Timeout: DWORD = INFINITE): TWaitResult;
begin
Result := WaitFor(Timeout);
If Result = wrSignaled then ResetEvent;
end;

//------------------------------------------------------------------------------

Function TEvent.SetEvent: Boolean;
begin
Result := Windows.SetEvent(fHandle);
If not Result then
  fLastError := GetLastError;
end;

//------------------------------------------------------------------------------

Function TEvent.ResetEvent: Boolean;
begin
Result := Windows.ResetEvent(fHandle);
If not Result then
  fLastError := GetLastError;
end;

//------------------------------------------------------------------------------

{$WARN SYMBOL_DEPRECATED OFF}
Function TEvent.PulseEvent: Boolean;
{$WARN SYMBOL_DEPRECATED ON}
begin
Result := Windows.PulseEvent(fHandle);
If not Result then
  fLastError := GetLastError;
end;


//==============================================================================
//--  TMutex implementation  ---------------------------------------------------
//==============================================================================

//------------------------------------------------------------------------------
//    TMutex - public methods
//------------------------------------------------------------------------------

constructor TMutex.Create(SecurityAttributes: PSecurityAttributes; InitialOwner: Boolean; const Name: String);
begin
inherited Create;
If SetAndRectifyName(Name) then
  SetAndCheckHandle(CreateMutex(SecurityAttributes,InitialOwner,PChar(StrToWin(fName))))
else
  SetAndCheckHandle(CreateMutex(SecurityAttributes,InitialOwner,nil));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TMutex.Create(const Name: String);
begin
Create(nil,False,Name);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TMutex.Create;
begin
Create(nil,False,'');
end;

//------------------------------------------------------------------------------

constructor TMutex.Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String);
begin
inherited Create;
SetAndRectifyName(Name);
SetAndCheckHandle(OpenMutex(DesiredAccess,InheritHandle,PChar(StrToWin(fName))));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TMutex.Open(const Name: String);
begin
Open(SYNCHRONIZE or MUTEX_MODIFY_STATE,False,Name);
end;

//------------------------------------------------------------------------------

Function TMutex.WaitForAndRelease(TimeOut: DWORD = INFINITE): TWaitResult;
begin
Result := WaitFor(Timeout);
If Result in [wrSignaled,wrAbandoned] then ReleaseMutex;
end;

//------------------------------------------------------------------------------

Function TMutex.ReleaseMutex: Boolean;
begin
Result := Windows.ReleaseMutex(fHandle);
If not Result then
  fLastError := GetLastError;
end;


//==============================================================================
//--  TSemaphore implementation  -----------------------------------------------
//==============================================================================

//------------------------------------------------------------------------------
//    TSemaphore - public methods
//------------------------------------------------------------------------------

constructor TSemaphore.Create(SecurityAttributes: PSecurityAttributes; InitialCount, MaximumCount: Integer; const Name: String);
begin
inherited Create;
If SetAndRectifyName(Name) then
  SetAndCheckHandle(CreateSemaphore(SecurityAttributes,InitialCount,MaximumCount,PChar(StrToWin(fName))))
else
  SetAndCheckHandle(CreateSemaphore(SecurityAttributes,InitialCount,MaximumCount,nil));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TSemaphore.Create(InitialCount, MaximumCount: Integer; const Name: String);
begin
Create(nil,InitialCount,MaximumCount,Name);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TSemaphore.Create(InitialCount, MaximumCount: Integer);
begin
Create(nil,InitialCount,MaximumCount,'');
end;

//------------------------------------------------------------------------------

constructor TSemaphore.Open(DesiredAccess: LongWord; InheritHandle: Boolean; const Name: String);
begin
inherited Create;
SetAndRectifyName(Name);
SetAndCheckHandle(OpenSemaphore(DesiredAccess,InheritHandle,PChar(StrToWin(fName))));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TSemaphore.Open(const Name: String);
begin
Open(SYNCHRONIZE or SEMAPHORE_MODIFY_STATE,False,Name);
end;
 
//------------------------------------------------------------------------------

Function TSemaphore.WaitForAndRelease(TimeOut: LongWord = INFINITE): TWaitResult;
begin
Result := WaitFor(Timeout);
If Result in [wrSignaled,wrAbandoned] then ReleaseSemaphore;
end;

//------------------------------------------------------------------------------

Function TSemaphore.ReleaseSemaphore(ReleaseCount: Integer; out PreviousCount: Integer): Boolean;
begin
Result := Windows.ReleaseSemaphore(fHandle,ReleaseCount,@PreviousCount);
If not Result then
  fLastError := GetLastError;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TSemaphore.ReleaseSemaphore: Boolean;
var
  Dummy:  Integer;
begin
Result := ReleaseSemaphore(1,Dummy);
end;


//==============================================================================
//--  TWaitableTimer implementation  -------------------------------------------
//==============================================================================

//------------------------------------------------------------------------------
//    TWaitableTimer - public methods
//------------------------------------------------------------------------------

constructor TWaitableTimer.Create(SecurityAttributes: PSecurityAttributes; ManualReset: Boolean; const Name: String);
begin
inherited Create;
If SetAndRectifyName(Name) then
  SetAndCheckHandle(CreateWaitableTimer(SecurityAttributes,ManualReset,PChar(StrToWin(fName))))
else
  SetAndCheckHandle(CreateWaitableTimer(SecurityAttributes,ManualReset,nil));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TWaitableTimer.Create(const Name: String);
begin
Create(nil,True,Name);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TWaitableTimer.Create;
begin
Create(nil,True,'');
end;

//------------------------------------------------------------------------------

constructor TWaitableTimer.Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String);
begin
inherited Create;
SetAndRectifyName(Name);
SetAndCheckHandle(OpenWaitableTimer(DesiredAccess,InheritHandle,PChar(StrToWin(fName))));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TWaitableTimer.Open(const Name: String);
begin
Open(SYNCHRONIZE or TIMER_MODIFY_STATE,False,Name);
end;

//------------------------------------------------------------------------------

Function TWaitableTimer.SetWaitableTimer(DueTime: Int64; Period: Integer; CompletionRoutine: TTimerAPCRoutine; ArgToCompletionRoutine: Pointer; Resume: Boolean): Boolean;
begin
Result := Windows.SetWaitableTimer(fHandle,{%H-}DueTime,Period,@CompletionRoutine,ArgToCompletionRoutine,Resume);
If not Result then
  fLastError := GetLastError;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TWaitableTimer.SetWaitableTimer(DueTime: Int64; Period: Integer = 0): Boolean;
begin
Result := SetWaitableTimer(DueTime,Period,nil,nil,False);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TWaitableTimer.SetWaitableTimer(DueTime: TDateTime; Period: Integer; CompletionRoutine: TTimerAPCRoutine; ArgToCompletionRoutine: Pointer; Resume: Boolean): Boolean;

  Function DateTimeToFileTime(DateTime: TDateTime): FileTime;
  var
    LocalTime:  TFileTime;
    SystemTime: TSystemTime;
  begin
    Result.dwLowDateTime := 0;
    Result.dwHighDateTime := 0;
    DateTimeToSystemTime(DateTime,SystemTime);
    If SystemTimeToFileTime(SystemTime,{%H-}LocalTime) then
      begin
        If not LocalFileTimeToFileTime(LocalTime,Result) then
          raise Exception.CreateFmt('LocalFileTimeToFileTime failed with error 0x%.8x.',[GetLastError]);
      end
    else raise Exception.CreateFmt('SystemTimeToFileTime failed with error 0x%.8x.',[GetLastError]);
  end;

begin
Result := SetWaitableTimer(Int64(DateTimeToFileTime(DueTime)),Period,CompletionRoutine,ArgToCompletionRoutine,Resume);
If not Result then
  fLastError := GetLastError;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TWaitableTimer.SetWaitableTimer(DueTime: TDateTime; Period: Integer = 0): Boolean;
begin
Result := SetWaitableTimer(DueTime,Period,nil,nil,False);
end;

//------------------------------------------------------------------------------

Function TWaitableTimer.CancelWaitableTimer: Boolean;
begin
Result := Windows.CancelWaitableTimer(fHandle);
If not Result then
  fLastError := GetLastError;
end;


end.

