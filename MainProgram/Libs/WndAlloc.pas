{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{==============================================================================}
{                                                                              }
{   WndAlloc                                                                   }
{                                                                              }
{   ©František Milt 2015-02-16                                                 }
{                                                                              }
{   Version 1.0                                                                }
{                                                                              }
{==============================================================================}
unit WndAlloc;

interface

uses
  Windows, Classes;

Function AllocateHWND(Method: TWndMethod): HWND;
procedure DeallocateHWND(Wnd: HWND);

implementation

uses
  Messages, SysUtils, SyncObjs;

{$IF not Defined(FPC) and (RTLVersion <= 15) and not Defined(x64)}
Function GetWindowLongPtr(hWnd: HWND; nIndex: LongInt): Pointer;
begin
Result := Pointer(GetWindowLong(hWnd,nIndex));
end;

Function SetWindowLongPtr(hWnd: HWND; nIndex: LongInt; dwNewLong: Pointer): Pointer;
begin
Result := Pointer(SetWindowLong(hWnd,nIndex,LongInt(dwNewLong)));
end;
{$IFEND}

//------------------------------------------------------------------------------

const
  MaxMediators = 512;
  
  UtilityWindowClassName = 'TUtilityWindow';

type
{$IFDEF x64}
  PtrUInt = UInt64;

  TMediator = packed record
    MOV_MethodInfo:   Array[0..1] of Byte;
    MethodInfo:       Pointer;
    MOV_HandlerAddr:  Array[0..1] of Byte;
    HandlerAddr:      Pointer;
    JMP_Reg:          Array[0..2] of Byte;
    _padding:         Byte;
    MethodCode:       Pointer;
    MethodData:       Pointer;
  end;

const
  def_Mediator: TMediator = (
    MOV_MethodInfo:   ($49,$ba);      //  MOV   R10,  qword
    MethodInfo:       nil;
    MOV_HandlerAddr:  ($48,$b8);      //  MOV   RAX,  qword
    HandlerAddr:      nil;
    JMP_Reg:          ($48,$ff,$e0);  //  JMP   RAX
    _padding:         $00;
    MethodCode:       nil;
    MethodData:       nil;
  );
{$ELSE}
  PtrUInt = LongWord;

  TMediator = packed record
    POP_ReturnAddr:   Byte;
    PUSH_MethodInfo:  Byte;
    MethodInfo:       Pointer;
    PUSH_ReturnAddr:  Byte;
    MOV_HandlerAddr:  Byte;
    HandlerAddr:      Pointer;
    JMP_Reg:          Array[0..1] of Byte;
    _padding:         Array[0..1] of  Byte;
    MethodCode:       Pointer;
    MethodData:       Pointer;
   end;

const
  def_Mediator: TMediator = (
    POP_ReturnAddr:   $58;        //  POP   EAX
    PUSH_MethodInfo:  $68;        //  PUSH  dword
    MethodInfo:       nil;
    PUSH_ReturnAddr:  $50;        //  PUSH  EAX
    MOV_HandlerAddr:  $b8;        //  MOV   EAX,  dword
    HandlerAddr:      nil;
    JMP_Reg:          ($ff,$e0);  //  JMP   EAX
    _padding:         ($00,$00);
    MethodCode:       nil;
    MethodData:       nil;
  );
{$ENDIF}

type
  PMediator = ^TMediator;

  TMediators = Array[0..Pred(MaxMediators)] of TMediator;
  PMediators = ^TMediators;

  TUtilityWindowsManager = class(TObject)
  private
    fSynchronizer:  TCriticalSection;
    fMediators:     PMediators;
    fMediatorCount: Integer;
  protected
    Function NewMediator(Method: TMethod): PMediator; virtual;
    procedure RemoveMediator(Mediator: PMediator); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    Function AllocateHWND(Method: TWndMethod): HWND; virtual;
    procedure DeallocateHWND(Wnd: HWND); virtual;
  end;

var
  UtilityWindowManager: TUtilityWindowsManager;


Function AllocateHWND(Method: TWndMethod): HWND;
begin
Result := UtilityWindowManager.AllocateHWND(Method);
end;

//------------------------------------------------------------------------------

procedure DeallocateHWND(Wnd: HWND);
begin
UtilityWindowManager.DeallocateHWND(Wnd);
end;

//==============================================================================

{$IFDEF x64}
Function WndHandler(Window: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM; MethodInfo: Pointer): LRESULT;
{$ELSE}
Function WndHandler(MethodInfo: Pointer; Window: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ENDIF}
var
  WndProc:  TMethod;
  Msg:      TMessage;
begin
WndProc.Data := TMethod(MethodInfo^).Data;
WndProc.Code := TMethod(MethodInfo^).Code;
If Assigned(TWndMethod(WndProc)) then
  begin
    Msg.msg := Message;
    Msg.wParam := wParam;
    Msg.lParam := lParam;
    TWndMethod(WndProc)(Msg);
    Result := Msg.Result
  end
else Result := DefWindowProc(Window,Message,wParam,lParam);
end;

{$IFDEF x64}
{$IFDEF FPC}{$ASMMODE intel}{$ENDIF}
procedure HandlerCaller; assembler;
asm
  SUB   RSP,  $8
  PUSH  R10
  PUSH  R9
  PUSH  R8
  PUSH  RDX
  PUSH  RCX
  CALL  WndHandler
  ADD   RSP,  $30
end;
{$ENDIF}

//==============================================================================

Function TUtilityWindowsManager.NewMediator(Method: TMethod): PMediator;
var
  i:  Integer;
begin
For i := 0 to Pred(MaxMediators) do
  If not Assigned(fMediators^[i].MethodInfo) then
    begin
      fMediators^[i] := def_Mediator;
      fMediators^[i].MethodInfo := Addr(fMediators^[i].MethodCode);
    {$IFDEF x64}
      fMediators^[i].HandlerAddr := @HandlerCaller;
    {$ELSE}
      fMediators^[i].HandlerAddr := @WndHandler;
    {$ENDIF}
      fMediators^[i].MethodCode := Method.Code;
      fMediators^[i].MethodData := Method.Data;
      Result := Addr(fMediators^[i]);
      Break;
    end;
end;

//------------------------------------------------------------------------------

procedure TUtilityWindowsManager.RemoveMediator(Mediator: PMediator);
begin
If ({%H-}PtrUInt(Mediator) >= {%H-}PtrUInt(fMediators)) and
   ({%H-}PtrUInt(Mediator) < ({%H-}PtrUInt(fMediators) + SizeOf(TMediators))) then
  Mediator^.MethodInfo := nil;
end;

//------------------------------------------------------------------------------

constructor TUtilityWindowsManager.Create;
begin
inherited;
fSynchronizer := TCriticalSection.Create;
fMediators := VirtualAlloc(nil,SizeOf(TMediators),MEM_COMMIT,PAGE_EXECUTE_READWRITE);
fMediatorCount := 0;
end;

//------------------------------------------------------------------------------

destructor TUtilityWindowsManager.Destroy;
begin
VirtualFree(fMediators,SizeOf(TMediators),MEM_RELEASE);
fSynchronizer.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TUtilityWindowsManager.AllocateHWND(Method: TWndMethod): HWND;
var
  Registered:         Boolean;
  TempClass:          TWndClass;
  UtilityWindowClass: TWndClass;
begin
Result := 0;
fSynchronizer.Enter;
try
  If fMediatorCount < MaxMediators then
    begin
      ZeroMemory(@UtilityWindowClass,SizeOf(TWndClass));
      Registered := Windows.GetClassInfo(hInstance,UtilityWindowClassName,{%H-}TempClass);
      If not Registered or (TempClass.lpfnWndProc <> @DefWindowProc) then
        begin
          If Registered then Windows.UnregisterClass(UtilityWindowClassName,hInstance);
          UtilityWindowClass.lpszClassName := UtilityWindowClassName;
          UtilityWindowClass.hInstance := hInstance;
          UtilityWindowClass.lpfnWndProc := @DefWindowProc;
          If Windows.RegisterClass(UtilityWindowClass) = 0 then
            raise Exception.CreateFmt('TUtilityWindowsManager.AllocateHWND: Unable to register hidden window class. %s',[SysErrorMessage(GetLastError)]);
        end;
      Result := CreateWindowEx(WS_EX_TOOLWINDOW,UtilityWindowClassName,'',WS_POPUP,0,0,0,0,0,0,hInstance,nil);
      If Result = 0 then
        raise Exception.CreateFmt('TUtilityWindowsManager.AllocateHWND: Unable to create hidden window. %s',[SysErrorMessage(GetLastError)]);
    {$IFDEF FPC}
      SetWindowLongPtr(Result,GWL_WNDPROC,{%H-}LONG_PTR(NewMediator(TMethod(Method))));
    {$ELSE}
      SetWindowLongPtr(Result,GWL_WNDPROC,NewMediator(TMethod(Method)));
    {$ENDIF}
      Inc(fMediatorCount);
    end
  else raise Exception.Create('TUtilityWindowsManager.AllocateHWND: Unable to create new mediator.');
finally
  fSynchronizer.Leave;
end;
end;

//------------------------------------------------------------------------------

procedure TUtilityWindowsManager.DeallocateHWND(Wnd: HWND);
var
  Mediator: PMediator;
begin
fSynchronizer.Enter;
try
{$IFDEF FPC}
  Mediator := {%H-}PMediator(GetWindowLongPtr(Wnd,GWL_WNDPROC));
{$ELSE}
  Mediator := GetWindowLongPtr(Wnd,GWL_WNDPROC);
{$ENDIF}
  DestroyWindow(Wnd);
  RemoveMediator(Mediator);    
  Dec(fMediatorCount);
  If fMediatorCount <= 0 then
    Windows.UnregisterClass(UtilityWindowClassName,hInstance);
finally
  fSynchronizer.Leave;
end;
end;

//==============================================================================

initialization
  UtilityWindowManager := TUtilityWindowsManager.Create;

finalization
  UtilityWindowManager.Free;

end.

