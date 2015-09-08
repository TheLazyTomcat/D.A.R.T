{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{==============================================================================}
{                                                                              }
{   Utility Window                                                             }
{                                                                              }
{   ©František Milt 2015-02-20                                                 }
{                                                                              }
{   Version 1.2                                                                }
{                                                                              }
{==============================================================================}
unit UtilityWindow;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  Windows, Messages, MulticastEvent;

type
  TMessageEvent = procedure(var Msg: TMessage; var Handled: Boolean) of object;

{==============================================================================}
{--- TMulticastMessageEvent declarationn --------------------------------------}
{==============================================================================}

  TMulticastMessageEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TMessageEvent): Integer; reintroduce;
    Function Add(Handler: TMessageEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TMessageEvent): Integer; reintroduce;
    procedure Call(var Msg: TMessage; var Handled: Boolean); reintroduce;
  end;

{==============================================================================}
{--- TUtilityWindow declarationn ----------------------------------------------}
{==============================================================================}

  TUtilityWindow = class(TObject)
  private
    fWindowHandle:  HWND;
    fOnMessage:     TMulticastMessageEvent;
  protected
    procedure WndProc(var Msg: TMessage); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessMessages(Synchronous: Boolean = False); virtual;
  published
    property WindowHandle: HWND read fWindowHandle;
    property OnMessage: TMulticastMessageEvent read fOnMessage;
  end;

implementation

uses
  SysUtils, Classes, WndAlloc;

{==============================================================================}
{--- TMulticastMessageEvent implementation ------------------------------------}
{==============================================================================}

{=== TMulticastMessageEvent // public methods =================================}

Function TMulticastMessageEvent.IndexOf(const Handler: TMessageEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastMessageEvent.Add(Handler: TMessageEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastMessageEvent.Remove(const Handler: TMessageEvent): Integer;
begin
Result := inherited Remove(TEvent(Handler));
end;

//------------------------------------------------------------------------------

procedure TMulticastMessageEvent.Call(var Msg: TMessage; var Handled: Boolean);
var
  i:          Integer;
  Processed:  Boolean;
begin
Processed := False;
For i := 0 to Pred(Count) do
  begin
    TMessageEvent(Methods[i])(Msg,Processed);
    If Processed then Handled := True;
  end;
end;

{==============================================================================}
{--- TUtilityWindow implementation --------------------------------------------}
{==============================================================================}

{=== TUtilityWindow // protected methods ======================================}

procedure TUtilityWindow.WndProc(var Msg: TMessage);
var
  Handled:  Boolean;
begin
Handled := False;
fOnMessage.Call(Msg,Handled);
If not Handled then
  Msg.Result := DefWindowProc(fWindowHandle,Msg.Msg,Msg.wParam,Msg.lParam);
end;

{=== TUtilityWindow // public methods =========================================}

constructor TUtilityWindow.Create;
begin
inherited;
fOnMessage := TMulticastMessageEvent.Create(Self);
fWindowHandle := WndAlloc.AllocateHWND(WndProc);
end;

//------------------------------------------------------------------------------

destructor TUtilityWindow.Destroy;
begin
fOnMessage.Clear;
WndAlloc.DeallocateHWND(fWindowHandle);
fOnMessage.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TUtilityWindow.ProcessMessages(Synchronous: Boolean = False);
var
  Msg:  TagMSG;
begin
If Synchronous then
  begin
    while Integer(GetMessage({%H-}Msg,fWindowHandle,0,0)) <> 0 do
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
  end
else
  begin
    while Integer(PeekMessage(Msg,fWindowHandle,0,0,PM_REMOVE)) <> 0 do
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
  end;
end;

end.
