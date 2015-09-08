{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{==============================================================================}
{                                                                              }
{   Multicast event handling class                                             }
{                                                                              }
{   ©František Milt 2014-05-07                                                 }
{                                                                              }
{   Version 1.0.2                                                              }
{                                                                              }
{==============================================================================}
unit MulticastEvent;

interface

uses
  Classes;

type
  PMethod = ^TMethod;
  TEvent = procedure of object;

{==============================================================================}
{--- TMulticastEvent declaration ----------------------------------------------}
{==============================================================================}

  TMulticastEvent = class(TObject)
  private
    fOwner:   TObject;
    fMethods: TList;
    Function GetMethods(Index: Integer): TMethod;
    Function GetMethodsCount: Integer;
  public
    constructor Create(aOwner: TObject = nil);
    destructor Destroy; override;
    Function IndexOf(const Handler: TEvent): Integer; virtual;
    Function Add(const Handler: TEvent; AllowDuplicity: Boolean = False): Integer; virtual;
    Function Remove(const Handler: TEvent; RemoveAll: Boolean = True): Integer; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    procedure Call; virtual;
    property Methods[Index: Integer]: TMethod read GetMethods;
  published
    property Owner: TObject read fOwner;
    property Count: Integer read GetMethodsCount;
  end;

{==============================================================================}
{--- TMulticastNotifyEvent declaration ----------------------------------------}
{==============================================================================}

  TMulticastNotifyEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TNotifyEvent): Integer; reintroduce;
    Function Add(const Handler: TNotifyEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TNotifyEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject); reintroduce;
  end;

implementation

{==============================================================================}
{--- TMulticastEvent implementation -------------------------------------------}
{==============================================================================}

{=== TMulticastEvent // Private routines ======================================}

Function TMulticastEvent.GetMethods(Index: Integer): TMethod;
begin
Result := TMethod(fMethods[Index]^);
end;

//------------------------------------------------------------------------------

Function TMulticastEvent.GetMethodsCount: Integer;
begin
Result := fMethods.Count;
end;

{=== TMulticastEvent // Public routines =======================================}

constructor TMulticastEvent.Create(aOwner: TObject = nil);
begin
inherited Create;
fOwner := aOwner;
fMethods := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TMulticastEvent.Destroy;
begin
Clear;
fMethods.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TMulticastEvent.IndexOf(const Handler: TEvent): Integer;
begin
For Result := 0 to Pred(fMethods.Count) do
  If (PMethod(fMethods[Result])^.Code = TMethod(Handler).Code) and
     (PMethod(fMethods[Result])^.Data = TMethod(Handler).Data) then Exit;
Result := -1;
end;

//------------------------------------------------------------------------------

Function TMulticastEvent.Add(const Handler: TEvent; AllowDuplicity: Boolean = False): Integer;
var
  NewItem:  PMethod;
begin
If Assigned(TMethod(Handler).Code) and Assigned(TMethod(Handler).Data) then
  begin
    Result := IndexOf(Handler);
    If (Result < 0) or AllowDuplicity then
      begin
        New(NewItem);
        NewItem^.Code := TMethod(Handler).Code;
        NewItem^.Data := TMethod(Handler).Data;
        Result := fMethods.Add(NewItem);
      end;
  end
else Result := -1;
end;

//------------------------------------------------------------------------------

Function TMulticastEvent.Remove(const Handler: TEvent; RemoveAll: Boolean = True): Integer;
begin
repeat
  Result := IndexOf(Handler);
  If Result >= 0 then Delete(Result);
until not RemoveAll or (Result < 0);
end;

//------------------------------------------------------------------------------

procedure TMulticastEvent.Delete(Index: Integer);
begin
Dispose(PMethod(fMethods[Index]));
fMethods.Delete(Index);
end;

//------------------------------------------------------------------------------

procedure TMulticastEvent.Clear;
var
  i:  Integer;
begin
For i := 0 to Pred(fMethods.Count) do Dispose(PMethod(fMethods[i]));
fMethods.Clear;
end;

//------------------------------------------------------------------------------

procedure TMulticastEvent.Call;
var
  i:  Integer;
begin
For i := 0 to Pred(fMethods.Count) do
  TEvent(fMethods[i]^);
end;



{==============================================================================}
{--- TMulticastNotifyEvent implementation -------------------------------------}
{==============================================================================}

{=== TMulticastNotifyEvent // Public routines =================================}

Function TMulticastNotifyEvent.IndexOf(const Handler: TNotifyEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastNotifyEvent.Add(const Handler: TNotifyEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastNotifyEvent.Remove(const Handler: TNotifyEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastNotifyEvent.Call(Sender: TObject);
var
  i:  Integer;
begin
For i := 0 to Pred(Count) do TNotifyEvent(Methods[i])(Sender);
end;

end.
