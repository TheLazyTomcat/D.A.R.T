{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SyncThread

    Very simple library providing TThread descendant that can synchronize its
    method with any other thread, not only main thread.

  ©František Milt 2018-04-05

  Version 1.0

  To propely use this library, you must do following:
  
    - create an instance of TSyncThreadSynchronizer in the thread you want to
      synchronize with
    - using the synchronizer, create a dispatcher (do NOT create it directly)
      and pass it to the thread that have to be synchronized (must be
      a descendant of TSyncThread class)
    - in the thread that have to be synchronized, call method Synchronize that
      accepts a dispatcher and pass dispatcher object created in the second step
      along with thread method you want to be synchronized
    - in the thread you are synchronizing with, you must call method Update of
      the synchronizer as often as possible - calling this method will do the
      synchronization itself
    - when you are done, free the dispatcher (you can do it in any thread)
      and synchronizer (do it in the same thread where you created it)

  NOTE - Method TSyncThreadSynchronizer.Update is synchronous. Meaning it will
         wait for any synchronization event and only when one occurs or the
         timeout runs out it will return.
         
  WARNING - Synchronizing a thread with itself will cause a deadlock!

  Dependencies:
    Messanger   - github.com/ncs-sniper/Lib.Messanger
    AuxTypes    - github.com/ncs-sniper/Lib.AuxTypes
    MemVector   - github.com/ncs-sniper/Lib.MemVector
    WinSyncObjs - github.com/ncs-sniper/Lib.WinSyncObjs
    StrRect     - github.com/ncs-sniper/Lib.StrRect

===============================================================================}
unit SyncThread;

{$IFDEF FPC}
  {$MODE ObjFPC}{$H+}
  {$MODESWITCH CLASSICPROCVARS+}
{$ENDIF}

interface

uses
  Classes,
  Messanger;

{===============================================================================
--------------------------------------------------------------------------------
                              TSyncThreadDispatcher
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSyncThreadDispatcher - class declaration
===============================================================================}

type
  TSyncThreadDispatcher = class(TObject)
  private
    fEndpoint:  TMessangerEndpoint;
  public
    constructor Create(Endpoint: TMessangerEndpoint);
    destructor Destroy; override;
    procedure Synchronize(Method: TThreadMethod);
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              TSyncThreadSynchronizer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSyncThreadSynchronizer - class declaration
===============================================================================}

  TSyncThreadSynchronizer = class(TMessanger)
  private
    fEndpoint:  TMessangerEndpoint;
  protected
    procedure EndpointMessageHandler(Sender: TObject; Msg: TMsgrMessage; var Flags: TMsgrDispatchFlags); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    Function CreateDispatcher: TSyncThreadDispatcher; virtual;
    procedure Update(Timeout: LongWord = 0); virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TSyncThread
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSyncThread - class declaration
===============================================================================}

  TSyncThread = class(TThread)
  protected
    procedure Synchronize(Method: TThreadMethod; Dispatcher: TSyncThreadDispatcher); overload; virtual;
  end;

implementation

uses
  SysUtils;

{===============================================================================
    Private constants
===============================================================================}

const
  ST_SYNC_MESSAGE_ID = $51235707; // just a random number

  ST_SYNC_ENDPOINT_ID = 0;

{===============================================================================
--------------------------------------------------------------------------------
                              TSyncThreadDispatcher
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSyncThreadDispatcher - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSyncThreadDispatcher - public methods
-------------------------------------------------------------------------------}

constructor TSyncThreadDispatcher.Create(Endpoint: TMessangerEndpoint);
begin
inherited Create;
fEndpoint := Endpoint;
end;

//------------------------------------------------------------------------------

destructor TSyncThreadDispatcher.Destroy;
begin
fEndpoint.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSyncThreadDispatcher.Synchronize(Method: TThreadMethod);
var
  SyncException:  Exception;
begin
{
  clean-up any received messages - the dispatcher is not supposed to receive
  any message, so there is no point in processing them
}
fEndpoint.TraverseMessages;
SyncException := nil;
// following line will wait for the sent message to be processed
fEndpoint.SendMessageAndWait(ST_SYNC_ENDPOINT_ID,ST_SYNC_MESSAGE_ID,
                             {%H-}TMSGRParam(TMethod(Method).Code),
                             {%H-}TMSGRParam(TMethod(Method).Data),
                             {%H-}TMSGRParam(@SyncException));
// check if exception occurred, and if so, raise it
If Assigned(SyncException) then
  raise SyncException;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              TSyncThreadSynchronizer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSyncThreadSynchronizer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSyncThreadSynchronizer - protected methods
-------------------------------------------------------------------------------}

procedure TSyncThreadSynchronizer.EndpointMessageHandler(Sender: TObject; Msg: TMsgrMessage; var Flags: TMsgrDispatchFlags);
var
  Method: TMethod;
begin
If (Msg.Parameter1 = ST_SYNC_MESSAGE_ID) and (mdfSynchronousMessage in Flags) then
  begin
    // get code and data of the synchronized method from message params
    Method.Code := {%H-}Pointer(Msg.Parameter2);
    Method.Data := {%H-}Pointer(Msg.Parameter3);
    // execute the method in current context
    try
      TThreadMethod(Method);
    except
      {%H-}PPointer(Msg.Parameter4)^ := AcquireExceptionObject;
    end;
  end;
end;

{-------------------------------------------------------------------------------
    TSyncThreadSynchronizer - public methods
-------------------------------------------------------------------------------}

constructor TSyncThreadSynchronizer.Create;
begin
inherited Create;
fEndpoint := CreateEndpoint(ST_SYNC_ENDPOINT_ID);
fEndpoint.OnMessageTraversing := EndpointMessageHandler;
end;

//------------------------------------------------------------------------------

destructor TSyncThreadSynchronizer.Destroy;
begin
fEndpoint.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TSyncThreadSynchronizer.CreateDispatcher: TSyncThreadDispatcher;
begin
Result := TSyncThreadDispatcher.Create(CreateEndpoint);
end;

//------------------------------------------------------------------------------

procedure TSyncThreadSynchronizer.Update(Timeout: LongWord = 0);
begin
fEndpoint.Cycle(Timeout)
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TSyncThread
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSyncThread - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSyncThread - protected methods
-------------------------------------------------------------------------------}

procedure TSyncThread.Synchronize(Method: TThreadMethod; Dispatcher: TSyncThreadDispatcher);
begin
Dispatcher.Synchronize(Method);
end;

end.
