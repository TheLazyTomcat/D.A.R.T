{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Messanger

  Small library for thread-safe intraprocess communication.

  ©František Milt 2017-03-24

  Version 1.2.1

  Notes:
    - do not create instance of class TMessangerEndpoint directly by calling
      its constructor, instead use method(s) TMessanger.CreateEndpoint
    - manage creation of all endpoints in one thread (a thread that is managing
      TMessanger instance) and then pass them to threads that needs them, do
      not create endpoints from other threads
    - on the other hand, free endpoints from threads that are using them, never
      free them from thread that is managing TMessanger instance
    - before freeing TMessanger instance, make sure all endpoints are freed from
      their respective threads, otherwise an exception will be raised when
      TMessanger instance is freed
    - use synchronous messages only when really necessary, bulk of the
      communication should be asynchronous
    - do not send synchronous messages from event handlers - it is possible as
      long as synchronous dispatch is not active (in that case, send function
      will fail), but discouraged

  Dependencies:
    AuxTypes    - github.com/ncs-sniper/Lib.AuxTypes
    MemVector   - github.com/ncs-sniper/Lib.MemVector
    WinSyncObjs - github.com/ncs-sniper/Lib.WinSyncObjs
    StrRect     - github.com/ncs-sniper/Lib.StrRect

===============================================================================}
unit Messanger;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$TYPEINFO ON}

interface

uses
  Windows, SysUtils, Classes, AuxTypes, MemVector, WinSyncObjs;

type
  TMsgrEndpointID = UInt16;       PMsgrEndpointID = ^TMsgrEndpointID;
  TMsgrPriority   = Int32;        PMsgrPriority   = ^TMsgrPriority;
  TMsgrTimeStamp  = Int64;        PMsgrTimeStamp  = ^TMsgrTimeStamp;
  TMsgrParam      = PtrInt;       PMsgrParam      = ^TMsgrParam;
  TMsgrOpaque     = Pointer;      PMsgrOpaque     = ^TMsgrOpaque;

  TMsgrMessage = packed record
    Sender:     TMsgrEndpointID;
    Target:     TMsgrEndpointID;
    Priority:   TMsgrPriority;
    TimeStamp:  TMsgrTimeStamp;
    Parameter1: TMsgrParam;
    Parameter2: TMsgrParam;
    Parameter3: TMsgrParam;
    Parameter4: TMsgrParam;
    Reserved:   TMsgrOpaque;
  end;
  PMsgrMessage = ^TMsgrMessage;

const
  MSGR_ID_BROADCAST = TMsgrEndpointID(High(TMsgrEndpointID));

  MSGR_PRIORITY_MINIMAL       = TMsgrPriority(-100000);
  MSGR_PRIORITY_EXTREME_LOW   = TMsgrPriority(-10000);
  MSGR_PRIORITY_VERY_LOW      = TMsgrPriority(-1000);
  MSGR_PRIORITY_LOW           = TMsgrPriority(-100);
  MSGR_PRIORITY_BELOW_NORMAL  = TMsgrPriority(-10);
  MSGR_PRIORITY_NORMAL        = TMsgrPriority(0);
  MSGR_PRIORITY_ABOVE_NORMAL  = TMsgrPriority(10);
  MSGR_PRIORITY_HIGH          = TMsgrPriority(100);
  MSGR_PRIORITY_VERY_HIGH     = TMsgrPriority(1000);
  MSGR_PRIORITY_EXTREME_HIGH  = TMsgrPriority(10000);
  MSGR_PRIORITY_ABSOLUTE      = TMsgrPriority(100000);

  MSGR_PRIORITY_MIN = MSGR_PRIORITY_MINIMAL;
  MSGR_PRIORITY_MAX = MSGR_PRIORITY_ABSOLUTE;

{==============================================================================}
{------------------------------------------------------------------------------}
{                              TMsgrMessageVector                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TMsgrMessageVector - declaration                                           }
{==============================================================================}

type
  TMsgrMessageVector = class(TMemVector)
  protected
    Function GetItem(Index: Integer): TMsgrMessage; virtual;
    procedure SetItem(Index: Integer; Value: TMsgrMessage); virtual;
    Function ItemCompare(Item1,Item2: Pointer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(Memory: Pointer; Count: Integer); overload;
    Function First: TMsgrMessage; reintroduce;
    Function Last: TMsgrMessage; reintroduce;
    Function IndexOf(Item: TMsgrMessage): Integer; reintroduce;
    Function Add(Item: TMsgrMessage): Integer; reintroduce;
    procedure Insert(Index: Integer; Item: TMsgrMessage); reintroduce;
    Function Remove(Item: TMsgrMessage): Integer; reintroduce;
    Function Extract(Item: TMsgrMessage): TMsgrMessage; reintroduce;
    property Items[Index: Integer]: TMsgrMessage read GetItem write SetItem; default;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                          TMsgrBufferedMessagesVector                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TMsgrBufferedMessagesVector - declaration                                  }
{==============================================================================}

  TMsgrBufferedMessageVector = class(TMsgrMessageVector)
  protected
    Function ItemCompare(Item1,Item2: Pointer): Integer; override;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                              TMessangerEndpoint                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TMessangerEndpoint - declaration                                           }
{==============================================================================}

  TMsgrDispatchFlag = (mdfRemoveMessage,mdfStopTraversing,mdfAutoCycle,
                       mdfUndeliveredMessage,mdfBufferedMessage,
                       mdfSynchronousMessage,mdfSynchronousDispatch);

  TMsgrDispatchFlags = set of TMsgrDispatchFlag;

  TMsgrWaitResult = (mwrNewMessage,mwrTimeOut,mwrError);

  TMsgrSyncMsgsWaiters = packed record
    Outgoing: THandle;
    Incoming: THandle;
  end;

  TMsgrMessageEvent = procedure(Sender: TObject; Msg: TMsgrMessage; var Flags: TMsgrDispatchFlags) of object;

  TMessanger = class; // forward declaration

  TMessangerEndpoint = class(TOBject)
  private
    fEndpointID:              TMsgrEndpointID;
    fMessanger:               TMessanger;
    fAutoBuffSend:            Boolean;
    fAutoCycle:               Boolean;
    fSynchronousDispatch:     Boolean;
    // synchronizers
    fIncomingSynchronizer:    TCriticalSection;     // protects vector of incoming messages
    fIncomingMsgsWaiter:      TEvent;               // used for waiting for new incoming messages
    fIncomingSyncMsgsWaiter:  TEvent;               // indicates incoming synchronous message
    fOutgoingSyncMsgsWaiter:  TEvent;               // used for waiting on sent synchronous messages
    fSyncMsgsWaiters:         TMsgrSyncMsgsWaiters; // for use in WaitForMultipleObjects
    // vectors
    fIncomingMessages:        TMsgrMessageVector;
    fReceivedMessages:        TMsgrMessageVector;
    fBufferedMessages:        TMsgrBufferedMessageVector;
    fUndeliveredMessages:     TMsgrMessageVector;   // used to store undelivered buffered messages
    // events
    fOnMessageTraversing:     TMsgrMessageEvent;
    fOnUndeliveredMessage:    TMsgrMessageEvent;
    fOnDestroying:            TNotifyEvent;
    Function GetMessageCount: Integer;
    Function GetMessage(Index: Integer): TMsgrMessage;
  protected
    procedure AddMessages(Messages: PMsgrMessage; Count: Integer); virtual; // called from other thread (sender)
    procedure SynchronousDispatch; virtual;
    procedure UndeliveredDispatch(Msg: TMsgrMessage; BufferedMessage: Boolean = False); virtual;
  public
    constructor Create(EndpointID: TMsgrEndpointID; Messanger: TMessanger);
    destructor Destroy; override;
    Function SendMessage(TargetID: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL): Boolean; overload; virtual;
    Function SendMessage(Msg: TMsgrMessage): Boolean; overload; virtual;
    Function SendMessageAndWait(TargetID: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL): Boolean; overload; virtual;
    Function SendMessageAndWait(Msg: TMsgrMessage): Boolean; overload; virtual;
    procedure BufferMessage(TargetID: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL); overload; virtual;
    procedure BufferMessage(Msg: TMsgrMessage); overload; virtual;
    procedure SendBufferedMessages; virtual;
    Function WaitForNewMessage(TimeOut: DWORD): TMsgrWaitResult; virtual;
    procedure FetchMessages; virtual;
    Function TraverseMessages: Boolean; virtual;
    procedure Cycle(MessageWaitTimeOut: DWORD); virtual;
    procedure AutoCycle(MessageWaitTimeOut: DWORD); virtual;
    procedure ClearMessages; virtual;
    property Messages[Index: Integer]: TMsgrMessage read GetMessage;
  published
    property EndpointID: TMsgrEndpointID read fEndpointID;
    property AutoBuffSend: Boolean read fAutoBuffSend write fAutoBuffSend;
    property MessageCount: Integer read GetMessageCount;
    property OnMessageTraversing: TMsgrMessageEvent read fOnMessageTraversing write fOnMessageTraversing;
    property OnUndeliveredMessage: TMsgrMessageEvent read fOnUndeliveredMessage write fOnUndeliveredMessage;
    property OnDestroying: TNotifyEvent read fOnDestroying write fOnDestroying;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                                  TMessanger                                  }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TMessanger - declaration                                                   }
{==============================================================================}

  TMsgrEndpoints = array of TMessangerEndpoint;

  TMessanger = class(TObject)
  private
    fEndpoints:     TMsgrEndpoints;
    fSynchronizer:  TMultiReadExclusiveWriteSynchronizer;
    Function GetEndpointCapacity: Integer;
    Function GetEndpointCount: Integer;
    Function GetEndpoint(Index: Integer): TMessangerEndpoint;
  protected
    procedure RemoveEndpoint(EndpointID: TMsgrEndpointID); virtual;
    Function SendMessage(Msg: TMsgrMessage): Boolean; virtual;
    procedure SendBufferedMessages(Messages,Undelivered: TMsgrMessageVector); virtual;
  public
    constructor Create(EndpointCapacity: TMsgrEndpointID = 1024);
    destructor Destroy; override;
    Function IDAvailable(EndpointID: TMsgrEndpointID): Boolean; virtual;
    Function CreateEndpoint: TMessangerEndpoint; overload; virtual;
    Function CreateEndpoint(EndpointID: TMsgrEndpointID): TMessangerEndpoint; overload; virtual;
    property Endpoints[Index: Integer]: TMessangerEndpoint read GetEndpoint;
  published
    property EndpointCapacity: Integer read GetEndpointCapacity;
    property EndpointCount: Integer read GetEndpointCount;
  end;

{==============================================================================}
{   Auxiliary functions - declaration                                          }
{==============================================================================}

Function GetTimeStamp: TMsgrTimeStamp;
Function BuildMessage(Sender, Target: TMsgrEndpointID; Priority: TMsgrPriority; TimeStamp: TMsgrTimeStamp; P1,P2,P3,P4: TMsgrParam): TMsgrMessage;

implementation

{==============================================================================}
{   Auxiliary functions - implementation                                       }
{==============================================================================}

Function GetTimeStamp: TMsgrTimeStamp;
begin
Result := 0;
If not QueryPerformanceCounter(Result) then
  raise Exception.CreateFmt('GetTimeStamp: Cannot obtain time stamp (0x%.8x).',[GetLastError]);
end;

//------------------------------------------------------------------------------

Function BuildMessage(Sender, Target: TMsgrEndpointID; Priority: TMsgrPriority; TimeStamp: TMsgrTimeStamp; P1,P2,P3,P4: TMsgrParam): TMsgrMessage;
begin
Result.Sender := Sender;
Result.Target := Target;
Result.Priority := Priority;
Result.TimeStamp := TimeStamp;
Result.Parameter1 := P1;
Result.Parameter2 := P2;
Result.Parameter3 := P3;
Result.Parameter4 := P4;
Result.Reserved := nil;
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                              TMsgrMessageVector                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TMsgrMessageVector - implementation                                        }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMsgrMessageVector - protected methods                                     }
{------------------------------------------------------------------------------}

Function TMsgrMessageVector.GetItem(Index: Integer): TMsgrMessage;
begin
Result := TMsgrMessage(GetItemPtr(Index)^);
end;

//------------------------------------------------------------------------------

procedure TMsgrMessageVector.SetItem(Index: Integer; Value: TMsgrMessage);
begin
SetItemPtr(Index,@Value);
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.ItemCompare(Item1,Item2: Pointer): Integer;
var
  AssignMask: Byte;
begin
If Assigned(TMsgrMessage(Item1^).Reserved) then
  AssignMask := 1
else
  AssignMask := 0;
If Assigned(TMsgrMessage(Item2^).Reserved) then
  AssignMask := AssignMask or 2;
case AssignMask of
  1:  Result := -1; // first is assigned, second is not => change order
  2:  Result := +1; // first in not assigned, second is => keep order
else
 {0,3} // both are assigned or not assigned, decide upon other parameters
  Result := TMsgrMessage(Item2^).Priority - TMsgrMessage(Item1^).Priority;
  If TMsgrMessage(Item2^).TimeStamp < TMsgrMessage(Item1^).TimeStamp then
    Inc(Result)
  else
    Dec(Result);
end;
end;

{------------------------------------------------------------------------------}
{   TMsgrMessageVector - public methods                                        }
{------------------------------------------------------------------------------}

constructor TMsgrMessageVector.Create;
begin
inherited Create(SizeOf(TMsgrMessage));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TMsgrMessageVector.Create(Memory: Pointer; Count: Integer);
begin
inherited Create(Memory,Count,SizeOf(TMsgrMessage));
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.First: TMsgrMessage;
begin
Result := TMsgrMessage(inherited First^);
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.Last: TMsgrMessage;
begin
Result := TMsgrMessage(inherited Last^);
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.IndexOf(Item: TMsgrMessage): Integer;
begin
Result := inherited IndexOf(@Item);
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.Add(Item: TMsgrMessage): Integer;
begin
Result := inherited Add(@Item);
end;

//------------------------------------------------------------------------------

procedure TMsgrMessageVector.Insert(Index: Integer; Item: TMsgrMessage);
begin
inherited Insert(Index,@Item);
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.Remove(Item: TMsgrMessage): Integer;
begin
Result := inherited Remove(@Item);
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.Extract(Item: TMsgrMessage): TMsgrMessage;
begin
Result := TMsgrMessage(inherited Extract(@Item)^);
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                          TMsgrBufferedMessagesVector                         }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TMsgrBufferedMessagesVector - implementation                               }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMsgrBufferedMessagesVector - protected methods                            }
{------------------------------------------------------------------------------}

Function TMsgrBufferedMessageVector.ItemCompare(Item1,Item2: Pointer): Integer;
begin
Result := TMsgrMessage(Item2^).Target - TMsgrMessage(Item1^).Target;
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                              TMessangerEndpoint                              }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TMessangerEndpoint - implementation                                        }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMessangerEndpoint - pivate methods                                        }
{------------------------------------------------------------------------------}

Function TMessangerEndpoint.GetMessageCount: Integer;
begin
Result := fReceivedMessages.Count;
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.GetMessage(Index: Integer): TMsgrMessage;
begin
If (Index >= fReceivedMessages.LowIndex) and (Index <= fReceivedMessages.HighIndex) then
  Result := fReceivedMessages[Index]
else
  raise Exception.CreateFmt('TMessangerEndpoint.GetMessage: Index (%d) out of bounds.',[Index]);
end;

{------------------------------------------------------------------------------}
{   TMessangerEndpoint - protected methods                                     }
{------------------------------------------------------------------------------}

procedure TMessangerEndpoint.AddMessages(Messages: PMsgrMessage; Count: Integer);
begin
fIncomingSynchronizer.Enter;
try
  fIncomingMessages.Append(Messages,Count);
  fIncomingMsgsWaiter.SetEvent;
  If (Count = 1) and Assigned(Messages^.Reserved) then
    fIncomingSyncMsgsWaiter.SetEvent;
finally
  fIncomingSynchronizer.Leave;
end;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.SynchronousDispatch;
var
  i:      Integer;
  Flags:  TMsgrDispatchFlags;
begin
fSynchronousDispatch := True;
try
  FetchMessages;
  For i := fReceivedMessages.HighIndex downto fReceivedMessages.LowIndex do
    If Assigned(fReceivedMessages[i].Reserved) then
      begin
        // prepare flags
        Flags := [mdfSynchronousMessage,mdfSynchronousDispatch];
        If fAutoCycle then Include(Flags,mdfAutoCycle);
        // call event
        If Assigned(fOnMessageTraversing) then
          fOnMessageTraversing(Self,fReceivedMessages[i],Flags);  // <<<
        // process output flags
        fAutoCycle := fAutoCycle and (mdfAutoCycle in Flags);
        TEvent(fReceivedMessages[i].Reserved).SetEvent;
        fReceivedMessages.Delete(i);
      end;
finally
  fSynchronousDispatch := False;
end;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.UndeliveredDispatch(Msg: TMsgrMessage; BufferedMessage: Boolean = False);
var
  Flags:  TMsgrDispatchFlags;
begin
If Assigned(fOnUndeliveredMessage) then
  begin
    // prepare flags
    Flags := [mdfUndeliveredMessage];
    If Assigned(Msg.Reserved) then Include(Flags,mdfSynchronousMessage);
    If BufferedMessage then Include(Flags,mdfBufferedMessage);
    If fAutoCycle then Include(Flags,mdfAutoCycle);
    // call event
    fOnUndeliveredMessage(Self,Msg,Flags);
    // process output flags
    fAutoCycle := fAutoCycle and (mdfAutoCycle in Flags);
  end;
end;

{------------------------------------------------------------------------------}
{   TMessangerEndpoint - public methods                                        }
{------------------------------------------------------------------------------}

constructor TMessangerEndpoint.Create(EndpointID: TMsgrEndpointID; Messanger: TMessanger);
begin
inherited Create;
fEndpointID := EndpointID;
fMessanger := Messanger;
fAutoBuffSend := False;
fAutoCycle := False;
fSynchronousDispatch := False;
// synchronizers
fIncomingSynchronizer := TCriticalSection.Create;
fIncomingMsgsWaiter := TEvent.Create(nil,False,False,'');
fIncomingSyncMsgsWaiter := TEvent.Create(nil,False,False,'');
fOutgoingSyncMsgsWaiter := TEvent.Create(nil,True,False,'');
fSyncMsgsWaiters.Outgoing := fOutgoingSyncMsgsWaiter.Handle;
fSyncMsgsWaiters.Incoming := fIncomingSyncMsgsWaiter.Handle;
// vectors
fIncomingMessages := TMsgrMessageVector.Create;
fReceivedMessages := TMsgrMessageVector.Create;
fBufferedMessages := TMsgrBufferedMessageVector.Create;
fUndeliveredMessages := TMsgrMessageVector.Create;
end;

//------------------------------------------------------------------------------

destructor TMessangerEndpoint.Destroy;
begin
fMessanger.RemoveEndpoint(fEndpointID);
If Assigned(fOnDestroying) then
  fOnDestroying(Self);
FetchMessages;
ClearMessages;
// vectors
fUndeliveredMessages.Free;
fBufferedMessages.Free;
fReceivedMessages.Free;
fIncomingMessages.Free;
// synchronizers
fOutgoingSyncMsgsWaiter.Free;
fIncomingSyncMsgsWaiter.Free;
fIncomingMsgsWaiter.Free;
fIncomingSynchronizer.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.SendMessage(TargetID: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL): Boolean;
begin
Result := SendMessage(BuildMessage(fEndpointID,TargetID,Priority,GetTimeStamp,P1,P2,P3,P4));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TMessangerEndpoint.SendMessage(Msg: TMsgrMessage): Boolean;
begin
If fAutoBuffSend then
  SendBufferedMessages;
Msg.Reserved := nil;
Result := fMessanger.SendMessage(Msg);
If not Result then
  UndeliveredDispatch(Msg);
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.SendMessageAndWait(TargetID: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL): Boolean;
begin
Result := SendMessageAndWait(BuildMessage(fEndpointID,TargetID,Priority,GetTimeStamp,P1,P2,P3,P4));
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.SendMessageAndWait(Msg: TMsgrMessage): Boolean;
var
  ContinueWaiting:  Boolean;
begin
Result := False;
If fAutoBuffSend then
  SendBufferedMessages;
If not fSynchronousDispatch and (Msg.Target <> MSGR_ID_BROADCAST) then
  begin
    Msg.Reserved := Pointer(fOutgoingSyncMsgsWaiter);
    fOutgoingSyncMsgsWaiter.ResetEvent;
    ContinueWaiting := True;
    If fMessanger.SendMessage(Msg) then
      while ContinueWaiting do
        case WaitForMultipleObjects(2,Addr(fSyncMsgsWaiters),False,INFINITE) of
              WAIT_OBJECT_0:  begin
                                Result := True;
                                ContinueWaiting := False;
                              end;
          WAIT_OBJECT_0 + 1:  SynchronousDispatch;
        else
          ContinueWaiting := False;
        end
  end;
If not Result then
  UndeliveredDispatch(Msg);
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.BufferMessage(TargetID: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL);
begin
BufferMessage(BuildMessage(fEndpointID,TargetID,Priority,GetTimeStamp,P1,P2,P3,P4));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TMessangerEndpoint.BufferMessage(Msg: TMsgrMessage);
begin
Msg.Reserved := nil;
fBufferedMessages.Add(Msg);
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.SendBufferedMessages;
var
  i:  Integer;
begin
If fBufferedMessages.Count > 0 then
  begin
    fBufferedMessages.Sort;
    fMessanger.SendBufferedMessages(fBufferedMessages,fUndeliveredMessages);
    fBufferedMessages.Clear;
  end;
For i := fUndeliveredMessages.LowIndex to fUndeliveredMessages.HighIndex do
  UndeliveredDispatch(fUndeliveredMessages[i],True);
fUndeliveredMessages.Clear;
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.WaitForNewMessage(TimeOut: DWORD): TMsgrWaitResult;
begin
case fIncomingMsgsWaiter.WaitFor(TimeOut) of
  wrTimeOut:  Result := mwrTimeOut;
  wrSignaled: Result := mwrNewMessage;
else
  Result := mwrError;
end;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.FetchMessages;
begin
fIncomingSynchronizer.Enter;
try
  fReceivedMessages.Append(fIncomingMessages);
  fIncomingMessages.Clear;
finally
  fIncomingSynchronizer.Leave;
end;
fReceivedMessages.Sort;
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.TraverseMessages: Boolean;
var
  i:      Integer;
  Flags:  TMsgrDispatchFlags;
begin
Result := True;
If Assigned(fOnMessageTraversing) then
  begin
    For i := fReceivedMessages.HighIndex downto fReceivedMessages.LowIndex do
      begin
        // prepare flags
        Flags := [mdfRemoveMessage];
        If Assigned(fReceivedMessages[i].Reserved) then Include(Flags,mdfSynchronousMessage);
        If fAutoCycle then Include(Flags,mdfAutoCycle);
        // call event
        fOnMessageTraversing(Self,fReceivedMessages[i],Flags);  // <<<
        // process output flags
        fAutoCycle := fAutoCycle and (mdfAutoCycle in Flags);
        If mdfRemoveMessage in Flags then
          begin
            If Assigned(fReceivedMessages[i].Reserved) then
              TEvent(fReceivedMessages[i].Reserved).SetEvent;
            fReceivedMessages.Delete(i);
          end;
        If mdfStopTraversing in Flags then
          begin
            Result := False;
            Break{i};
          end;
      end;
  end
else ClearMessages;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.Cycle(MessageWaitTimeOut: DWORD);
begin
If WaitForNewMessage(MessageWaitTimeOut) = mwrNewMessage then
  begin
    FetchMessages;
    TraverseMessages;
  end;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.AutoCycle(MessageWaitTimeOut: DWORD);
begin
If not fAutoCycle then
  begin
    fAutoCycle := True;
    while fAutoCycle do
      Cycle(MessageWaitTimeOut);
  end;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.ClearMessages;
var
  i:  Integer;
begin
For i := fReceivedMessages.LowIndex to fReceivedMessages.HighIndex do
  If Assigned(fReceivedMessages[i].Reserved) then
    TEvent(fReceivedMessages[i].Reserved).SetEvent;
fReceivedMessages.Clear;
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                                  TMessanger                                  }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TMessanger - implementation                                                }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TMessanger - private methods                                               }
{------------------------------------------------------------------------------}

Function TMessanger.GetEndpointCapacity: Integer;
begin
Result := Length(fEndpoints);
end;

//------------------------------------------------------------------------------

Function TMessanger.GetEndpointCount: Integer;
var
  i:  Integer;
begin
fSynchronizer.BeginRead;
try
  Result := 0;
  For i := Low(fEndpoints) to High(fEndpoints) do
    If Assigned(fEndpoints[i]) then Inc(Result);
finally
  fSynchronizer.EndRead;
end;
end;

//------------------------------------------------------------------------------

Function TMessanger.GetEndpoint(Index: Integer): TMessangerEndpoint;
begin
Result := nil;
fSynchronizer.BeginRead;
try
  If (Index >= Low(fEndpoints)) and (Index <= High(fEndpoints)) then
    Result := fEndpoints[Index]
  else
    raise Exception.CreateFmt('TMessanger.GetEndpoint: Index (%d) out of bounds.',[Index]);
finally
  fSynchronizer.EndRead;
end;
end;

{------------------------------------------------------------------------------}
{   TMessanger - protected methods                                             }
{------------------------------------------------------------------------------}

procedure TMessanger.RemoveEndpoint(EndpointID: TMsgrEndpointID);
begin
fSynchronizer.BeginWrite;
try
  If EndpointID <= High(fEndpoints) then
    fEndpoints[EndpointID] := nil
  else
    raise Exception.CreateFmt('TMessanger.RemoveEndpoint: EndpointID (%d) out of bounds.',[EndpointID]);
finally
  fSynchronizer.EndWrite;
end;
end;

//------------------------------------------------------------------------------

Function TMessanger.SendMessage(Msg: TMsgrMessage): Boolean;
var
  i:  Integer;
begin
Result := False;
fSynchronizer.BeginRead;
try
  If Msg.Target = MSGR_ID_BROADCAST then
    begin
      For i := Low(fEndpoints) to High(fEndpoints) do
        If Assigned(fEndpoints[i]) then
          begin
            fEndpoints[i].AddMessages(@Msg,1);
            Result := True;
          end;
    end
  else
    begin
      If Msg.Target <= High(fEndpoints) then
        If Assigned(fEndpoints[Msg.Target]) then
          begin
            fEndpoints[Msg.Target].AddMessages(@Msg,1);
            Result := True;
          end;
    end;
finally
  fSynchronizer.EndRead;
end;
end;

//------------------------------------------------------------------------------

procedure TMessanger.SendBufferedMessages(Messages,Undelivered: TMsgrMessageVector);
var
  i:        Integer;
  StartIdx: Integer;
  Count:    Integer;

  Function DoSending(Start,Cnt: Integer): Boolean;
  var
    ii: Integer;
  begin
    Result := False;
    If Messages[Start].Target = MSGR_ID_BROADCAST then
      begin
        For ii := Low(fEndpoints) to High(fEndpoints) do
          If Assigned(fEndpoints[ii]) then
            begin
              fEndpoints[ii].AddMessages(Messages.Pointers[Start],Cnt);
              Result := True;
            end;
      end
    else
      begin
        If Messages[Start].Target <= High(fEndpoints) then
          If Assigned(fEndpoints[Messages[Start].Target]) then
            begin
              fEndpoints[Messages[Start].Target].AddMessages(Messages.Pointers[Start],Cnt);
              Result := True;  
            end;
      end;
  end;

begin
If Messages.Count > 0 then
  begin
    fSynchronizer.BeginRead;
    try
      StartIdx := Messages.LowIndex;
      while StartIdx <= Messages.HighIndex do
        begin
          Count := 1;
          For i := Succ(StartIdx) to Messages.HighIndex do
            If Messages[StartIdx].Target = Messages[i].Target then
              Inc(Count)
            else
              Break{i};
          If not DoSending(StartIdx,Count) then
            Undelivered.Append(Messages.Pointers[StartIdx],Count);
          StartIdx := StartIdx + Count;
        end;
    finally
      fSynchronizer.EndRead;
    end;
  end;
end;

{------------------------------------------------------------------------------}
{   TMessanger - public methods                                                }
{------------------------------------------------------------------------------}

constructor TMessanger.Create(EndpointCapacity: TMsgrEndpointID = 1024);
begin
inherited Create;
// High(TMsgrEndpointID) = $FFFF(65535) is reserved for broadcast
If EndpointCapacity < High(TMsgrEndpointID) then
  SetLength(fEndpoints,EndpointCapacity)
else
  raise Exception.CreateFmt('TMessanger.Create: Required capacity (%d) is too high.',[EndpointCapacity]);
fSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
end;

//------------------------------------------------------------------------------

destructor TMessanger.Destroy;
begin
If EndpointCount > 0 then
  raise Exception.Create('TMessanger.Destroy: Not all endpoints were freed.');
fSynchronizer.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TMessanger.IDAvailable(EndpointID: TMsgrEndpointID): Boolean;
begin
fSynchronizer.BeginRead;
try
  If EndpointID <= High(fEndpoints) then
    Result := not Assigned(fEndpoints[EndpointID])
  else
    Result := False;
finally
  fSynchronizer.EndRead;
end;
end;

//------------------------------------------------------------------------------

Function TMessanger.CreateEndpoint: TMessangerEndpoint;
var
  i,Idx:  Integer;
begin
Result := nil;
fSynchronizer.BeginWrite;
try
  Idx := -1;
  For i := Low(fEndpoints) to High(fEndpoints) do
    If not Assigned(fEndpoints[i]) then
      begin
        Idx := i;
        Break {For i};
      end;
  If Idx >= 0 then
    begin
      Result := TMessangerEndpoint.Create(TMsgrEndpointID(Idx),Self);
      fEndpoints[Idx] := Result;
    end
  else raise Exception.Create('TMessanger.CreateEndpoint: No endpoint slot available.');
finally
  fSynchronizer.EndWrite;
end;
end;

//------------------------------------------------------------------------------

Function TMessanger.CreateEndpoint(EndpointID: TMsgrEndpointID): TMessangerEndpoint;
begin
Result := nil;
fSynchronizer.BeginWrite;
try
  If EndpointID <= High(fEndpoints) then
    begin
      If not Assigned(fEndpoints[EndpointID]) then
        begin
          Result := TMessangerEndpoint.Create(EndpointID,Self);
          fEndpoints[EndpointID] := Result;
        end
      else raise Exception.CreateFmt('TMessanger.CreateEndpoint: Requested endpoint ID (%d) is already taken.',[EndpointID]);
    end
  else raise Exception.CreateFmt('TMessanger.CreateEndpoint: Requested endpoint ID (%d) is not allocated.',[EndpointID]);
finally
  fSynchronizer.EndWrite;
end;
end;

end.
