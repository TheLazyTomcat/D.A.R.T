{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Concurrent tasks

  ©František Milt 2018-04-05

  Version 1.1.1

  To use this unit, create a descendant of class TCNTSTask and put the threaded
  code into method Main (override it). Then pass instance of this class to an
  instance of TCNTSManager. Manager will automatically start the task when
  resources get available, or you can start the task manually (this will pause
  other task when there are no running slots available).
  You can call any public method of TCNTSTask from Main, but remember to protect
  any shared data you want to use, tasks don't have any mean of thread-safety
  protection. In Main, you should call method Cycle regularly if you want to use
  integrated messaging system (in that case, also remember to override method
  ProcessMessage - put a code that will process incoming messages there).

  The implementation of method Main is entirely up to you, but suggested
  template is as follows:

    Function TTestTask.Main: Boolean;
    begin
      while not Terminated do
        begin
          Cycle;
          [user code]
          [progress signalling]
        end;
      Result := not Terminated;
    end;

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -    

  Dependencies:
    AuxTypes    - github.com/ncs-sniper/Lib.AuxTypes
    Messanger   - github.com/ncs-sniper/Lib.Messanger
    MemVector   - github.com/ncs-sniper/Lib.MemVector
    WinSyncObjs - github.com/ncs-sniper/Lib.WinSyncObjs
    StrRect     - github.com/ncs-sniper/Lib.StrRect

===============================================================================}
unit ConcurrentTasks;

{$IF not(Defined(WINDOWS) or Defined(MSWINDOWS))}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$TYPEINFO ON}

interface

uses
  Classes,
  AuxTypes, Messanger, WinSyncObjs;

{===============================================================================
    Messages
===============================================================================}

const
  CNTS_MSGR_ENDPOINT_MANAGER = 0;

  CNTS_MSG_USER      = 0;
  CNTS_MSG_TERMINATE = 1;
  CNTS_MSG_PROGRESS  = 2;
  CNTS_MSG_COMPLETED = 3;

type
  TCNTSMessageEndpoint = TMsgrEndpointID;
  TCNTSMessageParam    = TMsgrParam;
  TCNTSMessageResult   = TMsgrParam;

  TCNTSMessage = record
    Sender: TCNTSMessageEndpoint;
    Param1: TCNTSMessageParam;
    Param2: TCNTSMessageParam;
    Result: TCNTSMessageResult;
  end;

  TCNTSMessageEvent = procedure(Sender: TObject; var Msg: TCNTSMessage) of object;

{===============================================================================
--------------------------------------------------------------------------------
                                   TCNTSTask
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TCNTSTask - declaration
===============================================================================}

  TCNTSTask = class(TObject)
  private
    fCommEndpoint:  TMessangerEndpoint;
    fPauseObject:   TEvent;
    fTerminated:    Integer;
    Function GetTerminated: Boolean;
    procedure SetTerminated(Value: Boolean);
  protected
    procedure MessageHandler(Sender: TObject; Msg: TMsgrMessage; var Flags: TMsgrDispatchFlags); virtual;
  public
    procedure SetInternals(CommEndpoint: TMessangerEndpoint; PauseObject: TEvent);  // static
    procedure Execute;                                                              // static
    Function PostMessage(Param1,Param2: TCNTSMessageParam): Boolean; virtual;
    Function SendMessage(Param1,Param2: TCNTSMessageParam): TCNTSMessageResult; virtual;
    procedure SignalProgress(Progress: Double); virtual;
    procedure Cycle; virtual;
    procedure ProcessMessage(var Msg: TCNTSMessage); virtual;
    Function Main: Boolean; virtual; abstract;
  published
    property Terminated: Boolean read GetTerminated write SetTerminated;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TCNTSThread
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TCNTSThread - declaration
===============================================================================}

  TCNTSThread = class(TThread)
  private
    fTaskObject:    TCNTSTask;
    fCommEndpoint:  TMessangerEndpoint;
  protected
    procedure Execute; override;
  public
    constructor Create(TaskObject: TCNTSTask; CommEndpoint: TMessangerEndpoint; PauseObject: TEvent);
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TCNTSManager
--------------------------------------------------------------------------------
===============================================================================}

  TCNTSTaskState = (
    tsReady,      // not running
    tsRunning,    // running
    tsQueued,     // not running, paused
    tsPaused,     // running, paused
    tsWaiting,    // running, paused internally (can be automatically unpaused)
    tsCompleted,  // completed, returned true
    tsAborted);   // completed, returned false

  TCNTSTaskItem = record
    State:      TCNTSTaskState;
    TaskObject: TCNTSTask;
    Progress:   Double;
  end;
  PCNTSTaskItem = ^TCNTSTaskItem;

  TCNTSTaskItemFull = record
    PublicPart:     TCNTSTaskItem;
    CommEndpoint:   TMessangerEndpoint;
    PauseObject:    TEvent;
    AssignedThread: TCNTSThread;
  end;
  PCNTSTaskItemFull = ^TCNTSTaskItemFull;

  TCNTSTasks = array of TCNTSTaskItemFull;

  TCNTSTaskEvent = procedure(Sender: TObject; TaskIndex: Integer) of object;

{===============================================================================
    TCNTSManager - declaration
===============================================================================}

  TCNTSManager = class(TObject)
  private
    fOwnsTaskObjects:     Boolean;
    fTasks:               TCNTSTasks;
    fMaxConcurrentTasks:  Integer;
    fMessanger:           TMessanger;
    fCommEndpoint:        TMessangerEndpoint;
    fInternalAction:      Boolean;
    fOnMessage:           TCNTSMessageEvent;
    fOnTaskState:         TCNTSTaskEvent;
    fOnTaskProgress:      TCNTSTaskEvent;
    fOnTaskCompleted:     TCNTSTaskEvent;
    fOnTaskRemove:        TCNTSTaskEvent;
    fOnChange:            TNotifyEvent;
    Function GetTaskCount: Integer;
    Function GetTask(Index: Integer): TCNTSTaskItem;
    procedure SetMaxConcurrentTasks(Value: Integer);
  protected
    procedure MessageHandler(Sender: TObject; Msg: TMsgrMessage; var Flags: TMsgrDispatchFlags); virtual;
    procedure ManageRunningTasks(IgnoreTask: Integer = -1); virtual;
  public
    class Function GetProcessorCount: Integer; virtual;
    constructor Create(OwnsTaskObjects: Boolean = True);
    destructor Destroy; override;
    procedure Update(WaitTimeOut: LongWord = 0); virtual;
    Function LowIndex: Integer; virtual;
    Function HighIndex: Integer; virtual;
    Function First: TCNTSTaskItem; virtual;
    Function Last: TCNTSTaskItem; virtual;
    Function IndexOfTask(TaskObject: TCNTSTask): Integer; overload; virtual;
    Function IndexOfTask(CommEndpointID: TCNTSMessageEndpoint): Integer; overload; virtual;
    Function AddTask(TaskObject: TCNTSTask): Integer; virtual;
    procedure Insert(Index: Integer; TaskObject: TCNTSTask); virtual;
    procedure Move(CurIdx, NewIdx: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer); virtual;
    Function RemoveTask(TaskObject: TCNTSTask): Integer; virtual;
    procedure DeleteTask(TaskIndex: Integer); virtual;
    Function ExtractTask(TaskObject: TCNTSTask): TCNTSTask; virtual;
    procedure ClearTasks; virtual;
    procedure ClearCompletedTasks; virtual;
    procedure StartTask(TaskIndex: Integer); virtual;
    procedure PauseTask(TaskIndex: Integer); virtual;
    procedure ResumeTask(TaskIndex: Integer); virtual;
    procedure StopTask(TaskIndex: Integer); virtual;
    procedure WaitForRunningTasksToComplete; virtual;
    Function PostMessage(TaskIndex: Integer; Param1,Param2: TCNTSMessageParam): Boolean; virtual;
    Function SendMessage(TaskIndex: Integer; Param1,Param2: TCNTSMessageParam): TCNTSMessageResult; virtual;
    Function GetRunningTaskCount: Integer; virtual;
    Function GetActiveTaskCount(CountPaused: Boolean = False): Integer; virtual;
    property Tasks[Index: Integer]: TCNTSTaskItem read GetTask; default;
  published
    property TaskCount: Integer read GetTaskCount;
    property MaxConcurrentTasks: Integer read fMaxConcurrentTasks write SetMaxConcurrentTasks;
    property OnMessage: TCNTSMessageEvent read fOnMessage write fOnMessage;
    property OnTaskState: TCNTSTaskEvent read fOnTaskState write fOnTaskState;
    property OnTaskProgress: TCNTSTaskEvent read fOnTaskProgress write fOnTaskProgress;
    property OnTaskCompleted: TCNTSTaskEvent read fOnTaskCompleted write fOnTaskCompleted; 
    property OnTaskRemove: TCNTSTaskEvent read fOnTaskRemove write fOnTaskRemove;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

implementation

uses
  Windows, SysUtils;

{===============================================================================
    External functions
===============================================================================}

procedure GetNativeSystemInfo(lpSystemInfo: PSystemInfo); stdcall; external kernel32;

{===============================================================================
--------------------------------------------------------------------------------
                                   TCNTSTask
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TCNTSTask - implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TCNTSTask - private methods
-------------------------------------------------------------------------------}

Function TCNTSTask.GetTerminated: Boolean;
begin
Result := InterlockedExchangeAdd(fTerminated,0) <> 0;
end;

//------------------------------------------------------------------------------

procedure TCNTSTask.SetTerminated(Value: Boolean);
begin
If Value then InterlockedExchange(fTerminated,-1)
  else InterlockedExchange(fTerminated,0);
end;

{-------------------------------------------------------------------------------
    TCNTSTask - protected methods
-------------------------------------------------------------------------------}

procedure TCNTSTask.MessageHandler(Sender: TObject; Msg: TMsgrMessage; var Flags: TMsgrDispatchFlags);
var
  InternalMessage:  TCNTSMessage;
begin
case Msg.Parameter1 of
  CNTS_MSG_USER:
    begin
      InternalMessage.Sender := Msg.Sender;
      InternalMessage.Param1 := Msg.Parameter2;
      InternalMessage.Param2 := Msg.Parameter3;
      InternalMessage.Result := 0;
      ProcessMessage(InternalMessage);
      If mdfSynchronousMessage in Flags then
        TCNTSMessageResult({%H-}Pointer(Msg.Parameter4)^) := InternalMessage.Result;
    end;
  CNTS_MSG_TERMINATE:
    Terminated := True;
end;
end;

{-------------------------------------------------------------------------------
    TCNTSTask - public methods
-------------------------------------------------------------------------------}

procedure TCNTSTask.SetInternals(CommEndpoint: TMessangerEndpoint; PauseObject: TEvent);
begin
fCommEndpoint := CommEndpoint;
fCommEndpoint.OnMessageTraversing := MessageHandler;
fPauseObject := PauseObject;
end;

//------------------------------------------------------------------------------

procedure TCNTSTask.Execute;
begin
Cycle;
fCommEndpoint.SendMessage(CNTS_MSGR_ENDPOINT_MANAGER,CNTS_MSG_COMPLETED,Ord(Main),0,0)
end;

//------------------------------------------------------------------------------

Function TCNTSTask.PostMessage(Param1,Param2: TCNTSMessageParam): Boolean;
begin
Result := fCommEndpoint.SendMessage(CNTS_MSGR_ENDPOINT_MANAGER,CNTS_MSG_USER,Param1,Param2,0);
end;

//------------------------------------------------------------------------------

Function TCNTSTask.SendMessage(Param1,Param2: TCNTSMessageParam): TCNTSMessageResult;
begin
fCommEndpoint.SendMessageAndWait(CNTS_MSGR_ENDPOINT_MANAGER,CNTS_MSG_USER,Param1,Param2,{%H-}TMsgrParam(Addr(Result)));
end;

//------------------------------------------------------------------------------

procedure TCNTSTask.SignalProgress(Progress: Double);
var
  ValueLo:  TCNTSMessageParam;
  ValueHi:  TCNTSMessageParam;
begin
ValueLo := TCNTSMessageParam(UInt32(Addr(Progress)^));
ValueHi := TCNTSMessageParam({%H-}PUInt32({%H-}PtrUInt(Addr(Progress)) + SizeOf(UInt32))^);
fCommEndpoint.SendMessage(CNTS_MSGR_ENDPOINT_MANAGER,CNTS_MSG_PROGRESS,ValueLo,ValueHi,0);
end;

//------------------------------------------------------------------------------

procedure TCNTSTask.Cycle;
begin
fCommEndpoint.Cycle(0); // do not wait
fPauseObject.WaitFor;
end;

//------------------------------------------------------------------------------

procedure TCNTSTask.ProcessMessage(var Msg: TCNTSMessage);
begin
Msg.Result := 0;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TCNTSThread
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TCNTSThread - declaration
===============================================================================}

{-------------------------------------------------------------------------------
    TCNTSThread - protected methods
-------------------------------------------------------------------------------}

procedure TCNTSThread.Execute;
begin
fTaskObject.Execute;
end;

{-------------------------------------------------------------------------------
    TCNTSThread - public methods
-------------------------------------------------------------------------------}

constructor TCNTSThread.Create(TaskObject: TCNTSTask; CommEndpoint: TMessangerEndpoint; PauseObject: TEvent);
begin
inherited Create(False);
Priority := tpLower;
FreeOnTerminate := False;
fTaskObject := TaskObject;
fCommEndpoint := CommEndpoint;
fTaskObject.SetInternals(CommEndpoint,PauseObject);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TCNTSManager
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TCNTSManager - declaration
===============================================================================}

{-------------------------------------------------------------------------------
    TCNTSManager - private methods
-------------------------------------------------------------------------------}

Function TCNTSManager.GetTaskCount: Integer;
begin
Result := Length(fTasks);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.GetTask(Index: Integer): TCNTSTaskItem;
begin
If (Index >= Low(fTasks)) and (Index <= High(fTasks)) then
  Result := fTasks[Index].PublicPart
else
  raise Exception.CreateFmt('TCNTSManager.GetTask: Index(%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.SetMaxConcurrentTasks(Value: Integer);
begin
If Value >= 1 then
  begin
    fMaxConcurrentTasks := Value;
    ManageRunningTasks;
  end
else raise Exception.CreateFmt('TCNTSManager.SetMaxConcurrentTasks: Cannot assign value smaller than 1 (%d).',[Value]);
end;

{-------------------------------------------------------------------------------
    TCNTSManager - protected methods
-------------------------------------------------------------------------------}

procedure TCNTSManager.MessageHandler(Sender: TObject; Msg: TMsgrMessage; var Flags: TMsgrDispatchFlags);
var
  InternalMessage:  TCNTSMessage;
  Index:            Integer;
begin
case Msg.Parameter1 of
  CNTS_MSG_USER:
    begin
      InternalMessage.Sender := Msg.Sender;
      InternalMessage.Param1 := Msg.Parameter2;
      InternalMessage.Param2 := Msg.Parameter3;
      InternalMessage.Result := 0;
      If Assigned(fOnMessage) then
        fOnMessage(Self,InternalMessage);
      If mdfSynchronousMessage in Flags then
        TCNTSMessageResult({%H-}Pointer(Msg.Parameter4)^) := InternalMessage.Result;
    end;
  CNTS_MSG_PROGRESS:
    begin
      Index := IndexOfTask(Msg.Sender);
      If Index >= 0 then
        begin
          UInt32(Addr(fTasks[Index].PublicPart.Progress)^) := UInt32(Msg.Parameter2);
          {%H-}PUInt32({%H-}PtrUInt(Addr(fTasks[Index].PublicPart.Progress)) + SizeOf(UInt32))^ := UInt32(Msg.Parameter3); 
          If Assigned(fOnTaskProgress) then
            fOnTaskProgress(Sender,Index);
        end;
    end;
  CNTS_MSG_COMPLETED:
    begin
      Index := IndexOfTask(Msg.Sender);
      If Index >= 0 then
        begin
          If Assigned(fTasks[Index].AssignedThread) then
            fTasks[Index].AssignedThread.WaitFor;
          If Msg.Parameter2 <> 0 then
            fTasks[Index].PublicPart.State := tsCompleted
          else
            fTasks[Index].PublicPart.State := tsAborted;
          If Assigned(fOnTaskState) then
            fOnTaskState(Self,Index);
          If Assigned(fOnTaskCompleted) then
            fOnTaskCompleted(Self,Index);
        end;
      ManageRunningTasks;
    end;
end;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.ManageRunningTasks(IgnoreTask: Integer = -1);
var
  RunCount: Integer;
  i:        Integer;
begin
fInternalAction := True;
try
  RunCount := fMaxConcurrentTasks - GetRunningTaskCount;
  If RunCount > 0 then
    begin
      For i := Low(fTasks) to High(fTasks) do
        If RunCount > 0 then
          begin
            If (fTasks[i].PublicPart.State in [tsReady,tsWaiting]) and (i <> IgnoreTask) then
              begin
                StartTask(i);
                Dec(RunCount);
              end;
          end
        else Break{For i};
    end
  else If RunCount < 0 then
    begin
      For i := High(fTasks) downto Low(fTasks) do
        If RunCount < 0 then
          begin
            If (fTasks[i].PublicPart.State = tsRunning) and (i <> IgnoreTask) then
              begin
                PauseTask(i);
                Inc(RunCount);
              end;
          end
        else Break{For i};
    end;
finally
  fInternalAction := False;
end;
end;

{-------------------------------------------------------------------------------
    TCNTSManager - public methods
-------------------------------------------------------------------------------}

class Function TCNTSManager.GetProcessorCount: Integer;
var
  SysInfo:  TSystemInfo;
begin
GetNativeSystemInfo(@SysInfo);
Result := Integer(SysInfo.dwNumberOfProcessors);
If Result < 1 then
  Result := 1;
end;

//------------------------------------------------------------------------------

constructor TCNTSManager.Create(OwnsTaskObjects: Boolean = True);
begin
inherited Create;
fOwnsTaskObjects := OwnsTaskObjects;
SetLength(fTasks,0);
fMaxConcurrentTasks := GetProcessorCount;
fMessanger := TMessanger.Create;
fCommEndpoint := fMessanger.CreateEndpoint(0);
fCommEndpoint.OnMessageTraversing := MessageHandler;
fInternalAction := False;
end;

//------------------------------------------------------------------------------

destructor TCNTSManager.Destroy;
begin
ClearTasks;
fCommEndpoint.Free;
fMessanger.Free;
SetLength(fTasks,0);
inherited;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.Update(WaitTimeOut: LongWord = 0);
begin
fCommEndpoint.Cycle(WaitTimeOut);
ManageRunningTasks;
end;

//------------------------------------------------------------------------------

Function TCNTSManager.LowIndex: Integer;
begin
Result := Low(fTasks);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.HighIndex: Integer;
begin
Result := High(fTasks);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.First: TCNTSTaskItem;
begin
Result := GetTask(LowIndex);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.Last: TCNTSTaskItem;
begin
Result := GetTask(HighIndex);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.IndexOfTask(TaskObject: TCNTSTask): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := Low(fTasks) to High(fTasks) do
  If fTasks[i].PublicPart.TaskObject = TaskObject then
    begin
      Result := i;
      Break;
    end;
end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

Function TCNTSManager.IndexOfTask(CommEndpointID: TCNTSMessageEndpoint): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := Low(fTasks) to High(fTasks) do
  If fTasks[i].CommEndpoint.EndpointID = CommEndpointID then
    begin
      Result := i;
      Break;
    end;
end;

//------------------------------------------------------------------------------

Function TCNTSManager.AddTask(TaskObject: TCNTSTask): Integer;
var
  NewTaskItem:  TCNTSTaskItemFull;
begin
NewTaskItem.PublicPart.State := tsReady;
NewTaskItem.PublicPart.TaskObject := TaskObject;
NewTaskItem.PublicPart.Progress := 0.0;
NewTaskItem.CommEndpoint := fMessanger.CreateEndpoint;
NewTaskItem.PauseObject := TEvent.Create(nil,True,True,'');
// thread is created only when the task is started
NewTaskItem.AssignedThread := nil;
SetLength(fTasks,Length(fTasks) + 1);
Result := High(fTasks);
fTasks[Result] := NewTaskItem;
If Assigned(fOnChange) then
  fOnChange(Self);
If Assigned(fOnTaskState) then
  fOnTaskState(Self,Result);
ManageRunningTasks;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.Insert(Index: Integer; TaskObject: TCNTSTask);
var
  NewTaskItem:  TCNTSTaskItemFull;
  i:            Integer;
begin
If (Index >= Low(fTasks)) and (Index <= High(fTasks)) then
  begin
    NewTaskItem.PublicPart.State := tsReady;
    NewTaskItem.PublicPart.TaskObject := TaskObject;
    NewTaskItem.PublicPart.Progress := 0.0;
    NewTaskItem.CommEndpoint := fMessanger.CreateEndpoint;
    NewTaskItem.PauseObject := TEvent.Create(nil,True,True,'');
    NewTaskItem.AssignedThread := nil;
    SetLength(fTasks,Length(fTasks) + 1);
    For i := High(fTasks) downto Succ(Index) do
      fTasks[i] := fTasks[i - 1];
    fTasks[Index] := NewTaskItem;
    If Assigned(fOnChange) then
      fOnChange(Self);
    If Assigned(fOnTaskState) then
      fOnTaskState(Self,Index);
    ManageRunningTasks;
  end
else If Index = Length(fTasks) then
  AddTask(TaskObject)
else
  raise Exception.CreateFmt('TCNTSManager.Insert: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.Move(CurIdx, NewIdx: Integer);
var
  TempItem: TCNTSTaskItemFull;
  i:        Integer;
begin
If CurIdx <> NewIdx then
  begin
    If (CurIdx < Low(fTasks)) or (CurIdx > High(fTasks)) then
      raise Exception.CreateFmt('TCNTSManager.Move: CurIdx (%d) out of bounds.',[CurIdx]);
    If (NewIdx < Low(fTasks)) or (NewIdx > High(fTasks)) then
      raise Exception.CreateFmt('TCNTSManager.Move: NewIdx (%d) out of bounds.',[NewIdx]);
    TempItem := fTasks[CurIdx];
    If NewIdx > CurIdx then
      For i := CurIdx to Pred(NewIdx) do
        fTasks[i] := fTasks[i + 1]
    else
      For i := CurIdx downto Succ(NewIdx) do
        fTasks[i] := fTasks[i - 1];
    fTasks[NewIdx] := TempItem;
    If Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.Exchange(Index1, Index2: Integer);
var
  TempItem: TCNTSTaskItemFull;
begin
If Index1 <> Index2 then
  begin
    If (Index1 < Low(fTasks)) or (Index1 > High(fTasks)) then
      raise Exception.CreateFmt('TCNTSManager.Exchange: Index1 (%d) out of bounds.',[Index1]);
    If (Index2 < Low(fTasks)) or (Index2 > High(fTasks)) then
      raise Exception.CreateFmt('TCNTSManager.Exchange: Index2 (%d) out of bounds.',[Index2]);
    TempItem := fTasks[Index1];
    fTasks[Index1] := fTasks[Index2];
    fTasks[Index2] := TempItem;
    If Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------

Function TCNTSManager.RemoveTask(TaskObject: TCNTSTask): Integer;
begin
Result := IndexOfTask(TaskObject);
If Result >= 0 then DeleteTask(Result);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.DeleteTask(TaskIndex: Integer);
var
  i:  Integer;
begin
If (TaskIndex >= Low(fTasks)) and (TaskIndex <= High(fTasks)) then
  begin
    If not(fTasks[TaskIndex].PublicPart.State in [tsRunning,tsPaused,tsWaiting]) then
      begin
        If Assigned(fTasks[TaskIndex].AssignedThread) then
          begin
            fTasks[TaskIndex].AssignedThread.WaitFor;
            FreeAndNil(fTasks[TaskIndex].AssignedThread);
          end;
        If Assigned(fTasks[TaskIndex].CommEndpoint) then
          begin
            fTasks[TaskIndex].CommEndpoint.OnMessageTraversing := nil;
            FreeAndNil(fTasks[TaskIndex].CommEndpoint);
          end;
        fTasks[TaskIndex].PauseObject.Free;
        If fOwnsTaskObjects then
          fTasks[TaskIndex].PublicPart.TaskObject.Free
        else
          If Assigned(fOnTaskRemove) then fOnTaskRemove(Self,TaskIndex);
        For i := TaskIndex to Pred(High(fTasks)) do
          fTasks[i] := fTasks[i + 1];
        SetLength(fTasks,Length(fTasks) - 1);
        If Assigned(fOnChange) then
          fOnChange(Self);
      end
    else raise Exception.CreateFmt('TCNTSManager.DeleteTask: Cannot delete running task (#%d).',[TaskIndex]);
  end
else raise Exception.CreateFmt('TCNTSManager.DeleteTask: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.ExtractTask(TaskObject: TCNTSTask): TCNTSTask;
var
  Index:  Integer;
  i:      Integer;
begin
Index := IndexOfTask(TaskObject);
If Index >= 0 then
  begin
    If not(fTasks[Index].PublicPart.State in [tsRunning,tsPaused,tsWaiting]) then
      begin
        If Assigned(fTasks[Index].AssignedThread) then
          begin
            fTasks[Index].AssignedThread.WaitFor;
            FreeAndNil(fTasks[Index].AssignedThread);
          end;
        If Assigned(fTasks[Index].CommEndpoint) then
          begin
            fTasks[Index].CommEndpoint.OnMessageTraversing := nil;
            FreeAndNil(fTasks[Index].CommEndpoint);
          end;
        fTasks[Index].PauseObject.Free;
        Result := fTasks[Index].PublicPart.TaskObject;
        For i := Index to Pred(High(fTasks)) do
          fTasks[i] := fTasks[i + 1];
        SetLength(fTasks,Length(fTasks) - 1);
        If Assigned(fOnChange) then
          fOnChange(Self);
      end
    else raise Exception.CreateFmt('TCNTSManager.ExtractTask: Cannot extract running task (#%d).',[Index]);
  end
else Result := nil;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.ClearTasks;
var
  i:      Integer;
  OldMCT: Integer;
begin
OldMCT := fMaxConcurrentTasks;
fMaxConcurrentTasks := High(fMaxConcurrentTasks);
try
  For i := Low(fTasks) to High(fTasks) do
    If fTasks[i].PublicPart.State in [tsPaused,tsWaiting] then
      ResumeTask(i);
  For i := High(fTasks) downto Low(fTasks) do
    begin
      If fTasks[i].PublicPart.State = tsRunning then
        begin
          fTasks[i].PublicPart.TaskObject.Terminated := True;
          fTasks[i].AssignedThread.WaitFor;
        end;
      fCommEndpoint.Cycle(0);
      DeleteTask(i);
    end;
  If Assigned(fOnChange) then
    fOnChange(Self);
finally
  fMaxConcurrentTasks := OldMCT;
end;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.ClearCompletedTasks;
var
  i:  Integer;
begin
Update;
For i := High(fTasks) downto Low(fTasks) do
  If fTasks[i].PublicPart.State = tsCompleted then
    DeleteTask(i);
If Assigned(fOnChange) then
  fOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.StartTask(TaskIndex: Integer);
begin
If (TaskIndex >= Low(fTasks)) and (TaskIndex <= High(fTasks)) then
  case fTasks[TaskIndex].PublicPart.State of
    tsReady:
      begin
        fTasks[TaskIndex].PublicPart.State := tsRunning;
        fTasks[TaskIndex].AssignedThread :=
          TCNTSThread.Create(fTasks[TaskIndex].PublicPart.TaskObject,
                             fTasks[TaskIndex].CommEndpoint,
                             fTasks[TaskIndex].PauseObject);
        ManageRunningTasks(TaskIndex);
        If Assigned(fOnTaskState) then
          fOnTaskState(Self,TaskIndex);
      end;
    tsQueued,
    tsPaused,
    tsWaiting:
      ResumeTask(TaskIndex);
  end
else raise Exception.CreateFmt('TCNTSManager.StartTask: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.PauseTask(TaskIndex: Integer);
begin
If (TaskIndex >= Low(fTasks)) and (TaskIndex <= High(fTasks)) then
  case fTasks[TaskIndex].PublicPart.State of
    tsReady:
      begin
        fTasks[TaskIndex].PublicPart.State := tsQueued;
        If Assigned(fOnTaskState) then
          fOnTaskState(Self,TaskIndex);
      end;
    tsRunning:
      begin
        If fInternalAction then
          fTasks[TaskIndex].PublicPart.State := tsWaiting
        else
          fTasks[TaskIndex].PublicPart.State := tsPaused;
        fTasks[TaskIndex].PauseObject.ResetEvent;
        If Assigned(fOnTaskState) then
          fOnTaskState(Self,TaskIndex);
        ManageRunningTasks;
      end;
    tsWaiting:
      begin
        fTasks[TaskIndex].PublicPart.State := tsPaused;
        If Assigned(fOnTaskState) then
          fOnTaskState(Self,TaskIndex);
      end;
  end
else raise Exception.CreateFmt('TCNTSManager.StartTask: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.ResumeTask(TaskIndex: Integer);
begin
If (TaskIndex >= Low(fTasks)) and (TaskIndex <= High(fTasks)) then
  case fTasks[TaskIndex].PublicPart.State of
    tsQueued:
      begin
        fTasks[TaskIndex].PublicPart.State := tsReady;
        If Assigned(fOnTaskState) then
          fOnTaskState(Self,TaskIndex);
      end;
    tsPaused,
    tsWaiting:
      begin
        fTasks[TaskIndex].PublicPart.State := tsRunning;
        fTasks[TaskIndex].PauseObject.SetEvent;
        ManageRunningTasks(TaskIndex);
        If Assigned(fOnTaskState) then
          fOnTaskState(Self,TaskIndex);
      end;
  end
else raise Exception.CreateFmt('TCNTSManager.StartTask: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.StopTask(TaskIndex: Integer);
begin
If (TaskIndex >= Low(fTasks)) and (TaskIndex <= High(fTasks)) then
  begin
    If fTasks[TaskIndex].PublicPart.State in [tsRunning,tsPaused,tsWaiting] then
      begin
        fCommEndpoint.SendMessage(fTasks[TaskIndex].CommEndpoint.EndpointID,CNTS_MSG_TERMINATE,0,0,0);
        fTasks[TaskIndex].PublicPart.TaskObject.Terminated := True;
        ResumeTask(TaskIndex);
      end;
  end
else raise Exception.CreateFmt('TCNTSManager.StartTask: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.WaitForRunningTasksToComplete;
var
  i:  Integer;
begin
For i := Low(fTasks) to High(fTasks) do
  If fTasks[i].PublicPart.State = tsRunning then
    fTasks[i].AssignedThread.WaitFor;
end;

//------------------------------------------------------------------------------

Function TCNTSManager.PostMessage(TaskIndex: Integer; Param1,Param2: TCNTSMessageParam): Boolean;
begin
If (TaskIndex >= Low(fTasks)) and (TaskIndex <= High(fTasks)) then
  Result := fCommEndpoint.SendMessage(fTasks[TaskIndex].CommEndpoint.EndpointID,CNTS_MSG_USER,Param1,Param2,0)
else
  raise Exception.CreateFmt('TCNTSManager.PostMessage: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.SendMessage(TaskIndex: Integer; Param1,Param2: TCNTSMessageParam): TCNTSMessageResult;
begin
If (TaskIndex >= Low(fTasks)) and (TaskIndex <= High(fTasks)) then
  fCommEndpoint.SendMessageAndWait(fTasks[TaskIndex].CommEndpoint.EndpointID,CNTS_MSG_USER,Param1,Param2,{%H-}TMsgrParam(Addr(Result)))
else
  raise Exception.CreateFmt('TCNTSManager.PostMessage: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.GetRunningTaskCount: Integer;
var
  i:  Integer;
begin
Result := 0;
For i := Low(fTasks) to High(fTasks) do
  If fTasks[i].PublicPart.State = tsRunning then
    Inc(Result);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.GetActiveTaskCount(CountPaused: Boolean = False): Integer;
var
  i:  Integer;
begin
Result := 0;
For i := Low(fTasks) to High(fTasks) do
  begin
    If fTasks[i].PublicPart.State in [tsReady,tsRunning] then
      Inc(Result)
    else If CountPaused and (fTasks[i].PublicPart.State in [tsQueued,tsPaused,tsWaiting]) then
      Inc(Result);    
  end;
end;

end.
