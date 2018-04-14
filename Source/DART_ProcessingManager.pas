{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_ProcessingManager;

{$INCLUDE DART_defs.inc}

interface

uses
  Classes, ComCtrls,
  AuxTypes, ProgressTracker, SyncThread,
  DART_ProcessingSettings, DART_Repairer, DART_ProcessingThread;

{===============================================================================
--------------------------------------------------------------------------------
                             TDARTProcessingManager
--------------------------------------------------------------------------------
===============================================================================}

type
  TDARTArchiveProcessingStatus = (apsUnknown,apsReady,apsPaused,apsSuccess,
                                  apsWarning,apsError,apsProcessing,apsHeartbeat,
                                  apsUnresponsive);

const
  DART_ArchiveProcessingStatusStrings: array[TDARTArchiveProcessingStatus] of String =
    ('Unknown','Ready','Paused... %.0f%%','Successfuly completed',
     'Completed with warnings','Error','Processing... %.0f%%','Heartbeating...',
     'Unresponsive');

type
  TDARTArchiveListItem = record
    Path:                       String;
    Name:                       String;
    Size:                       Int64;
    ProcessingStatus:           TDARTArchiveProcessingStatus;
    ArchiveProcessingSettings:  TDARTArchiveProcessingSettings;
    ResultInfo:                 TDARTResultInfo;
    ProgressStageNode:          TProgressTracker;
  end;
  PDARTArchiveListItem = ^TDARTArchiveListItem;

  TDARTArchiveList = record
    Arr:    array of TDARTArchiveListItem;
    Count:  Integer;
  end;

  TDARTProcessingManagerStatus = (pmsReady,pmsProcessing,pmsPaused,
                                  pmsTerminating,pmsPausedTerminating);

  TDARTArchiveChangeEvent = procedure(Sender: TObject; Index: Integer) of object;

{===============================================================================
    TDARTProcessingManager - class declaration
===============================================================================}

  TDARTProcessingManager = class(TObject)
  private
    fVisualListing:       TListView;
    fSynchronizer:        TSyncThreadSynchronizer;
    fStatus:              TDARTProcessingManagerStatus;
    fArchiveList:         TDARTArchiveList;
    fProgressTracker:     TProgressTracker;
    fMemoryLimit:         TMemSize;
    fProcessedArchIdx:    Integer;
    fProcessingThread:    TDARTProcessingThread;
    fDefferedDestThrds:   array of TDARTProcessingThread;
    fHeartbeat:           Integer;
    fHeartbeatCounter:    Integer;
    fHeartbeatActThrsld:  Integer;
    fHeartbeatFailThrsld: Integer;
    fOnArchiveProgress:   TDARTArchiveChangeEvent;
    fOnArchiveStatus:     TDARTArchiveChangeEvent;
    fOnManagerStatus:     TNotifyEvent;
    Function GetArchivePtr(Index: Integer): PDARTArchiveListItem;
    Function GetArchive(Index: Integer): TDARTArchiveListItem;
    Function GetProgress: Double;
  protected
    Function CheckIndex(Index: Integer): Boolean; virtual;
    procedure DoArchiveProgress(Index: Integer); virtual;
    procedure DoArchiveStatus(Index: Integer); virtual;
    procedure DoManagerStatus; virtual;
    procedure AddToVisualListing(Item: TDARTArchiveListItem); virtual;
    procedure DeferThreadDestruction(Thread: TDARTProcessingThread); virtual;
    procedure RunDeferredThreadDestruction; virtual;
    procedure ThreadProgressHandler(Sender: TObject; ArchiveIndex: Integer; Progress: Double); virtual;
    Function CreateProcessingThread: TDARTProcessingThread;
  public
    class Function SetTargetPathFromSourcePath(var CommonProcSett: TDART_PS_Common): String; virtual;
    constructor Create(VisualListing: TListView; VisualApp: Boolean = True);
    destructor Destroy; override;
    Function IndexOf(const ArchivePath: String): Integer; virtual;
    Function Add(const ArchivePath: String): Integer; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    procedure ClearCompleted; virtual;
    Function CompletedItemCount: Integer; virtual;
    procedure StartProcessing; virtual;
    procedure PauseProcessing; virtual;
    procedure ResumeProcessing; virtual;
    procedure StopProcessing; virtual;
    procedure EndProcessingAndWait; virtual;
    procedure Update(Timeout: LongWord); virtual;
    Function HeartbeatCheck: Boolean; virtual;
    property Pointers[Index: Integer]: PDARTArchiveListItem read GetArchivePtr;
    property Archives[Index: Integer]: TDARTArchiveListItem read GetArchive; default;
  published
    property Status: TDARTProcessingManagerStatus read fStatus;
    property Count: Integer read fArchiveList.Count;
    property ProcessedArchiveIndex: Integer read fProcessedArchIdx;
    property Progress: Double read GetProgress;
    property HeartbeatActivationThreshold: Integer read fHeartbeatActThrsld write fHeartbeatActThrsld;
    property HeartbeatFailureThreshold: Integer read fHeartbeatFailThrsld write fHeartbeatFailThrsld;
    property OnArchiveProgress: TDARTArchiveChangeEvent read fOnArchiveProgress write fOnArchiveProgress;
    property OnArchiveStatus: TDARTArchiveChangeEvent read fOnArchiveStatus write fOnArchiveStatus;
    property OnManagerStatus: TNotifyEvent read fOnManagerStatus write fOnManagerStatus;
  end;

implementation

uses
  Windows, SysUtils,
  WinFileInfo,
  DART_Auxiliary, DART_Format_SCS, DART_Format_ZIP;

{===============================================================================
--------------------------------------------------------------------------------
                             TDARTProcessingManager
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTProcessingManager - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTProcessingManager - private methods
-------------------------------------------------------------------------------}

Function TDARTProcessingManager.GetArchivePtr(Index: Integer): PDARTArchiveListItem;
begin
If CheckIndex(Index) then
  Result := Addr(fArchiveList.Arr[Index])
else
  raise Exception.CreateFmt('TDARTProcessingManager.GetArchivePtr: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TDARTProcessingManager.GetArchive(Index: Integer): TDARTArchiveListItem;
begin
If CheckIndex(Index) then
  Result := fArchiveList.Arr[Index]
else
  raise Exception.CreateFmt('TDARTProcessingManager.GetArchive: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TDARTProcessingManager.GetProgress: Double;
begin
Result := fProgressTracker.Progress;
end;

{-------------------------------------------------------------------------------
    TDARTProcessingManager - protected methods
-------------------------------------------------------------------------------}

Function TDARTProcessingManager.CheckIndex(Index: Integer): Boolean;
begin
Result := (Index >= Low(fArchiveList.Arr)) and (Index < fArchiveList.Count)
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.DoArchiveProgress(Index: Integer);
begin
If Assigned(fOnArchiveProgress) then fOnArchiveProgress(Self,Index);
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.DoArchiveStatus(Index: Integer);
begin
If Assigned(fOnArchiveStatus) then fOnArchiveStatus(Self,Index);
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.DoManagerStatus;
begin
If Assigned(fOnManagerStatus) then fOnManagerStatus(Self);
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.AddToVisualListing(Item: TDARTArchiveListItem);
begin
If Assigned(fVisualListing) then
  with fVisualListing.Items.Add do
    begin
      SubItems.Add(Item.Name);
      SubItems.Add(SizeToStr(Item.Size));
      SubItems.Add(DART_ArchiveTypeStrings[Item.ArchiveProcessingSettings.Common.SelectedArchiveType]);
      SubItems.Add(Format(DART_RepairMethodStrings[Item.ArchiveProcessingSettings.Common.RepairMethod],
        [DART_KnownArchiveTypeStrings[Item.ArchiveProcessingSettings.Common.ConvertTo]]));
      SubItems.Add(DART_ArchiveProcessingStatusStrings[Item.ProcessingStatus]);
      ImageIndex := Ord(Item.ProcessingStatus);
    end;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.DeferThreadDestruction(Thread: TDARTProcessingThread);
begin
SetLength(fDefferedDestThrds,Length(fDefferedDestThrds) + 1);
fDefferedDestThrds[High(fDefferedDestThrds)] := Thread;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.RunDeferredThreadDestruction;
var
  i:  Integer;
begin
For i := Low(fDefferedDestThrds) to High(fDefferedDestThrds) do
   begin
    fDefferedDestThrds[i].WaitFor;  // if the thread ended, this should return immediately
    fDefferedDestThrds[i].Free;
  end;
SetLength(fDefferedDestThrds,0);
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.ThreadProgressHandler(Sender: TObject; ArchiveIndex: Integer; Progress: Double);
begin
// note the processing thread is stopped when this method is executed
If fArchiveList.Arr[fProcessedArchIdx].ProcessingStatus in [apsHeartbeat,apsUnresponsive] then
  begin
    fArchiveList.Arr[fProcessedArchIdx].ProcessingStatus := apsProcessing;
    OnArchiveStatus(Self,fProcessedArchIdx);
  end;
If fStatus in [pmsProcessing,pmsTerminating] then
  begin
    fHeartbeatCounter := 0;
    If (Progress >= 0.0) and (Progress <= 1.0) then
      begin // normal progress
        fArchiveList.Arr[ArchiveIndex].ProgressStageNode.Progress := Progress;
        DoArchiveProgress(ArchiveIndex);
      end
    else // progress is out of interval <0,1> (processing is done)
      begin
        // processing of the file is done at this point, set info and propagate
        fArchiveList.Arr[ArchiveIndex].ProgressStageNode.Progress := 1.0;
        DoArchiveProgress(ArchiveIndex);
        fArchiveList.Arr[ArchiveIndex].ResultInfo := fProcessingThread.ResultInfo;
        EnsureThreadSafety(fArchiveList.Arr[ArchiveIndex].ResultInfo); 
        case fArchiveList.Arr[ArchiveIndex].ResultInfo.ResultState of
          rsNormal:   fArchiveList.Arr[ArchiveIndex].ProcessingStatus := apsSuccess;
          rsWarning:  fArchiveList.Arr[ArchiveIndex].ProcessingStatus := apsWarning;
          rsError:    fArchiveList.Arr[ArchiveIndex].ProcessingStatus := apsError;
        else
          fArchiveList.Arr[ArchiveIndex].ProcessingStatus := apsUnknown;
        end;
        DoArchiveStatus(ArchiveIndex);
        // thread has completed, get rid of it
        fProcessingThread.OnArchiveProgress := nil;
        DeferThreadDestruction(fProcessingThread);
        // process next file if there is any
        If (fProcessedArchIdx < Pred(fArchiveList.Count)) and (fStatus <> pmsTerminating) then
          begin
            // process next file
            Inc(fProcessedArchIdx);
            fArchiveList.Arr[fProcessedArchIdx].ProcessingStatus := apsProcessing;
            DoArchiveStatus(fProcessedArchIdx);
            fProcessingThread := CreateProcessingThread;
            fProcessingThread.OnArchiveProgress := ThreadProgressHandler;
            fProcessingThread.StartProcessing;
          end
        else
          begin
            // last file processed or processing terminated, do not continue
            fProcessingThread := nil;
            fStatus := pmsReady;
            fProcessedArchIdx := -1;
          end;
        // propagate new manager status
        DoManagerStatus;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TDARTProcessingManager.CreateProcessingThread: TDARTProcessingThread;
begin
If Assigned(fSynchronizer) then
  Result := TDARTProcessingThread.Create(fProcessedArchIdx,fArchiveList.Arr[fProcessedArchIdx].ArchiveProcessingSettings,
                                         @fHeartbeat,fSynchronizer.CreateDispatcher)
else
  Result := TDARTProcessingThread.Create(fProcessedArchIdx,fArchiveList.Arr[fProcessedArchIdx].ArchiveProcessingSettings,
                                         @fHeartbeat,nil);
end;

{-------------------------------------------------------------------------------
    TDARTProcessingManager - public methods
-------------------------------------------------------------------------------}

class Function TDARTProcessingManager.SetTargetPathFromSourcePath(var CommonProcSett: TDART_PS_Common): String;
begin
case CommonProcSett.RepairMethod of
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  rmRebuild:
    CommonProcSett.TargetPath := ExtractFilePath(CommonProcSett.ArchivePath) +
      'repaired_' + ExtractFileName(CommonProcSett.ArchivePath);
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  rmExtract:
    If AnsiSameText(ExtractFileName(CommonProcSett.ArchivePath),ExtractFileExt(CommonProcSett.ArchivePath)) then
      // archive does not have proper name (actual name starts with a dot - meaning it is seen as an extension)
      CommonProcSett.TargetPath := IncludeTrailingPathDelimiter(ExtractFilePath(CommonProcSett.ArchivePath) + 'repaired')
    else If Length(ExtractFileExt(CommonProcSett.ArchivePath)) > 0 then
      // archive does have a proper name and extension
      CommonProcSett.TargetPath := IncludeTrailingPathDelimiter(ChangeFileExt(CommonProcSett.ArchivePath,''))
    else
      // archive does have a proper name but no extension
      CommonProcSett.TargetPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(CommonProcSett.ArchivePath) + '_repaired');
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  rmConvert:
    case CommonProcSett.ConvertTo of
      katSCS: CommonProcSett.TargetPath := ExtractFilePath(CommonProcSett.ArchivePath) + 'repaired_' +
                ChangeFileExt(ExtractFileName(CommonProcSett.ArchivePath),DART_SCS_DefaultExt);
      katZIP: CommonProcSett.TargetPath := ExtractFilePath(CommonProcSett.ArchivePath) + 'repaired_' +
                ChangeFileExt(ExtractFileName(CommonProcSett.ArchivePath),DART_ZIP_DefaultExt);
    else
      CommonProcSett.TargetPath := '';
    end;
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -              
else
  CommonProcSett.TargetPath := '';
end;
Result := CommonProcSett.TargetPath;
end;

//------------------------------------------------------------------------------

constructor TDARTProcessingManager.Create(VisualListing: TListView; VisualApp: Boolean = True);
begin
inherited Create;
fVisualListing := VisualListing;
If VisualApp then
  fSynchronizer := nil
else
  fSynchronizer := TSyncThreadSynchronizer.Create;
fStatus := pmsReady;
SetLength(fArchiveList.Arr,0);
fArchiveList.Count := 0;
fProgressTracker := TProgressTracker.Create;
fProgressTracker.ConsecutiveStages := True;
fMemoryLimit := Trunc(DART_GetAvailableMemory * 0.25);
fProcessedArchIdx := -1;
fProcessingThread := nil;
fHeartbeat := 0;
fHeartbeatCounter := 0;
fHeartbeatActThrsld := 10;
fHeartbeatFailThrsld := 20;
fOnArchiveProgress := nil;
fOnArchiveStatus := nil;
fOnManagerStatus := nil;
end;

//------------------------------------------------------------------------------

destructor TDARTProcessingManager.Destroy;
begin
RunDeferredThreadDestruction;
fProgressTracker.Free;
If Assigned(fSynchronizer) then
  fSynchronizer.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TDARTProcessingManager.IndexOf(const ArchivePath: String): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := Low(fArchiveList.Arr) to Pred(fArchiveList.Count) do
  If AnsiSameText(ArchivePath,fArchiveList.Arr[i].Path) then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TDARTProcessingManager.Add(const ArchivePath: String): Integer;
var
  Index:  Integer;
begin
If fStatus = pmsReady then
  begin
    Result := IndexOf(ArchivePath);
    If Result < 0 then
      begin
        // grow list
        If fArchiveList.Count >= Length(fArchiveList.Arr) then
          SetLength(fArchiveList.Arr,Length(fArchiveList.Arr) + 32);
        Result := fArchiveList.Count;
        Inc(fArchiveList.Count);
        with fArchiveList.Arr[Result] do
          begin
            // fill item info
            Path := ArchivePath;
            Name := ExtractFileName(Path);
            Size := DART_GetFileSize(Path);
            ProcessingStatus := apsReady;
            ArchiveProcessingSettings := DART_DefaultArchiveProcessingSettings;
            ResultInfo := DART_DefaultResultInfo;
            Index := fProgressTracker.Add(Size);
            ProgressStageNode := fProgressTracker.StageObjects[Index];
            // prepare processing settings
            ArchiveProcessingSettings.Common.ArchivePath := Path;
            // get probable archive type from signature
            case DART_GetFileSignature(Path) of
              DART_SCS_ArchiveSignature:  ArchiveProcessingSettings.Common.OriginalArchiveType := atSCS_sig;
              DART_ZIP_ArchiveSignature:  ArchiveProcessingSettings.Common.OriginalArchiveType := atZIP_sig;
            else
              ArchiveProcessingSettings.Common.OriginalArchiveType := atZIP_dft;
            end;
            ArchiveProcessingSettings.Common.SelectedArchiveType := ArchiveProcessingSettings.Common.OriginalArchiveType;
            // set conversion to zip as a default repair method for SCS# archives, rebuild for others
            case ArchiveProcessingSettings.Common.SelectedArchiveType of
              atSCS_sig,atSCS_frc:
                begin
                  ArchiveProcessingSettings.Common.RepairMethod := rmRebuild;
                  ArchiveProcessingSettings.Common.ConvertTo := katZIP;
                end;
              atZIP_sig,atZIP_frc,atZIP_dft:
                begin
                  ArchiveProcessingSettings.Common.RepairMethod := rmRebuild;
                  ArchiveProcessingSettings.Common.ConvertTo := katSCS;
                end;
            else
              ArchiveProcessingSettings.Common.RepairMethod := rmRebuild;
              ArchiveProcessingSettings.Common.ConvertTo := katUnknown;
            end;
            SetTargetPathFromSourcePath(ArchiveProcessingSettings.Common);
            ArchiveProcessingSettings.Auxiliary.InMemoryProcessingAllowed := Size <= fMemoryLimit;
          end;
        // add new archive to visual listing
        AddToVisualListing(fArchiveList.Arr[Result]);
        // propagate change
        DoArchiveStatus(Result);
      end;
  end
else raise Exception.CreateFmt('TDARTProcessingManager.Add: Manager status (%d) prevents addition.',[Ord(fStatus)]);
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.Delete(Index: Integer);
var
  i:  Integer;
begin
If fStatus = pmsReady then
  begin
    If CheckIndex(Index) then
      begin
        // remove item from the list
        For i := Index to (fArchiveList.Count - 2) do
          fArchiveList.Arr[i] := fArchiveList.Arr[i + 1];
        Dec(fArchiveList.Count);
        // delete progress node
        fProgressTracker.Delete(Index);
        // remove item from visual listing
        fVisualListing.Items.Delete(Index);
      end
    else raise Exception.CreateFmt('TDARTProcessingManager.Delete: Index (%d) out of bounds.',[Index]);
  end
else raise Exception.CreateFmt('TDARTProcessingManager.Delete: Manager status (%d) prevents deletion.',[Ord(fStatus)]);
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.Clear;
begin
If fStatus = pmsReady then
  begin
    // clear the list
    fArchiveList.Count := 0;
    // clear progress node
    fProgressTracker.Clear;
    // clear visual listing
    fVisualListing.Clear;
  end
else raise Exception.CreateFmt('TDARTProcessingManager.Clear: Manager status (%d) prevents deletion.',[Ord(fStatus)]);
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.ClearCompleted;
var
  i:  Integer;
begin
If fStatus = pmsReady then
  begin
    For i := Pred(fArchiveList.Count) downto Low(fArchiveList.Arr) do
      If fArchiveList.Arr[i].ProcessingStatus in [apsSuccess,apsWarning] then Delete(i);
  end
else raise Exception.CreateFmt('TDARTProcessingManager.ClearCompleted: Manager status (%d) prevents deletion.',[Ord(fStatus)]);
end;

//------------------------------------------------------------------------------

Function TDARTProcessingManager.CompletedItemCount: Integer;
var
  i:  Integer;
begin
Result := 0;
For i := Low(fArchiveList.Arr) to Pred(fArchiveList.Count) do
  If fArchiveList.Arr[i].ProcessingStatus = apsSuccess then
    Inc(Result);
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.StartProcessing;
var
  i:  Integer;
begin
RunDeferredThreadDestruction;
If (fArchiveList.Count > 0) and (fStatus = pmsReady) then
  begin
    // reset heartbeat
    fHeartbeat := 0;
    fHeartbeatCounter := 0;
    // reset progress
    For i := fProgressTracker.LowIndex to fProgressTracker.HighIndex do
      fProgressTracker.SetStageProgress(i,0.0);
    // reset processing status of all archives
    For i := Low(fArchiveList.Arr) to Pred(fArchiveList.Count) do
      begin
        fArchiveList.Arr[i].ProcessingStatus := apsReady;
        DoArchiveStatus(i);
      end;
    fProcessedArchIdx := Low(fArchiveList.Arr);
    fArchiveList.Arr[fProcessedArchIdx].ProgressStageNode.Progress := 0.0;
    fArchiveList.Arr[fProcessedArchIdx].ProcessingStatus := apsProcessing;
    DoArchiveStatus(fProcessedArchIdx);
    // create first processing thread
    fProcessingThread := CreateProcessingThread;
    fProcessingThread.OnArchiveProgress := ThreadProgressHandler;
    // set the manager to processing state
    fStatus := pmsProcessing;
    DoManagerStatus;
    // start the processing
    fProcessingThread.StartProcessing;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.PauseProcessing;
begin
If Assigned(fProcessingThread) and (fStatus in [pmsProcessing,pmsTerminating]) then
  begin
    fProcessingThread.PauseProcessing;
    fArchiveList.Arr[fProcessedArchIdx].ProcessingStatus := apsPaused;
    DoArchiveStatus(fProcessedArchIdx);
    case fStatus of
      pmsProcessing:  fStatus := pmsPaused;
      pmsTerminating: fStatus := pmsPausedTerminating;
    else
      fStatus := pmsPaused;
    end;
    DoManagerStatus;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.ResumeProcessing;
begin
If Assigned(fProcessingThread) and (fStatus in [pmsPaused,pmsPausedTerminating]) then
  begin
    fProcessingThread.ResumeProcessing;
    fArchiveList.Arr[fProcessedArchIdx].ProcessingStatus := apsProcessing;
    DoArchiveStatus(fProcessedArchIdx);
    case fStatus of
      pmsPaused:            fStatus := pmsProcessing;
      pmsPausedTerminating: fStatus := pmsTerminating;
    else
      fStatus := pmsPaused;
    end;
    DoManagerStatus;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.StopProcessing;
var
  OrigStatus: TDARTPRocessingManagerStatus;
begin
OrigStatus := fStatus;
If Assigned(fProcessingThread) then
  case fStatus of
    pmsPaused,
    pmsProcessing:  begin
                      fStatus := pmsTerminating;
                      DoManagerStatus;
                      fProcessingThread.StopProcessing;
                      If OrigStatus = pmsPaused then
                        fProcessingThread.ResumeProcessing;
                    end;
    pmsPausedTerminating,
    pmsTerminating: begin // kills the thread immediately without proper resource cleanup
                      If OrigStatus <> pmsPausedTerminating then
                        fProcessingThread.PauseProcessing;
                      // let's pretend the currently processed file was not touched
                      fArchiveList.Arr[fProcessedArchIdx].ProcessingStatus := apsReady;
                      DoArchiveStatus(fProcessedArchIdx);
                      // dirty things follows...
                      fProcessingThread.OnArchiveProgress := nil;
                      TerminateThread(fProcessingThread.Handle,0);  // bang!
                      fProcessingThread.ResumeProcessing;
                      fProcessingThread := nil;
                      // put manager into ready state
                      fStatus := pmsReady;
                      DoManagerStatus;
                    end;
  else
    RunDeferredThreadDestruction;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.EndProcessingAndWait;
begin
If Assigned(fProcessingThread) then
  begin
    fStatus := pmsTerminating;
    DoManagerStatus;
    fProcessingThread.StopProcessing;   // does not stop the thread, only sets a flag
    fProcessingThread.ResumeProcessing; // in case it is paused
  {$IFDEF DevelNotes}
    {$MESSAGE 'read here'}
  {$ENDIF}
  {
    Following line might hung if there is problem in processing.

    There is a posibility to use a WinAPI wait function, and if it times-out
    then the thread would be forcefully terminated using TerminateThread
    function.

    There is big problem with that - processing thread will, in 100% of cases,
    be waiting for message processing for synchronized progress by this point.
    This message processing never occurrs since the main thread is waiting here.
    This results in simple fact - the wait function would allways time-out,
    meaning the processing thread is hard-killed everytime. Not good.

    I will be searching for solution.
  }
    fProcessingThread.WaitFor;
    fProcessingThread := nil;   // the thread should be in defered destruction list by this point
  end;
RunDeferredThreadDestruction;
end;

//------------------------------------------------------------------------------

procedure TDARTProcessingManager.Update(Timeout: LongWord);
begin
If Assigned(fSynchronizer) then
  fSynchronizer.Update(Timeout);  
end;

//------------------------------------------------------------------------------

Function TDARTProcessingManager.HeartbeatCheck: Boolean;
begin
If fStatus = pmsProcessing then
  begin
    If InterlockedExchange(fHeartbeat,0) = 0 then
      begin
        // there was no progress done since last heartbeat
        Inc(fHeartbeatCounter);
        case fArchiveList.Arr[fProcessedArchIdx].ProcessingStatus of
          apsProcessing:
            If fHeartbeatCounter >= fHeartbeatActThrsld then
              begin
                fArchiveList.Arr[fProcessedArchIdx].ProcessingStatus := apsHeartbeat;
                OnArchiveStatus(Self,fProcessedArchIdx);
              end;
          apsHeartbeat:
            If fHeartbeatCounter >= fHeartbeatFailThrsld then
              begin
                fArchiveList.Arr[fProcessedArchIdx].ProcessingStatus := apsUnresponsive;
                OnArchiveStatus(Self,fProcessedArchIdx);
              end;
        end;
        Result := False;
      end
    else
      begin
        fHeartbeatCounter := 0;
        // there was some progress or current file is not in processing mode
        If fArchiveList.Arr[fProcessedArchIdx].ProcessingStatus in [apsHeartbeat,apsUnresponsive] then
          begin
            fArchiveList.Arr[fProcessedArchIdx].ProcessingStatus := apsProcessing;
            OnArchiveStatus(Self,fProcessedArchIdx);
          end;
        Result := True;
      end;
  end
else Result := True;
end;

end.
