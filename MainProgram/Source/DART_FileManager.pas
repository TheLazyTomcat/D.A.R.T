{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_FileManager;

{$INCLUDE DART_defs.inc}

interface

uses
  Classes, ComCtrls,
  DART_ProcessingSettings, DART_Repairer, DART_RepairerThread;

{==============================================================================}
{------------------------------------------------------------------------------}
{                                 TFileManager                                 }
{------------------------------------------------------------------------------}
{==============================================================================}

type
  TFileProcessingStatus = (fstUnknown = 0,fstReady,fstSuccess,fstWarning,
                           fstError,fstProcessing);

const
  FileProcessingStatusStrArr: array[TFileProcessingStatus] of String =
    ('Unknown','Ready','Successfuly completed','Completed with warnings',
     'Error','Processing... 0%');

type
  TFileListItem = record
    Path:                 String;
    Name:                 String;
    Size:                 Int64;
    ProcessingStatus:     TFileProcessingStatus;
    ProcessingSettings:   TFileProcessingSettings;
    GlobalProgressOffset: Single;
    GlobalProgressRange:  Single;
    Progress:             Single;
    ResultInfo:           TResultInfo;
  end;
  PFileListItem = ^TFileListItem;

  TFileManagerStatus = (mstReady,mstProcessing,mstTerminating);

  TFileChangeEvent = procedure(Sender: TObject; Index: Integer) of object;

{==============================================================================}
{   TFileManager - class declaration                                           }
{==============================================================================}

  TFileManager = class(TObject)
  private
    fVisualListing:     TListView;
    fManagerStatus:     TFileManagerStatus;
    fFileList:          array of TFileListItem;
    fMemoryLimit:       UInt64;
    fDeferredThreads:   array of TThread;
    fProcessedFileIdx:  Integer;
    fRepairerThread:    TRepairerThread;
    fGlobalProgress:    Single;
    fOnFileProgress:    TFileChangeEvent;
    fOnFileStatus:      TFileChangeEvent;
    fOnManagerStatus:   TNotifyEvent;
    Function GetFileCount: Integer;
    Function GetFilePtr(Index: Integer): PFileListItem;
    Function GetFile(Index: Integer): TFileListItem;
  protected
    procedure DoFileProgress(Index: Integer); virtual;
    procedure DoFileStatus(Index: Integer); virtual;
    procedure DoManagerStatus; virtual;
    procedure AddToVisualListing(Item: TFileListItem); virtual;
    procedure DeferThreadDestruction(Thread: TThread); virtual;
    procedure RunDeferredThreadDestruction; virtual;
    procedure ThreadProgressHandler(Sender: TObject; FileIndex: Integer; Progress: Single); virtual;
  public
    constructor Create(VisualListing: TListView);
    destructor Destroy; override;
    Function IndexOf(const FilePath: String): Integer; virtual;
    Function Add(const FilePath: String): Integer; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    procedure ClearCompleted; virtual;
    Function ContainsCompletedItem: Boolean; virtual;
    procedure StartProcessing; virtual;
    procedure PauseProcessing; virtual;
    procedure ResumeProcessing; virtual;
    procedure StopProcessing; virtual;
    procedure EndProcessingAndWait; virtual;
    property Pointers[Index: Integer]: PFileListItem read GetFilePtr;
    property Items[Index: Integer]: TFileListItem read GetFile; default;
  published
    property ManagerStatus: TFileManagerStatus read fManagerStatus; 
    property Count: Integer read GetFileCount;
    property ProcessedFileIndex: Integer read fProcessedFileIdx;
    property GlobalProgress: Single read fGlobalProgress; 
    property OnFileProgress: TFileChangeEvent read fOnFileProgress write fOnFileProgress;
    property OnFileStatus: TFileChangeEvent read fOnFileStatus write fOnFileStatus;
    property OnManagerStatus: TNotifyEvent read fOnManagerStatus write fOnManagerStatus;
  end;

implementation

uses
  Windows, SysUtils,
  WinFileInfo,
  DART_Auxiliary, DART_Repairer_ZIP, DART_Repairer_SCS;

{==============================================================================}
{   TFilesManager - class implementation                                       }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TFilesManager - private methods                                            }
{------------------------------------------------------------------------------}

Function TFileManager.GetFileCount: Integer;
begin
Result := Length(fFileList);
end;

//------------------------------------------------------------------------------

Function TFileManager.GetFilePtr(Index: Integer): PFileListItem;
begin
If (Index >= Low(fFileList)) and (Index <= High(fFileList)) then
  Result := Addr(fFileList[Index])
else
  raise Exception.CreateFmt('TFileManager.GetFilePtr: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TFileManager.GetFile(Index: Integer): TFileListItem;
begin
Result := GetFilePtr(Index)^;
end;

{------------------------------------------------------------------------------}
{   TFilesManager - protected methods                                          }
{------------------------------------------------------------------------------}

procedure TFileManager.DoFileProgress(Index: Integer);
begin
If Assigned(fOnFileProgress) then fOnFileProgress(Self,Index);
end;

//------------------------------------------------------------------------------

procedure TFileManager.DoFileStatus(Index: Integer);
begin
If Assigned(fOnFileStatus) then fOnFileStatus(Self,Index);
end;

//------------------------------------------------------------------------------

procedure TFileManager.DoManagerStatus;
begin
If Assigned(fOnManagerStatus) then fOnManagerStatus(Self);
end;

//------------------------------------------------------------------------------

procedure TFileManager.AddToVisualListing(Item: TFileListItem);
begin
with fVisualListing.Items.Add do
  begin
    SubItems.Add(Item.Name);
    SubItems.Add(SizeToStr(Item.Size));
    SubItems.Add(FileTypeStrArr[Item.ProcessingSettings.Common.FileType]);
    SubItems.Add(RepairerMethodStrArr[Item.ProcessingSettings.Common.RepairMethod]);
    SubItems.Add(FileProcessingStatusStrArr[Item.ProcessingStatus]);
    ImageIndex := Ord(Item.ProcessingStatus);
  end;
end;

//------------------------------------------------------------------------------

procedure TFileManager.DeferThreadDestruction(Thread: TThread);
begin
SetLength(fDeferredThreads,Length(fDeferredThreads) + 1);
fDeferredThreads[High(fDeferredThreads)] := Thread;
end;

//------------------------------------------------------------------------------

procedure TFileManager.RunDeferredThreadDestruction;
var
  i:  Integer;
begin
For i := Low(fDeferredThreads) to High(fDeferredThreads) do
  begin
    fDeferredThreads[i].WaitFor;
    fDeferredThreads[i].Free;
  end;
SetLength(fDeferredThreads,0);
end;

//------------------------------------------------------------------------------

procedure TFileManager.ThreadProgressHandler(Sender: TObject; FileIndex: Integer; Progress: Single);
begin
If fManagerStatus in [mstProcessing,mstTerminating] then
  begin
    If (Progress >= 0.0) and (Progress <= 1.0) then
      begin
        fFileList[FileIndex].Progress := Progress;
        fGlobalProgress := fFileList[FileIndex].GlobalProgressOffset +
          fFileList[FileIndex].GlobalProgressRange * Progress;
        DoFileProgress(FileIndex);
      end
    else // progress is out of interval <0,1> (processing is done)
      begin
        // processing of the file is done at this point, set info and propagate
        fGlobalProgress := fFileList[FileIndex].GlobalProgressOffset +
          fFileList[FileIndex].GlobalProgressRange;
        fFileList[FileIndex].Progress := 1.0;
        DoFileProgress(FileIndex);
        fFileList[FileIndex].ResultInfo := fRepairerThread.ResultInfo;
        case fFileList[FileIndex].ResultInfo.ResultState of
          rsNormal:   fFileList[FileIndex].ProcessingStatus := fstSuccess;
          rsWarning:  fFileList[FileIndex].ProcessingStatus := fstWarning;
          rsError:    fFileList[FileIndex].ProcessingStatus := fstError;
        else
          fFileList[FileIndex].ProcessingStatus := fstUnknown;
        end;
        DoFileStatus(FileIndex);
        // thread has completed, get rid of it
        fRepairerThread.OnFileProgress := nil;
        DeferThreadDestruction(fRepairerThread);
        If (fProcessedFileIdx < High(fFileList)) and (fManagerStatus <> mstTerminating) then
          begin
            // process next file
            Inc(fProcessedFileIdx);
            fFileList[fProcessedFileIdx].ProcessingStatus := fstProcessing;
            DoFileStatus(fProcessedFileIdx);
            fRepairerThread := TRepairerThread.Create(fProcessedFileIdx,fFileList[fProcessedFileIdx].ProcessingSettings);
            fRepairerThread.OnFileProgress := ThreadProgressHandler;
            fRepairerThread.StartProcessing;
          end
        else
          begin
            // last file processed or processing terminated, do not continue
            fRepairerThread := nil;
            fManagerStatus := mstReady;
            fProcessedFileIdx := -1;
          end;
        // propagate new status
        DoManagerStatus;
      end;
  end;
end;

{------------------------------------------------------------------------------}
{   TFilesManager - public methods                                             }
{------------------------------------------------------------------------------}

constructor TFileManager.Create(VisualListing: TListView);
begin
inherited Create;
fVisualListing := VisualListing;
fManagerStatus := mstReady;
fMemoryLimit := Trunc(GetAvailableMemory * 0.25);
fProcessedFileIdx := -1;
fRepairerThread := nil;
fGlobalProgress := 0.0;
end;

//------------------------------------------------------------------------------

destructor TFileManager.Destroy;
begin
RunDeferredThreadDestruction;
inherited;
end;

//------------------------------------------------------------------------------

Function TFileManager.IndexOf(const FilePath: String): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := Low(fFileList) to High(fFileList) do
  If AnsiSameText(FilePath,fFileList[i].Path) then
    begin
      Result := i;
      Break;
    end;
end;

//------------------------------------------------------------------------------

Function TFileManager.Add(const FilePath: String): Integer;
begin
If fManagerStatus = mstReady then
  begin
    Result := IndexOf(FilePath);
    If Result < 0 then
      begin
        SetLength(fFileList,Length(fFileList) + 1);
        Result := High(fFileList);
        with fFileList[Result] do
          begin
            // fill item info
            Path := FilePath;
            Name := ExtractFileName(Path);
            Size := GetFileSize(Path);
            ProcessingStatus := fstReady;
            ProcessingSettings := DefaultFileProcessingSettings;
            ProcessingSettings.Common.FilePath := Path;
            case GetFileSignature(Path) of
              FileSignature_SCS: ProcessingSettings.Common.FileType := atSCS_sig;
              FileSignature_ZIP: ProcessingSettings.Common.FileType := atZIP_sig;
            else
              ProcessingSettings.Common.FileType := atZIP_dft;
            end;
            ProcessingSettings.Common.OriginalFileType := ProcessingSettings.Common.FileType;
            ProcessingSettings.Common.TargetPath := ExtractFilePath(Path) + 'repaired_' + Name;
            ProcessingSettings.Other.InMemoryProcessingAllowed := Size <= fMemoryLimit;
            GlobalProgressOffset := 0;
            GlobalProgressRange := 0;
            Progress := 0;
            ResultInfo := DefaultResultInfo;
          end;
        // add new file to visual listing
        AddToVisualListing(fFileList[Result]);
        // propagate change
        DoFileStatus(Result);
      end;
  end
else raise Exception.CreateFmt('TFileManager.Add: Manager status (%d) prevents addition.',[Ord(fManagerStatus)]);
end;

//------------------------------------------------------------------------------

procedure TFileManager.Delete(Index: Integer);
var
  i:  Integer;
begin
If fManagerStatus = mstReady then
  begin
    If (Index >= Low(fFileList)) and (Index <= High(fFileList)) then
      begin
        // remove item from the list
        For i := Index to Pred(High(fFileList)) do
          fFileList[i] := fFileList[i + 1];
        SetLength(fFileList,Length(fFileList) - 1);
        // remove item from visual listing
        fVisualListing.Items.Delete(Index);
      end
    else raise Exception.CreateFmt('TFileManager.Delete: Index (%d) out of bounds.',[Index]);
  end
else raise Exception.CreateFmt('TFileManager.Delete: Manager status (%d) prevents deletion.',[Ord(fManagerStatus)]);
end;

//------------------------------------------------------------------------------

procedure TFileManager.Clear;
begin
If fManagerStatus = mstReady then
  begin
    // clear the list
    SetLength(fFileList,0);
    // clear visual listing
    fVisualListing.Clear;
  end
else raise Exception.CreateFmt('TFileManager.Clear: Manager status (%d) prevents deletion.',[Ord(fManagerStatus)]);
end;

//------------------------------------------------------------------------------

procedure TFileManager.ClearCompleted;
var
  i:  Integer;
begin
If fManagerStatus = mstReady then
  begin
    For i := High(fFileList) downto Low(fFileList) do
      If fFileList[i].ProcessingStatus in [fstSuccess,fstWarning] then Delete(i);
  end
else raise Exception.CreateFmt('TFileManager.ClearCompleted: Manager status (%d) prevents deletion.',[Ord(fManagerStatus)]);
end;

//------------------------------------------------------------------------------

Function TFileManager.ContainsCompletedItem: Boolean;
var
  i:  Integer;
begin
Result := False;
For i := Low(fFileList) to High(fFileList) do
  If fFileList[i].ProcessingStatus = fstSuccess then
    begin
      Result := True;
      Break;
    end;
end;

//------------------------------------------------------------------------------

procedure TFileManager.StartProcessing;
var
  i:            Integer;
  OverallSize:  Int64;
begin
RunDeferredThreadDestruction;
If (Count > 0) and (fManagerStatus = mstReady) then
  begin
    // prepare progress info for individual files...
    // get overall size of all files and prepare the files
    OverallSize := 0;
    For i := Low(fFileList) to High(fFileList) do
      begin
        OverallSize := OverallSize + fFileList[i].Size;
        fFileList[i].Progress := 0.0;
        fFileList[i].ProcessingStatus := fstReady;
        DoFileStatus(i);
      end;
    // calculate global progress fractions for each file
    For i := Low(fFileList) to High(fFileList) do
      begin
        // offset...
        If i > Low(fFileList) then
          fFileList[i].GlobalProgressOffset := fFileList[i - 1].GlobalProgressOffset + fFileList[i - 1].GlobalProgressRange
        else
          fFileList[i].GlobalProgressOffset := 0.0;
        // range...
        If OverallSize > 0 then
          fFileList[i].GlobalProgressRange := fFileList[i].Size / OverallSize
        else
          fFileList[i].GlobalProgressRange := 1 / Length(fFileList); 
      end;
    fProcessedFileIdx := Low(fFileList);
    fFileList[fProcessedFileIdx].ProcessingStatus := fstProcessing;
    DoFileStatus(fProcessedFileIdx);
    // create first repairer thread
    fRepairerThread := TRepairerTHread.Create(fProcessedFileIdx,fFileList[fProcessedFileIdx].ProcessingSettings);
    fRepairerThread.OnFileProgress := ThreadProgressHandler;
    // set the manager to processing state
    fManagerStatus := mstProcessing;
    fGlobalProgress := 0.0;
    DoManagerStatus;
    // start the processing
    fRepairerThread.StartProcessing;
  end;
end;

//------------------------------------------------------------------------------

procedure TFileManager.PauseProcessing;
begin
If Assigned(fRepairerThread) then
  fRepairerThread.PauseProcessing;
end;

//------------------------------------------------------------------------------

procedure TFileManager.ResumeProcessing;
begin
If Assigned(fRepairerThread) then
  fRepairerThread.ResumeProcessing;
end;

//------------------------------------------------------------------------------

procedure TFileManager.StopProcessing;
begin
If Assigned(fRepairerThread) then
  case fManagerStatus of
    mstProcessing:  begin
                      fManagerStatus := mstTerminating;
                      DoManagerStatus;
                      fRepairerThread.StopProcessing;
                    end;
    mstTerminating: begin // kills the thread without proper resource cleanup
                      fRepairerThread.PauseProcessing;
                      // let's pretend the currently processed file was not touched
                      fFileList[fProcessedFileIdx].ProcessingStatus := fstReady;
                      DoFileStatus(fProcessedFileIdx);
                      // dirty things follows...
                      fRepairerThread.OnFileProgress := nil;
                      TerminateThread(fRepairerThread.Handle,0); // bang!
                      fRepairerThread.ResumeProcessing;
                      fRepairerThread := nil;
                      // put manager into ready state
                      fManagerStatus := mstReady;
                      DoManagerStatus;
                    end;
  else
    RunDeferredThreadDestruction;
  end;
end;

//------------------------------------------------------------------------------

procedure TFileManager.EndProcessingAndWait;
begin
If Assigned(fRepairerThread) then
  begin
    fManagerStatus := mstTerminating;
    DoManagerStatus;
    fRepairerThread.StopProcessing;   // does not stop the thread, only sets a flag
    fRepairerThread.ResumeProcessing; // in case it is paused
    fRepairerThread.WaitFor;
    fRepairerThread := nil; // the thread should be in defered destruction list by now
  end;
RunDeferredThreadDestruction;
end;

end.
