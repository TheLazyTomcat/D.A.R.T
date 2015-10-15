{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit FilesManager;

interface

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

uses
  Classes, Repairer;

{==============================================================================}
{------------------------------------------------------------------------------}
{                                 TFilesManager                                }
{------------------------------------------------------------------------------}
{==============================================================================}

type
{$IF defined(FPC) or (CompilerVersion >= 18)}
  TMemSize = UInt64;
{$ELSE}
  TMemSize = Int64;
{$IFEND}

  TFileStatus = (fstUnknown,fstReady,fstProcessing,fstDone,fstError);

  TFileListItem = record
    Path:                 String;
    Name:                 String;
    Size:                 Int64;
    Status:               TFileStatus;
    ProcessingSettings:   TProcessingSettings;
    Progress:             Single;
    ErrorInfo:            TErrorInfo;
    GlobalProgressOffset: Single;
    GlobalProgressRange:  Single;
  end;
  PFileListItem = ^TFileListItem;

  TProgressEvent = procedure(Sender: TObject; FileIndex: Integer) of object;

  TManagerStatus = (mstReady,mstProcessing,mstTerminating);

{==============================================================================}
{   TFilesManager - Class declaration                                          }
{==============================================================================}

  TFilesManager = class(TObject)
  private
    fStatus:            TManagerStatus;
    fFileList:          array of TFileListItem;
    fProcessingFile:    Integer;
    fRepairer:          TRepairerThread;
    fTerminatingThread: array of TThread;
    fProgress:          Single;
    fAvailableMemory:   TMemSize;
    fOnProgress:        TProgressEvent;
    fOnFileStatus:      TProgressEvent;
    fOnStatus:          TNotifyEvent;
    Function GetCount: Integer;
    Function GetPointer(Index: Integer): PFileListItem;
    Function GetItem(Index: Integer): TFileListItem;
  protected
    procedure DoProgress(FileIndex: Integer); virtual;
    procedure DoFileStatus(FileIndex: Integer); virtual;
    procedure DoStatus; virtual;
    procedure GetAvailableMemory; virtual;
    procedure AddTerminatingThread(Thread: TThread); virtual;
    procedure FreeTerminatedThreads; virtual;
    procedure ThreadProgressHandler(Sender: TObject; Progress: Single); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    Function IndexOf(const FilePath: String): Integer; virtual;
    Function Add(const FilePath: String): Integer; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    Function CompletedItems: Boolean; virtual;
    procedure StartProcessing; virtual;
    procedure PauseProcessing; virtual;
    procedure ResumeProcessing; virtual;
    procedure StopProcessing; virtual;
    procedure EndProcessingAndWait; virtual;
    property Pointers[Index: Integer]: PFileListItem read GetPointer;
    property Items[Index: Integer]: TFileListItem read GetItem; default;
  published
    property Status: TManagerStatus read fStatus;
    property Count: Integer read GetCount;
    property ProcessingFile: Integer read fProcessingFile;
    property Progress: Single read fProgress;
    property OnProgress: TProgressEvent read fOnProgress write fOnProgress;
    property OnFileStatus: TProgressEvent read fOnFileStatus write fOnFileStatus;
    property OnStatus: TNotifyEvent read fOnStatus write fOnStatus;
  end;

implementation

uses
  Windows, SysUtils;

type
  TMemoryStatusEx = record
    dwLength:                 LongWord;
    dwMemoryLoad:             LongWord;
    ullTotalPhys:             TMemSize;
    ullAvailPhys:             TMemSize;
    ullTotalPageFile:         TMemSize;
    ullAvailPageFile:         TMemSize;
    ullTotalVirtual:          TMemSize;
    ullAvailVirtual:          TMemSize;
    ullAvailExtendedVirtual:  TMemSize;
  end;
  PMemoryStatusEx = ^TMemoryStatusEx;

Function GlobalMemoryStatusEx(lpBuffer: PMemoryStatusEx): BOOL; stdcall; external kernel32;

{==============================================================================}
{   Auxiliary routines                                                         }
{==============================================================================}

Function GetFileSize(const FilePath: String): Int64;
var
  SearchResult: TSearchRec;
begin
{$IFDEF FPC}
If FindFirst(UTF8ToAnsi(FilePath),faAnyFile,SearchResult) = 0 then
{$ELSE}
If FindFirst(FilePath,faAnyFile,SearchResult) = 0 then
{$ENDIF}
  begin
    {$WARN SYMBOL_PLATFORM OFF}
    Int64Rec(Result).Hi := SearchResult.FindData.nFileSizeHigh;
    Int64Rec(Result).Lo := SearchResult.FindData.nFileSizeLow;
    {$WARN SYMBOL_PLATFORM ON}
    FindClose(SearchResult);
    end
  else Result := 0;
end;

{==============================================================================}
{   TFilesManager - Class implementation                                       }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TFilesManager - Private methods                                            }
{------------------------------------------------------------------------------}

Function TFilesManager.GetCount: Integer;
begin
Result := Length(fFileList);
end;

//------------------------------------------------------------------------------

Function TFilesManager.GetPointer(Index: Integer): PFileListItem;
begin
If (Index >= Low(fFileList)) and (Index <= High(fFileList)) then
  Result := Addr(fFileList[Index])
else
  raise Exception.CreateFmt('TFilesManager.GetPointer: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TFilesManager.GetItem(Index: Integer): TFileListItem;
begin
If (Index >= Low(fFileList)) and (Index <= High(fFileList)) then
  Result := fFileList[Index]
else
  raise Exception.CreateFmt('TFilesManager.GetItem: Index (%d) out of bounds.',[Index]);
end;

{------------------------------------------------------------------------------}
{   TFilesManager - Protected methods                                          }
{------------------------------------------------------------------------------}

procedure TFilesManager.DoProgress(FileIndex: Integer);
begin
If Assigned(fOnProgress) then fOnProgress(Self,FileIndex);
end;

//------------------------------------------------------------------------------

procedure TFilesManager.DoFileStatus(FileIndex: Integer);
begin
If Assigned(fOnFileStatus) then fOnFileStatus(Self,FileIndex);
end;

//------------------------------------------------------------------------------

procedure TFilesManager.DoStatus;
begin
If Assigned(fOnStatus) then fOnStatus(Self);
end;

//------------------------------------------------------------------------------

procedure TFilesManager.GetAvailableMemory;
var
  MemStat:  TMemoryStatusEx;
begin
FillChar({%H-}MemStat,SizeOf(TMemoryStatusEx),0);
MemStat.dwLength := SizeOf(TMemoryStatusEx);
If not GlobalMemoryStatusEx(@MemStat) then
  raise Exception.CreateFmt('GlobalMemoryStatusEx has failed with error %.8x.',[GetLastError]);
{$IFNDEF x64}
If MemStat.ullTotalPhys > $0000000080000000 then
  fAvailableMemory := $0000000080000000
else
{$ENDIF}
  fAvailableMemory := MemStat.ullTotalPhys;
end;

//------------------------------------------------------------------------------

procedure TFilesManager.AddTerminatingThread(Thread: TThread);
begin
SetLength(fTerminatingThread,Length(fTerminatingThread) + 1);
fTerminatingThread[High(fTerminatingThread)] := Thread;
end;

//------------------------------------------------------------------------------

procedure TFilesManager.FreeTerminatedThreads;
var
  i:  Integer;
begin
For i := Low(fTerminatingThread) to High(fTerminatingThread) do
  begin
    fTerminatingThread[i].WaitFor;
    fTerminatingThread[i].Free;
  end;
SetLength(fTerminatingThread,0);
end;

//------------------------------------------------------------------------------

procedure TFilesManager.ThreadProgressHandler(Sender: TObject; Progress: Single);
begin
FreeTerminatedThreads;
If Status in [mstProcessing,mstTerminating] then
  begin
    If fFileList[fProcessingFile].Status <> fstProcessing then
      begin
        fFileList[fProcessingFile].Status := fstProcessing;
        DoFileStatus(fProcessingFile);
      end;
    If (Progress >= 0.0) and (Progress <= 1.0) then
      begin
        fFileList[fProcessingFile].Progress := Progress;
        fProgress := fFileList[fProcessingFile].GlobalProgressOffset +
          fFileList[fProcessingFile].GlobalProgressRange * Progress;
        DoProgress(fProcessingFile);
      end
    else
      begin
        fProgress := fFileList[fProcessingFile].GlobalProgressOffset +
          fFileList[fProcessingFile].GlobalProgressRange;      
        If Progress > 1.0 then
          begin
            fFileList[fProcessingFile].Progress := 1.0;
            DoProgress(fProcessingFile);
            fFileList[fProcessingFile].Status := fstDone;
            DoFileStatus(fProcessingFile);
          end
        else
          begin
            fFileList[fProcessingFile].Progress := 1.0;
            DoProgress(fProcessingFile);            
            fFileList[fProcessingFile].Status := fstError;
            fFileList[fProcessingFile].ErrorInfo := fRepairer.ErrorData;
            DoFileStatus(fProcessingFile);
          end;
        fRepairer.OnProgress := nil;
        If (fProcessingFile < High(fFileList)) and (Status <> mstTerminating) then
          begin
            Inc(fProcessingFile);
            AddTerminatingThread(fRepairer);  
            fRepairer := TRepairerThread.Create(fFileList[fProcessingFile].ProcessingSettings,fFileList[fProcessingFile].Path);
            fRepairer.OnProgress := ThreadProgressHandler;
            fRepairer.Start;
          end
        else
          begin
            AddTerminatingThread(fRepairer); 
            fRepairer := nil;
            fStatus := mstReady;
            fProcessingFile := -1;
          end;
        DoStatus;
      end;
  end;
end;

{------------------------------------------------------------------------------}
{   TFilesManager - Public methods                                             }
{------------------------------------------------------------------------------}

constructor TFilesManager.Create;
begin
inherited;
fStatus := mstReady;
fProcessingFile := -1;
GetAvailableMemory;
end;

//------------------------------------------------------------------------------

destructor TFilesManager.Destroy;
begin
FreeTerminatedThreads;
inherited;
end;

//------------------------------------------------------------------------------

Function TFilesManager.IndexOf(const FilePath: String): Integer;
begin
For Result := Low(fFileList) to High(fFileList) do
  If AnsiSameText(FilePath,fFileList[Result].Path) then Exit;
Result := -1;
end;
 
//------------------------------------------------------------------------------

Function TFilesManager.Add(const FilePath: String): Integer;
begin
If fStatus = mstReady then
  begin
    SetLength(fFileList,Length(fFileList) + 1);
    Result := High(fFileList);
    with fFileList[Result] do
      begin
        Path := FilePath;
        Name := ExtractFileName(Path);
        Size := GetFileSize(Path);
        Status := fstReady;
        ProcessingSettings := DefaultProcessingSettings;
        ProcessingSettings.RepairData := ExtractFilePath(Path) + 'unlocked_' + Name;
        ProcessingSettings.OtherSettings.InMemoryProcessingAllowed := Size <= Trunc(fAvailableMemory * 0.25);
        DoFileStatus(Result);
      end;
  end
else
  raise Exception.Create('TFilesManager.Add: Manager status prevents addition.');
end;
 
//------------------------------------------------------------------------------

procedure TFilesManager.Delete(Index: Integer);
var
  i:  Integer;
begin
If fStatus = mstReady then
  begin
    If (Index >= Low(fFileList)) and (Index <= High(fFileList)) then
      begin
        For i := Index to Pred(High(fFileList)) do
          fFileList[i] := fFileList[i + 1];
        SetLength(fFileList,Length(fFileList) - 1);
      end
    else
      raise Exception.CreateFmt('TFilesManager.Delete: Index (%d) out of bounds.',[Index]);
  end
else
  raise Exception.Create('TFilesManager.Delete: Manager status prevents deletion.');
end;

//------------------------------------------------------------------------------

procedure TFilesManager.Clear;
begin
If fStatus = mstReady then
  SetLength(fFileList,0)
else
  raise Exception.Create('TFilesManager.Clear: Manager status prevents deletion.');
end;

//------------------------------------------------------------------------------

Function TFilesManager.CompletedItems: Boolean;
var
  i:  Integer;
begin
Result := False;
For i := Low(fFileList) to High(fFileList) do
  If Items[i].Status = fstDone then
    begin
      Result := True;
      Break;
    end;
end;

//------------------------------------------------------------------------------

procedure TFilesManager.StartProcessing;
var
  i:            Integer;
  OverallSize:  Int64;
begin
FreeTerminatedThreads;
If (Status = mstReady) and (Length(fFileList) > 0) then
  begin
    OverallSize := 0;
    For i := Low(fFileList) to High(fFileList) do
      with fFileList[i] do
        begin
          Status := fstReady;
          Progress := 0.0;
          Inc(OverallSize,Size);
          DoFileStatus(i);
        end;
    For i := Low(fFileList) to High(fFileList) do
      begin
        If OverallSize > 0 then
          fFileList[i].GlobalProgressRange := fFileList[i].Size / OverallSize
        else
          fFileList[i].GlobalProgressRange := 1 / Length(fFileList);
        If i > Low(fFileList) then
          fFileList[i].GlobalProgressOffset := fFileList[i - 1].GlobalProgressOffset + fFileList[i - 1].GlobalProgressRange
        else
          fFileList[i].GlobalProgressOffset := 0.0;
      end; 
    fProcessingFile := 0;        
    fRepairer := TRepairerThread.Create(fFileList[fProcessingFile].ProcessingSettings,fFileList[fProcessingFile].Path);
    fRepairer.OnProgress := ThreadProgressHandler;
    fStatus := mstProcessing;
    DoStatus;
    fRepairer.Start;
  end;
end;

//------------------------------------------------------------------------------

procedure TFilesManager.PauseProcessing;
begin
If Assigned(fRepairer) then
  fRepairer.Pause;
end;

//------------------------------------------------------------------------------

procedure TFilesManager.ResumeProcessing;
begin
If Assigned(fRepairer) then
  fRepairer.Start;
end;

//------------------------------------------------------------------------------

procedure TFilesManager.StopProcessing;
begin
If Assigned(fRepairer) then
  case fStatus of
    mstProcessing:  begin
                      fStatus := mstTerminating;
                      DoStatus;
                      fRepairer.Stop;
                    end;
    mstTerminating: begin
                      fFileList[fProcessingFile].Status := fstReady;
                      DoFileStatus(fProcessingFile);
                      TerminateThread(fRepairer.Handle,0);
                      fRepairer.OnProgress := nil;
                      ResumeProcessing;
                      fRepairer := nil;
                      fStatus := mstReady;
                      DoStatus;
                    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFilesManager.EndProcessingAndWait;
begin
ResumeProcessing;
If Assigned(fRepairer) then
  begin
    fStatus := mstTerminating;
    fRepairer.Stop;
    fRepairer.WaitFor;
    FreeAndNil(fRepairer);
  end;
FreeTerminatedThreads;  
end;

end.
