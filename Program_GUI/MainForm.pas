unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, XPMan, StdCtrls, Menus, ImgList,
  DART_ProcessingManager;

type
{$IFNDEF FPC}
  TListView = class(ComCtrls.TListView)
  protected
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  end;
{$ENDIF}

  TfMainForm = class(TForm)
    lblArchiveList: TLabel;
    lvArchiveList: TListView;
    btnStartProcessing: TButton;
    bvlProgressSplit: TBevel;
    lblOverallProgress: TLabel;
    pbOverallProgress: TProgressBar;
    lblArchiveProgress: TLabel;
    pbArchiveProgress: TProgressBar;
    sbStatusBar: TStatusBar;
    tmrHearbeatTimer: TTimer;    
    tmrAnimTimer: TTimer;    
    ilArchiveList: TImageList;
    diaOpenArchive: TOpenDialog;
    pmArchiveListMenu: TPopupMenu;
    pmiAL_Add: TMenuItem;
    pmiAL_Remove: TMenuItem;
    pmiAL_Clear: TMenuItem;
    N1: TMenuItem;
    pmiAL_ProcessingSettings: TMenuItem;
    pmiAL_ResultInfo: TMenuItem;
    N2: TMenuItem;
    pmiAL_ClearCompleted: TMenuItem;
    N3: TMenuItem;
    pmiAL_Tools: TMenuItem;
    oXPManifest: TXPManifest;
    btnPauseProcessing: TButton;
    btnStopProcessing: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lvArchiveListDblClick(Sender: TObject);
    procedure pmArchiveListMenuPopup(Sender: TObject);
    procedure pmiAL_AddClick(Sender: TObject);
    procedure pmiAL_RemoveClick(Sender: TObject);
    procedure pmiAL_ClearClick(Sender: TObject);
    procedure pmiAL_ProcessingSettingsClick(Sender: TObject);
    procedure pmiAL_ResultInfoClick(Sender: TObject);
    procedure pmiAL_ClearCompletedClick(Sender: TObject);
    procedure btnStartProcessingClick(Sender: TObject);
    procedure btnPauseProcessingClick(Sender: TObject);
    procedure btnStopProcessingClick(Sender: TObject);
    procedure tmrHearbeatTimerTimer(Sender: TObject);    
    procedure tmrAnimTimerTimer(Sender: TObject);
  private
    fDefaultAppTitle:     String;
    fProgressAppTitle:    String;
    fDefaultFormCaption:  String;
    fProgressFormCaption: String;
  public
    ProcessingManager:  TDARTProcessingManager;
    procedure LoadCopyrightInfo;
    procedure LoadFilesFromParams;
    procedure SetDropAccept(AcceptDrop: Boolean);
    procedure OnArchiveProgress(Sender: TObject; ArchiveIndex: Integer);
    procedure OnArchiveStatus(Sender: TObject; ArchiveIndex: Integer);
    procedure OnManagerStatus(Sender: TObject);
  end;

var
  fMainForm: TfMainForm;

implementation

{$R *.dfm}

uses
  ShellAPI,
  StrRect, WinFileInfo, TaskbarProgress,
  DART_Auxiliary, DART_ProcessingSettings,
  ResultInfoForm, ProcSettingsForm;

const
  ANIM_IMG_Processing_First = 9;
  ANIM_IMG_Processing_Count = 8;
  ANIM_IMG_Heartbeat_First  = 17;
  ANIM_IMG_Heartbeat_Count  = 5;

//LIST_COLUMN_Icon   = -1;
//LIST_COLUMN_Name   = 0;
//LIST_COLUMN_Size   = 1;
  LIST_COLUMN_Type   = 2;
  LIST_COLUMN_Method = 3;
  LIST_COLUMN_State  = 4;

{$IFNDEF FPC}
procedure TListView.WMDropFiles(var Msg: TWMDropFiles);
var
  FileCount:  LongWord;
  i:          LongWord;
  FileName:   String;
begin
inherited;
try
  FileCount := DragQueryFile(Msg.Drop,$FFFFFFFF,nil,0);
  If (fMainForm.ProcessingManager.Status = pmsReady) and (FileCount > 0) then
    begin
      For i := 0 to Pred(FileCount) do
        begin
          SetLength(FileName,DragQueryFile(Msg.Drop,i,nil,0) + 1);
          DragQueryFile(Msg.Drop,i,PChar(FileName),Length(FileName));
          SetLength(FileName,Length(FileName) - 1);
          FileName := WinToStr(FileName);
          If DART_FileExists(FileName) then
            fMainForm.ProcessingManager.Add(FileName);
        end;
    end;
finally
  DragFinish(Msg.Drop);
end;
end;
{$ENDIF}

//==============================================================================

procedure TfMainForm.LoadCopyrightInfo;
begin
with TWinFileInfo.Create(WFI_LS_LoadVersionInfo or WFI_LS_LoadFixedFileInfo or WFI_LS_DecodeFixedFileInfo) do
try
  sbStatusBar.Panels[0].Text :=
    VersionInfoValues[VersionInfoTranslations[0].LanguageStr,'LegalCopyright'] + ', version ' +
    VersionInfoValues[VersionInfoTranslations[0].LanguageStr,'ProductVersion'] + ' (' +
    {$IFDEF FPC}'L'{$ELSE}'D'{$ENDIF}{$IFDEF x64}+ '64 '{$ELSE}+ '32 '{$ENDIF} +
    '#' + IntToStr(VersionInfoFixedFileInfoDecoded.FileVersionMembers.Build)
    {$IFDEF Debug}+ ' debug'{$ENDIF} + ')';
finally
  Free;
end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.LoadFilesFromParams;
var
  i:  Integer;
begin
If ParamCount > 0 then
  For i := 1 to ParamCount do
    If Length(ParamStr(i)) > 0 then
      If DART_FileExists(RTLToStr(ParamStr(i))) then
        ProcessingManager.Add(DART_ExpandFileName(RTLToStr(ParamStr(i))));
end;

//------------------------------------------------------------------------------

procedure TfMainForm.SetDropAccept(AcceptDrop: Boolean);
begin
DragAcceptFiles(lvArchiveList.Handle,AcceptDrop);
{$IFDEF FPC}
Self.AllowDropFiles := True;
DragAcceptFiles(Handle,False);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TfMainForm.OnArchiveProgress(Sender: TObject; ArchiveIndex: Integer);
var
  Temp: Integer;
begin
If ProcessingManager[ArchiveIndex].ProcessingStatus in [apsProcessing,apsPaused] then
  begin
    // entry progress
    Temp := Trunc(pbArchiveProgress.Max * ProcessingManager[ArchiveIndex].ProgressStageNode.Progress);
    If Temp <> pbArchiveProgress.Position then
      begin
        lvArchiveList.Items[ArchiveIndex].SubItems[LIST_COLUMN_State] :=
          Format(DART_ArchiveProcessingStatusStrings[ProcessingManager[ArchiveIndex].ProcessingStatus],
            [ProcessingManager[ArchiveIndex].ProgressStageNode.Progress * 100]);
        pbArchiveProgress.Position := Temp;
      end;
    // overall progress
    Temp := Trunc(pbOverallProgress.Max * ProcessingManager.Progress);
    If Temp <> pbOverallProgress.Position then
      begin
        pbOverallProgress.Position := Temp;
        If ProcessingManager.Status in [pmsProcessing,pmsTerminating,pmsPaused,pmsPausedTerminating] then
          begin
            Application.Title := Format(fProgressAppTitle,[ProcessingManager.Progress * 100]);
            Caption := Format(fProgressFormCaption,[ProcessingManager.Progress * 100]);
            SetTaskbarProgressValue(ProcessingManager.Progress);
          end;  
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.OnArchiveStatus(Sender: TObject; ArchiveIndex: Integer);
begin
lvArchiveList.Items[ArchiveIndex].SubItems[LIST_COLUMN_State] :=
  Format(DART_ArchiveProcessingStatusStrings[ProcessingManager[ArchiveIndex].ProcessingStatus],
    [ProcessingManager[ArchiveIndex].ProgressStageNode.Progress * 100]);
lvArchiveList.Items[ArchiveIndex].ImageIndex := Ord(ProcessingManager[ArchiveIndex].ProcessingStatus);
lvArchiveList.Items[ArchiveIndex].MakeVisible(False);
tmrAnimTimer.Tag := 0;
tmrAnimTimer.Enabled := ProcessingManager[ArchiveIndex].ProcessingStatus in [apsProcessing,apsHeartbeat];
If tmrAnimTimer.Enabled then
  tmrAnimTimer.OnTimer(nil);
end;

//------------------------------------------------------------------------------

procedure TfMainForm.OnManagerStatus(Sender: TObject);
begin
{
  Possible changes of processing manager states:

    pmsReady              ->  pmsProcessing
    pmsProcessing         ->  pmsReady
    pmsProcessing         ->  pmsPaused
    pmsProcessing         ->  pmsTerminating
    pmsPaused             ->  pmsProcessing
    pmsTerminating        ->  pmsReady
    pmsTerminating        ->  pmsPausedTerminating
    pmsPausedTerminating  ->  pmsTerminating
}
pmArchiveListMenu.OnPopup(nil);
SetDropAccept(ProcessingManager.Status = pmsReady);
btnStartProcessing.Visible := ProcessingManager.Status = pmsReady;
btnPauseProcessing.Visible := ProcessingManager.Status <> pmsReady;
btnStopProcessing.Visible := ProcessingManager.Status <> pmsReady;
case ProcessingManager.Status of
  pmsPaused:      begin
                    btnPauseProcessing.Caption := 'Resume processing';
                    SetTaskbarProgressState(tpsPaused);
                  end;
  pmsPausedTerminating:
                  begin
                    btnPauseProcessing.Caption := 'Resume termination';
                    SetTaskbarProgressState(tpsPaused);
                  end;
  pmsReady:       begin
                    btnPauseProcessing.Caption := 'Pause processing';
                    btnStopProcessing.Caption := 'Stop processing';
                    Application.Title := fDefaultAppTitle;
                    Caption := fDefaultFormCaption;
                    SetTaskbarProgressState(tpsNoProgress);
                  end;
  pmsProcessing:  begin
                    btnPauseProcessing.Caption := 'Pause processing';
                    SetTaskbarProgressState(tpsNormal);
                  end;
  pmsTerminating: begin
                    btnPauseProcessing.Caption := 'Pause termination';
                    btnStopProcessing.Caption := 'Terminating, please wait...';
                    SetTaskbarProgressState(tpsNormal);
                  end;
end;
end;

//==============================================================================

procedure TfMainForm.FormCreate(Sender: TObject);
begin
lvArchiveList.DoubleBuffered := True;
pbOverallProgress.DoubleBuffered := True;
pbArchiveProgress.DoubleBuffered := True;
sbStatusBar.DoubleBuffered := True;
fDefaultAppTitle := Application.Title;
fProgressAppTitle := fDefaultAppTitle + ' - %.0f%%';
fDefaultFormCaption := Caption;
fProgressFormCaption := fDefaultFormCaption + ' (%.0f%%)';
LoadCopyrightInfo;
ProcessingManager := TDARTProcessingManager.Create(lvArchiveList,True);
ProcessingManager.OnArchiveProgress := OnArchiveProgress;
ProcessingManager.OnArchiveStatus := OnArchiveStatus;
ProcessingManager.OnManagerStatus := OnManagerStatus;
tmrHearbeatTimer.Enabled := True;
OnManagerStatus(nil);
diaOpenArchive.InitialDir := ExtractFileDir(RTLToStr(ParamStr(0)));
pmiAL_ProcessingSettings.ShortCut := ShortCut(Ord('S'),[ssAlt]);
pmiAL_ResultInfo.ShortCut := ShortCut(Ord('R'),[ssAlt]);
pmiAL_ClearCompleted.ShortCut := ShortCut(Ord('C'),[ssAlt]);
LoadFilesFromParams;
SetDropAccept(True);
end;

//------------------------------------------------------------------------------

procedure TfMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
ProcessingManager.EndProcessingAndWait;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.FormDestroy(Sender: TObject);
begin
SetDropAccept(False);
ProcessingManager.Free;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.FormResize(Sender: TObject);
var
  i:        Integer;
  NewWidth: Integer;
begin
// list view
NewWidth := lvArchiveList.Width - (2 * GetSystemMetrics(SM_CXEDGE)) - GetSystemMetrics(SM_CXVSCROLL);
For i := 0 to Pred(lvArchiveList.Columns.Count) do
  If i <> 1 then Dec(NewWidth,lvArchiveList.Columns[i].Width);
lvArchiveList.Columns[1].Width := NewWidth;
// buttons 
btnPauseProcessing.Width := (ClientWidth - 24) div 2;
btnStopProcessing.Left := btnPauseProcessing.Left + btnPauseProcessing.Width + 8;
btnStopProcessing.Width := (ClientWidth - 24) div 2;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.lvArchiveListDblClick(Sender: TObject);
begin
If ProcessingManager.Status = pmsReady then
  begin
    If lvArchiveList.ItemIndex >= 0 then
      begin
        case ProcessingManager[lvArchiveList.ItemIndex].ProcessingStatus of
          apsSuccess,apsWarning,apsError:
            pmiAL_ResultInfo.OnClick(nil);
        else
          pmiAL_ProcessingSettings.OnClick(nil);
        end;
      end
    else pmiAL_Add.OnClick(nil);
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.pmArchiveListMenuPopup(Sender: TObject);
begin
pmiAL_Add.Enabled := ProcessingManager.Status = pmsReady;
pmiAL_Remove.Enabled := (ProcessingManager.Status = pmsReady) and (lvArchiveList.SelCount > 0);
pmiAL_Clear.Enabled := (ProcessingManager.Status = pmsReady) and (lvArchiveList.Items.Count > 0);
pmiAL_ProcessingSettings.Enabled := (ProcessingManager.Status = pmsReady) and (lvArchiveList.SelCount = 1);
If (ProcessingManager.Status = pmsReady) and (lvArchiveList.SelCount = 1) then
  pmiAL_ResultInfo.Enabled := ProcessingManager[lvArchiveList.Selected.Index].ProcessingStatus in [apsSuccess,apsWarning,apsError]
else
  pmiAL_ResultInfo.Enabled := False;
pmiAL_ClearCompleted.Enabled := (ProcessingManager.Status = pmsReady) and (ProcessingManager.CompletedItemCount > 0);
end;
 
//------------------------------------------------------------------------------

procedure TfMainForm.pmiAL_AddClick(Sender: TObject);
var
  i:  Integer;
begin
If ProcessingManager.Status = pmsReady then
  If diaOpenArchive.Execute then
    begin
      lvArchiveList.Items.BeginUpdate;
      try
        For i := 0 to Pred(diaOpenArchive.Files.Count) do
          ProcessingManager.Add(diaOpenArchive.Files[i]);
      finally
        lvArchiveList.Items.EndUpdate;
      end;
    end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.pmiAL_RemoveClick(Sender: TObject);
var
  i:        Integer;
  MsgText:  String;
begin
If (ProcessingManager.Status = pmsReady) and (lvArchiveList.SelCount > 0) then
  begin
    If lvArchiveList.SelCount > 1 then
      MsgText := 'Are you sure you want to remove selected files (%d)?'
    else
      MsgText := 'Are you sure you want to remove selected file?';
    If MessageDlg(Format(MsgText,[lvArchiveList.SelCount]),mtConfirmation,[mbYes,mbNo],0) = mrYes then
      For i := Pred(lvArchiveList.Items.Count) downto 0 do
        If lvArchiveList.Items[i].Selected then
          ProcessingManager.Delete(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.pmiAL_ClearClick(Sender: TObject);
begin
If (ProcessingManager.Status = pmsReady) and (lvArchiveList.Items.Count > 0) then
  If MessageDlg('Are you sure you want to clear the entire list?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
    ProcessingManager.Clear;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.pmiAL_ProcessingSettingsClick(Sender: TObject);
begin
If (ProcessingManager.Status = pmsReady) and (lvArchiveList.SelCount = 1) then
  begin
    fProcSettingsForm.ShowProcessingSettings(ProcessingManager.Pointers[lvArchiveList.ItemIndex]^.ProcessingSettings);
    lvArchiveList.Items[lvArchiveList.ItemIndex].SubItems[LIST_COLUMN_Type] :=
      DART_ArchiveTypeStrings[ProcessingManager[lvArchiveList.ItemIndex].ProcessingSettings.Common.SelectedArchiveType];
    lvArchiveList.Items[lvArchiveList.ItemIndex].SubItems[LIST_COLUMN_Method] :=
      Format(DART_RepairMethodStrings[ProcessingManager[lvArchiveList.ItemIndex].ProcessingSettings.Common.RepairMethod],
             [DART_KnownArchiveTypeStrings[ProcessingManager[lvArchiveList.ItemIndex].ProcessingSettings.Common.ConvertTo]]);
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.pmiAL_ResultInfoClick(Sender: TObject);
begin
If (ProcessingManager.Status = pmsReady) and (lvArchiveList.SelCount = 1) then
  If ProcessingManager[lvArchiveList.Selected.Index].ProcessingStatus in [apsSuccess,apsWarning,apsError] then
    fResultInfoForm.ShowResultInformation(ProcessingManager[lvArchiveList.ItemIndex]);
end;

//------------------------------------------------------------------------------

procedure TfMainForm.pmiAL_ClearCompletedClick(Sender: TObject);
begin
If (ProcessingManager.Status = pmsReady) then
  ProcessingManager.ClearCompleted;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.btnStartProcessingClick(Sender: TObject);
begin
If (ProcessingManager.Count > 0) and (ProcessingManager.Status = pmsReady) then
  ProcessingManager.StartProcessing
else
  MessageDlg('There are no files selected for processing.',mtInformation,[mbOK],0);
end;

//------------------------------------------------------------------------------

procedure TfMainForm.btnPauseProcessingClick(Sender: TObject);
begin
case ProcessingManager.Status of
  pmsProcessing,pmsTerminating:   ProcessingManager.PauseProcessing;
  pmsPaused,pmsPausedTerminating: ProcessingManager.ResumeProcessing;
end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.btnStopProcessingClick(Sender: TObject);
begin
case ProcessingManager.Status of
  pmsPaused,pmsProcessing:
    ProcessingManager.StopProcessing;

  pmsPausedTerminating,pmsTerminating:
    begin
      ProcessingManager.PauseProcessing;
      If MessageDlg('The program is waiting for the processing thread to be normally terminated.'+ sLineBreak +
                    'You can initiate forced termination, but it will cause resource leak and other problems.' + sLineBreak +
                    'In that case, you are strongly advised to restart the program before further use.' + sLineBreak + sLineBreak +
                    'Are you sure you want to force processing thread to terminate?' ,mtWarning,[mbYes,mbNo],0) = mrYes then
        ProcessingManager.StopProcessing
      else
        ProcessingManager.ResumeProcessing;
    end;
end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.tmrHearbeatTimerTimer(Sender: TObject);
begin
ProcessingManager.HeartbeatCheck;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.tmrAnimTimerTimer(Sender: TObject);
begin
If ProcessingManager.ProcessedArchiveIndex >= 0 then
  case ProcessingManager[ProcessingManager.ProcessedArchiveIndex].ProcessingStatus of
    apsProcessing:
      begin
        If tmrAnimTimer.Tag > Pred(ANIM_IMG_Processing_Count) then
          tmrAnimTimer.Tag := 0;
        lvArchiveList.Items[ProcessingManager.ProcessedArchiveIndex].ImageIndex :=
          ANIM_IMG_Processing_First + tmrAnimTimer.Tag;
        tmrAnimTimer.Tag := tmrAnimTimer.Tag + 1
      end;
    apsHeartbeat:
      begin
        If tmrAnimTimer.Tag > Pred(ANIM_IMG_Heartbeat_Count * 2) then
          tmrAnimTimer.Tag := 0;
        If tmrAnimTimer.Tag <= Pred(ANIM_IMG_Heartbeat_Count) then
          lvArchiveList.Items[ProcessingManager.ProcessedArchiveIndex].ImageIndex :=
          ANIM_IMG_Heartbeat_First + tmrAnimTimer.Tag;
        tmrAnimTimer.Tag := tmrAnimTimer.Tag + 1
      end;
  end;
end;

end.
