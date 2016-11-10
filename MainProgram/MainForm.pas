{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit MainForm;

{$INCLUDE 'Source\DART_defs.inc'}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Menus, {$IFNDEF FPC}Messages, ImgList, XPMan,{$ENDIF}
  DART_FileManager;

type
{$IFNDEF FPC}
  TListView = class(ComCtrls.TListView)
  protected
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  end;
{$ENDIF}

  { TfMainForm }

  TfMainForm = class(TForm)
    lblFiles: TLabel;
    lvFiles: TListView;
    btnProcessing: TButton;
    bvlProgressSplit: TBevel;
    lblOverallProgress: TLabel;
    prbOverallProgress: TProgressBar;
    lblFileProgress: TLabel;
    prbFileProgress: TProgressBar;
    stbStatusBar: TStatusBar;
  {$IFNDEF FPC}
    oXPManifest: TXPManifest;
  {$ENDIF}
    mnuFiles: TPopupMenu;
    mfAdd: TMenuItem;
    mfRemove: TMenuItem;
    mfClear: TMenuItem;
    N1: TMenuItem;
    mfSettings: TMenuItem;
    mfResultInfo: TMenuItem;
    N2: TMenuItem;
    mfClearCompleted: TMenuItem;
    diaOpenDialog: TOpenDialog;
    tmrAnimTimer: TTimer;
    imlFileIcons: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lvFilesDblClick(Sender: TObject);
    procedure mnuFilesPopup(Sender: TObject);
    procedure mfAddClick(Sender: TObject);
    procedure mfRemoveClick(Sender: TObject);
    procedure mfClearClick(Sender: TObject);
    procedure mfSettingsClick(Sender: TObject);
    procedure mfResultInfoClick(Sender: TObject);
    procedure mfClearCompletedClick(Sender: TObject);
    procedure btnProcessingClick(Sender: TObject);
    procedure tmrAnimTimerTimer(Sender: TObject);
  {$IFDEF FPC}
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  {$ENDIF}
  protected
    fDefaultAppTitle:     String;
    fProgressAppTitle:    String;
    fDefaultFormCaption:  String;
    fProgressFormCaption: String;
  public
    FileManager:  TFileManager;
    procedure LoadCopyrightInfo;
    procedure LoadFilesFromParams;
    procedure SetDropAccept(AcceptDrop: Boolean);    
    procedure OnFileProgress(Sender: TObject; FileIndex: Integer);
    procedure OnFileStatus(Sender: TObject; FileIndex: Integer);
    procedure OnManagerStatus(Sender: TObject);
  end;

var
  fMainForm: TfMainForm;

implementation

uses
  Windows, ShellAPI,
  WinFileInfo, TaskbarProgress,
  DART_ProcessingSettings, DART_Repairer,
  ResultInfoForm, PrcsSettingsForm
{$IFDEF FPC_NonUnicode}
  , LazUTF8
  {$IFDEF FPC_NonUnicode_NoUTF8RTL}
  , LazFileUtils
  {$ENDIF}
{$ENDIF};

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

const
  AnimImgIdx_First = 6;
  AnimImgIdx_Count = 7;

{%H-}List_IconColumn   = -1;
{%H-}List_NameColumn   = 0;
{%H-}List_SizeColumn   = 1;
     List_TypeColumn   = 2;
     List_MethodColumn = 3;
     List_StateColumn  = 4;

//==============================================================================

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
  If (fMainForm.FileManager.ManagerStatus = mstReady) and (FileCount > 0) then
    begin
      For i := 0 to Pred(FileCount) do
        begin
          SetLength(FileName,DragQueryFile(Msg.Drop,i,nil,0) + 1);
          DragQueryFile(Msg.Drop,i,PChar(FileName),Length(FileName));
          SetLength(FileName,Length(FileName) - 1);
          If FileExists(FileName) then
            fMainForm.FileManager.Add(FileName);
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
  begin
    stbStatusBar.Panels[0].Text :=
      VersionInfoValues[VersionInfoTranslations[0].LanguageStr,'LegalCopyright'] + ', version ' +
      VersionInfoValues[VersionInfoTranslations[0].LanguageStr,'ProductVersion'] + ' ' +
      {$IFDEF FPC}'L'{$ELSE}'D'{$ENDIF}{$IFDEF x64}+ '64 '{$ELSE}+ '32 '{$ENDIF} + zlib_method_str +
      ' #' + IntToStr(VersionInfoFixedFileInfoDecoded.FileVersionMembers.Build)
      {$IFDEF Debug}+ ' debug'{$ENDIF};
    Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.LoadFilesFromParams;
var
  i:        Integer;
  FileName: String;
begin
If ParamCount > 0 then
  For i := 1 to ParamCount do
    begin
    {$IFDEF FPC_NonUnicode_NoUTF8RTL}
      FileName := SysToUTF8(ParamStr(i));
      If FileExistsUTF8(FileName) then
        FileManager.Add(ExpandFileNameUTF8(FileName));
    {$ELSE}
      FileName := ParamStr(i);
      If FileExists(FileName) then
        FileManager.Add(ExpandFileName(FileName));
    {$ENDIF}
    end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.SetDropAccept(AcceptDrop: Boolean);
begin
DragAcceptFiles(lvFiles.Handle,AcceptDrop);
{$IFDEF FPC}
Self.AllowDropFiles := True;
DragAcceptFiles(Handle,False);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TfMainForm.OnFileProgress(Sender: TObject; FileIndex: Integer);
var
  Temp: Integer;
begin
If FileManager[FileIndex].ProcessingStatus = fstProcessing then
  begin
    // entry progress
    Temp := Trunc(prbFileProgress.Max * FileManager[FileIndex].Progress);
    If Temp <> prbFileProgress.Position then
      begin
        lvFiles.Items[FileIndex].SubItems[List_StateColumn] := Format('Processing... %.0f%%',[FileManager[FileIndex].Progress * 100]);
        prbFileProgress.Position := Temp;
      end;
    // overall progress
    Temp := Trunc(prbOverallProgress.Max * FileManager.GlobalProgress);
    If Temp <> prbOverallProgress.Position then
      begin
        prbOverallProgress.Position := Temp;
        If FileManager.ManagerStatus in [mstProcessing, mstTerminating] then
          begin
            Application.Title := Format(fProgressAppTitle,[FileManager.GlobalProgress * 100]);
            Caption := Format(fProgressFormCaption,[FileManager.GlobalProgress * 100]);
            SetTaskbarProgressValue(FileManager.GlobalProgress);
          end;  
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.OnFileStatus(Sender: TObject; FileIndex: Integer);
begin
lvFiles.Items[FileIndex].SubItems[List_StateColumn] := FileProcessingStatusStrArr[FileManager[FileIndex].ProcessingStatus];
lvFiles.Items[FileIndex].ImageIndex := Ord(FileManager[FileIndex].ProcessingStatus);
If FileManager[FileIndex].ProcessingStatus <> fstProcessing then
  tmrAnimTimer.Tag := 0;
tmrAnimTimer.Enabled := FileManager[FileIndex].ProcessingStatus = fstProcessing
end;

//------------------------------------------------------------------------------

procedure TfMainForm.OnManagerStatus(Sender: TObject);
begin
mnuFiles.OnPopup(nil);
SetDropAccept(FileManager.ManagerStatus = mstReady);
case FileManager.ManagerStatus of
  mstReady:       begin
                    btnProcessing.Caption := 'Start processing';
                    Application.Title := fDefaultAppTitle;
                    Caption := fDefaultFormCaption;
                    SetTaskbarProgressState(tpsNoProgress);
                  end;
  mstProcessing:  begin
                    btnProcessing.Caption := 'Stop processing';
                    SetTaskbarProgressState(tpsNormal);
                  end;
  mstTerminating: btnProcessing.Caption := 'Terminating processing, please wait...';
end;
end;

//==============================================================================

procedure TfMainForm.FormCreate(Sender: TObject);
begin
lvFiles.DoubleBuffered := True;
prbOverallProgress.DoubleBuffered := True;
prbFileProgress.DoubleBuffered := True;
stbStatusBar.DoubleBuffered := True;
fDefaultAppTitle := Application.Title;
fProgressAppTitle := fDefaultAppTitle + ' - %.0f%%';
fDefaultFormCaption := Caption;
fProgressFormCaption := fDefaultFormCaption + ' (%.0f%%)';
LoadCopyrightInfo;
FileManager := TFileManager.Create(lvFiles);
FileManager.OnFileProgress := OnFileProgress;
FileManager.OnFileStatus := OnFileStatus;
FileManager.OnManagerStatus := OnManagerStatus;
OnManagerStatus(nil);
{$IFDEF FPC_NonUnicode}
diaOpenDialog.InitialDir := ExtractFileDir(SysToUTF8(ParamStr(0)));
{$ELSE}
diaOpenDialog.InitialDir := ExtractFileDir(ParamStr(0));
{$ENDIF}
mfSettings.ShortCut := ShortCut(Ord('S'),[ssAlt]);
mfResultInfo.ShortCut := ShortCut(Ord('R'),[ssAlt]);
mfClearCompleted.ShortCut := ShortCut(Ord('C'),[ssAlt]);
LoadFilesFromParams;
SetDropAccept(True);
end;

//------------------------------------------------------------------------------

procedure TfMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
FileManager.EndProcessingAndWait;
end;
   
//------------------------------------------------------------------------------

procedure TfMainForm.FormDestroy(Sender: TObject);
begin
SetDropAccept(False);
FileManager.Free;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.FormResize(Sender: TObject);
var
  i:        Integer;
  NewWidth: Integer;
begin
NewWidth := lvFiles.Width - (2 * GetSystemMetrics(SM_CXEDGE)) - GetSystemMetrics(SM_CXVSCROLL);
For i := 0 to Pred(lvFiles.Columns.Count) do
  If i <> 1 then Dec(NewWidth,lvFiles.Columns[i].Width);
lvFiles.Columns[1].Width := NewWidth;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.lvFilesDblClick(Sender: TObject);
begin
If FileManager.ManagerStatus = mstReady then
  begin
    If lvFiles.ItemIndex >= 0 then
      begin
        case FileManager[lvFiles.ItemIndex].ProcessingStatus of
          fstSuccess,fstWarning,fstError:
            mfResultInfo.OnClick(nil);
        else
          mfSettings.OnClick(nil);
        end;
      end
    else mfAdd.OnClick(nil);
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.mnuFilesPopup(Sender: TObject);
begin
mfAdd.Enabled := FileManager.ManagerStatus = mstReady;
mfRemove.Enabled := (FileManager.ManagerStatus = mstReady) and (lvFiles.SelCount > 0);
mfClear.Enabled := (FileManager.ManagerStatus = mstReady) and (lvFiles.Items.Count > 0);
mfSettings.Enabled := (FileManager.ManagerStatus = mstReady) and (lvFiles.SelCount = 1);
If (FileManager.ManagerStatus = mstReady) and (lvFiles.SelCount = 1) then
  mfResultInfo.Enabled := FileManager[lvFiles.Selected.Index].ProcessingStatus in [fstSuccess,fstWarning,fstError]
else mfResultInfo.Enabled := False;
mfClearCompleted.Enabled := (FileManager.ManagerStatus = mstReady) and FileManager.ContainsCompletedItem;
end;
   
//------------------------------------------------------------------------------

procedure TfMainForm.mfAddClick(Sender: TObject);
var
  i:  Integer;
begin
If FileManager.ManagerStatus = mstReady then
  If diaOpenDialog.Execute then
    begin
      lvFiles.Items.BeginUpdate;
      try
        For i := 0 to Pred(diaOpenDialog.Files.Count) do
          FileManager.Add(diaOpenDialog.Files[i]);
      finally
        lvFiles.Items.EndUpdate;
      end;
    end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.mfRemoveClick(Sender: TObject);
var
  i:        Integer;
  MsgText:  String;
begin
If (FileManager.ManagerStatus = mstReady) and (lvFiles.SelCount > 0) then
  begin
    If lvFiles.SelCount > 1 then
      MsgText := 'Are you sure you want to remove selected files (%d)?'
    else
      MsgText := 'Are you sure you want to remove selected file?';
    If MessageDlg(Format(MsgText,[lvFiles.SelCount]),mtConfirmation,[mbYes,mbNo],0) = mrYes then
      For i := Pred(lvFiles.Items.Count) downto 0 do
        If lvFiles.Items[i].Selected then
          FileManager.Delete(i);
  end;
end;
   
//------------------------------------------------------------------------------

procedure TfMainForm.mfClearClick(Sender: TObject);
begin
If (FileManager.ManagerStatus = mstReady) and (lvFiles.Items.Count > 0) then
  If MessageDlg('Are you sure you want to clear the entire list?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
    FileManager.Clear;
end;
   
//------------------------------------------------------------------------------

procedure TfMainForm.mfSettingsClick(Sender: TObject);
begin
If (FileManager.ManagerStatus = mstReady) and (lvFiles.SelCount = 1) then
  begin
    fPrcsSettingsForm.ShowProcessingSettings(FileManager.Pointers[lvFiles.ItemIndex]^.ProcessingSettings);
    lvFiles.Items[lvFiles.ItemIndex].SubItems[List_TypeColumn] :=
      FileTypeStrArr[FileManager[lvFiles.ItemIndex].ProcessingSettings.Common.FileType];
    lvFiles.Items[lvFiles.ItemIndex].SubItems[List_MethodColumn] :=
      RepairerMethodStrArr[FileManager[lvFiles.ItemIndex].ProcessingSettings.Common.RepairMethod];
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.mfResultInfoClick(Sender: TObject);
begin
If (FileManager.ManagerStatus = mstReady) and (lvFiles.SelCount = 1) then
  If FileManager[lvFiles.Selected.Index].ProcessingStatus in [fstSuccess,fstWarning,fstError] then
    fResultInfoForm.ShowResultInformation(FileManager[lvFiles.ItemIndex]);
end;
    
//------------------------------------------------------------------------------

procedure TfMainForm.mfClearCompletedClick(Sender: TObject);
begin
If (FileManager.ManagerStatus = mstReady) then
  FileManager.ClearCompleted;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.btnProcessingClick(Sender: TObject);
begin
If FileManager.Count > 0 then
  case FileManager.ManagerStatus of
    mstReady:       FileManager.StartProcessing;
    mstProcessing:  FileManager.StopProcessing;
    mstTerminating: begin
                      FileManager.PauseProcessing;
                      SetTaskbarProgressState(tpsPaused);
                      If MessageDlg('The program is waiting for the processing thread to be normally terminated.'+ sLineBreak +
                                    'You can initiate forced termination, but it will cause resource leak and other problems.' + sLineBreak +
                                    'In that case, you are strongly advised to restart the program before further use.' + sLineBreak + sLineBreak +
                                    'Are you sure you want to force processing thread to terminate?' ,mtWarning,[mbYes,mbNo],0) = mrYes then
                        FileManager.StopProcessing
                      else
                        FileManager.ResumeProcessing;
                    end;
  end
else MessageDlg('There are no files selected for processing.',mtInformation,[mbOK],0);
end;

//------------------------------------------------------------------------------

procedure TfMainForm.tmrAnimTimerTimer(Sender: TObject);
begin
If FileManager.ProcessedFileIndex >= 0 then
  begin
    lvFiles.Items[FileManager.ProcessedFileIndex].ImageIndex := AnimImgIdx_First + tmrAnimTimer.Tag;
    If tmrAnimTimer.Tag < AnimImgIdx_Count then
      tmrAnimTimer.Tag := tmrAnimTimer.Tag + 1
    else
      tmrAnimTimer.Tag := 0;
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF FPC}
procedure TfMainForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i:  Integer;
begin
If FileManager.ManagerStatus = mstReady then
  For i := Low(FileNames) to High(FileNames) do
  {$IFDEF FPC_NonUnicode_NoUTF8RTL}
    If FileExistsUTF8(FileNames[i]) then
  {$ELSE}
    If FileExists(FileNames[i]) then
  {$ENDIF}
      FileManager.Add(FileNames[i]);
end;
{$ENDIF}

end.
