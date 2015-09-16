{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, XPMan, ExtCtrls, Menus, ImgList,
  FilesManager;

type
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
    oXPManifest: TXPManifest;
    mnuFiles: TPopupMenu;
    mfAdd: TMenuItem;
    mfRemove: TMenuItem;
    mfClear: TMenuItem;
    N1: TMenuItem;
    mfSettings: TMenuItem;
    mfErrorInfo: TMenuItem;
    N2: TMenuItem;
    mfClearCompleted: TMenuItem;
    diaOpenDialog: TOpenDialog;
    tmrAnimTimer: TTimer;
    imlFileIcons: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure lvFilesDblClick(Sender: TObject);
    procedure lvFilesInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
    procedure mnuFilesPopup(Sender: TObject);
    procedure mfAddClick(Sender: TObject);
    procedure mfRemoveClick(Sender: TObject);
    procedure mfClearClick(Sender: TObject);
    procedure mfSettingsClick(Sender: TObject);
    procedure mfErrorInfoClick(Sender: TObject);
    procedure mfClearCompletedClick(Sender: TObject);
    procedure btnProcessingClick(Sender: TObject); 
    procedure tmrAnimTimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    FilesManager: TFilesManager;
    procedure OnProgress(Sender: TObject; FileIndex: Integer);
    procedure OnFileStatus(Sender: TObject; FileIndex: Integer);
    procedure OnStatus(Sender: TObject);
  end;

var
  fMainForm: TfMainForm;

implementation

uses
  ErrorForm, PrcsSettingsForm;

{$R *.dfm}

procedure TfMainForm.OnProgress(Sender: TObject; FileIndex: Integer);
var
  Temp: Integer;
begin
If FilesManager[FileIndex].Status = fstProcessing then
  begin
    Temp := Trunc(prbFileProgress.Max * FilesManager[FileIndex].Progress);
    If Temp <> prbFileProgress.Position then
      begin
        lvFiles.Items[FileIndex].SubItems[1] := Format('Processing... %.0f%%',[FilesManager[FileIndex].Progress * 100]);
        prbFileProgress.Position := Temp;
      end;
    Temp := Trunc(prbOverallProgress.Max * FilesManager.Progress);
    If Temp <> prbOverallProgress.Position then
      prbOverallProgress.Position := Temp;
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.OnFileStatus(Sender: TObject; FileIndex: Integer);
begin
If lvFiles.Items[FileIndex].SubItems[0] = '' then
  lvFiles.Items[FileIndex].SubItems[0] := FilesManager[FileIndex].Name;
case FilesManager[FileIndex].Status of
  fstReady:
    begin
      lvFiles.Items[FileIndex].SubItems[1] := 'Ready';
      lvFiles.Items[FileIndex].ImageIndex := 1;
    end;
  fstDone:
    begin
      lvFiles.Items[FileIndex].SubItems[1] := 'Done';
      lvFiles.Items[FileIndex].ImageIndex := 2;
    end;
  fstError:
    begin
      lvFiles.Items[FileIndex].SubItems[1] := 'Error';
      lvFiles.Items[FileIndex].ImageIndex := 3;
    end;
  fstProcessing:
    begin
      lvFiles.Items[FileIndex].SubItems[1] := 'Processing... 0%';
      lvFiles.Items[FileIndex].ImageIndex := 4;
      tmrAnimTimer.Tag := 0;  
    end;
else
 {fstUnknown}
  lvFiles.Items[FileIndex].SubItems[1] := 'Unknown';
  lvFiles.Items[FileIndex].ImageIndex := 0;
end;
tmrAnimTimer.Enabled := FilesManager[FileIndex].Status = fstProcessing;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.OnStatus(Sender: TObject);
begin
case FilesManager.Status of
  mstReady:       btnProcessing.Caption := 'Start processing';
  mstProcessing:  btnProcessing.Caption := 'Stop processing';
  mstTerminating: btnProcessing.Caption := 'Terminating processing, please wait...';
end;
end;
    
//==============================================================================

procedure TfMainForm.FormCreate(Sender: TObject);
begin
stbStatusBar.DoubleBuffered := True;
lvFiles.DoubleBuffered := True;
prbOverallProgress.DoubleBuffered := True;
prbFileProgress.DoubleBuffered := True;
FilesManager := TFilesManager.Create;
FilesManager.OnProgress := OnProgress;
FilesManager.OnFileStatus := OnFileStatus;
FilesManager.OnStatus := OnStatus;
OnStatus(nil);
diaOpenDialog.InitialDir := ExtractFileDir(ParamStr(0));
end;
   
//------------------------------------------------------------------------------

procedure TfMainForm.FormDestroy(Sender: TObject);
begin
FilesManager.Free;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
FilesManager.EndProcessingAndWait;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.FormResize(Sender: TObject);
begin
lvFiles.Columns.Items[1].Width := lvFiles.Width - (lvFiles.Columns.Items[0].Width + lvFiles.Columns.Items[2].Width + 25);
end;

//------------------------------------------------------------------------------

procedure TfMainForm.lvFilesDblClick(Sender: TObject);
begin
If (lvFiles.ItemIndex >= 0) and (FilesManager.Status = mstReady) then
  case FilesManager[lvFiles.ItemIndex].Status of
    fstError: fErrorForm.ShowErrorInformations(FilesManager[lvFiles.ItemIndex].Name,FilesManager[lvFiles.ItemIndex].ErrorInfo);
  else
    fPrcsSettingsForm.ShowProcessingSettings(FilesManager[lvFiles.ItemIndex].Path,FilesManager.Pointers[lvFiles.ItemIndex]^.ProcessingSettings);
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.lvFilesInfoTip(Sender: TObject; Item: TListItem;
  var InfoTip: String);
begin
InfoTip := FilesManager[Item.Index].Path;
end;
   
//------------------------------------------------------------------------------

procedure TfMainForm.mnuFilesPopup(Sender: TObject);
begin
mfAdd.Enabled := FilesManager.Status = mstReady;
mfRemove.Enabled := (FilesManager.Status = mstReady) and (lvFiles.SelCount > 0);
mfClear.Enabled := (FilesManager.Status = mstReady) and (lvFiles.Items.Count > 0);
mfSettings.Enabled := (FilesManager.Status = mstReady) and (lvFiles.SelCount = 1);
mfErrorInfo.Enabled := (FilesManager.Status = mstReady) and (lvFiles.SelCount = 1) and (FilesManager[lvFiles.Selected.Index].Status = fstError);
mfClearCompleted.Enabled := (FilesManager.Status = mstReady) and FilesManager.CompletedItems;
end;
   
//------------------------------------------------------------------------------

procedure TfMainForm.mfAddClick(Sender: TObject);
var
  i:  Integer;
begin
If diaOpenDialog.Execute then
  begin
    lvFiles.Items.BeginUpdate;
    try
      For i := 0 to Pred(diaOpenDialog.Files.Count) do
        If FilesManager.IndexOf(diaOpenDialog.Files[i]) < 0 then
          begin
            with lvFiles.Items.Add do
              begin
                SubItems.Add('');
                SubItems.Add('');
              end;
            FilesManager.Add(diaOpenDialog.Files[i])
          end;
    finally
      lvFiles.Items.EndUpdate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.mfRemoveClick(Sender: TObject);
var
  i:  Integer;
begin
If MessageDlg(Format('Are you sure you want remove selected files (%d)?',[lvFiles.SelCount]),mtConfirmation,[mbYes,mbNo],0) = mrYes then
  For i := Pred(FilesManager.Count) downto 0 do
    If lvFiles.Items[i].Selected then
      begin
        lvFiles.Items.Delete(i);
        FilesManager.Delete(i);
      end;
end;
   
//------------------------------------------------------------------------------

procedure TfMainForm.mfClearClick(Sender: TObject);
begin
If MessageDlg('Are you sure you want clear the entire list?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
  begin
    lvFiles.Clear;
    FilesManager.Clear;
  end;
end;
   
//------------------------------------------------------------------------------

procedure TfMainForm.mfSettingsClick(Sender: TObject);
begin
If lvFiles.ItemIndex >= 0 then
  fPrcsSettingsForm.ShowProcessingSettings(FilesManager[lvFiles.ItemIndex].Path,FilesManager.Pointers[lvFiles.ItemIndex]^.ProcessingSettings);
end;
     
//------------------------------------------------------------------------------

procedure TfMainForm.mfErrorInfoClick(Sender: TObject);
begin
If lvFiles.ItemIndex >= 0 then
  fErrorForm.ShowErrorInformations(FilesManager[lvFiles.ItemIndex].Name,FilesManager[lvFiles.ItemIndex].ErrorInfo);
end;
    
//------------------------------------------------------------------------------

procedure TfMainForm.mfClearCompletedClick(Sender: TObject);
var
  i:  Integer;
begin
For i := Pred(FilesManager.Count) downto 0 do
  If FilesManager[i].Status = fstDone then
    begin
      lvFiles.Items.Delete(i);
      FilesManager.Delete(i);
    end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.btnProcessingClick(Sender: TObject);
begin
case FilesManager.Status of
  mstReady:       FilesManager.StartProcessing;
  mstProcessing:  FilesManager.StopProcessing;
  mstTerminating: begin
                    FilesManager.PauseProcessing;
                    If MessageDlg('The program is waiting for the processing thread to be normally terminated.'+ sLineBreak +
                                  'You can initiate forced termination, but it will cause resource leak and other problems.' + sLineBreak +
                                  'In that case, you are strongly advised to restart the program before further use.' + sLineBreak + sLineBreak +
                                  'Are you sure you want to force processing thread to terminate?' ,mtWarning,[mbYes,mbNo],0) = mrYes then
                      FilesManager.StopProcessing
                    else
                      FilesManager.ResumeProcessing;  
                  end;
end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.tmrAnimTimerTimer(Sender: TObject);
begin
If FilesManager.ProcessingFile >= 0 then
  begin
    lvFiles.Items[FilesManager.ProcessingFile].ImageIndex := 5 + tmrAnimTimer.Tag;
    If tmrAnimTimer.Tag < 7 then
      tmrAnimTimer.Tag := tmrAnimTimer.Tag + 1
    else
      tmrAnimTimer.Tag := 0;
  end;
end;

end.
