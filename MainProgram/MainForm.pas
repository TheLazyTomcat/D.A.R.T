{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit MainForm;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Menus, {$IFNDEF FPC}Messages, ImgList, XPMan,{$ENDIF}
  FilesManager;

type
{$IFNDEF FPC}
  TListView = class(ComCtrls.TListView)
  protected
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  end;
{$ENDIF}

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
    mfErrorInfo: TMenuItem;
    N2: TMenuItem;
    mfClearCompleted: TMenuItem;
    diaOpenDialog: TOpenDialog;
    tmrAnimTimer: TTimer;
    imlFileIcons: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure lvFilesDblClick(Sender: TObject);
    procedure mnuFilesPopup(Sender: TObject);
    procedure mfAddClick(Sender: TObject);
    procedure mfRemoveClick(Sender: TObject);
    procedure mfClearClick(Sender: TObject);
    procedure mfSettingsClick(Sender: TObject);
    procedure mfErrorInfoClick(Sender: TObject);
    procedure mfClearCompletedClick(Sender: TObject);
    procedure btnProcessingClick(Sender: TObject);
    procedure tmrAnimTimerTimer(Sender: TObject);
  {$IFDEF FPC}
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  {$ENDIF}
  protected
    fSettingsChanged: Boolean;
    procedure SettingsChanged(FileIndex: Integer);
  public
    FilesManager: TFilesManager;
    procedure LoadCopyrightInfo;
    procedure OnProgress(Sender: TObject; FileIndex: Integer);
    procedure OnFileStatus(Sender: TObject; FileIndex: Integer);
    procedure OnStatus(Sender: TObject);
    procedure LoadFilesFromParams;
    procedure SetDropAccept(AcceptDrop: Boolean);
  end;

var
  fMainForm: TfMainForm;

implementation

uses
  {$IFNDEF FPC}Windows,{$ELSE}FileUtil,{$ENDIF} ShellAPI,
  ErrorForm, PrcsSettingsForm, Repairer, WinFileInfo;

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

const
{%H-}List_IconColumn   = -1;
     List_NameColumn   = 0;
     List_SizeColumn   = 1;
     List_MethodColumn = 2;
     List_StateColumn  = 3;

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
  If (fMainForm.FilesManager.Status = mstReady) and (FileCount > 0) then
    begin
      For i := 0 to Pred(FileCount) do
        begin
          SetLength(FileName,DragQueryFile(Msg.Drop,i,nil,0));
          DragQueryFile(Msg.Drop,i,PChar(FileName),Length(FileName) + 1);
          If FileExists(FileName) and (fMainForm.FilesManager.IndexOf(FileName) < 0) then
            begin
              with Self.Items.Add do
                begin
                  SubItems.Add('');
                  SubItems.Add('');
                  SubItems.Add('');
                  SubItems.Add('');
                end;
              fMainForm.FilesManager.Add(FileName)
            end;
        end;
    end;
finally
  DragFinish(Msg.Drop);
end;
end;
{$ENDIF}

//==============================================================================

procedure TfMainForm.SettingsChanged(FileIndex: Integer);
begin
fSettingsChanged := True;
try
  OnFileStatus(Self,FileIndex);
finally;
  fSettingsChanged := False;
end
end;

//==============================================================================

procedure TfMainForm.LoadCopyrightInfo;
begin
with TWinFileInfo.Create(WFI_LS_LoadVersionInfo or WFI_LS_LoadFixedFileInfo or WFI_LS_DecodeFixedFileInfo) do
  begin
    stbStatusBar.Panels[0].Text := {$IFDEF FPC}AnsiToUTF8({$ENDIF}
      VersionInfoValues[VersionInfoTranslations[0].LanguageStr,'LegalCopyright'] + ', version ' +
      VersionInfoValues[VersionInfoTranslations[0].LanguageStr,'ProductVersion'] + ' ' +
      {$IFDEF FPC}'L'{$ELSE}'D'{$ENDIF}{$IFDEF x64}+ '64 '{$ELSE}+ '32 '{$ENDIF} + zlib_method_str +
      ' #' + IntToStr(VersionInfoFixedFileInfoDecoded.FileVersionMembers.Build)
      {$IFDEF Debug}+ ' debug'{$ENDIF}{$IFDEF FPC}){$ENDIF};
    Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.OnProgress(Sender: TObject; FileIndex: Integer);
var
  Temp: Integer;
begin
If FilesManager[FileIndex].Status = fstProcessing then
  begin
    Temp := Trunc(prbFileProgress.Max * FilesManager[FileIndex].Progress);
    If Temp <> prbFileProgress.Position then
      begin
        lvFiles.Items[FileIndex].SubItems[List_StateColumn] := Format('Processing... %.0f%%',[FilesManager[FileIndex].Progress * 100]);
        prbFileProgress.Position := Temp;
      end;
    Temp := Trunc(prbOverallProgress.Max * FilesManager.Progress);
    If Temp <> prbOverallProgress.Position then
      prbOverallProgress.Position := Temp;
  end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.OnFileStatus(Sender: TObject; FileIndex: Integer);

  Function FileSizeStr(FileSize: Int64): String;
  const
    BinaryPrefix: Array[0..8] of String = ('','Ki','Mi','Gi','Ti','Pi','Ei','Zi','Yi');
  var
    ShiftCounter:   Integer;
    DecimalDigits:  Integer;
  begin
    ShiftCounter := 0;
    DecimalDigits := 0;
    If FileSize <> 0 then
      begin
        while (FileSize shr (ShiftCounter * 10)) <> 0 do
          Inc(ShiftCounter);
        Dec(ShiftCounter);  
        case FileSize shr (ShiftCounter * 10) of
          0..9:   DecimalDigits := 2;
         10..99:  DecimalDigits := 1;
        else
          DecimalDigits := 0;
        end;
      end;
    Result := FloatToStrF(FileSize / (Int64(1) shl (ShiftCounter * 10)),ffFixed,18,DecimalDigits) + ' ' + BinaryPrefix[ShiftCounter] + 'B';
  end;

begin
If (lvFiles.Items[FileIndex].SubItems[List_NameColumn] = '') or fSettingsChanged then
  begin
    lvFiles.Items[FileIndex].SubItems[List_NameColumn] := FilesManager[FileIndex].Name;
    lvFiles.Items[FileIndex].SubItems[List_SizeColumn] := FileSizeStr(FilesManager[FileIndex].Size);
    case FilesManager[FileIndex].ProcessingSettings.RepairMethod of
      rmRebuild:  lvFiles.Items[FileIndex].SubItems[List_MethodColumn] := 'Rebuild file';
      rmExtract:  lvFiles.Items[FileIndex].SubItems[List_MethodColumn] := 'Extract archive';
    else
      lvFiles.Items[FileIndex].SubItems[List_MethodColumn] := 'Unknown';
    end;
  end;
case FilesManager[FileIndex].Status of
  fstReady:
    begin
      lvFiles.Items[FileIndex].SubItems[List_StateColumn] := 'Ready';
      lvFiles.Items[FileIndex].ImageIndex := 1;
    end;
  fstDone:
    begin
      lvFiles.Items[FileIndex].SubItems[List_StateColumn] := 'Done';
      lvFiles.Items[FileIndex].ImageIndex := 2;
    end;
  fstError:
    begin
      lvFiles.Items[FileIndex].SubItems[List_StateColumn] := 'Error';
      lvFiles.Items[FileIndex].ImageIndex := 3;
    end;
  fstProcessing:
    begin
      lvFiles.Items[FileIndex].SubItems[List_StateColumn] := 'Processing... 0%';
      lvFiles.Items[FileIndex].ImageIndex := 4;
      tmrAnimTimer.Tag := 0;  
    end;
else
 {fstUnknown}
  lvFiles.Items[FileIndex].SubItems[List_StateColumn] := 'Unknown';
  lvFiles.Items[FileIndex].ImageIndex := 0;
end;
tmrAnimTimer.Enabled := FilesManager[FileIndex].Status = fstProcessing;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.OnStatus(Sender: TObject);
begin
mnuFiles.OnPopup(nil);
SetDropAccept(FilesManager.Status = mstReady);
case FilesManager.Status of
  mstReady:       btnProcessing.Caption := 'Start processing';
  mstProcessing:  btnProcessing.Caption := 'Stop processing';
  mstTerminating: btnProcessing.Caption := 'Terminating processing, please wait...';
end;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.LoadFilesFromParams;
var
  i:  Integer;
begin
If ParamCount > 0 then
  For i := 1 to ParamCount do
    If {$IFDEF FPC}FileExistsUTF8(ParamStr(i)){$ELSE}FileExists(ParamStr(i)){$ENDIF}  and (FilesManager.IndexOf(ParamStr(i)) < 0) then
      begin
        with lvFiles.Items.Add do
          begin
            SubItems.Add('');
            SubItems.Add('');
            SubItems.Add('');
            SubItems.Add('');
          end;
      {$IFDEF FPC}
        FilesManager.Add(ExpandFileNameUTF8(ParamStr(i)));
      {$ELSE}
        FilesManager.Add(ExpandFileName(ParamStr(i)));
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
    
//==============================================================================

procedure TfMainForm.FormCreate(Sender: TObject);
begin
fSettingsChanged := False;
stbStatusBar.DoubleBuffered := True;
lvFiles.DoubleBuffered := True;
prbOverallProgress.DoubleBuffered := True;
prbFileProgress.DoubleBuffered := True;
LoadCopyrightInfo;
FilesManager := TFilesManager.Create;
FilesManager.OnProgress := OnProgress;
FilesManager.OnFileStatus := OnFileStatus;
FilesManager.OnStatus := OnStatus;
OnStatus(nil);
diaOpenDialog.InitialDir := ExtractFileDir(ParamStr(0));
{$IFNDEF FPC}
mfSettings.ShortCut := ShortCut(Ord('S'),[ssAlt]);
mfErrorInfo.ShortCut := ShortCut(Ord('E'),[ssAlt]);
mfClearCompleted.ShortCut := ShortCut(Ord('C'),[ssAlt]);
{$ENDIF}
LoadFilesFromParams;
SetDropAccept(True);
end;
   
//------------------------------------------------------------------------------

procedure TfMainForm.FormDestroy(Sender: TObject);
begin
SetDropAccept(False);
FilesManager.Free;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
FilesManager.EndProcessingAndWait;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.FormResize(Sender: TObject);
var
  i:        Integer;
  NewWidth: Integer;
begin
NewWidth := lvFiles.Width + lvFiles.Columns[1].Width - 25;
For i := 0 to Pred(lvFiles.Columns.Count) do
  Dec(NewWidth,lvFiles.Columns[i].Width);
lvFiles.Columns[1].Width := NewWidth;
end;

//------------------------------------------------------------------------------

procedure TfMainForm.lvFilesDblClick(Sender: TObject);
begin
If FilesManager.Status = mstReady then
  begin
    If lvFiles.ItemIndex >= 0 then
      begin
        case FilesManager[lvFiles.ItemIndex].Status of
          fstError: fErrorForm.ShowErrorInformation(FilesManager[lvFiles.ItemIndex]);
        else
          fPrcsSettingsForm.ShowProcessingSettings(FilesManager.Pointers[lvFiles.ItemIndex]^);
          SettingsChanged(lvFiles.ItemIndex);
        end;
      end
    else mfAdd.OnClick(nil);
  end;
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
If FilesManager.Status = mstReady then
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
  i:        Integer;
  MsgText:  String;
begin
If (FilesManager.Status = mstReady) and (lvFiles.SelCount > 0) then
  begin
    If lvFiles.SelCount > 1 then
      MsgText := 'Are you sure you want remove selected files (%d)?'
    else
      MsgText := 'Are you sure you want remove selected file?';
    If MessageDlg(Format(MsgText,[lvFiles.SelCount]),mtConfirmation,[mbYes,mbNo],0) = mrYes then
      For i := Pred(FilesManager.Count) downto 0 do
        If lvFiles.Items[i].Selected then
          begin
            lvFiles.Items.Delete(i);
            FilesManager.Delete(i);
          end;
  end;
end;
   
//------------------------------------------------------------------------------

procedure TfMainForm.mfClearClick(Sender: TObject);
begin
If (FilesManager.Status = mstReady) and (lvFiles.Items.Count > 0) then
  If MessageDlg('Are you sure you want clear the entire list?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
    begin
      lvFiles.Clear;
      FilesManager.Clear;
    end;
end;
   
//------------------------------------------------------------------------------

procedure TfMainForm.mfSettingsClick(Sender: TObject);
begin
If (FilesManager.Status = mstReady) and (lvFiles.SelCount = 1) then
  begin
    fPrcsSettingsForm.ShowProcessingSettings(FilesManager.Pointers[lvFiles.ItemIndex]^);
    SettingsChanged(lvFiles.ItemIndex);
  end;
end;
     
//------------------------------------------------------------------------------

procedure TfMainForm.mfErrorInfoClick(Sender: TObject);
begin
If (FilesManager.Status = mstReady) and (lvFiles.SelCount = 1) and (FilesManager[lvFiles.Selected.Index].Status = fstError) then
  fErrorForm.ShowErrorInformation(FilesManager[lvFiles.ItemIndex]);
end;
    
//------------------------------------------------------------------------------

procedure TfMainForm.mfClearCompletedClick(Sender: TObject);
var
  i:  Integer;
begin
If (FilesManager.Status = mstReady) and FilesManager.CompletedItems then
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
If FilesManager.Count > 0 then
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
  end
else MessageDlg('There are no files selected for processing.',mtInformation,[mbOK],0);
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

//------------------------------------------------------------------------------

{$IFDEF FPC}
procedure TfMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i:  Integer;
begin
If FilesManager.Status = mstReady then
  For i := Low(FileNames) to High(FileNames) do
    If FileExistsUTF8(FileNames[i]) and (fMainForm.FilesManager.IndexOf(FileNames[i]) < 0) then
      begin
        with lvFiles.Items.Add do
          begin
            SubItems.Add('');
            SubItems.Add('');
            SubItems.Add('');
            SubItems.Add('');
          end;
        fMainForm.FilesManager.Add(FileNames[i])
      end;
end;
{$ENDIF}

end.
