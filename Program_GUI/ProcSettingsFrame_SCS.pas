unit ProcSettingsFrame_SCS;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, StdCtrls,
  DART_ProcessingSettings, Menus;

type
  TOptionDescriptionEvent = procedure(Sender: TObject; DescriptionTag: Integer) of object;

  TfrmProcSettingsFrame_SCS = class(TFrame)
    pnlBackground: TPanel;
    gbEntries: TGroupBox;
    cbIgnoreCRC32: TCheckBox;
    cbIgnoreCompressionFlag: TCheckBox;
    cbIgnoreDictID: TCheckBox;
    gbPathResolve: TGroupBox;
    cbAssumeCityHash: TCheckBox;
    cbUsePredefinedPaths: TCheckBox;
    cbExtractUnresolvedEntries: TCheckBox;
    lblCustomPaths: TLabel;
    meCustomPaths: TMemo;
    lblHelpArchives: TLabel;
    meHelpArchives: TMemo;
    btnHelpArchivesMenu: TButton;
    gbContentParsing: TGroupBox;
    gbBruteForce: TGroupBox;
    lblHint1: TLabel;
    lblHint2: TLabel;
    pmHelpArchivesMenu: TPopupMenu;
    mi_HAM_Browse: TMenuItem;
    mi_HAM_N1: TMenuItem;
    mi_HAM_EST2: TMenuItem;
    mi_HAM_ATS: TMenuItem;
    diaHelpArchivesOpen: TOpenDialog;
    procedure meCustomPathsKeyPress(Sender: TObject; var Key: Char);
    procedure meHelpArchivesKeyPress(Sender: TObject; var Key: Char);
    procedure btnHelpArchivesMenuClick(Sender: TObject);
    procedure mi_HAM_BrowseClick(Sender: TObject);
  private
    fArchiveProcessingSettings: TDARTArchiveProcessingSettings;
    fLoading:                   Boolean;
    fOnOptionDescription:       TOptionDescriptionEvent;
    fProcessingSettings:        TDART_PS_SCS;
    fGameInstallDirs:           array of String;
  protected
    procedure GetInstalledGames;
  public
    procedure Initialize;
    procedure SettingsToFrame;
    procedure FrameToSettings;
    procedure ShowProcessingSettings(const ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
    procedure RetrieveProcessingSettings(var ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
  published
    procedure CheckBoxClick(Sender: TObject);
    procedure OptionMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure LoadGameFiles(Sender: TObject);
    property OnOptionDescription: TOptionDescriptionEvent read fOnOptionDescription write fOnOptionDescription;
  end;

implementation

{$R *.dfm}

uses
  Registry,
  StrRect,
  DART_Auxiliary;


procedure TfrmProcSettingsFrame_SCS.GetInstalledGames;
type
  TKnownGameItem = record
    RegistryRoot: HKEY;
    RegistryKey:  String;
    ValueName:    String;
  end;
const
{
   Note steam installations are not included.
   If anyone knows a reliable way of how to obtain where individual games are
   installed by steam, please let me know.
}
  KnownGamesInfoArray: array[0..1] of TKnownGameItem = (
    (RegistryRoot: HKEY_LOCAL_MACHINE;
     RegistryKey:  '\SOFTWARE\SCS Software\Euro Truck Simulator 2';
     ValueName:    'InstallDir'),
    (RegistryRoot: HKEY_LOCAL_MACHINE;
     RegistryKey:  '\SOFTWARE\SCS Software\American Truck Simulator';
     ValueName:    'InstallDir'));
var
  Reg:  TRegistry;
  i:    Integer;
begin
SetLength(fGameInstallDirs,Length(KnownGamesInfoArray));
Reg := TRegistry.Create;
try
  For i := Low(KnownGamesInfoArray) to High(KnownGamesInfoArray) do
    begin
      Reg.RootKey := KnownGamesInfoArray[i].RegistryRoot;
      If Reg.KeyExists(KnownGamesInfoArray[i].RegistryKey) then
        If Reg.OpenKeyReadOnly(KnownGamesInfoArray[i].RegistryKey) then
          begin
            If Reg.ValueExists(KnownGamesInfoArray[i].ValueName) then
              fGameInstallDirs[i] := Reg.ReadString(KnownGamesInfoArray[i].ValueName);
            Reg.CloseKey;
          end;
    end;
finally
  Reg.Free;
end;
end;

//==============================================================================

procedure TfrmProcSettingsFrame_SCS.Initialize;
var
  i:  Integer;

  Function GetGameFilesMenuItem(IDX: Integer): TMenuItem;
  begin
    case IDX of
      0:  Result := mi_HAM_EST2;
      1:  Result := mi_HAM_ATS;
    else
      Result := nil;
    end;
  end;

begin
diaHelpArchivesOpen.InitialDir := ExtractFileDir(RTLToStr(ParamStr(0)));
GetInstalledGames;
For i := Low(fGameInstallDirs) to High(fGameInstallDirs) do
  If fGameInstallDirs[i] <> '' then
    begin
      with GetGameFilesMenuItem(i) do
        begin
          Visible := True;
          Tag := i;
        end;
      mi_HAM_N1.Visible := True;
    end
  else GetGameFilesMenuItem(i).Visible := False;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_SCS.SettingsToFrame;
var
  i:  Integer;
begin
// entry
cbIgnoreCRC32.Checked := fProcessingSettings.Entry.IgnoreCRC32;
cbIgnoreCompressionFlag.Checked := fProcessingSettings.Entry.IgnoreCompressionFlag;
cbIgnoreDictID.Checked := fProcessingSettings.Entry.IgnoreDictionaryID;
// path resolve
cbAssumeCityHash.Checked := fProcessingSettings.PathResolve.AssumeCityHash;
cbUsePredefinedPaths.Checked := fProcessingSettings.PathResolve.UsePredefinedPaths;
cbExtractUnresolvedEntries.Checked := fProcessingSettings.PathResolve.ExtractedUnresolvedEntries;
// custom paths
meCustomPaths.Lines.BeginUpdate;
try
  meCustomPaths.Clear;
  For i := Low(fProcessingSettings.PathResolve.CustomPaths) to High(fProcessingSettings.PathResolve.CustomPaths) do
    meCustomPaths.Lines.Add(fProcessingSettings.PathResolve.CustomPaths[i]);
finally
  meCustomPaths.Lines.EndUpdate;
end;
// help files
meHelpArchives.Lines.BeginUpdate;
try
  meHelpArchives.Clear;
  For i := Low(fProcessingSettings.PathResolve.HelpArchives) to High(fProcessingSettings.PathResolve.HelpArchives) do
    meHelpArchives.Lines.Add(fProcessingSettings.PathResolve.HelpArchives[i]);
finally
  meHelpArchives.Lines.EndUpdate;
end;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_SCS.FrameToSettings;
var
  i,Count:  Integer;
begin
// entry
fProcessingSettings.Entry.IgnoreCRC32 := cbIgnoreCRC32.Checked;
fProcessingSettings.Entry.IgnoreCompressionFlag := cbIgnoreCompressionFlag.Checked;
fProcessingSettings.Entry.IgnoreDictionaryID := cbIgnoreDictID.Checked;
// path resolve
fProcessingSettings.PathResolve.AssumeCityHash := cbAssumeCityHash.Checked;
fProcessingSettings.PathResolve.UsePredefinedPaths := cbUsePredefinedPaths.Checked;
fProcessingSettings.PathResolve.ExtractedUnresolvedEntries := cbExtractUnresolvedEntries.Checked;
// custom paths
Count := 0;
SetLength(fProcessingSettings.PathResolve.CustomPaths,meCustomPaths.Lines.Count);
For i := 0 to Pred(meCustomPaths.Lines.Count) do
  If Length(meCustomPaths.Lines[i]) > 0 then
    begin
      fProcessingSettings.PathResolve.CustomPaths[Count] := meCustomPaths.Lines[i];
      Inc(Count);
    end;
SetLength(fProcessingSettings.PathResolve.CustomPaths,Count);
// help files
Count := 0;
SetLength(fProcessingSettings.PathResolve.HelpArchives,meHelpArchives.Lines.Count);
For i := 0 to Pred(meHelpArchives.Lines.Count) do
  If Length(meHelpArchives.Lines[i]) > 0 then
    begin
      fProcessingSettings.PathResolve.HelpArchives[Count] := meHelpArchives.Lines[i];
      Inc(Count);
    end;
SetLength(fProcessingSettings.PathResolve.HelpArchives,Count);
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_SCS.ShowProcessingSettings(const ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
fProcessingSettings := ArchiveProcessingSettings.SCS;
fArchiveProcessingSettings := ArchiveProcessingSettings;
fLoading := True;
try
  SettingsToFrame;
finally
  fLoading := False;
end;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_SCS.RetrieveProcessingSettings(var ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
FrameToSettings;
ArchiveProcessingSettings.SCS := fProcessingSettings;
end;

//==============================================================================

procedure TfrmProcSettingsFrame_SCS.CheckBoxClick(Sender: TObject);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_SCS.OptionMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
If (Sender is TControl) and Assigned(fOnOptionDescription) then
  fOnOptionDescription(Self,TControl(Sender).Tag);
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_SCS.GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Control:  TControl;
begin
If Sender is TGroupBox then
  begin
    Control := TGroupBox(Sender).ControlAtPos(Point(X,Y),True,True);
    If Assigned(Control) and (Control is TCheckBox) then
      OptionMouseMove(Control,Shift,X,Y);
  end;
end;

//==============================================================================

procedure TfrmProcSettingsFrame_SCS.meCustomPathsKeyPress(Sender: TObject;
  var Key: Char);
begin
If Key = ^A then
  begin
    Key := #0;
    meCustomPaths.SelectAll;
  end;
end;

procedure TfrmProcSettingsFrame_SCS.meHelpArchivesKeyPress(Sender: TObject;
  var Key: Char);
begin
If Key = ^A then
  begin
    Key := #0;
    meHelpArchives.SelectAll;
  end;
end;

procedure TfrmProcSettingsFrame_SCS.btnHelpArchivesMenuClick(Sender: TObject);
var
  PopupPoint: TPoint;
begin
PopupPoint := gbPathResolve.ClientToScreen(Point(btnHelpArchivesMenu.Left,btnHelpArchivesMenu.BoundsRect.Bottom));
pmHelpArchivesMenu.Popup(PopupPoint.X,PopupPoint.Y);
end;

procedure TfrmProcSettingsFrame_SCS.mi_HAM_BrowseClick(Sender: TObject);
var
  i:  Integer;
begin
If diaHelpArchivesOpen.Execute then
  begin
    meHelpArchives.Lines.BeginUpdate;
    try
      For i := 0 to Pred(diaHelpArchivesOpen.Files.Count) do
        If not AnsiSameText(diaHelpArchivesOpen.Files[i],fArchiveProcessingSettings.Common.ArchivePath) and
          (meHelpArchives.Lines.IndexOf(diaHelpArchivesOpen.Files[i]) < 0) then
          meHelpArchives.Lines.Add(diaHelpArchivesOpen.Files[i]);
    finally
      meHelpArchives.Lines.EndUpdate;
    end
  end;
end;

procedure TfrmProcSettingsFrame_SCS.LoadGameFiles(Sender: TObject);
var
  FileList: TStringList;
  i:        Integer;

  procedure GetListing(const Path: String; Files: TStringList);
  var
    SearchRec: TSearchRec;
  begin
    If DART_FindFirst(IncludeTrailingPathDelimiter(Path) + '*.scs',faAnyFile,SearchRec) = 0 then
    try
      repeat
        Files.Add(IncludeTrailingPathDelimiter(Path) + SearchRec.Name);
      until DART_FindNext(SearchRec) <> 0;
    finally
      DART_FindClose(SearchRec);
    end;
  end;

begin
If Sender is TMenuItem then
  begin
    FileList := TStringList.Create;
    try
      GetListing(fGameInstallDirs[TMenuItem(Sender).Tag],FileList);
      meHelpArchives.Lines.BeginUpdate;
      try
        For i := 0 to Pred(FileList.Count) do
          If meHelpArchives.Lines.IndexOf(FileList[i]) < 0 then
            meHelpArchives.Lines.Add(FileList[i]);
      finally
        meHelpArchives.Lines.EndUpdate;
      end;
    finally
      FileList.Free;
    end;
  end;
end;

end.
