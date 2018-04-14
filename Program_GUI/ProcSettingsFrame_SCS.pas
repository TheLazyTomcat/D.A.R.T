{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit ProcSettingsFrame_SCS;

{$INCLUDE '..\Source\DART_defs.inc'}

interface

uses
  {$IFNDEF FPC}Windows,{$ENDIF} SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Menus,
  DART_ProcessingSettings, Spin;

type
{$IFNDEF FPC}
  // combobox in delphi does not have public OnMouseMove event
  TComboBox = class(StdCtrls.TComboBox)
  published
    property OnMouseMove;
  end;
{$ENDIF}

  TOptionDescriptionEvent = procedure(Sender: TObject; DescriptionTag: Integer) of object;

  TfrmProcSettingsFrame_SCS = class(TFrame)
    pnlBackground: TPanel;
    lblPresets: TLabel;
    cmbPresets: TComboBox;    
    gbGeneral: TGroupBox;
    cbEntryTabInMem: TCheckBox;    
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
    cbCPParseContent: TCheckBox;
    cbCPParseEverything: TCheckBox;
    cbCPParseHelpArch: TCheckBox;
    cbCPParseEverythingInHlpArch: TCheckBox;
    cbCPPrintableASCIIOnly: TCheckBox;
    cbCPLimitedCharSet: TCheckBox;
    lblBinaryThreshold: TLabel;
    seBinaryThreshold: TSpinEdit;
    seMinPathLength: TSpinEdit;
    lblMinPathLength: TLabel;
    gbBruteForce: TGroupBox;
    cbBFActivate: TCheckBox;
    cbBFMultithreaded: TCheckBox;
    cbBFUseKnownPaths: TCheckBox;
    cbBFPrintableASCIIOnly: TCheckBox;
    cbBFLimitedCharSet: TCheckBox;
    lblBFMaxPathLength: TLabel;
    seBFMaxPathLength: TSpinEdit;
    pmHelpArchivesMenu: TPopupMenu;
    mi_HAM_Browse: TMenuItem;
    mi_HAM_N1: TMenuItem;
    mi_HAM_ETS2: TMenuItem;
    mi_HAM_ATS: TMenuItem;
    diaHelpArchivesOpen: TOpenDialog; 
    procedure meCustomPathsKeyPress(Sender: TObject; var Key: Char);
    procedure meHelpArchivesKeyPress(Sender: TObject; var Key: Char);
    procedure btnHelpArchivesMenuClick(Sender: TObject);
    procedure mi_HAM_BrowseClick(Sender: TObject);
    procedure cmbPresetsChange(Sender: TObject);
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
    procedure OptionMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure LoadGameFiles(Sender: TObject);
    property OnOptionDescription: TOptionDescriptionEvent read fOnOptionDescription write fOnOptionDescription;
  end;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
  Registry, Types,
  StrRect,
  DART_Auxiliary, DART_ProcessingSettings_Presets;


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
      0:  Result := mi_HAM_ETS2;
      1:  Result := mi_HAM_ATS;
    else
      Result := nil;
    end;
  end;

begin
diaHelpArchivesOpen.InitialDir := ExtractFileDir(RTLToStr(ParamStr(0)));
GetInstalledGames;
// game files
For i := Low(fGameInstallDirs) to High(fGameInstallDirs) do
  If Length(fGameInstallDirs[i]) > 0 then
    begin
      with GetGameFilesMenuItem(i) do
        begin
          Visible := True;
          Tag := i;
        end;
      mi_HAM_N1.Visible := True;
    end
  else GetGameFilesMenuItem(i).Visible := False;
// presets
cmbPresets.Items.BeginUpdate;
try
  For i := Low(DART_PS_SCS_Presets) to High(DART_PS_SCS_Presets) do
    cmbPresets.Items.Add(DART_PS_SCS_Presets[i].PresetName);
  If cmbPresets.Items.Count > 0 then
    cmbPresets.ItemIndex := 0
  else
    cmbPresets.ItemIndex := -1;
finally
  cmbPresets.Items.EndUpdate;
end;
cmbPresets.OnMouseMove := OptionMouseMove;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_SCS.SettingsToFrame;
var
  i:  Integer;
begin
// general
cbEntryTabInMem.Checked := fProcessingSettings.EntryTabInMem;
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
    meCustomPaths.Lines.Add(AnsiToStr(fProcessingSettings.PathResolve.CustomPaths[i]));
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
// content parsing
cbCPParseContent.Checked := fProcessingSettings.PathResolve.ContentParsing.ParseContent;
cbCPParseEverything.Checked := fProcessingSettings.PathResolve.ContentParsing.ParseEverything;
cbCPParseHelpArch.Checked := fProcessingSettings.PathResolve.ContentParsing.ParseHelpArchives;
cbCPParseEverythingInHlpArch.Checked := fProcessingSettings.PathResolve.ContentParsing.ParseEverythingInHlpArch;
cbCPPrintableASCIIOnly.Checked := fProcessingSettings.PathResolve.ContentParsing.PrintableASCIIOnly;
cbCPLimitedCharSet.Checked := fProcessingSettings.PathResolve.ContentParsing.LimitedCharacterSet;
seBinaryThreshold.Value := Trunc(fProcessingSettings.PathResolve.ContentParsing.BinaryThreshold * 1000);
seMinPathLength.Value := fProcessingSettings.PathResolve.ContentParsing.MinPathLength;
// brute force
cbBFActivate.Checked := fProcessingSettings.PathResolve.BruteForce.ActivateBruteForce;
cbBFMultithreaded.Checked := fProcessingSettings.PathResolve.BruteForce.Multithreaded;
cbBFUseKnownPaths.Checked := fProcessingSettings.PathResolve.BruteForce.UseKnownPaths;
cbBFPrintableASCIIOnly.Checked := fProcessingSettings.PathResolve.BruteForce.PrintableASCIIOnly;
cbBFLimitedCharSet.Checked := fProcessingSettings.PathResolve.BruteForce.LimitedCharSet;
seBFMaxPathLength.Value := fProcessingSettings.PathResolve.BruteForce.PathLengthLimit;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_SCS.FrameToSettings;
var
  i,Count:  Integer;
begin
// general
fProcessingSettings.EntryTabInMem := cbEntryTabInMem.Checked;
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
      fProcessingSettings.PathResolve.CustomPaths[Count] := StrToAnsi(meCustomPaths.Lines[i]);
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
// content parsing
fProcessingSettings.PathResolve.ContentParsing.ParseContent := cbCPParseContent.Checked;
fProcessingSettings.PathResolve.ContentParsing.ParseEverything := cbCPParseEverything.Checked;
fProcessingSettings.PathResolve.ContentParsing.ParseHelpArchives := cbCPParseHelpArch.Checked;
fProcessingSettings.PathResolve.ContentParsing.ParseEverythingInHlpArch := cbCPParseEverythingInHlpArch.Checked;
fProcessingSettings.PathResolve.ContentParsing.PrintableASCIIOnly := cbCPPrintableASCIIOnly.Checked;
fProcessingSettings.PathResolve.ContentParsing.LimitedCharacterSet := cbCPLimitedCharSet.Checked;
fProcessingSettings.PathResolve.ContentParsing.BinaryThreshold := seBinaryThreshold.Value / 1000;
fProcessingSettings.PathResolve.ContentParsing.MinPathLength := seMinPathLength.Value;
// brute force
fProcessingSettings.PathResolve.BruteForce.ActivateBruteForce := cbBFActivate.Checked;
fProcessingSettings.PathResolve.BruteForce.Multithreaded := cbBFMultithreaded.Checked;
fProcessingSettings.PathResolve.BruteForce.UseKnownPaths := cbBFUseKnownPaths.Checked;
fProcessingSettings.PathResolve.BruteForce.PrintableASCIIOnly := cbBFPrintableASCIIOnly.Checked;
fProcessingSettings.PathResolve.BruteForce.LimitedCharSet := cbBFLimitedCharSet.Checked;
fProcessingSettings.PathResolve.BruteForce.PathLengthLimit := seBFMaxPathLength.Value;
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
cbCPParseContent.OnClick(cbCPParseContent);
cbCPParseHelpArch.OnClick(cbCPParseHelpArch);
cbCPPrintableASCIIOnly.OnClick(cbCPPrintableASCIIOnly);
cbBFActivate.OnClick(cbBFActivate);
cbBFPrintableASCIIOnly.OnClick(cbBFPrintableASCIIOnly);
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
If (Sender is TCheckBox) and not fLoading then
  case TCheckBox(Sender).Tag of
    1241,   //cbCPParseContent
    1243:   //cbCPParseHelpArch
      begin
        cbCPParseEverything.Enabled := cbCPParseContent.Checked;
        cbCPParseEverythingInHlpArch.Enabled := cbCPParseHelpArch.Checked;
        cbCPPrintableASCIIOnly.Enabled := cbCPParseContent.Checked or cbCPParseHelpArch.Checked;
        cbCPLimitedCharSet.Enabled := (cbCPParseContent.Checked or cbCPParseHelpArch.Checked) and
                                      cbCPPrintableASCIIOnly.Checked;
        lblBinaryThreshold.Enabled := cbCPParseContent.Checked or cbCPParseHelpArch.Checked;
        seBinaryThreshold.Enabled := cbCPParseContent.Checked or cbCPParseHelpArch.Checked;
        lblMinPathLength.Enabled := cbCPParseContent.Checked or cbCPParseHelpArch.Checked;
        seMinPathLength.Enabled := cbCPParseContent.Checked or cbCPParseHelpArch.Checked;
      end;
    1245:   //cbCPPrintableASCIIOnly
      cbCPLimitedCharSet.Enabled := TCheckBox(Sender).Checked and
        (cbCPParseContent.Checked or cbCPParseHelpArch.Checked);
    1261:   //cbBFActivate
      begin
        cbBFMultithreaded.Enabled := TCheckBox(Sender).Checked;
        cbBFUseKnownPaths.Enabled := TCheckBox(Sender).Checked;
        cbBFPrintableASCIIOnly.Enabled := TCheckBox(Sender).Checked;
        cbBFLimitedCharSet.Enabled := TCheckBox(Sender).Checked and cbBFPrintableASCIIOnly.Checked;
        lblBFMaxPathLength.Enabled := TCheckBox(Sender).Checked;
        seBFMaxPathLength.Enabled := TCheckBox(Sender).Checked;
      end;
    1264:   //cbBFPrintableASCIIOnly
      cbBFLimitedCharSet.Enabled := TCheckBox(Sender).Checked and cbBFActivate.Checked;
  end;
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
    If Assigned(Control) and ((Control is TCheckBox) or (Control is TSpinEdit)) then
      OptionMouseMove(Control,Shift,X,Y);
  end;
end;

//==============================================================================

procedure TfrmProcSettingsFrame_SCS.cmbPresetsChange(Sender: TObject);
begin
If not fLoading and (cmbPresets.ItemIndex >= 0) then
  begin
    fProcessingSettings := DART_PS_SCS_Presets[cmbPresets.ItemIndex].PresetData;
    SettingsToFrame;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_SCS.meCustomPathsKeyPress(Sender: TObject;
  var Key: Char);
begin
If Key = ^A then
  begin
    Key := #0;
    meCustomPaths.SelectAll;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_SCS.meHelpArchivesKeyPress(Sender: TObject;
  var Key: Char);
begin
If Key = ^A then
  begin
    Key := #0;
    meHelpArchives.SelectAll;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_SCS.btnHelpArchivesMenuClick(Sender: TObject);
var
  PopupPoint: TPoint;
begin
PopupPoint := gbPathResolve.ClientToScreen(Point(btnHelpArchivesMenu.Left,btnHelpArchivesMenu.BoundsRect.Bottom));
pmHelpArchivesMenu.Popup(PopupPoint.X,PopupPoint.Y);
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_SCS.LoadGameFiles(Sender: TObject);
var
  FileList: TStringList;
  i:        Integer;

  procedure GetListing(const Path: String; Files: TStringList);
  var
    SearchRec: TSearchRec;
  begin
    If DART_FindFirst(IncludeTrailingPathDelimiter(Path) + '*.scs',faAnyFile,{%H-}SearchRec) = 0 then
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
