{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program SCS_Unlocker;

uses
  FastMM4,
  Forms,
  MainForm in '..\MainForm.pas' {fMainForm},
  CRC32 in '..\Libs\CRC32.pas',
  Repairer in '..\Repairer.pas',
  FilesManager in '..\FilesManager.pas',
  MulticastEvent in '..\Libs\MulticastEvent.pas',
  WndAlloc in '..\Libs\WndAlloc.pas',
  UtilityWindow in '..\Libs\UtilityWindow.pas',
  ErrorForm in '..\ErrorForm.pas' {fErrorForm},
  PrcsSettingsForm in '..\PrcsSettingsForm.pas' {fPrcsSettingsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SCS Unlocker';
  Application.CreateForm(TfMainForm, fMainForm);
  Application.CreateForm(TfErrorForm, fErrorForm);
  Application.CreateForm(TfPrcsSettingsForm, fPrcsSettingsForm);
  Application.Run;
end.
