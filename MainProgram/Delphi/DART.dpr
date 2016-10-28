{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program DART;

uses
  FastMM4,
  Forms,
  Repairer in '..\Repairer.pas',
  FilesManager in '..\FilesManager.pas',
  MainForm in '..\MainForm.pas' {fMainForm},
  PrcsSettingsForm in '..\PrcsSettingsForm.pas' {fPrcsSettingsForm},
  ErrorForm in '..\ErrorForm.pas' {fErrorForm},
  DART_Auxiliary in '..\Source\DART_Auxiliary.pas',
  DART_FileManager in '..\Source\DART_FileManager.pas',
  DART_ProcessingSettings in '..\Source\DART_ProcessingSettings.pas',
  DART_Repairer_SCS in '..\Source\DART_Repairer_SCS.pas',
  DART_Repairer_ZIP in '..\Source\DART_Repairer_ZIP.pas',
  DART_Repairer in '..\Source\DART_Repairer.pas',
  DART_RepairerThread in '..\Source\DART_RepairerThread.pas',
  DART_MemoryBuffer in '..\Source\DART_MemoryBuffer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'D.A.R.T';
  Application.CreateForm(TfMainForm, fMainForm);
  Application.CreateForm(TfErrorForm, fErrorForm);
  Application.CreateForm(TfPrcsSettingsForm, fPrcsSettingsForm);
  Application.Run;
end.
