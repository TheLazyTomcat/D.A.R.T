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
  TaskbarProgress in '..\TaskbarProgress.pas',
  MainForm in '..\MainForm.pas' {fMainForm},
  PrcsSettingsForm in '..\PrcsSettingsForm.pas' {fPrcsSettingsForm},
  ErrorForm in '..\ErrorForm.pas' {fErrorForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'D.A.R.T';
  Application.CreateForm(TfMainForm, fMainForm);
  Application.CreateForm(TfErrorForm, fErrorForm);
  Application.CreateForm(TfPrcsSettingsForm, fPrcsSettingsForm);
  Application.Run;
end.
