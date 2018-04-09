{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program DART;

uses
  Forms,
  MainForm in '..\MainForm.pas' {fMainForm},
  ResultInfoForm in '..\ResultInfoForm.pas' {fResultInfoForm},
  ProcSettingsFrame_ZIP in '..\ProcSettingsFrame_ZIP.pas' {frmProcSettingsFrame_ZIP: TFrame},
  ProcSettingsFrame_SCS in '..\ProcSettingsFrame_SCS.pas' {frmProcSettingsFrame_SCS: TFrame},
  ProcSettingsForm in '..\ProcSettingsForm.pas' {fProcSettingsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'D.A.R.T';
  Application.CreateForm(TfMainForm, fMainForm);
  Application.CreateForm(TfResultInfoForm, fResultInfoForm);
  Application.CreateForm(TfProcSettingsForm, fProcSettingsForm);
  Application.Run;
end.
