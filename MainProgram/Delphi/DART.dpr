{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program DART;

uses
  FastMM4,
  Forms,

  DART_Auxiliary          in '..\Source\DART_Auxiliary.pas',
  DART_MemoryBuffer       in '..\Source\DART_MemoryBuffer.pas',
  DART_AnsiStringList     in '..\Source\DART_AnsiStringList.pas',
  DART_PathDeconstructor  in '..\Source\DART_PathDeconstructor.pas',
  DART_ProcessingSettings in '..\Source\DART_ProcessingSettings.pas',

  DART_Repairer             in '..\Source\DART_Repairer.pas',
  DART_Repairer_ZIP         in '..\Source\DART_Repairer_ZIP.pas',
  DART_Repairer_ZIP_Rebuild in '..\Source\DART_Repairer_ZIP_Rebuild.pas',
  DART_Repairer_ZIP_Extract in '..\Source\DART_Repairer_ZIP_Extract.pas',
  DART_Repairer_SCS         in '..\Source\DART_Repairer_SCS.pas',
  DART_Repairer_SCS_Rebuild in '..\Source\DART_Repairer_SCS_Rebuild.pas',
  DART_Repairer_SCS_Extract in '..\Source\DART_Repairer_SCS_Extract.pas',

  DART_RepairerThread in '..\Source\DART_RepairerThread.pas',
  DART_FileManager    in '..\Source\DART_FileManager.pas',

  MainForm             in '..\MainForm.pas' {fMainForm},
  ResultInfoForm       in '..\ResultInfoForm.pas' {fResultInfoForm},
  ProcSettingsZIPFrame in '..\ProcSettingsZIPFrame.pas' {frmProcSettingsZIP: TFrame},
  ProcSettingsSCSFrame in '..\ProcSettingsSCSFrame.pas' {frmProcSettingsSCS: TFrame},
  PrcsSettingsForm     in '..\PrcsSettingsForm.pas' {fPrcsSettingsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'D.A.R.T';
  Application.CreateForm(TfMainForm, fMainForm);
  Application.CreateForm(TfResultInfoForm, fResultInfoForm);
  Application.CreateForm(TfPrcsSettingsForm, fPrcsSettingsForm);
  Application.Run;
end.
