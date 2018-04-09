{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program DART;

{$INCLUDE '..\..\Source\DART_defs.inc'}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainForm,
  ResultInfoForm,
  ProcSettingsFrame_SCS,
  ProcSettingsFrame_ZIP,
  ProcSettingsForm;

{$R *.res}

begin
  Application.Title:='D.A.R.T';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfMainForm, fMainForm);
  Application.CreateForm(TfResultInfoForm, fResultInfoForm);
  Application.CreateForm(TfProcSettingsForm, fProcSettingsForm);
  Application.Run;
end.

