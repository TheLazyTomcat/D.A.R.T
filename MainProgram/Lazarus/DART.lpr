{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
program DART;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  SysUtils,
  Forms,

  DART_Auxiliary,
  DART_MemoryBuffer,
  DART_AnsiStringList,
  DART_PathDeconstructor,
  DART_ProcessingSettings,

  DART_Format_ZIP,
  DART_Format_SCS,

  DART_Repairer,
  DART_Repairer_ZIP,
  DART_Repairer_ZIP_Rebuild,
  DART_Repairer_ZIP_Extract,
  DART_Repairer_ZIP_Convert,
  DART_Repairer_SCS,
  DART_Repairer_SCS_Rebuild,
  DART_Repairer_SCS_Extract,
  DART_Repairer_SCS_Convert,

  DART_RepairerThread,
  DART_FileManager,

  MainForm,
  ResultInfoForm,
  ProcSettingsZIPFrame,
  ProcSettingsSCSFrame,
  ProcSettingsForm;

{$R *.res}

begin
  Application.Title:='D.A.R.T';
{$IFDEF Debug}
  If FileExists(ExtractFilePath(ParamStr(0)) + 'heap.trc') then
    DeleteFile(ExtractFilePath(ParamStr(0)) + 'heap.trc');
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + 'heap.trc');
{$ENDIF}
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfMainForm, fMainForm);
  Application.CreateForm(TfResultInfoForm, fResultInfoForm);
  Application.CreateForm(TfProcSettingsForm, fProcSettingsForm);
  Application.Run;
end.

