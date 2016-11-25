{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_RepairerThread;

{$INCLUDE DART_defs.inc}

interface

uses
  Classes,
  WinSyncObjs,
  DART_ProcessingSettings, DART_Repairer;

{==============================================================================}
{------------------------------------------------------------------------------}
{                                TRepairerThread                               }
{------------------------------------------------------------------------------}
{==============================================================================}

type
  TFileProgressEvent = procedure(Sender: TObject; FileIndex: Integer; Progress: Single) of object;

{==============================================================================}
{   TRepairerThread - class declaration                                        }
{==============================================================================}

  TRepairerThread = class(TThread)
  private
    sync_Progress:            Single;
    fFileIndex:               Integer;
    fFileProcessingSettings:  TFileProcessingSettings;
    fFlowControlObject:       TEvent;
    fRepairer:                TRepairer;
    fResultInfo:              TResultInfo;
    fOnFileProgress:          TFileProgressEvent;
  protected
    procedure sync_DoProgress; virtual;
    procedure ProgressHandler(Sender: TObject; Progress: Single); virtual;
    procedure Execute; override;
  public
    constructor Create(FileIndex: Integer; FileProcessingSettings: TFileProcessingSettings);
    destructor Destroy; override;
    procedure StartProcessing; virtual;
    procedure PauseProcessing; virtual;
    procedure ResumeProcessing; virtual;
    procedure StopProcessing; virtual;
    property FileProcessingSettings: TFileProcessingSettings read fFileProcessingSettings;
    property ResultInfo: TResultInfo read fResultInfo;
  published
    property FileIndex: Integer read fFileIndex;
    property OnFileProgress: TFileProgressEvent read fOnFileProgress write fOnFileProgress;
  end;

implementation

uses
  SysUtils,
  DART_Repairer_ZIP_Rebuild, DART_Repairer_ZIP_Extract,
  DART_Repairer_SCS_Rebuild, DART_Repairer_SCS_Extract;

{==============================================================================}
{------------------------------------------------------------------------------}
{                                TRepairerThread                               }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TRepairerThread - class implementation                                     }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TRepairerThread - protected methods                                        }
{------------------------------------------------------------------------------}

procedure TRepairerThread.sync_DoProgress;
begin
If Assigned(fOnFileProgress) then fOnFileProgress(Self,fFileIndex,sync_Progress);
end;

//------------------------------------------------------------------------------

procedure TRepairerThread.ProgressHandler(Sender: TObject; Progress: Single);
begin
sync_Progress := Progress;
If (Progress < 0.0) or (Progress > 1.0) then
  fResultInfo := fRepairer.ResultInfo;
Synchronize(sync_DoProgress);
end;

//------------------------------------------------------------------------------

procedure TRepairerThread.Execute;
begin
fRepairer.Run;
end;

{------------------------------------------------------------------------------}
{   TRepairerThread - public methods                                           }
{------------------------------------------------------------------------------}

constructor TRepairerThread.Create(FileIndex: Integer; FileProcessingSettings: TFileProcessingSettings);
begin
inherited Create(True);
FreeOnTerminate := False;
fFileIndex := FileIndex;
// the settings cannot be changed after this point, so thread safety is not an issue
fFileProcessingSettings := FileProcessingSettings;
fFlowControlObject := TEvent.Create;
// select proper repairer according to file type and required operation
case fFileProcessingSettings.Common.FileType of
  atSCS_sig,atSCS_frc:
    case fFileProcessingSettings.Common.RepairMethod of
      rmRebuild:  fRepairer := TRepairer_SCS_Rebuild.Create(fFlowControlObject,fFileProcessingSettings);
      rmExtract:  fRepairer := TRepairer_SCS_Extract.Create(fFlowControlObject,fFileProcessingSettings);
    else
      raise Exception.CreateFmt('TRepairerThread.Create: Unknown repair method (%d).',[Ord(fFileProcessingSettings.Common.RepairMethod)]);
    end;
  atZIP_sig,atZIP_frc,atZIP_dft:
    case fFileProcessingSettings.Common.RepairMethod of
      rmRebuild:  fRepairer := TRepairer_ZIP_Rebuild.Create(fFlowControlObject,fFileProcessingSettings);
      rmExtract:  fRepairer := TRepairer_ZIP_Extract.Create(fFlowControlObject,fFileProcessingSettings);
    else
      raise Exception.CreateFmt('TRepairerThread.Create: Unknown repair method (%d).',[Ord(fFileProcessingSettings.Common.RepairMethod)]);
    end;
else
  raise Exception.CreateFmt('TRepairerThread.Create: Unknown file type (%d).',[Ord(fFileProcessingSettings.Common.FileType)]);
end;
fRepairer.OnProgress := ProgressHandler;
end;

//------------------------------------------------------------------------------

destructor TRepairerThread.Destroy;
begin
fRepairer.Free;
fFlowControlObject.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TRepairerThread.StartProcessing;
begin
fFlowControlObject.SetEvent;
{$IF Defined(FPC) or (CompilerVersion >= 21)} // Delphi 2010+
Start;
{$ELSE}
Resume;
{$IFEND}
end;

//------------------------------------------------------------------------------

procedure TRepairerThread.PauseProcessing;
begin
fFlowControlObject.ResetEvent;
end;

//------------------------------------------------------------------------------

procedure TRepairerThread.ResumeProcessing;
begin
fFlowControlObject.SetEvent;
end;

//------------------------------------------------------------------------------

procedure TRepairerThread.StopProcessing;
begin
fRepairer.Stop;
end;

end.
