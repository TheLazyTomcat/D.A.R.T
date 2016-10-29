{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer;

{$INCLUDE DART_defs.inc}

interface

uses
  SysUtils, Classes,
  AuxTypes, WinSyncObjs,
  DART_ProcessingSettings, DART_MemoryBuffer;

type
  ERepairerException = class(Exception);

  TResultState = (rsUndefined,rsNormal,rsWarning,rsError);

  TResultInfoError = record
    MethodIndex:    Integer;
    MethodName:     String;
    ExceptionClass: String;
    ExceptionText:  String;
  end;

  TResultInfoWarning = record
    Warnings: array of String;
  end;

  TResultInfo = record
    ResultState:  TResultState;
    RepairerInfo: String;
    WarningInfo:  TResultInfoWarning;
    ErrorInfo:    TResultInfoError;
  end;

const
  DefaultResultInfo: TResultInfo = (
    ResultState:  rsUndefined;
    RepairerInfo: '';
    WarningInfo: (
      Warnings: nil);
    ErrorInfo: (
      MethodIndex:    -1;
      MethodName:     '';
      ExceptionClass: '';
      ExceptionText:  ''));

type
  TProgressEvent = procedure(Sender: TObject; Progress: Single) of object;

  TProgressStage = record
    Offset: Single;
    Range:  Single;
  end;

const
  PROCSTAGEIDX_NoProgress = -100;
  PROCSTAGEIDX_Direct     = -1;
  PROCSTAGEIDX_Default    = 0;
  PROCSTAGEIDX_Custom     = 1;
  PROCSTAGEIDX_Loading    = 2;
  PROCSTAGEIDX_Saving     = 3;

{==============================================================================}
{   TRepairer - class declaration                                              }
{==============================================================================}
type
  TRepairer = class(TObject)
  private
    fExpectedSignature:       UInt32;
    fFlowControlObject:       TEvent;
    fTerminatedFlag:          Integer;
    fResultInfo:              TResultInfo;
    fOnProgress:              TProgressEvent;
  protected
    fArchiveStream:           TStream;
    fLocalFormatSettings:     TFormatSettings;
    fFileProcessingSettings:  TFileProcessingSettings;
    fProgressStages:          array of TProgressStage;
    // preallocated memory buffers
    fIO_Buffer:               TMemoryBuffer;
    fCED_Buffer:              TMemoryBuffer;
    fUED_BUffer:              TMemoryBuffer;
    // initialization methods
    //procedure RectifyFileProcessingSettings; virtual; abstract;
    //procedure InitializeData; virtual; abstract;
    procedure InitializeProgress; virtual;
    // flow control and progress report methods
    procedure DoProgress(ProgressStageIdx: Integer; Data: Single); virtual;
    procedure DoWarning(const WarningText: String); virtual;
    procedure DoError(MethodIndex: Integer; const ErrorText: String; Values: array of const); overload; virtual;
    procedure DoError(MethodIndex: Integer; const ErrorText: String); overload; virtual;
    // common processing functions
    procedure CheckArchiveSignature; virtual;
    procedure ProgressedLoadFile(const FileName: String; Stream: TStream; ProgressStage: Integer); virtual;
    procedure ProgressedSaveFile(const FileName: String; Stream: TStream; ProgressStage: Integer); virtual;
    // memory buffers management
    procedure AllocateMemoryBuffers; virtual;
    procedure FreeMemoryBuffers; virtual;
    // main processing methods
    procedure MainProcessing; virtual;
    //procedure ArchiveProcessing; virtual; abstract; // <- all the fun must happen here
  public
    // helper methods
    class Function CreateFileStream(const FileName: String; Mode: Word): TFileStream; virtual;
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; virtual;
    constructor Create(FlowControlObject: TEvent; FileProcessingSettings: TFileProcessingSettings);
    destructor Destroy; override;
    procedure Run; virtual;
    procedure Stop; virtual;
    Function Terminated: Boolean; virtual;
  published
    property ExpectedSignature: UInt32 read fExpectedSignature write fExpectedSignature;
    property ResultInfo: TResultInfo read fResultInfo;
    property OnProgress: TProgressEvent read fOnProgress write fOnProgress;
  end;

implementation

uses
  Windows;

const
  // Size of the buffer used in progress-aware stream reading and writing
  IO_BufferSize  = $100000; {1MiB}

  // Initial size of buffer used to hold compressed entry data
  CED_BufferSize = $100000 * 8; {8MiB}

  // Initial size of buffer used to hold uncompressed entry data
  UED_BufferSize = $100000 * 16; {16MiB}

{==============================================================================}
{   TRepairer - class implementation                                           }
{==============================================================================}

procedure TRepairer.InitializeProgress;
begin
SetLength(fProgressStages,4);
// all values that are not explicitly set are equal to 0.0
fProgressStages[PROCSTAGEIDX_Default].Range := 1.0;
If fFileProcessingSettings.Common.InMemoryProcessing then
  begin
    case fFileProcessingSettings.Common.RepairMethod of
      rmRebuild: begin
                   fProgressStages[PROCSTAGEIDX_Loading].Range := 0.3;
                   fProgressStages[PROCSTAGEIDX_Saving].Range := 0.3;
                 end;
      rmExtract: fProgressStages[PROCSTAGEIDX_Loading].Range := 0.4;
    end;
    fProgressStages[PROCSTAGEIDX_Saving].Offset := 1.0 - fProgressStages[PROCSTAGEIDX_Saving].Range;
  end;
end;

//------------------------------------------------------------------------------

procedure TRepairer.DoProgress(ProgressStageIdx: Integer; Data: Single);
begin
fFlowControlObject.WaitFor;
If Terminated then
  begin
    InterlockedExchange(fTerminatedFlag,0);  
    DoError(-1,'Processing terminated. Data can be in inconsistent state.');
  end;
If (ProgressStageIdx >= Low(fProgressStages)) and (ProgressStageIdx <= High(fProgressStages)) then
  begin
    If Data > 1.0 then Data := 1.0;
    Data := fProgressStages[ProgressStageIdx].Offset + (fProgressStages[ProgressStageIdx].Range * Data);
    If Assigned(fOnProgress) then fOnProgress(Self,Data);
  end
else
  begin
    If ProgressStageIdx <> PROCSTAGEIDX_NoProgress then
      If Assigned(fOnProgress) then fOnProgress(Self,Data);
  end;
end;

//------------------------------------------------------------------------------

procedure TRepairer.DoWarning(const WarningText: String);
begin
fResultInfo.ResultState := rsWarning;
with fResultInfo.WarningInfo do
  begin
    SetLength(Warnings,Length(Warnings) + 1);
    Warnings[High(Warnings)] := WarningText;
  end;
end;

//------------------------------------------------------------------------------

procedure TRepairer.DoError(MethodIndex: Integer; const ErrorText: String; Values: array of const);
begin
fResultInfo.ErrorInfo.MethodIndex := MethodIndex;
raise ERepairerException.Create(Format(ErrorText,Values,fLocalFormatSettings));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TRepairer.DoError(MethodIndex: Integer; const ErrorText: String);
begin
DoError(MethodIndex,ErrorText,[]);
end;

//------------------------------------------------------------------------------

procedure TRepairer.CheckArchiveSignature;
var
  Signature: UInt32;
begin
fArchiveStream.Seek(0,soBeginning);
If fArchiveStream.Read({%H-}Signature,SizeOf(Signature)) >= SizeOf(Signature) then
  begin
    If Signature <> fExpectedSignature then
      DoError(1,'Bad file signature (0x%.8x).',[Signature]);
  end
else DoError(1,'File is too small to contain valid signature (%d bytes).',[fArchiveStream.Size]);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProgressedLoadFile(const FileName: String; Stream: TStream; ProgressStage: Integer);
var
  FileStream: TFileStream;
  BytesRead:  Integer;
begin
DoProgress(ProgressStage,0.0);
FileStream := CreateFileStream(FileName,fmOpenRead or fmShareDenyWrite);
try
  Stream.Seek(0,soFromBeginning);
  Stream.Size := FileStream.Size; // prevents reallocations
  If FileStream.Size > 0 then
    repeat
      BytesRead := FileStream.Read(fIO_Buffer.Memory^,fIO_Buffer.Size);
      Stream.Write(fIO_Buffer.Memory^,BytesRead);
      DoProgress(ProgressStage,FileStream.Position / FileStream.Size);
    until BytesRead <= 0;
finally
  FileStream.Free;
end;
DoProgress(ProgressStage,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProgressedSaveFile(const FileName: String; Stream: TStream; ProgressStage: Integer);
var
  FileStream: TFileStream;
  BytesRead:  Integer;
begin
DoProgress(ProgressStage,0.0);
FileStream := CreateFileStream(FileName,fmOpenRead or fmShareDenyWrite);
try
  Stream.Seek(0,soFromBeginning);
  FileStream.Size := Stream.Size;  
  If Stream.Size > 0 then
    repeat
      BytesRead := Stream.Read(fIO_Buffer.Memory^,fIO_Buffer.Size);
      FileStream.Write(fIO_Buffer.Memory^,BytesRead);
      DoProgress(ProgressStage,Stream.Position / Stream.Size);
    until BytesRead <= 0;
finally
  FileStream.Free;
end;
DoProgress(ProgressStage,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.AllocateMemoryBuffers;
begin
AllocateMemoryBuffer(fIO_Buffer,IO_BufferSize);
AllocateMemoryBuffer(fCED_Buffer,CED_BufferSize);
AllocateMemoryBuffer(fUED_BUffer,UED_BUfferSize);
end;

//------------------------------------------------------------------------------

procedure TRepairer.FreeMemoryBuffers;
begin
FreeMemoryBuffer(fIO_Buffer);
FreeMemoryBuffer(fCED_Buffer);
FreeMemoryBuffer(fUED_BUffer);
end;

//------------------------------------------------------------------------------

procedure TRepairer.MainProcessing;
var
  i:  Integer;
begin
fResultInfo.ResultState := rsNormal;
DoProgress(PROCSTAGEIDX_Direct,0.0);
try
  AllocateMemoryBuffers;
  try
    If fFileProcessingSettings.Common.InMemoryProcessing then
      fArchiveStream := TMemoryStream.Create
    else
      fArchiveStream := CreateFileStream(fFileProcessingSettings.Common.FilePath,fmOpenRead or fmShareDenyWrite);
    try
      If fFileProcessingSettings.Common.InMemoryProcessing then
        ProgressedLoadFile(fFileProcessingSettings.Common.FilePath,fArchiveStream,PROCSTAGEIDX_Loading);
      If fArchiveStream.Size <= 0 then
        DoError(0,'Input file does not contain any data.');
      If not fFileProcessingSettings.Common.IgnoreFileSignature then
        CheckArchiveSignature;
      //ArchiveProcessing;
    finally
      fArchiveStream.Free;
    end;

    {$message 'testing'}
    For i := 0 to 100 do
      begin
        Sleep(10);
        DoProgress(PROCSTAGEIDX_Default,i/100);
      end;
    DoWarning('test');

  finally
    FreeMemoryBuffers;
  end;
  DoProgress(PROCSTAGEIDX_Direct,2.0);
except
  on E: Exception do
    begin
      fResultInfo.ResultState := rsError;
      fResultInfo.ErrorInfo.MethodName := GetMethodNameFromIndex(fResultInfo.ErrorInfo.MethodIndex);
      fResultInfo.ErrorInfo.ExceptionClass := E.ClassName;
      fResultInfo.ErrorInfo.ExceptionText := E.Message;
      DoProgress(PROCSTAGEIDX_Direct,-1.0);
    end;
end;
end;

//==============================================================================

class Function TRepairer.CreateFileStream(const FileName: String; Mode: Word): TFileStream;
begin
{$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
Result := TFileStream.Create(UTF8ToSys(FileName),Mode);
{$ELSE}
Result := TFileStream.Create(FileName,Mode);
{$IFEND}
end;

//------------------------------------------------------------------------------

class Function TRepairer.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  0:  Result := 'MainProcessing';
  1:  Result := 'CheckArchiveSignature;';
else
  Result := 'unknown method';
end;
end;

//------------------------------------------------------------------------------

constructor TRepairer.Create(FlowControlObject: TEvent; FileProcessingSettings: TFileProcessingSettings);
begin
inherited Create;
fExpectedSignature := 0;
fFlowControlObject := FlowControlObject;
fTerminatedFlag := 0;
fResultInfo := DefaultResultInfo;
fResultInfo.RepairerInfo := Format('%s(0x%p)',[Self.ClassName,Pointer(Self)]);
{$WARN SYMBOL_PLATFORM OFF}
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,{%H-}fLocalFormatSettings);
{$WARN SYMBOL_PLATFORM ON}
fFileProcessingSettings := FileProcessingSettings;
SetLength(fProgressStages,0);
// initialization
//RectifyFileProcessingSettings;
//InitializeData;
InitializeProgress;
end;

//------------------------------------------------------------------------------

destructor TRepairer.Destroy;
begin
inherited;
end;

//------------------------------------------------------------------------------

procedure TRepairer.Run;
begin
InterlockedExchange(fTerminatedFlag,0);
MainProcessing;
end;

//------------------------------------------------------------------------------

procedure TRepairer.Stop;
begin
InterlockedExchange(fTerminatedFlag,-1);
end;

//------------------------------------------------------------------------------

Function TRepairer.Terminated: Boolean;
begin
Result := InterlockedExchangeAdd(fTerminatedFlag,0) <> 0;
end;

end.
