{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Resolver_BruteForce;

{$INCLUDE DART_defs.inc}

interface

uses
  ConcurrentTasks,
  DART_Format_SCS, DART_Common, DART_ProcessingSettings, DART_Resolver;

{===============================================================================
--------------------------------------------------------------------------------
                            TDARTResolver_BruteForce
--------------------------------------------------------------------------------
===============================================================================}

type
  TDARTUsedKnownPaths = record
    Arr:    array of AnsiString;
    Count:  Integer;
  end;

  TDARTAlphabet = record
    Letters:  array[Byte] of AnsiChar;
    Count:    Integer;
  end;

{===============================================================================
    TDARTResolver_BruteForce - class declaration
===============================================================================}
type
  TDARTResolver_BruteForce = class(TDARTResolver)
  private
    fBruteForceSettings:        TDART_PS_SCS_PathResolve_BruteForce;
    fUsedKnownPaths:            TDARTUsedKnownPaths;
    fAlphabet:                  TDARTAlphabet;
    // multithreading
    fTasksManager:              TCNTSManager;
    fUpdateCounter:             Integer;
    fProcessorShadow:           AnsiString;
    fProcessorBuffer:           AnsiString;
    fProcessingTerminating:     Boolean;
    fProcessingTerminated:      Boolean;
  protected
    // single thread (local) processing
    procedure BF_MainProcessing_SingleThreaded(Progress: Boolean = True); virtual;
    // multithreaded processing
    procedure BF_MainProcessing_MultiThreaded; virtual;
    Function BF_AdvanceProcessorBuffer: Boolean; virtual;
    procedure BF_Processor_OnProgress(Sender: TObject; {%H-}TaskIndex: Integer); virtual;
    procedure BF_Processor_OnCompleted(Sender: TObject; TaskIndex: Integer); virtual;
  public
    constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
    destructor Destroy; override;
    procedure Initialize(const ArchiveStructure: TDART_SCS_ArchiveStructure); override;
    procedure Run; override;
    procedure Stop; override;
  end;

implementation

uses
  SysUtils,
  AuxTypes, CITY, StrRect;

{===============================================================================
--------------------------------------------------------------------------------
                            TDARTBruteForceProcessor
--------------------------------------------------------------------------------
===============================================================================}

const
  DART_RES_BF_LimitedCharSet: AnsiString = '0123456789abcdefghijklmnopqrstuvwxyz_.-/';

{===============================================================================
    TDARTBruteForceProcessor - class declaration
===============================================================================}
type
  TDARTBruteForceProcessor = class(TCNTSTask)
  private
    fPauseControlObject:        TDARTPauseObject;
    fArchiveProcessingSettings: TDARTArchiveProcessingSettings;
    fBruteForceSettings:        TDART_PS_SCS_PathResolve_BruteForce;
    fUsedKnownPaths:            TDARTUsedKnownPaths;
    fUnresolved:                TDARTUnresolvedEntries;
    fResolved:                  TDARTResolvedEntries;
    fHashType:                  UInt32;
    fAlphabet:                  TDARTAlphabet;
    fBuffer:                    AnsiString;
    fLastUpdateCounter:         Integer;
    Function GetResolved(Index: Integer): TDARTResolvedEntry;
  protected
    Function Unresolved_IndexOf(Hash: TDARTHash64): Integer; virtual;
    procedure Unresolved_MoveToResolved(Index: Integer; const Path: AnsiString); virtual;
  public
    constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
    procedure Initialize(const UsedKnownPaths: TDARTUsedKnownPaths; const Unresolved: TDARTUnresolvedEntries; HashType: UInt32; const Alphabet: TDARTAlphabet); virtual;
    procedure SetNewBuffer(const Buffer: AnsiString); virtual;
    Function Main: Boolean; override;
    property Resolved[Index: Integer]: TDARTResolvedEntry read GetResolved;
    property ResolvedCount: Integer read fResolved.Count;
    property LastUpdateCounter: Integer read fLastUpdateCounter write fLastUpdateCounter;
  end;

{===============================================================================
    Unit-wide Private constants
===============================================================================}

const
  DART_RES_MultThrLength = 3; // length of path that is calculated in thread

{===============================================================================
--------------------------------------------------------------------------------
                            TDARTBruteForceProcessor
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTBruteForceProcessor - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTBruteForceProcessor - private methods
-------------------------------------------------------------------------------}

Function TDARTBruteForceProcessor.GetResolved(Index: Integer): TDARTResolvedEntry;
begin
If (Index >= Low(fResolved.Arr)) and (Index < fResolved.Count) then
  begin
    Result := fResolved.Arr[Index];
    UniqueString(Result.Path);
  end
else raise Exception.CreateFmt('TDARTBruteForceProcessor.GetResolved: Index (%d) out of bounds.',[Index]);
end;

{-------------------------------------------------------------------------------
    TDARTBruteForceProcessor - protected methods
-------------------------------------------------------------------------------}

Function TDARTBruteForceProcessor.Unresolved_IndexOf(Hash: TDARTHash64): Integer;
var
  i,Min,Max:  Integer;
begin
Result := -1;
Min := 0;
Max := Pred(fUnresolved.Count);
If Max < 10 then
  begin
    // for short lists use normal linear search
    For i := 0 to Max do
      If HashCompare(fUnresolved.Arr[i],Hash) = 0 then
        begin
          Result := i;
          Break{For i};
        end;
  end
else
  // for longer lists, use binary search
  while Max >= min do
    begin
      i := ((max - Min) shr 1) + Min;
      // i-th entry has lower hash than is requested
      If HashCompare(fUnresolved.Arr[i],Hash) > 0 then Min := i + 1
        // i-th entry has higher hash than is requested
        else If HashCompare(fUnresolved.Arr[i],Hash) < 0 then Max := i - 1
          else begin
            // i-th entry has the requested hash
            Result := i;
            Break{while};
          end;
    end;
end;

//------------------------------------------------------------------------------

procedure TDARTBruteForceProcessor.Unresolved_MoveToResolved(Index: Integer; const Path: AnsiString);
var
  i:  Integer;
begin
If fResolved.Count >= Length(fResolved.Arr) then
  SetLength(fResolved.Arr,Length(fResolved.Arr) + 1024);
fResolved.Arr[fResolved.Count].Hash := fUnresolved.Arr[Index];
fResolved.Arr[fResolved.Count].Path := Path;
Inc(fResolved.Count);
For i := Index to (fUnresolved.Count - 2) do
  fUnresolved.Arr[i] := fUnresolved.Arr[i + 1];
Dec(fUnresolved.Count);
end;

{-------------------------------------------------------------------------------
    TDARTBruteForceProcessor - public methods
-------------------------------------------------------------------------------}

constructor TDARTBruteForceProcessor.Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
inherited Create;
fPauseControlObject := PauseControlObject;
fArchiveProcessingSettings := ArchiveProcessingSettings;
EnsureThreadSafety(fArchiveProcessingSettings);
fBruteForceSettings := fArchiveProcessingSettings.SCS.PathResolve.BruteForce;
end;

//------------------------------------------------------------------------------

procedure TDARTBruteForceProcessor.Initialize(const UsedKnownPaths: TDARTUsedKnownPaths; const Unresolved: TDARTUnresolvedEntries; HashType: UInt32; const Alphabet: TDARTAlphabet);
var
  i:  Integer;
begin
// prepare list of used known paths
If fBruteForceSettings.UseKnownPaths then
  begin
    SetLength(fUsedKnownPaths.Arr,UsedKnownPaths.Count);
    fUsedKnownPaths.Count := UsedKnownPaths.Count;
    For i := Low(fUsedKnownPaths.Arr) to Pred(fUsedKnownPaths.Count) do
      begin
        fUsedKnownPaths.Arr[i] := UsedKnownPaths.Arr[i];
        UniqueString(fUsedKnownPaths.Arr[i]);
      end;
  end;
// prepare list of unresolved
SetLength(fUnresolved.Arr,Unresolved.Count);
fUnresolved.Count := Unresolved.Count;
For i := Low(fUnresolved.Arr) to Pred(fUnresolved.Count) do
  fUnresolved.Arr[i] := Unresolved.Arr[i];
// prepare hashing function
fHashType := HashType;
// prepare alphabet
fAlphabet := Alphabet;
end;

//------------------------------------------------------------------------------

procedure TDARTBruteForceProcessor.SetNewBuffer(const Buffer: AnsiString);
begin
fBuffer := Buffer;
UniqueString(fBuffer);
fResolved.Count := 0;
end;

//------------------------------------------------------------------------------

Function TDARTBruteForceProcessor.Main: Boolean;
var
  PosInBuff:  Integer;
  i, Index:   Integer;
  Shadow:     AnsiString;
  Buffer:     AnsiString;
begin
//prepare data
PosInBuff := 1;
SetLength(Shadow,DART_RES_MultThrLength);
FillChar(PAnsiChar(Shadow)^,Length(Shadow),0);
Buffer := fBuffer;
For i := 1 to DART_RES_MultThrLength do
  Buffer[i] := fAlphabet.Letters[0];
// main cycle
while not Terminated and (PosInBuff <= DART_RES_MultThrLength) and (fUnresolved.Count > 0)  do
  begin
    // alphabet cycle
    For i := 0 to Pred(fAlphabet.Count) do
      begin
        Shadow[1] := AnsiChar(i);
        Buffer[1] := fAlphabet.Letters[i];
        Index := Unresolved_IndexOf(PathHash(Buffer,fHashType));
        If Index >= 0 then
          Unresolved_MoveToResolved(Index,Buffer);
      end;
    // propagate cycle
    For i := 1 to DART_RES_MultThrLength do
      begin
        If Ord(Shadow[i]) < Pred(fAlphabet.Count) then
          begin
            Shadow[i] := AnsiChar(Ord(Shadow[i]) + 1);
            Buffer[i] := fAlphabet.Letters[Ord(Shadow[i])];
            Break{For i};
          end
        else
          begin
            Shadow[i] := AnsiChar(0);
            Buffer[i] := fAlphabet.Letters[Ord(Shadow[i])];
            If Succ(i) > PosInBuff then
              PosInBuff := Succ(i);
          end;
      end;
    SignalProgress(0.0);
    Cycle;  // processing of messaged
    fPauseControlObject.WaitFor;
  end;
Result := not Terminated;
end; 


{===============================================================================
--------------------------------------------------------------------------------
                            TDARTResolver_BruteForce
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTResolver_BruteForce - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTResolver_BruteForce - protected methods
-------------------------------------------------------------------------------}

procedure TDARTResolver_BruteForce.BF_MainProcessing_SingleThreaded(Progress: Boolean = True);
var
  Shadow: AnsiString;
  Buffer: AnsiString;
  i,j:    Integer;
  Index:  Integer;
begin
If Progress then DoProgress(0.0);
// prepare buffers
SetLength(Shadow,fBruteForceSettings.PathLengthLimit);
FillChar(PAnsiChar(Shadow)^,Length(Shadow),0);
SetLength(Buffer,1);
// main cycle
while (Length(Buffer) <= fBruteForceSettings.PathLengthLimit) and (fUnresolved.Count > 0) do
  begin
    // alphabet cycle
    For i := 0 to Pred(fAlphabet.Count) do
      begin
        Shadow[1] := AnsiChar(i);
        Buffer[1] := fAlphabet.Letters[i];
        // check hash of the buffer itself
        Index := Unresolved_IndexOf(PathHash(Buffer,fHashType));
        If Index < 0 then
          begin
            // bare hash not found, search in combination with used known paths
            For j := Low(fUsedKnownPaths.Arr) to Pred(fUsedKnownPaths.Count) do
              begin
              Index := Unresolved_IndexOf(PathHash(fUsedKnownPaths.Arr[j] + Buffer,fHashType));
                If Index >= 0 then
                  Unresolved_MoveToResolved(Index,fUsedKnownPaths.Arr[j] + Buffer);
              end;
          end
        else Unresolved_MoveToResolved(Index,Buffer);
      end;
    // propagate cycle
    For i := 1 to Length(Shadow) do
      begin
        If Ord(Shadow[i]) < Pred(fAlphabet.Count) then
          begin
            Shadow[i] := AnsiChar(Ord(Shadow[i]) + 1);
            Buffer[i] := fAlphabet.Letters[Ord(Shadow[i])];
            Break{For i};
          end
        else
          begin
            Shadow[i] := AnsiChar(0);
            Buffer[i] := fAlphabet.Letters[Ord(Shadow[i])];
            If (Length(Buffer) <= i) then
              begin
                SetLength(Buffer,i + 1);
                Buffer[i + 1] := fAlphabet.Letters[0];
                Break{For i};
              end;
          end;
      end;
    // not really needed, but must be here to ensure responsiveness
    DoProgress(fResolved.Count / (fUnresolved.Count + fResolved.Count));
  end;
If Progress then DoProgress(1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_BruteForce.BF_MainProcessing_MultiThreaded;
var
  i:          Integer;
  Processor:  TDARTBruteForceProcessor;
begin
DoProgress(0.0);
// do first three places in this thread
i := fBruteForceSettings.PathLengthLimit;
try
  fBruteForceSettings.PathLengthLimit := DART_RES_MultThrLength;
  BF_MainProcessing_SingleThreaded(False);
finally
  fBruteForceSettings.PathLengthLimit := i;
end;
// prepare events
fTasksManager.OnTaskProgress := BF_Processor_OnProgress;
fTasksManager.OnTaskCompleted := BF_Processor_OnCompleted;
// do longer in threads
If fUnresolved.Count > 0 then
  begin
    // prepare data
    fUpdateCounter := 0;
    SetLength(fProcessorShadow,fBruteForceSettings.PathLengthLimit);
    FillChar(PAnsiChar(fProcessorShadow)^,Length(fProcessorShadow),0);
    fProcessorBuffer := StrToAnsi(StringOfChar(#0,DART_RES_MultThrLength));
    fProcessingTerminated := False;
    fProcessingTerminating := False;
    // create processors
    For i := 0 to Pred(fTasksManager.MaxConcurrentTasks) do
      If BF_AdvanceProcessorBuffer then
        begin
          Processor := TDARTBruteForceProcessor.Create(fPauseControlObject,fArchiveProcessingSettings);
          Processor.Initialize(fUsedKnownPaths,fUnresolved,fHashType,fAlphabet);
          Processor.SetNewBuffer(fProcessorBuffer);
          Processor.LastUpdateCounter := fUpdateCounter;
          fTasksManager.AddTask(Processor);
        end
      else Break{For i};
    If fTasksManager.TaskCount > 0 then
      begin
        // start processors
        For i := 0 to Pred(fTasksManager.TaskCount) do
          fTasksManager.StartTask(i);
        // main cycle
        while not fProcessingTerminated do
          fTasksManager.Update(500);
      end;
  end;
DoProgress(1.0);
end;

//------------------------------------------------------------------------------

Function TDARTResolver_BruteForce.BF_AdvanceProcessorBuffer: Boolean;
var
  i:  Integer;
begin
Result := True;
If Length(fProcessorBuffer) > DART_RES_MultThrLength then
  For i := Succ(DART_RES_MultThrLength) to fBruteForceSettings.PathLengthLimit do
    begin
      If Length(fProcessorBuffer) < i then
        SetLength(fProcessorBuffer,i);
      If Ord(fProcessorShadow[i]) < Pred(fAlphabet.Count) then
        begin
          fProcessorShadow[i] := AnsiChar(Ord(fProcessorShadow[i]) + 1);
          fProcessorBuffer[i] := fAlphabet.Letters[Ord(fProcessorShadow[i])];
          Break{For i};
        end
      else
        begin
          fProcessorShadow[i] := AnsiChar(0);
          fProcessorBuffer[i] := fAlphabet.Letters[Ord(fProcessorShadow[i])];
          If i >= fBruteForceSettings.PathLengthLimit then
            Result := False;
        end;
    end
else
  begin
    SetLength(fProcessorBuffer,DART_RES_MultThrLength + 1);
    fProcessorBuffer[Length(fProcessorBuffer)] :=
      fAlphabet.Letters[Ord(fProcessorShadow[Length(fProcessorBuffer)])];
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_BruteForce.BF_Processor_OnProgress(Sender: TObject; TaskIndex: Integer);
begin
DoProgress(fResolved.Count / (fUnresolved.Count + fResolved.Count));
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_BruteForce.BF_Processor_OnCompleted(Sender: TObject; TaskIndex: Integer);
var
  Processor:  TDARTBruteForceProcessor;
  i,Index:    Integer;
  TempPath:   AnsiString;
begin
Processor := TDARTBruteForceProcessor(fTasksManager.Tasks[TaskIndex].TaskObject);
fTasksManager.DeleteTask(TaskIndex);  // task is not freed here
// get resolved
If Processor.ResolvedCount > 0 then
  begin
    For i := 0 to Pred(Processor.ResolvedCount) do
      begin
        Index := Unresolved_IndexOf(Processor.Resolved[i].Hash);
        If Index >= 0 then
          begin
            TempPath := Processor.Resolved[i].Path;
            UniqueString(TempPath);
            Unresolved_MoveToResolved(Index,TempPath);
          end;
      end;
    Inc(fUpdateCounter);
  end;
If (fUnresolved.Count > 0) and BF_AdvanceProcessorBuffer and not fProcessingTerminating then
  begin
    If Processor.LastUpdateCounter <> fUpdateCounter then
      Processor.Initialize(fUsedKnownPaths,fUnresolved,fHashType,fAlphabet);
    Processor.SetNewBuffer(fProcessorBuffer);
    Processor.LastUpdateCounter := fUpdateCounter;
    Index := fTasksManager.AddTask(Processor);
    fTasksManager.StartTask(Index);
  end
else
  begin
    Processor.Free;
    fProcessingTerminated := fTasksManager.TaskCount <= 0;
  end;
end;

{-------------------------------------------------------------------------------
    TDARTResolver_BruteForce - public methods
-------------------------------------------------------------------------------}

constructor TDARTResolver_BruteForce.Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
inherited Create(PauseControlObject,ArchiveProcessingSettings);
fBruteForceSettings := fArchiveProcessingSettings.SCS.PathResolve.BruteForce;
fTasksManager := TCNTSManager.Create(False);
end;

//------------------------------------------------------------------------------

destructor TDARTResolver_BruteForce.Destroy;
begin
fTasksManager.WaitForRunningTasksToComplete;
fTasksManager.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_BruteForce.Initialize(const ArchiveStructure: TDART_SCS_ArchiveStructure);
var
  i:  Integer;
begin
inherited;
// prepare list of used known paths
If fBruteForceSettings.UseKnownPaths then
  For i := Low(ArchiveStructure.KnownPaths.Arr) to Pred(ArchiveStructure.KnownPaths.Count) do
    If (Length(ArchiveStructure.KnownPaths.Arr[i].Path) > 0) and ArchiveStructure.KnownPaths.Arr[i].Directory then
      begin
        If fUsedKnownPaths.Count >= Length(fUsedKnownPaths.Arr) then
          SetLength(fUsedKnownPaths.Arr,Length(fUsedKnownPaths.Arr) + 1024);
        fUsedKnownPaths.Arr[fUsedKnownPaths.Count] := ArchiveStructure.KnownPaths.Arr[i].Path + DART_SCS_PathDelim;
        Inc(fUsedKnownPaths.Count);
      end;
// prepare alphabet
If fBruteForceSettings.PrintableASCIIOnly then
  begin
    If fBruteForceSettings.LimitedCharSet then
      begin
        // '0'..'9', 'a'..'z', '_', '.', '-', '/'
        For i := Low(fAlphabet.Letters) to Pred(Length(DART_RES_BF_LimitedCharSet)) do
          fAlphabet.Letters[i] := DART_RES_BF_LimitedCharSet[i + 1];
        fAlphabet.Count := Length(DART_RES_BF_LimitedCharSet);
      end
    else
      begin
        // #32 .. #127
        For i := Low(fAlphabet.Letters) to 95 do
          fAlphabet.Letters[i] := AnsiChar(i + 32);
        fAlphabet.Count := 96;
      end;
  end
else
  begin
    For i := Low(fAlphabet.Letters) to High(fAlphabet.Letters) do
      fAlphabet.Letters[i] := AnsiChar(i);
    fAlphabet.Count := Length(fAlphabet.Letters);
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_BruteForce.Run;
begin
If fBruteForceSettings.PathLengthLimit > 0 then
  begin
    If fBruteForceSettings.Multithreaded and
      (fBruteForceSettings.PathLengthLimit > DART_RES_MultThrLength) then
      BF_MainProcessing_MultiThreaded
    else
      BF_MainProcessing_SingleThreaded;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTResolver_BruteForce.Stop;
var
  i:  Integer;
begin
fProcessingTerminating := True;
For i := 0 to Pred(fTasksManager.TaskCount) do
  fTasksManager.StopTask(i);
end;


end.
