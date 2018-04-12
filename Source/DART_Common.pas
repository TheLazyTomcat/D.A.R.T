{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Common;

{$INCLUDE DART_defs.inc}

interface

uses
  SysUtils,
  AuxTypes, WinSyncObjs, CRC32, ProgressTracker;

{===============================================================================
    Known paths types and functions
===============================================================================}

type
  TDARTHash64 = UInt64;

  TDARTKnownPath = record
    Path:       AnsiString;
    Directory:  Boolean;
    Hash:       TCRC32;
    Hash64:     TDARTHash64;
  end;

  TDARTKnownPaths = record
    Arr:    array of TDARTKnownPath;
    Count:  Integer;
  end;

Function HashCompare(A,B: TDARTHash64): Integer;
Function PathHash(const Path: AnsiString; HashType: UInt32): TDARTHash64;

{===============================================================================
    Progress stage information types and functions
===============================================================================}

type
  TDARTProgressStageInfo = record
    ParentStage:  TProgressTracker;
    StageIndex:   Integer;
  end;

  TDART_PSI = TDARTProgressStageInfo;

Function ProgressStageInfo(ParentStage: TProgressTracker; StageIndex: Integer): TDARTProgressStageInfo; {$IFDEF CanInline}inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                            EDARTProcessingException
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    EDARTProcessingException - class declaration
===============================================================================}
type
  EDARTProcessingException = class(Exception)
  private
    fFaultObjectRef:    TObject;
    fFaultObjectClass:  String;
    fFaultFunctionIdx:  Integer;
    fFaultFunctionName: String;
  public
    constructor Create(const Text: String; ObjectRef: TObject; FunctionIdx: Integer; const FunctionName: String);
  published
    property FaultObjectRef: TObject read fFaultObjectRef;
    property FaultObjectClass: String read fFaultObjectClass;
    property FaultFunctionIdx: Integer read fFaultFunctionIdx;
    property FaultFunctionName: String read fFaultFunctionName;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TDARTPauseObject
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTPauseObject - class declaration
===============================================================================}

  TDARTPauseObject = WinSyncObjs.TEvent;

implementation

uses
  CITY,
  DART_Format_SCS;

{===============================================================================
    Known paths functions implementation
===============================================================================}

Function HashCompare(A,B: TDARTHash64): Integer;
begin
If AuxTypes.NativeUInt64 then
  begin
    If A < B then Result := 1
      else If A > B then Result := -1
        else Result := 0;
  end
else
  begin{%H-}
    If Int64Rec(A).Hi <> Int64Rec(B).Hi then
      begin
        If Int64Rec(A).Hi < Int64Rec(B).Hi then Result := 2
          else Result := -2
      end
    else
      begin
        If Int64Rec(A).Lo < Int64Rec(B).Lo then Result := 1
          else If Int64Rec(A).Lo > Int64Rec(B).Lo then Result := -1
            else Result := 0;
      end;
  end;
end;

{===============================================================================
    Progress stage information functions imlementation
===============================================================================}

Function ProgressStageInfo(ParentStage: TProgressTracker; StageIndex: Integer): TDARTProgressStageInfo;
begin
Result.ParentStage := ParentStage;
Result.StageIndex := StageIndex;
end;

//------------------------------------------------------------------------------

Function PathHash(const Path: AnsiString; HashType: UInt32): TDARTHash64;
begin
case HashType of
  DART_SCS_HASH_City: Result := TDARTHash64(CityHash64(PAnsiChar(Path),Length(Path) * SizeOf(AnsiChar)));
else
  Result := 0;
end;
end;

{===============================================================================
--------------------------------------------------------------------------------
                            EDARTProcessingException
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    EDARTProcessingException - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    EDARTProcessingException - public methods
-------------------------------------------------------------------------------}

constructor EDARTProcessingException.Create(const Text: String; ObjectRef: TObject; FunctionIdx: Integer; const FunctionName: String);
begin
inherited Create(Text);
fFaultObjectRef := ObjectRef;
fFaultObjectClass := ObjectRef.ClassName;
fFaultFunctionIdx := FunctionIdx;
fFaultFunctionName := FunctionName;
end;

end.
