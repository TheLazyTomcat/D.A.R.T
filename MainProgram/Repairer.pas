{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit Repairer;

interface

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

{$DEFINE preallocated_buffers}

{$DEFINE zlib_lib}
{.$DEFINE zlib_lib_dll}

{$IFDEF FPC}
  {$IFNDEF zlib_lib}
    {$UNDEF zlib_lib_dll}
  {$ENDIF}
{$ELSE}
  {$UNDEF zlib_lib}
  {$UNDEF zlib_lib_dll}
{$ENDIF}

uses
  Classes, AuxTypes;

// Text constant identifying what method is used to call zlib ------------------
const
{$IFDEF FPC}
  {$IFDEF zlib_lib}
    {$IFDEF zlib_lib_dll}
      zlib_method_str = 'D';  // ZLib is loaded from a DLL
    {$ELSE}
      zlib_method_str = 'S';  // ZLib is statically linked
    {$ENDIF}
  {$ELSE}
    zlib_method_str = 'P';    // PasZLib is used
  {$ENDIF}
{$ELSE}
  zlib_method_str = 'E';      // ZLibEx (delphi zlib) is used (statically linked)
{$ENDIF}

{==============================================================================}
{------------------------------------------------------------------------------}
{                               ZIP related stuff                              }
{------------------------------------------------------------------------------}
{==============================================================================}

//--- Bit flags ----------------------------------------------------------------
const
  ZBF_Encrypted        = $00000001; // bit 0
  ZBF_DataDescriptor   = $00000008; // bit 3
  ZBF_StrongEncryption = $00000040; // bit 6


//--- Signatures ---------------------------------------------------------------

  LocalFileHeaderSignature            = $04034b50;
  DataDescriptorSignature             = $08074b50;
  CentralDirectoryFileHeaderSignature = $02014b50;
  EndOfCentralDirectorySignature      = $06054b50;

type
//--- Local file header --------------------------------------------------------
  TLocalFileHeaderRecord = packed record
    Signature:              LongWord;
    VersionNeededToExtract: Byte;
    OSNeededForExtraction:  Byte;
    GeneralPurposeBitFlag:  Word;
    CompressionMethod:      Word;
    LastModFileTime:        Word;
    LastModFileDate:        Word;
    CRC32:                  LongWord;
    CompressedSize:         LongWord;
    UncompressedSize:       LongWord;
    FileNameLength:         Word;
    ExtraFieldLength:       Word;
  end;

  TLocalFileHeader = record
    BinPart:    TLocalFileHeaderRecord;
    FileName:   AnsiString;
    ExtraField: AnsiString;
  end;

//--- Data descriptor record ---------------------------------------------------

  TDataDescriptorRecord = packed record
    Signature:        LongWord;
    CRC32:            LongWord;
    CompressedSize:   LongWord;
    UncompressedSize: LongWord;
  end;

//--- Central directory file header --------------------------------------------

  TCentralDirectoryFileHeaderRecord = packed record
    Signature:                    LongWord;
    VersionMadeBy:                Byte;
    HostOS:                       Byte;
    VersionNeededToExtract:       Byte;
    OSNeededForExtraction:        Byte;
    GeneralPurposeBitFlag:        Word;
    CompressionMethod:            Word;
    LastModFileTime:              Word;
    LastModFileDate:              Word;
    CRC32:                        LongWord;
    CompressedSize:               LongWord;
    UncompressedSize:             LongWord;
    FileNameLength:               Word;
    ExtraFieldLength:             Word;
    FileCommentLength:            Word;
    DiskNumberStart:              Word;
    InternalFileAttributes:       Word;
    ExternalFileAttributes:       LongWord;
    RelativeOffsetOfLocalHeader:  LongWord;
  end;

  TCentralDirectoryFileHeader = record
    BinPart:      TCentralDirectoryFileHeaderRecord;
    FileName:     AnsiString;
    ExtraField:   AnsiString;
    FileComment:  AnsiString;
  end;

//--- End of central directory record ------------------------------------------

  TEndOfCentralDirectoryRecord = packed record
    Signature:                        LongWord;
    NumberOfThisDisk:                 Word;
    CentralDirectoryStartDiskNumber:  Word;
    EntriesOnDisk:                    Word;
    Entries:                          Word;
    CentralDirectorySize:             LongWord;
    CentralDirectoryOffset:           LongWord;
    CommentLength:                    Word;
  end;

  TEndOfCentralDirectory = record
    BinPart:  TEndOfCentralDirectoryRecord;
    Comment:  AnsiString;
  end;

//--- Utility data -------------------------------------------------------------

  TUtilityData = record
    OriginalLocalHeaderOffset:  LongWord;
    DataOffset:                 Int64;
    NeedsCRC32:                 Boolean;
    NeedsSizes:                 Boolean;
  end;

//--- Main structure -----------------------------------------------------------

  TEntry = record
    LocalHeader:            TLocalFileHeader;
    DataDescriptor:         TDataDescriptorRecord;
    CentralDirectoryHeader: TCentralDirectoryFileHeader;
    UtilityData:            TUtilityData;
  end;

  TFileStructure = record
    Entries:                array of TEntry;
    EndOfCentralDirectory:  TEndOfCentralDirectory;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                           Processing settings stuff                          }
{------------------------------------------------------------------------------}
{==============================================================================}

  TRepairMethod = (rmUnknown,rmRebuild,rmExtract);

  TEndOfCentralDirectoryProcessingSettings = record
    IgnoreEndOfCentralDirectory:    Boolean;
    IgnoreDiskSplit:                Boolean;
    IgnoreNumberOfEntries:          Boolean;
    IgnoreCentralDirectoryOffset:   Boolean;
    IgnoreComment:                  Boolean;
    LimitSearch:                    Boolean;
  end;

  TCentralDirectoryProcessingSettings = record
    IgnoreCentralDirectory:       Boolean;
    IgnoreSignature:              Boolean;
    IgnoreVersions:               Boolean;
    ClearEncryptionFlags:         Boolean;
    IgnoreCompressionMethod:      Boolean;
    IgnoreModTime:                Boolean;
    IgnoreModDate:                Boolean;
    IgnoreCRC32:                  Boolean;
    IgnoreSizes:                  Boolean;
    IgnoreInternalFileAttributes: Boolean;
    IgnoreExternalFileAttributes: Boolean;
    IgnoreLocalHeaderOffset:      Boolean;
    IgnoreExtraField:             Boolean;
    IgnoreFileComment:            Boolean;
  end;

  TLocalHeaderProcessingSettings = record
    IgnoreLocalHeaders:       Boolean;
    IgnoreSignature:          Boolean;
    IgnoreVersions:           Boolean;
    ClearEncryptionFlags:     Boolean;
    IgnoreCompressionMethod:  Boolean;
    IgnoreModTime:            Boolean;
    IgnoreModDate:            Boolean;
    IgnoreCRC32:              Boolean;
    IgnoreSizes:              Boolean;
    IgnoreFileName:           Boolean;
    IgnoreExtraField:         Boolean;
    IgnoreDataDescriptor:     Boolean;
  end;

  TOtherSettings = record
    InMemoryProcessingAllowed:  Boolean;
  end;

  TProcessingSettings = record
    RepairMethod:             TRepairMethod;
    RepairData:               String;
    IgnoreFileSignature:      Boolean;
    AssumeCompressionMethods: Boolean;
    InMemoryProcessing:       Boolean;
    IgnoreProcessingErrors:   Boolean;
    LogIgnoredErrors:         Boolean;
    EndOfCentralDirectory:    TEndOfCentralDirectoryProcessingSettings;
    CentralDirectory:         TCentralDirectoryProcessingSettings;
    LocalHeader:              TLocalHeaderProcessingSettings;
    OtherSettings:            TOtherSettings;
  end;

const
  DefaultProcessingSettings: TProcessingSettings = (
    RepairMethod:             rmRebuild;
    RepairData:               '';
    IgnoreFileSignature:      True;
    AssumeCompressionMethods: False;
    InMemoryProcessing:       False;
    IgnoreProcessingErrors:   False;
    LogIgnoredErrors:         False;  
    EndOfCentralDirectory: (
      IgnoreEndOfCentralDirectory:  False;
      IgnoreDiskSplit:              True;
      IgnoreNumberOfEntries:        False;
      IgnoreCentralDirectoryOffset: False;
      IgnoreComment:                True;
      LimitSearch:                  True); 
    CentralDirectory: (
      IgnoreCentralDirectory:       False;
      IgnoreSignature:              True;
      IgnoreVersions:               True;
      ClearEncryptionFlags:         True;
      IgnoreCompressionMethod:      True;
      IgnoreModTime:                False;
      IgnoreModDate:                False;
      IgnoreCRC32:                  True;
      IgnoreSizes:                  False;
      IgnoreInternalFileAttributes: False;
      IgnoreExternalFileAttributes: False;
      IgnoreLocalHeaderOffset:      False;
      IgnoreExtraField:             True;
      IgnoreFileComment:            True);
    LocalHeader: (
      IgnoreLocalHeaders:           False;
      IgnoreSignature:              True;
      IgnoreVersions:               True;
      ClearEncryptionFlags:         True;
      IgnoreCompressionMethod:      True;
      IgnoreModTime:                False;
      IgnoreModDate:                False;
      IgnoreCRC32:                  True;
      IgnoreSizes:                  True;
      IgnoreFileName:               False;
      IgnoreExtraField:             True;
      IgnoreDataDescriptor:         False);
    OtherSettings: (
      InMemoryProcessingAllowed:    False));

{==============================================================================}
{------------------------------------------------------------------------------}
{                                   TRepairer                                  }
{------------------------------------------------------------------------------}
{==============================================================================}

type
  TProgressStage = (psError,psProcessing,psLoading,psEOCDLoading,psCDHeadersLoading,
                    psLocalHeadersLoading,psEntriesProcessing,psSaving,psNoProgress);

  TProgressStageData = record
    Offset: Single;
    Range:  Single;
  end;

  TProgressInfo = array[TProgressStage] of TProgressStageData;

  TErrorInfo = record
    Source:         Pointer;
    SourceClass:    String;
    MethodIdx:      Integer;
    MethodName:     String;
    Text:           String;
    ThreadID:       LongWord;
    ExceptionClass: String;
  end;

  TProgressEvent = procedure(Sender: TObject; Progress: Single) of object;

  TBuffer = record
    Memory: Pointer;
    Size:   PtrUInt;
  end;

{==============================================================================}
{   TRepairer - Class declaration                                              }
{==============================================================================}
  TRepairer = class(TObject)
  private
    fTerminated:          Integer;  // 0 = continue, +x = internal error, -x = terminated
    fProcessingSettings:  TProcessingSettings;
    fInputFileName:       String;
    fInputFileStream:     TStream;
    fInputFileStructure:  TFileStructure;
    fProgressInfo:        TProgressInfo;
    fErrorData:           TErrorInfo;
  {$IFDEF preallocated_buffers}
    fIOBuffer:            TBuffer;
    fEntryCompressed:     TBuffer;
    fEntryUncompressed:   TBuffer;
  {$ENDIF}
    fIgnoredErrors:       TStringList;
    fOnProgress:          TProgressEvent;
  protected
    procedure ValidateProcessingSettings; virtual;
    procedure CheckInputFileSignature; virtual;
    Function FindSignature(Signature: LongWord; SearchBack: Boolean = False; SearchFrom: Int64 = -1; Limited: Boolean = False): Int64; virtual;
    procedure LoadEndOfCentralDirectory; virtual;
    procedure LoadCentralDirectory; virtual;
    procedure LoadLocalHeaders; virtual;
    procedure ReconstructLocalHeaders; virtual;
    procedure ReconstructCentralDirectoryHeaders; virtual;
    procedure ReconstructEndOfCentralDirectory; virtual;
    procedure ProgressLoadFile(FileName: String; Stream: TStream); virtual;
    procedure ProgressSaveFile(FileName: String; Stream: TStream); virtual;
    procedure ProgressStreamRead(Stream: TStream; Buffer: Pointer; Size: Integer; ProgressOffset, ProgressRange: Single); virtual;
    procedure ProgressStreamWrite(Stream: TStream; Buffer: Pointer; Size: Integer; ProgressOffset, ProgressRange: Single); virtual;
    procedure DecompressBuffer(InBuff: Pointer; InSize: Integer; out OutBuff: Pointer; out OutSize: Integer; ProgressOffset, ProgressRange: Single); virtual;
    procedure RebuildInputFile; virtual;
    procedure ExtractInputFile; virtual;
    procedure ProcessFile_Rebuild; virtual;
    procedure ProcessFile_Extract; virtual;
    procedure ProcessFile; virtual;
    procedure InitializeProgressInfo; virtual;
    procedure AllocateBuffers; virtual;
    procedure FreeBuffers; virtual;
    procedure DoProgress(ProgressStage: TProgressStage; Data: Single); virtual;
    procedure DoError(MethodIdx: Integer; ErrorText: String; Values: array of const; TerminationFlag: Integer = 1); overload; virtual;
    procedure DoError(MethodIdx: Integer; ErrorText: String; TerminationFlag: Integer = 1); overload; virtual;
  public
    constructor Create(ProcessingSettings: TProcessingSettings; InputFileName: String);
    destructor Destroy; override;
    procedure InitFileStructure; virtual;
    procedure Start; virtual;
    procedure Stop; virtual;
    property ProcessingSettings: TProcessingSettings read fProcessingSettings;
    property InputFileStructure: TFileStructure read fInputFileStructure;
    property ErrorData: TErrorInfo read fErrorData;
  published
    property InputFileName: String read fInputFileName;
    property OnProgress: TProgressEvent read fOnProgress write fOnProgress;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                                TRepairerThread                               }
{------------------------------------------------------------------------------}
{==============================================================================}

  TRepairerThread = class(TThread)
  private
    sync_Progress:  Single;
    fRepairer:      TRepairer;
    fErrorData:     TErrorInfo;
    fOnProgress:    TProgressEvent;
  protected
    procedure sync_DoProgress; virtual;
    procedure ProgressHandler(Sender: TObject; Progress: Single);
    procedure Execute; override;
  public
    constructor Create(ProcessingSettings: TProcessingSettings; InputFileName: String);
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Pause; virtual;
    procedure Stop; virtual;
    property ErrorData: TErrorInfo read fErrorData;
  published
    property OnProgress: TProgressEvent read fOnProgress write fOnProgress;
  end;

{$IF not Declared(FPC_FULLVERSION)}
const
(*
  Delphi 7 requires this, otherwise they throw error on comparison in
  {$IF FPC_FULLVERSION < ...} condition.
*)
  FPC_FULLVERSION = Integer(0);
{$IFEND}  

implementation

uses
  Windows, SysUtils, StrUtils, Math, CRC32
{$IFDEF FPC}
  , LazUTF8
  {$IF not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
  , LazFileUtils
  {$IFEND}
  {$IFNDEF zlib_lib}, PasZLib{$ENDIF}
{$ELSE}
  , ZLibExAPI
{$ENDIF};

{$IFDEF zlib_lib}
  {$I 'libs\lazarus.zlib.128\zlib_lib.pas'}
{$ENDIF}

const
{$IF not declared(FILE_WRITE_ATTRIBUTES)}
  FILE_WRITE_ATTRIBUTES = 256;
{$IFEND}

  // Size of the buffer used in progress-aware stream reading and writing
  IO_BufferSize  = $100000; {1MiB}

  // Initial size of buffer used to hold compressed entry data
  CED_BufferSize = $100000 * 8; {8MiB}

  // Initial size of buffer used to hold uncompressed entry data
  UED_BufferSize = $100000 * 16; {16MiB}

{==============================================================================}
{   Auxiliary routines                                                         }
{==============================================================================}

procedure AllocateBuffer(var Buff: TBuffer; Size: PtrUInt);
begin
GetMem(Buff.Memory,Size);
Buff.Size := Size;
end;

//------------------------------------------------------------------------------

procedure FreeBuffer(var Buff: TBuffer);
begin
FreeMem(Buff.Memory,Buff.Size);
Buff.Memory := nil;
Buff.Size := 0;
end;

//------------------------------------------------------------------------------

procedure ReallocateBuffer(var Buff: TBuffer; NewSize: PtrUInt);
begin
ReallocMem(Buff.Memory,NewSize);
Buff.Size := NewSize;
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                                   TRepairer                                  }
{------------------------------------------------------------------------------}
{==============================================================================}

type
  ERepairerException = class(Exception);

var
  ThreadFormatSettings: TFormatSettings;

const
  MethodNames: array[0..12] of String =
    ('CheckInputFileSignature','FindSignature','LoadEndOfCentralDirectory',
     'LoadCentralDirectory','LoadCentralDirectory.LoadCentralDirectoryHeader',
     'LoadLocalHeaders','LoadLocalHeaders.LoadLocalHeader','ProcessFile',
     'ProcessFile_Rebuild','ProcessFile_Extract','DecompressBuffer',
     'ExtractInputFile.SetFileTime','ExtractInputFile.SetDirTime');

{==============================================================================}
{   TRepairer - Class Implementation                                           }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TRepairer - Protected methods                                              }
{------------------------------------------------------------------------------}

procedure TRepairer.ValidateProcessingSettings;
begin
If fProcessingSettings.RepairMethod <> rmExtract then
  fProcessingSettings.IgnoreProcessingErrors := False;
If fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or
  fProcessingSettings.CentralDirectory.IgnoreLocalHeaderOffset then
  fProcessingSettings.LocalHeader.IgnoreLocalHeaders := False;
If fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
  begin
    If fProcessingSettings.LocalHeader.IgnoreCompressionMethod then
      fProcessingSettings.LocalHeader.IgnoreSizes := False;
    If fProcessingSettings.LocalHeader.IgnoreSizes then
      fProcessingSettings.LocalHeader.IgnoreCompressionMethod := False;
    fProcessingSettings.LocalHeader.IgnoreFileName := False;  
  end
else
  begin
    If fProcessingSettings.CentralDirectory.IgnoreCompressionMethod then
      fProcessingSettings.CentralDirectory.IgnoreSizes := False;
    If fProcessingSettings.CentralDirectory.IgnoreSizes then
      fProcessingSettings.CentralDirectory.IgnoreCompressionMethod := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TRepairer.CheckInputFileSignature;
var
  Signature:  LongWord;
begin
fInputFileStream.Seek(0,soFromBeginning);
If fInputFileStream.Read({%H-}Signature,SizeOf(LongWord)) >= SizeOf(LongWord) then
  begin
    If Signature <> LocalFileHeaderSignature then
      DoError(0,'Bad file signature (0x%.8x).',[Signature]);
  end
else DoError(0,'File is too small to contain valid signature (%d bytes).',[fInputFileStream.Size]);
end;

//------------------------------------------------------------------------------

Function TRepairer.FindSignature(Signature: LongWord; SearchBack: Boolean = False; SearchFrom: Int64 = -1; Limited: Boolean = False): Int64;
const
  BufferOverlap = SizeOf(LongWord) - 1;
var
  Buffer:         Pointer;
  BufferSize:     PtrUInt;
  CurrentOffset:  Int64;
  BytesRead:      LongWord;
  i:              LongWord;
begin
Result := -1;
{$IFDEF preallocated_buffers}
BufferSize := fIOBuffer.Size;
Buffer := fIOBuffer.Memory;
{$ELSE}
BufferSize := IO_BufferSize;
GetMem(Buffer,BufferSize);
try
{$ENDIF}
  If SearchFrom >= 0 then
    begin
      If SearchBack then
        CurrentOffset := fInputFileStream.Size - SearchFrom
      else
        CurrentOffset := SearchFrom;
    end
  else CurrentOffset := 0;
  repeat
    If SearchBack then
      fInputFileStream.Seek(-(CurrentOffset + BufferSize),soFromEnd)
    else
      fInputFileStream.Seek(CurrentOffset,soFromBeginning);
    BytesRead := fInputFileStream.Read(Buffer^,BufferSize);
    If BytesRead >= SizeOf(LongWord) then
      For i := 0 to (BytesRead - SizeOf(LongWord)) do
        If {%H-}PLongWord({%H-}PtrUInt(Buffer) + i)^ = Signature then
          begin
            Result := fInputFileStream.Position - BytesRead + i;
            Exit;
          end;
    Inc(CurrentOffset,BufferSize - BufferOverlap);
    DoProgress(psNoProgress,0.0);
  until (CurrentOffset > fInputFileStream.Size) or Limited;
{$IFNDEF preallocated_buffers}
finally
  FreeMem(Buffer,BufferSize);
end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TRepairer.LoadEndOfCentralDirectory;
var
  EOCDPosition: Int64;
begin
DoProgress(psEOCDLoading,0.0);
EOCDPosition := FindSignature(EndOfCentralDirectorySignature,True,-1,fProcessingSettings.EndOfCentralDirectory.LimitSearch);
DoProgress(psEOCDLoading,0.5);
If EOCDPosition >= 0 then
  begin
    fInputFileStream.Seek(EOCDPosition,soFromBeginning);
    If (fInputFileStream.Size - fInputFileStream.Position) >= SizeOf(TEndOfCentralDirectoryRecord) then
      begin
        with fInputFileStructure.EndOfCentralDirectory do
          begin
            fInputFileStream.ReadBuffer(BinPart,SizeOf(TEndOfCentralDirectoryRecord));
            DoProgress(psEOCDLoading,0.9);
            If fProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit then
              begin
                BinPart.NumberOfThisDisk := 0;
                BinPart.CentralDirectoryStartDiskNumber := 0;
                BinPart.Entries := BinPart.EntriesOnDisk;
              end;
            If fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries then
              begin
                BinPart.EntriesOnDisk := 0;
                BinPart.Entries := 0;
              end;
            If fProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset then
              BinPart.CentralDirectoryOffset := 0;            
            If fProcessingSettings.EndOfCentralDirectory.IgnoreComment then
              begin
                BinPart.CommentLength := 0;
                Comment := '';
              end
            else
              begin
                If (fInputFileStream.Size - fInputFileStream.Position) >= BinPart.CommentLength then
                  begin
                    SetLength(Comment,BinPart.CommentLength);
                    fInputFileStream.ReadBuffer(PAnsiChar(Comment)^,BinPart.CommentLength);
                  {$IFDEF FPC}
                    Comment := WinCPToUTF8(Comment);
                  {$ENDIF}
                  end
                else DoError(2,'Not enough data for end of central directory comment.');
              end;
          end;
      end
    else DoError(2,'Not enough data for end of central directory record.');
  end
else DoError(2,'End of central directory signature not found in the input stream.');
DoProgress(psEOCDLoading,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.LoadCentralDirectory;
var
  WorkingOffset:  Int64;
  i:              Integer;

  procedure LoadCentralDirectoryHeader(Index: Integer);
  begin
    with fInputFileStructure.Entries[Index].CentralDirectoryHeader do
      begin
        fInputFileStream.ReadBuffer(BinPart,SizeOf(TCentralDirectoryFileHeaderRecord));
        // binary part checks
        If fProcessingSettings.CentralDirectory.IgnoreSignature then
          BinPart.Signature := CentralDirectoryFileHeaderSignature
        else
          If BinPart.Signature <> CentralDirectoryFileHeaderSignature then
            DoError(4,'Bad central directory header signature (0x%.8x) for entry #%d.',[BinPart.Signature,Index]);
        If fProcessingSettings.CentralDirectory.IgnoreVersions then
          begin
            BinPart.VersionMadeBy := 20;
            BinPart.HostOS := 0;
            BinPart.VersionNeededToExtract := 20;
            BinPart.OSNeededForExtraction := 0;
          end;
        If fProcessingSettings.CentralDirectory.ClearEncryptionFlags then
          BinPart.GeneralPurposeBitFlag := BinPart.GeneralPurposeBitFlag and not Word(ZBF_Encrypted or ZBF_StrongEncryption);
        If fProcessingSettings.CentralDirectory.IgnoreCompressionMethod then
          begin
            If BinPart.CompressedSize = BinPart.UncompressedSize then
              BinPart.CompressionMethod := 0 {store}
            else
              BinPart.CompressionMethod := 8 {deflate};
          end
        else
          begin
            If not (BinPart.CompressionMethod in [0,8]) and not fProcessingSettings.AssumeCompressionMethods then
              DoError(4,'Unknown compression method (%d) in central directory header for entry #%d.',[BinPart.CompressionMethod,Index]);
          end;
        If fProcessingSettings.CentralDirectory.IgnoreModTime then
          BinPart.LastModFileTime := DateTimeToFileDate(Now) and $FFFF;
        If fProcessingSettings.CentralDirectory.IgnoreModDate then
          BinPart.LastModFileDate := DateTimeToFileDate(Now) shr 16;
        If fProcessingSettings.CentralDirectory.IgnoreCRC32 then
          BinPart.CRC32 := 0;
        If fProcessingSettings.CentralDirectory.IgnoreSizes then
          begin
            BinPart.CompressedSize := 0;
            BinPart.UncompressedSize := 0;
          end;
        If fProcessingSettings.CentralDirectory.IgnoreLocalHeaderOffset then
          BinPart.RelativeOffsetOfLocalHeader := 0;
        // load file name
        SetLength(FileName,BinPart.FilenameLength);
        fInputFileStream.ReadBuffer(PAnsiChar(FileName)^,BinPart.FileNameLength);
      {$IFDEF FPC}
        FileName := WinCPToUTF8(FileName);
      {$ENDIF}
        // file attributes must be done here because file name is required
        If fProcessingSettings.CentralDirectory.IgnoreInternalFileAttributes then
          BinPart.InternalFileAttributes := 0;
        If fProcessingSettings.CentralDirectory.IgnoreExternalFileAttributes then
          begin
            If ExtractFileName(AnsiReplaceStr(FileName,'/','\')) <> '' then
              BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_ARCHIVE
            else
              BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
          end;
        // load extra field
        If fProcessingSettings.CentralDirectory.IgnoreExtraField then
          begin
            fInputFileStream.Seek(BinPart.ExtraFieldLength,soFromCurrent);
            BinPart.ExtraFieldLength := 0;
          end
        else
          begin
            SetLength(ExtraField,BinPart.ExtraFieldLength);
            fInputFileStream.ReadBuffer(PAnsiChar(ExtraField)^,BinPart.ExtraFieldLength);
           end;
        // load file comment
        If fProcessingSettings.CentralDirectory.IgnoreFileComment then
          begin
            fInputFileStream.Seek(BinPart.FileCommentLength,soFromCurrent);
            BinPart.FileCommentLength := 0;
          end
        else
          begin
            SetLength(FileComment,BinPart.FileCommentLength);
            fInputFileStream.ReadBuffer(PAnsiChar(FileComment)^,BinPart.FileCommentLength);
          {$IFDEF FPC}
            FileComment := WinCPToUTF8(FileComment);
          {$ENDIF}
          end;
    end;
  end;

begin
DoProgress(psCDHeadersLoading,0.0);
If fProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset or
  fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  begin
    WorkingOffset := FindSignature(CentralDirectoryFileHeaderSignature);
    If WorkingOffset >= 0 then
      fInputFileStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset := WorkingOffset
    else
      DoError(3,'Start of central directory not found in the input stream.');
  end
else WorkingOffset := fInputFileStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset;
If fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries or
  fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  begin
    i := 0;
    repeat
      If (i + 1) > Length(fInputFileStructure.Entries) then
        SetLength(fInputFileStructure.Entries,Length(fInputFileStructure.Entries) + 1024);
      fInputFileStream.Seek(WorkingOffset,soFromBeginning);
      with fInputFileStructure.EndOfCentralDirectory.BinPart do
        DoProgress(psCDHeadersLoading,(WorkingOffset - CentralDirectoryOffset) / (fInputFileStream.Size - CentralDirectoryOffset));
      LoadCentralDirectoryHeader(i);
      WorkingOffset := FindSignature(CentralDirectoryFileHeaderSignature,False,fInputFileStream.Position);
      Inc(i);
    until WorkingOffset < 0;
    SetLength(fInputFileStructure.Entries,i);
    fInputFileStructure.EndOfCentralDirectory.BinPart.EntriesOnDisk := i;
    If fProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit then
      fInputFileStructure.EndOfCentralDirectory.BinPart.Entries := i;
  end
else
  begin
    fInputFileStream.Seek(WorkingOffset,soFromBeginning);
    For i := Low(fInputFileStructure.Entries) to High(fInputFileStructure.Entries) do
      begin
        LoadCentralDirectoryHeader(i);
        DoProgress(psCDHeadersLoading,(i + 1) / Length(fInputFileStructure.Entries));
      end;
  end;
DoProgress(psCDHeadersLoading,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.LoadLocalHeaders;
var
  WorkingOffset:  Int64;
  i:              Integer;

  procedure LoadLocalHeader(Index: Integer);
  var
    DescriptorOffset:  Int64;
  begin
    with fInputFileStructure.Entries[Index],fInputFileStructure.Entries[Index].LocalHeader do
      begin
        fInputFileStream.ReadBuffer(BinPart,SizeOf(TLocalFileHeaderRecord));
        // binary part checks
        If fProcessingSettings.LocalHeader.IgnoreSignature then
          BinPart.Signature := LocalFileHeaderSignature
        else
          If BinPart.Signature <> LocalFileHeaderSignature then
            DoError(6,'Bad local header signature (0x%.8x) for entry #%d.',[BinPart.Signature,Index]);
        If fProcessingSettings.LocalHeader.IgnoreVersions then
          begin
            BinPart.VersionNeededToExtract := 20;
            BinPart.OSNeededForExtraction := 0;
          end;
        If fProcessingSettings.LocalHeader.ClearEncryptionFlags then
          BinPart.GeneralPurposeBitFlag := BinPart.GeneralPurposeBitFlag and not Word(ZBF_Encrypted or ZBF_StrongEncryption);
        If fProcessingSettings.LocalHeader.IgnoreCompressionMethod then
          begin
            If BinPart.CompressedSize = BinPart.UncompressedSize then
              BinPart.CompressionMethod := 0 {store}
            else
              BinPart.CompressionMethod := 8 {deflate};
          end
        else  
          begin
            If not (BinPart.CompressionMethod in [0,8]) and not fProcessingSettings.AssumeCompressionMethods then
              DoError(6,'Unknown compression method (%d) in local header for entry #%d.',[BinPart.CompressionMethod,Index]);
          end;
        If fProcessingSettings.LocalHeader.IgnoreModTime then
          BinPart.LastModFileTime := DateTimeToFileDate(Now) and $FFFF;
        If fProcessingSettings.LocalHeader.IgnoreModDate then
          BinPart.LastModFileDate := DateTimeToFileDate(Now) shr 16;
        If fProcessingSettings.LocalHeader.IgnoreCRC32 then
          BinPart.CRC32 := 0;
        If fProcessingSettings.LocalHeader.IgnoreSizes then
          begin
            BinPart.CompressedSize := 0;
            BinPart.UncompressedSize := 0;
          end;
        // file name
        If fProcessingSettings.LocalHeader.IgnoreFileName then
          begin
            fInputFileStream.Seek(BinPart.FileNameLength,soFromCurrent);
            BinPart.FileNameLength := CentralDirectoryHeader.BinPart.FileNameLength;
            FileName := CentralDirectoryHeader.FileName;
          end
        else
          begin
            SetLength(FileName,BinPart.FileNameLength);
            fInputFileStream.ReadBuffer(PAnsiChar(FileName)^,BinPart.FileNameLength);
          {$IFDEF FPC}
            FileName := WinCPToUTF8(FileName);
          {$ENDIF}
          end;
        // extra field
        If fProcessingSettings.LocalHeader.IgnoreExtraField then
          begin
            fInputFileStream.Seek(BinPart.ExtraFieldLength,soFromCurrent);
            BinPart.ExtraFieldLength := 0;
          end
        else
          begin
            SetLength(ExtraField,BinPart.ExtraFieldLength);
            fInputFileStream.ReadBuffer(PAnsiChar(ExtraField)^,BinPart.ExtraFieldLength);
          end;
        fInputFileStructure.Entries[Index].UtilityData.DataOffset := fInputFileStream.Position;
        // read data descriptor if present
        If ((BinPart.GeneralPurposeBitFlag and ZBF_DataDescriptor) <> 0) and
          not fProcessingSettings.LocalHeader.IgnoreDataDescriptor then
          begin
            If fProcessingSettings.LocalHeader.IgnoreSizes then
              DescriptorOffset := FindSignature(DataDescriptorSignature,False,fInputFileStream.Position)
            else
              DescriptorOffset := fInputFileStream.Position + BinPart.CompressedSize;
            If (DescriptorOffset > 0) and ((DescriptorOffset + SizeOf(TDataDescriptorRecord)) <= fInputFileStream.Size) then
              begin
                fInputFileStream.Seek(DescriptorOffset,soFromBeginning);
                fInputFileStream.ReadBuffer(DataDescriptor,SizeOf(TDataDescriptorRecord));
                If fProcessingSettings.LocalHeader.IgnoreSignature then
                  DataDescriptor.Signature := DataDescriptorSignature
                else
                  begin
                    If DataDescriptor.Signature <> DataDescriptorSignature then
                      DoError(6,'Bad data descriptor signature (0x%.8x) for entry #%d.',[DataDescriptor.Signature,Index])
                  end;
                If fProcessingSettings.LocalHeader.IgnoreCRC32 then
                  DataDescriptor.CRC32 := 0
                else
                  BinPart.CRC32 := DataDescriptor.CRC32;
                If fProcessingSettings.LocalHeader.IgnoreSizes then
                  begin
                    DataDescriptor.CompressedSize := 0;
                    DataDescriptor.UncompressedSize := 0;
                  end
                else
                  begin
                    BinPart.CompressedSize := DataDescriptor.CompressedSize;
                    BinPart.UncompressedSize := DataDescriptor.UncompressedSize;
                  end;
              end
            else DoError(6,'Data descriptor was not found (%d).',[DescriptorOffset]);
          end
        else
          begin
            BinPart.GeneralPurposeBitFlag := BinPart.GeneralPurposeBitFlag and not ZBF_DataDescriptor;
            CentralDirectoryHeader.BinPart.GeneralPurposeBitFlag := CentralDirectoryHeader.BinPart.GeneralPurposeBitFlag and not ZBF_DataDescriptor;
          end;
      end;
  end;

  procedure CopyCentralToLocal(Index: Integer);
  begin
    with fInputFileStructure.Entries[Index] do
      begin
        LocalHeader.BinPart.Signature := LocalFileHeaderSignature;
        LocalHeader.BinPart.VersionNeededToExtract := CentralDirectoryHeader.BinPart.VersionNeededToExtract;
        LocalHeader.BinPart.OSNeededForExtraction := CentralDirectoryHeader.BinPart.OSNeededForExtraction;
        LocalHeader.BinPart.GeneralPurposeBitFlag := CentralDirectoryHeader.BinPart.GeneralPurposeBitFlag;
        LocalHeader.BinPart.CompressionMethod := CentralDirectoryHeader.BinPart.CompressionMethod;
        LocalHeader.BinPart.LastModFileTime := CentralDirectoryHeader.BinPart.LastModFileTime;
        LocalHeader.BinPart.LastModFileDate := CentralDirectoryHeader.BinPart.LastModFileDate;
        LocalHeader.BinPart.CRC32 := CentralDirectoryHeader.BinPart.CRC32;
        LocalHeader.BinPart.CompressedSize := CentralDirectoryHeader.BinPart.CompressedSize;
        LocalHeader.BinPart.UncompressedSize := CentralDirectoryHeader.BinPart.UncompressedSize;
        LocalHeader.BinPart.FileNameLength := CentralDirectoryHeader.BinPart.FileNameLength;
        LocalHeader.BinPart.ExtraFieldLength := CentralDirectoryHeader.BinPart.ExtraFieldLength;
        LocalHeader.FileName := CentralDirectoryHeader.FileName;
        LocalHeader.ExtraField := CentralDirectoryHeader.ExtraField;
        UtilityData.DataOffset := Int64(CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader) +
                                  SizeOf(TLocalFileHeaderRecord) +
                                  LocalHeader.BinPart.FileNameLength +
                                  LocalHeader.BinPart.ExtraFieldLength;
      end;
  end;

begin
DoProgress(psLocalHeadersLoading,0.0);
If fProcessingSettings.LocalHeader.IgnoreLocalHeaders then
  begin
    For i := Low(fInputFileStructure.Entries) to High(fInputFileStructure.Entries) do
      CopyCentralToLocal(i);
  end
else
  begin
    If Length(fInputFileStructure.Entries) > 0 then
      begin
        fInputFileStream.Seek(0,soFromBeginning);
        For i := Low(fInputFileStructure.Entries) to High(fInputFileStructure.Entries) do
          with fInputFileStructure.Entries[i].CentralDirectoryHeader do
            begin
              If fProcessingSettings.CentralDirectory.IgnoreLocalHeaderOffset or
                fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
                begin
                  WorkingOffset := FindSignature(LocalFileHeaderSignature,False,fInputFileStream.Position);
                  If WorkingOffset >= 0 then
                    begin
                      BinPart.RelativeOffsetOfLocalHeader := WorkingOffset;
                      fInputFileStream.Seek(WorkingOffset,soFromBeginning);
                    end
                  else DoError(5,'No local header found for entry #%d.',[i]);
                end
              else fInputFileStream.Seek(BinPart.RelativeOffsetOfLocalHeader,soFromBeginning);
              LoadLocalHeader(i);
              DoProgress(psLocalHeadersLoading,(i + 1) / Length(fInputFileStructure.Entries));
              If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
                If not AnsiSameText(FileName,fInputFileStructure.Entries[i].LocalHeader.FileName) then
                  DoError(5,'Mismatch in local and central directory file name for entry #%d (%s; %s).',[i,FileName,fInputFileStructure.Entries[i].LocalHeader.FileName]);
            end;
      end
    else
      begin
        i := 0;
        WorkingOffset := FindSignature(LocalFileHeaderSignature,False);
        If WorkingOffset >= 0 then
          repeat
            If (i + 1) > Length(fInputFileStructure.Entries) then
              SetLength(fInputFileStructure.Entries,Length(fInputFileStructure.Entries) + 1024);
            fInputFileStructure.Entries[i].CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader := WorkingOffset;
            fInputFileStream.Seek(WorkingOffset,soFromBeginning);
            with fInputFileStructure.EndOfCentralDirectory.BinPart do
              DoProgress(psLocalHeadersLoading,(WorkingOffset - CentralDirectoryOffset) / (fInputFileStream.Size - CentralDirectoryOffset));
            LoadLocalHeader(i);
            WorkingOffset := FindSignature(LocalFileHeaderSignature,False,fInputFileStream.Position);
            Inc(i);
          until WorkingOffset < 0;
        SetLength(fInputFileStructure.Entries,i);
      end;
  end;
DoProgress(psLocalHeadersLoading,1.0);  
end;

//------------------------------------------------------------------------------

procedure TRepairer.ReconstructLocalHeaders;
var
  i:  Integer;
begin
For i := Low(fInputFileStructure.Entries) to High(fInputFileStructure.Entries) do
  with fInputFileStructure.Entries[i] do
    begin
      If fProcessingSettings.LocalHeader.IgnoreVersions and not fProcessingSettings.CentralDirectory.IgnoreVersions then
        begin
          LocalHeader.BinPart.VersionNeededToExtract := CentralDirectoryHeader.BinPart.VersionNeededToExtract;
          LocalHeader.BinPart.OSNeededForExtraction := CentralDirectoryHeader.BinPart.OSNeededForExtraction;
        end;
      If fProcessingSettings.LocalHeader.IgnoreCompressionMethod and not fProcessingSettings.CentralDirectory.IgnoreCompressionMethod then
        LocalHeader.BinPart.CompressionMethod := CentralDirectoryHeader.BinPart.CompressionMethod;
      If fProcessingSettings.LocalHeader.IgnoreModTime and not fProcessingSettings.CentralDirectory.IgnoreModTime then
        LocalHeader.BinPart.LastModFileTime := CentralDirectoryHeader.BinPart.LastModFileTime;
      If fProcessingSettings.LocalHeader.IgnoreModDate and not fProcessingSettings.CentralDirectory.IgnoreModDate then
        LocalHeader.BinPart.LastModFileDate := CentralDirectoryHeader.BinPart.LastModFileDate;
      If fProcessingSettings.LocalHeader.IgnoreCRC32 and not fProcessingSettings.CentralDirectory.IgnoreCRC32 then
        begin
          LocalHeader.BinPart.CRC32 := CentralDirectoryHeader.BinPart.CRC32;
          DataDescriptor.CRC32 := CentralDirectoryHeader.BinPart.CRC32;
        end;
      If fProcessingSettings.LocalHeader.IgnoreSizes and not fProcessingSettings.CentralDirectory.IgnoreSizes then
        begin
          LocalHeader.BinPart.CompressedSize := CentralDirectoryHeader.BinPart.CompressedSize;
          LocalHeader.BinPart.UncompressedSize:= CentralDirectoryHeader.BinPart.UncompressedSize;
          DataDescriptor.CompressedSize := CentralDirectoryHeader.BinPart.CompressedSize;
          DataDescriptor.UncompressedSize := CentralDirectoryHeader.BinPart.UncompressedSize;
        end;
    end;
end;

//------------------------------------------------------------------------------

procedure TRepairer.ReconstructCentralDirectoryHeaders;
var
  i:  Integer;
begin
For i := Low(fInputFileStructure.Entries) to High(fInputFileStructure.Entries) do
  with fInputFileStructure.Entries[i] do
    begin
      If (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory and not fProcessingSettings.LocalHeader.IgnoreLocalHeaders) or
        (fProcessingSettings.CentralDirectory.IgnoreVersions and not fProcessingSettings.LocalHeader.IgnoreVersions) then
        begin
          CentralDirectoryHeader.BinPart.VersionMadeBy := LocalHeader.BinPart.VersionNeededToExtract;
          CentralDirectoryHeader.BinPart.HostOS := LocalHeader.BinPart.OSNeededForExtraction;
          CentralDirectoryHeader.BinPart.VersionNeededToExtract := LocalHeader.BinPart.VersionNeededToExtract;
          CentralDirectoryHeader.BinPart.OSNeededForExtraction := LocalHeader.BinPart.OSNeededForExtraction;
        end;
      If (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory and not fProcessingSettings.LocalHeader.IgnoreLocalHeaders) or
        (fProcessingSettings.CentralDirectory.IgnoreCompressionMethod and not fProcessingSettings.LocalHeader.IgnoreCompressionMethod) then
        CentralDirectoryHeader.BinPart.CompressionMethod := LocalHeader.BinPart.CompressionMethod;
      If (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory and not fProcessingSettings.LocalHeader.IgnoreLocalHeaders) or
        (fProcessingSettings.CentralDirectory.IgnoreModTime and not fProcessingSettings.LocalHeader.IgnoreModTime) then
        CentralDirectoryHeader.BinPart.LastModFileTime := LocalHeader.BinPart.LastModFileTime;
      If (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory and not fProcessingSettings.LocalHeader.IgnoreLocalHeaders) or
        (fProcessingSettings.CentralDirectory.IgnoreModDate and not fProcessingSettings.LocalHeader.IgnoreModDate) then
        CentralDirectoryHeader.BinPart.LastModFileDate := LocalHeader.BinPart.LastModFileDate;
      If (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory and not fProcessingSettings.LocalHeader.IgnoreLocalHeaders) or
        (fProcessingSettings.CentralDirectory.IgnoreCRC32 and not fProcessingSettings.LocalHeader.IgnoreCRC32) then
        CentralDirectoryHeader.BinPart.CRC32 := LocalHeader.BinPart.CRC32;
      If (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory and not fProcessingSettings.LocalHeader.IgnoreLocalHeaders) or
        (fProcessingSettings.CentralDirectory.IgnoreSizes and not fProcessingSettings.LocalHeader.IgnoreSizes) then
        begin
          CentralDirectoryHeader.BinPart.CompressedSize := LocalHeader.BinPart.CompressedSize;
          CentralDirectoryHeader.BinPart.UncompressedSize := LocalHeader.BinPart.UncompressedSize;
        end;
      If fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
        begin
          CentralDirectoryHeader.BinPart.Signature := CentralDirectoryFileHeaderSignature;
          CentralDirectoryHeader.BinPart.DiskNumberStart := 0;
          CentralDirectoryHeader.BinPart.InternalFileAttributes := 0;
          CentralDirectoryHeader.BinPart.ExternalFileAttributes := 0;
          CentralDirectoryHeader.BinPart.FileNameLength := LocalHeader.BinPart.FileNameLength;
          CentralDirectoryHeader.FileName := LocalHeader.FileName;
        end;
      fInputFileStructure.Entries[i].UtilityData.NeedsCRC32 :=
        (fProcessingSettings.LocalHeader.IgnoreLocalHeaders or fProcessingSettings.LocalHeader.IgnoreCRC32) and
        (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or fProcessingSettings.CentralDirectory.IgnoreCRC32);
      fInputFileStructure.Entries[i].UtilityData.NeedsSizes :=
        (fProcessingSettings.LocalHeader.IgnoreLocalHeaders or fProcessingSettings.LocalHeader.IgnoreSizes) and
        (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or fProcessingSettings.CentralDirectory.IgnoreSizes);
    end;
end;

//------------------------------------------------------------------------------

procedure TRepairer.ReconstructEndOfCentralDirectory;
var
  i:  Integer;
begin
with fInputFileStructure.EndOfCentralDirectory do
  begin
    If fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
      begin
        BinPart.Signature := EndOfCentralDirectorySignature;
        BinPart.NumberOfThisDisk := 0;
        BinPart.CentralDirectoryStartDiskNumber := 0;
        BinPart.CommentLength := 0;
        Comment := '';
      end;
    If fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory or fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries then
      begin
        BinPart.EntriesOnDisk := Length(fInputFileStructure.Entries);
        BinPart.Entries := Length(fInputFileStructure.Entries);
      end;
    BinPart.CentralDirectorySize := 0;
    For i := Low(fInputFileStructure.Entries) to High(fInputFileStructure.Entries) do
      begin
        Inc(BinPart.CentralDirectorySize,SizeOf(TCentralDirectoryFileHeaderRecord));
        Inc(BinPart.CentralDirectorySize,fInputFileStructure.Entries[i].CentralDirectoryHeader.BinPart.FileNameLength);
        Inc(BinPart.CentralDirectorySize,fInputFileStructure.Entries[i].CentralDirectoryHeader.BinPart.ExtraFieldLength);
        Inc(BinPart.CentralDirectorySize,fInputFileStructure.Entries[i].CentralDirectoryHeader.BinPart.FileCommentLength);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProgressLoadFile(FileName: String; Stream: TStream);
var
  FileStream: TFileStream;
  BytesRead:  Integer;
  Buffer:     Pointer;
  BufferSize: PtrUInt;
begin
DoProgress(psLoading,0.0);
{$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
FileStream := TFileStream.Create(UTF8ToSys(FileName),fmOpenRead or fmShareDenyWrite);
{$ELSE}
FileStream := TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
{$IFEND}
try
  Stream.Seek(0,soFromBeginning);
  If FileStream.Size > 0 then
    begin
    {$IFDEF preallocated_buffers}
      BufferSize := fIOBuffer.Size;
      Buffer := fIOBuffer.Memory;
    {$ELSE}
      BufferSize := IO_BufferSize;
      GetMem(Buffer,BufferSize);
      try
    {$ENDIF}      
        Stream.Size := FileStream.Size;
        repeat
          BytesRead := FileStream.Read(Buffer^,BufferSize);
          Stream.Write(Buffer^,BytesRead);
          DoProgress(psLoading,FileStream.Position / FileStream.Size);
        until BytesRead <= 0;
    {$IFNDEF preallocated_buffers}
      finally
        FreeMem(Buffer,BufferSize);
      end;
    {$ENDIF}
    end;
finally
  FileStream.Free;
end;
DoProgress(psLoading,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProgressSaveFile(FileName: String; Stream: TStream);
var
  FileStream: TFileStream;
  BytesRead:  Integer;
  Buffer:     Pointer;
  BufferSize: PtrUInt;
begin
DoProgress(psSaving,0.0);
{$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
FileStream := TFileStream.Create(UTF8ToSys(FileName),fmCreate or fmShareDenyWrite);
{$ELSE}
FileStream := TFileStream.Create(FileName,fmCreate or fmShareDenyWrite);
{$IFEND}
try
  Stream.Seek(0,soFromBeginning);
  If Stream.Size > 0 then
    begin
    {$IFDEF preallocated_buffers}
      BufferSize := fIOBuffer.Size;
      Buffer := fIOBuffer.Memory;
    {$ELSE}
      BufferSize := IO_BufferSize;
      GetMem(Buffer,BufferSize);
      try
    {$ENDIF}      
        repeat
          BytesRead := Stream.Read(Buffer^,BufferSize);
          FileStream.Write(Buffer^,BytesRead);
          DoProgress(psSaving,Stream.Position / Stream.Size);
        until BytesRead <= 0;
        FileStream.Size := Stream.Size;        
    {$IFNDEF preallocated_buffers}
      finally
        FreeMem(Buffer,BufferSize);
      end;
    {$ENDIF}
    end;
finally
  FileStream.Free;
end;
DoProgress(psSaving,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProgressStreamRead(Stream: TStream; Buffer: Pointer; Size: Integer; ProgressOffset, ProgressRange: Single);
var
  i,Max:  Integer;
begin
DoProgress(psEntriesProcessing,ProgressOffset);
Max := Ceil(Size / IO_BufferSize);
For i := 1 to Max do
  begin
    Stream.ReadBuffer(Buffer^,Min(IO_BufferSize,Size));
    Buffer := {%H-}Pointer({%H-}PtrUInt(Buffer) + IO_BufferSize);
    Dec(Size,IO_BufferSize);
    DoProgress(psEntriesProcessing,ProgressOffset + (ProgressRange * (i / Max)));
  end;
DoProgress(psEntriesProcessing,ProgressOffset + ProgressRange);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProgressStreamWrite(Stream: TStream; Buffer: Pointer; Size: Integer; ProgressOffset, ProgressRange: Single);
var
  i,Max:  Integer;
begin
DoProgress(psEntriesProcessing,ProgressOffset);
Max := Ceil(Size / IO_BufferSize);
For i := 1 to Max do
  begin
    Stream.WriteBuffer(Buffer^,Min(IO_BufferSize,Size));
    Buffer := {%H-}Pointer({%H-}PtrUInt(Buffer) + IO_BufferSize);
    Dec(Size,IO_BufferSize);
    DoProgress(psEntriesProcessing,ProgressOffset + (ProgressRange * (i / Max)));
  end;
DoProgress(psEntriesProcessing,ProgressOffset + ProgressRange);
end;

//------------------------------------------------------------------------------

procedure TRepairer.DecompressBuffer(InBuff: Pointer; InSize: Integer; out OutBuff: Pointer; out OutSize: Integer; ProgressOffset, ProgressRange: Single);
{$IFNDEF FPC}
type
  TZStream = TZStreamRec;
{$ENDIF}
var
  ZStream:    TZStream;
  SizeDelta:  Integer;
  ResultCode: Integer;

  Function RaiseDecompressionError(aResultCode: Integer): Integer;
  begin
    Result := aResultCode;
    If aResultCode < 0 then
      begin
      {$IF not defined(FPC) or defined(zlib_lib)}
        If Assigned(ZStream.msg) then
          DoError(10,' zlib: ' + z_errmsg[2 - aResultCode] + ' - ' + PAnsiChar(ZStream.msg))
        else
          DoError(10,'zlib: ' + z_errmsg[2 - aResultCode]);
      {$ELSE}
        If Length(ZStream.msg) > 0 then
          DoError(10,'zlib: ' + zError(2 - aResultCode) + ' - ' + ZStream.msg)
        else
          DoError(10,'zlib: ' + zError(2 - aResultCode));
      {$IFEND}
      end;
  end;

begin
DoProgress(psEntriesProcessing,ProgressOffset);
If InSize >= 0 then
  begin
    FillChar({%H-}ZStream,SizeOf(TZStream),0);
    RaiseDecompressionError(InflateInit2(ZStream,-15));
    SizeDelta := InSize + 255 and not 255;
    OutBuff := nil;
    OutSize := SizeDelta;
    try
      try
        ResultCode := Z_OK;  
        ZStream.next_in := InBuff;
        ZStream.avail_in := InSize;
        while (ResultCode <> Z_STREAM_END) and (ZStream.avail_in > 0) do
          repeat
          {$IFDEF preallocated_buffers}
            If fEntryUncompressed.Size < PtrUInt(OutSize) then
              ReallocateBuffer(fEntryUncompressed,OutSize);
            OutBuff := fEntryUncompressed.Memory;
          {$ELSE}
            ReallocMem(OutBuff,OutSize);
          {$ENDIF}
            ZStream.next_out := {%H-}Pointer({%H-}PtrUInt(OutBuff) + ZStream.total_out);
            ZStream.avail_out := Cardinal(OutSize) - ZStream.total_out;
            ResultCode := RaiseDecompressionError(Inflate(ZStream,Z_SYNC_FLUSH));
            DoProgress(psEntriesProcessing,ProgressOffset + (ProgressRange * ZStream.total_in / InSize));
            Inc(OutSize, SizeDelta);
          until (ResultCode = Z_STREAM_END) or (ZStream.avail_out > 0);
        OutSize := ZStream.total_out;
      {$IFNDEF preallocated_buffers}
        ReallocMem(OutBuff,OutSize);
      {$ENDIF}
      finally
        RaiseDecompressionError(InflateEnd(ZStream));
      end;
    except
    {$IFNDEF preallocated_buffers}
      If Assigned(OutBuff) then FreeMem(OutBuff,OutSize);
    {$ENDIF}  
      raise;
    end;
  end;
DoProgress(psEntriesProcessing,ProgressOffset + ProgressRange);
end;

//------------------------------------------------------------------------------

procedure TRepairer.RebuildInputFile;
var
  RebuildFileStream:        TStream;
  i:                        Integer;
  EntryFileBuffer:          Pointer;
  DecompressForProcessing:  Boolean;
  TempOffset:               Int64;
  UncompressedBuffer:       Pointer;
  UncompressedSize:         Integer;
  EntryProgressOffset:      Single;
  EntryProgressRange:       Single;
begin
DoProgress(psEntriesProcessing,0.0);
{$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
ForceDirectoriesUTF8(ExtractFileDir(fProcessingSettings.RepairData));
{$ELSE}
ForceDirectories(ExtractFileDir(fProcessingSettings.RepairData));
{$IFEND}
If fProcessingSettings.InMemoryProcessing then
  RebuildFileStream := TMemoryStream.Create
else
{$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
  RebuildFileStream := TFileStream.Create(UTF8ToSys(fProcessingSettings.RepairData),fmCreate or fmShareDenyWrite);
{$ELSE}
  RebuildFileStream := TFileStream.Create(fProcessingSettings.RepairData,fmCreate or fmShareDenyWrite);
{$IFEND}
try
  If fProcessingSettings.InMemoryProcessing then
    RebuildFileStream.Size := Trunc(fInputFileStream.Size * 1.1);
  EntryProgressOffset := 0.0;
  For i := Low(fInputFileStructure.Entries) to High(fInputFileStructure.Entries) do
    with fInputFileStructure.Entries[i] do
      begin
        UtilityData.OriginalLocalHeaderOffset := CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader;
        If UtilityData.NeedsSizes then
          begin
            If i < High(fInputFileStructure.Entries) then
              LocalHeader.BinPart.CompressedSize := LongWord(Int64(fInputFileStructure.Entries[i + 1].CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader) - UtilityData.DataOffset)
            else
              begin
                TempOffset := FindSignature(CentralDirectoryFileHeaderSignature);
                If TempOffset < 0 then
                  TempOffset := FindSignature(EndOfCentralDirectorySignature);
                If TempOffset >= UtilityData.DataOffset then
                  LocalHeader.BinPart.CompressedSize := LongWord(TempOffset - UtilityData.DataOffset)
                else
                  LocalHeader.BinPart.CompressedSize := LongWord(fInputFileStream.Size - UtilityData.DataOffset);
              end;
            DataDescriptor.CompressedSize := LocalHeader.BinPart.CompressedSize;
            CentralDirectoryHeader.BinPart.CompressedSize := LocalHeader.BinPart.CompressedSize;
          end;
        If fProcessingSettings.AssumeCompressionMethods then
          begin
            If (LocalHeader.BinPart.CompressedSize > 0) and (LocalHeader.BinPart.CompressionMethod <> 0) then
              begin
                LocalHeader.BinPart.CompressionMethod := 8;
                CentralDirectoryHeader.BinPart.CompressionMethod := 8;
              end
            else
              begin
                LocalHeader.BinPart.CompressionMethod := 0;
                CentralDirectoryHeader.BinPart.CompressionMethod := 0;
              end;
          end;
        DecompressForProcessing := (UtilityData.NeedsCRC32 or UtilityData.NeedsSizes) and (LocalHeader.BinPart.CompressionMethod <> 0);
        EntryProgressRange := LocalHeader.BinPart.CompressedSize / fInputFileStream.Size;
      {$IFDEF preallocated_buffers}
        If fEntryCompressed.Size < LocalHeader.BinPart.CompressedSize then
          ReallocateBuffer(fEntryCompressed,(LocalHeader.BinPart.CompressedSize + 255) and not 255);
        EntryFileBuffer := fEntryCompressed.Memory;
      {$ELSE}
        GetMem(EntryFileBuffer,LocalHeader.BinPart.CompressedSize);
        try
      {$ENDIF}
          fInputFileStream.Seek(UtilityData.DataOffset,soFromBeginning);
          ProgressStreamRead(fInputFileStream,EntryFileBuffer,LocalHeader.BinPart.CompressedSize,EntryProgressOffset,EntryProgressRange * 0.4);
          If DecompressForProcessing then
            begin
              DecompressBuffer(EntryFileBuffer,LocalHeader.BinPart.CompressedSize,UncompressedBuffer,UncompressedSize,EntryProgressOffset + (EntryProgressRange * 0.4),EntryProgressRange * 0.2);
              try
                If UtilityData.NeedsCRC32 then
                  begin
                    LocalHeader.BinPart.CRC32 := BufferCRC32(UncompressedBuffer^,UncompressedSize);
                    DataDescriptor.CRC32 := LocalHeader.BinPart.CRC32;
                    CentralDirectoryHeader.BinPart.CRC32 := LocalHeader.BinPart.CRC32;
                  end;
                If UtilityData.NeedsSizes then
                  begin
                    LocalHeader.BinPart.UncompressedSize := UncompressedSize;
                    DataDescriptor.UncompressedSize := UncompressedSize;
                    CentralDirectoryHeader.BinPart.UncompressedSize := UncompressedSize;
                  end;
              finally
              {$IFNDEF preallocated_buffers}
                FreeMem(UncompressedBuffer,UncompressedSize);
              {$ENDIF}
              end;
            end
          else
            begin
              If UtilityData.NeedsCRC32 then
                begin
                  LocalHeader.BinPart.CRC32 := BufferCRC32(EntryFileBuffer^,LocalHeader.BinPart.CompressedSize);
                  DataDescriptor.CRC32 := LocalHeader.BinPart.CRC32;
                  CentralDirectoryHeader.BinPart.CRC32 := LocalHeader.BinPart.CRC32;
                end;
              If UtilityData.NeedsSizes then
                begin
                  LocalHeader.BinPart.UncompressedSize := LocalHeader.BinPart.CompressedSize;
                  DataDescriptor.UncompressedSize := DataDescriptor.CompressedSize;
                  CentralDirectoryHeader.BinPart.UncompressedSize :=  CentralDirectoryHeader.BinPart.CompressedSize;
                end;
            end;
          CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader := LongWord(RebuildFileStream.Position);
          // write local header
          RebuildFileStream.WriteBuffer(LocalHeader.BinPart,SizeOf(TLocalFileHeaderRecord));
          RebuildFileStream.WriteBuffer(PAnsiChar(LocalHeader.FileName)^,LocalHeader.BinPart.FileNameLength);
          RebuildFileStream.WriteBuffer(PAnsiChar(LocalHeader.ExtraField)^,LocalHeader.BinPart.ExtraFieldLength);
          // write data
          ProgressStreamWrite(RebuildFileStream,EntryFileBuffer,LocalHeader.BinPart.CompressedSize,EntryProgressOffset + (EntryProgressRange * 0.6),EntryProgressRange * 0.4);
          // write data descriptor
          If (LocalHeader.BinPart.GeneralPurposeBitFlag and ZBF_DataDescriptor) <> 0 then
            RebuildFileStream.WriteBuffer(DataDescriptor,SizeOf(TDataDescriptorRecord));
      {$IFNDEF preallocated_buffers}
        finally
          FreeMem(EntryFileBuffer,LocalHeader.BinPart.CompressedSize);
        end;
      {$ENDIF}  
        EntryProgressOffset := (UtilityData.DataOffset + LocalHeader.BinPart.CompressedSize) / fInputFileStream.Size;
        DoProgress(psEntriesProcessing,EntryProgressOffset);
      end;
  fInputFileStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset := LongWord(RebuildFileStream.Position);
  // write central directory
  For i := Low(fInputFileStructure.Entries) to High(fInputFileStructure.Entries) do
    with fInputFileStructure.Entries[i].CentralDirectoryHeader do
      begin
        RebuildFileStream.WriteBuffer(BinPart,SizeOf(TCentralDirectoryFileHeaderRecord));
        RebuildFileStream.WriteBuffer(PAnsiChar(FileName)^,BinPart.FileNameLength);
        RebuildFileStream.WriteBuffer(PAnsiChar(ExtraField)^,BinPart.ExtraFieldLength);
        RebuildFileStream.WriteBuffer(PAnsiChar(FileComment)^,BinPart.FileCommentLength);
      end;
  // write end of central directory
  with fInputFileStructure.EndOfCentralDirectory do
    begin
      RebuildFileStream.WriteBuffer(BinPart,SizeOf(TEndOfCentralDirectoryRecord));
      RebuildFileStream.WriteBuffer(PAnsiChar(Comment)^,BinPart.CommentLength);
    end;
  // finalize
  RebuildFileStream.Size := RebuildFileStream.Position;
  If fProcessingSettings.InMemoryProcessing then
    ProgressSaveFile(fProcessingSettings.RepairData,RebuildFileStream);
finally
  RebuildFileStream.Free;
end;
DoProgress(psEntriesProcessing,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ExtractInputFile;
var
  i:                      Integer;
  FullFileName:           String;
  EntryOutputFileStream:  TFileStream;
  TempOffset:             Int64;
  EntryFileBuffer:        Pointer;
  UncompressedBuffer:     Pointer;
  UncompressedSize:       Integer;
  EntryProgressOffset:    Single;
  EntryProgressRange:     Single;

  procedure WriteFileTime(const FileName: String; LastModTime, LastModDate: Word; Directory: Boolean = False);
  var
    FileHandle: THandle;
    FileTime:   TFileTime;
  begin
    If Directory then
  {$IF Defined(FPC) and not Defined(Unicode)}
      FileHandle := CreateFile(PChar(UTF8ToWinCP(FileName)),FILE_WRITE_ATTRIBUTES,FILE_SHARE_READ or FILE_SHARE_WRITE,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL or FILE_FLAG_BACKUP_SEMANTICS,0)
    else
      FileHandle := CreateFile(PChar(UTF8ToWinCP(FileName)),FILE_WRITE_ATTRIBUTES,FILE_SHARE_READ or FILE_SHARE_WRITE,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
  {$ELSE}
      FileHandle := CreateFile(PChar(FileName),FILE_WRITE_ATTRIBUTES,FILE_SHARE_READ or FILE_SHARE_WRITE,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL or FILE_FLAG_BACKUP_SEMANTICS,0)
    else
      FileHandle := CreateFile(PChar(FileName),FILE_WRITE_ATTRIBUTES,FILE_SHARE_READ or FILE_SHARE_WRITE,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
  {$IFEND}
    If FileHandle <> INVALID_HANDLE_VALUE then
      try
        If DosDateTimeToFileTime(LastModDate,LastModTime,{%H-}FileTime) then
          begin
            If LocalFileTimeToFileTime(FileTime,FileTime) then
              begin
                If not SetFileTime(FileHandle,nil,nil,@FileTime) then
                  DoError(11,'Cannot write file time (%d).',[GetLastError]);
              end
            else DoError(11,'Cannot convert local file time to file time (%d).',[GetLastError]);
          end
        else DoError(11,'Cannot convert DOS time to file time (%d).',[GetLastError]);
      finally
        CloseHandle(FileHandle);
      end
    else DoError(11,'Cannot open file "%s" (%d).',[FileName,GetLastError]);
  end;

begin
DoProgress(psEntriesProcessing,0.0);
EntryProgressOffset := 0.0;
For i := Low(fInputFileStructure.Entries) to High(fInputFileStructure.Entries) do
  with fInputFileStructure.Entries[i] do
    try
      FullFileName := IncludeTrailingPathDelimiter(fProcessingSettings.RepairData) +
                      AnsiReplaceStr(LocalHeader.FileName,'/','\');
    {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
      If not DirectoryExistsUTF8(ExtractFileDir(FullFileName)) then
        ForceDirectoriesUTF8(ExtractFileDir(FullFileName));
    {$ELSE}
      If not DirectoryExists(ExtractFileDir(FullFileName)) then
        ForceDirectories(ExtractFileDir(FullFileName));
    {$IFEND}
      WriteFileTime(ExtractFileDir(FullFileName),LocalHeader.BinPart.LastModFileTime,LocalHeader.BinPart.LastModFileDate,True);
      If (ExtractFileName(FullFileName) <> '') and ((CentralDirectoryHeader.BinPart.ExternalFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0) then
        begin
        {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
          EntryOutputFileStream := TFileStream.Create(UTF8ToSys(FullFileName),fmCreate or fmShareDenyWrite);
        {$ELSE}
          EntryOutputFileStream := TFileStream.Create(FullFileName,fmCreate or fmShareDenyWrite);
        {$IFEND}
          try
            If UtilityData.NeedsSizes then
              begin
                If i < High(fInputFileStructure.Entries) then
                  LocalHeader.BinPart.CompressedSize := LongWord(Int64(fInputFileStructure.Entries[i + 1].CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader) - UtilityData.DataOffset)
                else
                  begin
                    TempOffset := FindSignature(CentralDirectoryFileHeaderSignature);
                    If TempOffset < 0 then
                      TempOffset := FindSignature(EndOfCentralDirectorySignature);
                    If TempOffset >= UtilityData.DataOffset then
                      LocalHeader.BinPart.CompressedSize := LongWord(TempOffset - UtilityData.DataOffset)
                    else
                      LocalHeader.BinPart.CompressedSize := LongWord(fInputFileStream.Size - UtilityData.DataOffset);
                  end;
              end;
            If fProcessingSettings.AssumeCompressionMethods then
              begin
                If (LocalHeader.BinPart.CompressedSize > 0) and (LocalHeader.BinPart.CompressionMethod <> 0) then
                  LocalHeader.BinPart.CompressionMethod := 8
                else
                  LocalHeader.BinPart.CompressionMethod := 0;
              end;
            EntryProgressRange := LocalHeader.BinPart.CompressedSize / fInputFileStream.Size;
          {$IFDEF preallocated_buffers}
            If fEntryCompressed.Size < LocalHeader.BinPart.CompressedSize then
              ReallocateBuffer(fEntryCompressed,(LocalHeader.BinPart.CompressedSize + 255) and not 255);
            EntryFileBuffer := fEntryCompressed.Memory;
          {$ELSE}
            GetMem(EntryFileBuffer,LocalHeader.BinPart.CompressedSize);
            try
          {$ENDIF}
              fInputFileStream.Seek(UtilityData.DataOffset,soFromBeginning);
              ProgressStreamRead(fInputFileStream,EntryFileBuffer,LocalHeader.BinPart.CompressedSize,EntryProgressOffset,EntryProgressRange * 0.4);
              case LocalHeader.BinPart.CompressionMethod of
                8:  begin
                      DecompressBuffer(EntryFileBuffer,LocalHeader.BinPart.CompressedSize,UncompressedBuffer,UncompressedSize,EntryProgressOffset + (EntryProgressRange * 0.4),EntryProgressRange * 0.2);
                      try
                        ProgressStreamWrite(EntryOutputFileStream,UncompressedBuffer,UncompressedSize,EntryProgressOffset + (EntryProgressRange * 0.6),EntryProgressRange * 0.4);
                      finally
                      {$IFNDEF preallocated_buffers}
                        FreeMem(UncompressedBuffer,UncompressedSize);
                      {$ENDIF}
                      end;
                    end;
              else
                ProgressStreamWrite(EntryOutputFileStream,EntryFileBuffer,LocalHeader.BinPart.CompressedSize,EntryProgressOffset + (EntryProgressRange * 0.6),EntryProgressRange * 0.4);
              end;
              EntryProgressOffset := (UtilityData.DataOffset + LocalHeader.BinPart.CompressedSize) / fInputFileStream.Size;
              DoProgress(psEntriesProcessing,EntryProgressOffset);
          {$IFNDEF preallocated_buffers}
            finally
              FreeMem(EntryFileBuffer,LocalHeader.BinPart.CompressedSize);
            end;
          {$ENDIF}
            EntryOutputFileStream.Size := EntryOutputFileStream.Position;
            WriteFileTime(FullFileName,LocalHeader.BinPart.LastModFileTime,LocalHeader.BinPart.LastModFileDate);
          finally
            EntryOutputFileStream.Free;
          end;
        end;
    except
     on E: Exception do
       begin
          If fProcessingSettings.IgnoreProcessingErrors and (InterlockedExchangeAdd(fTerminated,0) >= 0) then
            begin
              InterlockedExchange(fTerminated,0);
              fIgnoredErrors.Add(Format('%s: %s',[E.ClassName,E.Message]));
            end
          else raise;
        end;
    end;
DoProgress(psEntriesProcessing,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProcessFile_Rebuild;
begin
If AnsiSameText(fInputFileName,fProcessingSettings.RepairData) then
  DoError(8,'Output is directed into an input file, cannot proceed.');
If not fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  begin
    LoadEndOfCentralDirectory;
    If not fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries then
      SetLength(fInputFileStructure.Entries,fInputFileStructure.EndOfCentralDirectory.BinPart.EntriesOnDisk);
  end;
If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
  LoadCentralDirectory;
LoadLocalHeaders;
If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
  ReconstructLocalHeaders;
ReconstructCentralDirectoryHeaders;
ReconstructEndOfCentralDirectory;
If Length(fInputFileStructure.Entries) > 0 then
  RebuildInputFile
else
  DoError(8,'Input file does not contain any valid entries.');
DoProgress(psProcessing,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProcessFile_Extract;
begin
If not fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  begin
    LoadEndOfCentralDirectory;
    If not fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries then
      SetLength(fInputFileStructure.Entries,fInputFileStructure.EndOfCentralDirectory.BinPart.EntriesOnDisk);
  end;
If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
  LoadCentralDirectory;
LoadLocalHeaders;
If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
  ReconstructLocalHeaders;
ReconstructCentralDirectoryHeaders;
ReconstructEndOfCentralDirectory;
If Length(fInputFileStructure.Entries) > 0 then
  ExtractInputFile
else
  DoError(9,'Input file does not contain any valid entries.');
DoProgress(psProcessing,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer.ProcessFile;
begin
DoProgress(psProcessing,0.0);
try
  AllocateBuffers;
  try
    If fProcessingSettings.InMemoryProcessing then
      fInputFileStream := TMemoryStream.Create
    else
    {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
      fInputFileStream := TFileStream.Create(UTF8ToSys(fInputFileName),fmOpenRead or fmShareDenyWrite);
    {$ELSE}
      fInputFileStream := TFileStream.Create(fInputFileName,fmOpenRead or fmShareDenyWrite);
    {$IFEND}
    try
      If fProcessingSettings.InMemoryProcessing then
        ProgressLoadFile(fInputFileName,fInputFileStream);
      If fInputFileStream.Size <= 0 then
        DoError(7,'Input file does not contain any data.');
      If not fProcessingSettings.IgnoreFileSignature then
        CheckInputFileSignature;
      case fProcessingSettings.RepairMethod of
        rmRebuild:  ProcessFile_Rebuild;
        rmExtract:  ProcessFile_Extract;
      else
        DoError(7,'Unknown repair method (%d).',[Ord(fProcessingSettings.RepairMethod)]);
      end;
      DoProgress(psProcessing,2.0);
      If (fProcessingSettings.RepairMethod = rmExtract) and fProcessingSettings.IgnoreProcessingErrors and
        fProcessingSettings.LogIgnoredErrors and (fIgnoredErrors.Count > 0) then
        begin
        {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
          fIgnoredErrors.SaveToFile(UTF8ToSys(fInputFileName + '.ignored_errors.log'));
        {$ELSE}
          fIgnoredErrors.SaveToFile(fInputFileName + '.ignored_errors.log');
        {$IFEND}
        end;
    finally
      fInputFileStream.Free;
    end;
  finally
    FreeBuffers;
  end;
except
  on E: Exception do
    begin
      fErrorData.Text := E.Message;
      fErrorData.ExceptionClass := E.ClassName;
      DoProgress(psError,-1.0);
    end;
end;
end;

//------------------------------------------------------------------------------

procedure TRepairer.InitializeProgressInfo;
var
  InnerProcessingRange: Single;
begin
FillChar(fProgressInfo,SizeOf(TProgressInfo),0);
fProgressInfo[psError].Offset := -1.0;
fProgressInfo[psProcessing].Range := 1.0;
If fProcessingSettings.InMemoryProcessing then
  begin
    case fProcessingSettings.RepairMethod of
      rmRebuild:  begin
                    fProgressInfo[psLoading].Range := 0.3;
                    fProgressInfo[psSaving].Range := 0.3;
                  end;
      rmExtract:  fProgressInfo[psLoading].Range := 0.4;
    end;
    fProgressInfo[psSaving].Offset := 1 - fProgressInfo[psSaving].Range;
  end;
InnerProcessingRange := 1 - (fProgressInfo[psLoading].Range + fProgressInfo[psSaving].Range);
fProgressInfo[psEOCDLoading].Offset := fProgressInfo[psLoading].Range;
If not fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  fProgressInfo[psEOCDLoading].Range := 0.01 * InnerProcessingRange;
fProgressInfo[psCDHeadersLoading].Offset := fProgressInfo[psEOCDLoading].Offset + fProgressInfo[psEOCDLoading].Range;
If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
  fProgressInfo[psCDHeadersLoading].Range := 0.1 * InnerProcessingRange;
fProgressInfo[psLocalHeadersLoading].Offset := fProgressInfo[psCDHeadersLoading].Offset + fProgressInfo[psCDHeadersLoading].Range;
If not fProcessingSettings.LocalHeader.IgnoreLocalHeaders then
  fProgressInfo[psLocalHeadersLoading].Range := 0.1 * InnerProcessingRange;
fProgressInfo[psEntriesProcessing].Offset := fProgressInfo[psLocalHeadersLoading].Offset + fProgressInfo[psLocalHeadersLoading].Range;
fProgressInfo[psEntriesProcessing].Range := InnerProcessingRange - (fProgressInfo[psLocalHeadersLoading].Range + fProgressInfo[psCDHeadersLoading].Range + fProgressInfo[psEOCDLoading].Range);
end;

//------------------------------------------------------------------------------

procedure TRepairer.AllocateBuffers;
begin
{$IFDEF preallocated_buffers}
AllocateBuffer(fIOBuffer,IO_BufferSize);
AllocateBuffer(fEntryCompressed,CED_BufferSize);
AllocateBuffer(fEntryUncompressed,UED_BufferSize);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TRepairer.FreeBuffers;
begin
{$IFDEF preallocated_buffers}
FreeBuffer(fIOBuffer);
FreeBuffer(fEntryCompressed);
FreeBuffer(fEntryUncompressed);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TRepairer.DoProgress(ProgressStage: TProgressStage; Data: Single);
begin
If ProgressStage <> psProcessing then
  If Data > 1.0 then Data := 1.0;
If (ProgressStage <> psError) and (InterlockedExchange(fTerminated,0) <> 0) then
  DoError(-1,'Processing terminated. Data can be in inconsistent state.',-1);
If ProgressStage <> psNoProgress then
  begin
    Data := fProgressInfo[ProgressStage].Offset + (fProgressInfo[ProgressStage].Range * Data);
    If Assigned(fOnProgress) then fOnProgress(Self,Data);
  end;
end;

//------------------------------------------------------------------------------

procedure TRepairer.DoError(MethodIdx: Integer; ErrorText: String; Values: array of const; TerminationFlag: Integer = 1);
begin
fErrorData.MethodIdx := MethodIdx;
If (MethodIdx >= Low(MethodNames)) and (MethodIdx <= High(MethodNames)) then
  fErrorData.MethodName := MethodNames[MethodIdx]
else
  fErrorData.MethodName := 'unknown method';
InterlockedExchange(fTerminated,TerminationFlag);
raise ERepairerException.Create(Format(ErrorText,Values,ThreadFormatSettings));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TRepairer.DoError(MethodIdx: Integer; ErrorText: String; TerminationFlag: Integer = 1);
begin
DoError(MethodIdx,ErrorText,[],TerminationFlag);
end;

{------------------------------------------------------------------------------}
{   TRepairer - Public methods                                                 }
{------------------------------------------------------------------------------}

constructor TRepairer.Create(ProcessingSettings: TProcessingSettings; InputFileName: String);
begin
inherited Create;
fProcessingSettings := ProcessingSettings;
ValidateProcessingSettings;
fInputFileName := InputFileName;
InitFileStructure;
FillChar(fErrorData,SizeOf(TErrorInfo),0);
InitializeProgressInfo;
fErrorData.Source := Pointer(Self);
fErrorData.SourceClass := Self.ClassName;
fErrorData.MethodIdx := -1;
fErrorData.MethodName := 'unknown method';
fErrorData.ThreadID := GetCurrentThreadID;
fIgnoredErrors := TStringList.Create
end;

//------------------------------------------------------------------------------

destructor TRepairer.Destroy;
begin
fIgnoredErrors.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TRepairer.InitFileStructure;
begin
SetLength(fInputFileStructure.Entries,0);
FillChar(fInputFileStructure.EndOfCentralDirectory.BinPart,SizeOf(TEndOfCentralDirectoryRecord),0);
fInputFileStructure.EndOfCentralDirectory.Comment := '';
end;

//------------------------------------------------------------------------------

procedure TRepairer.Start;
begin
InterlockedExchange(fTerminated,0);
ProcessFile;
end;

//------------------------------------------------------------------------------

procedure TRepairer.Stop;
begin
InterlockedExchange(fTerminated,-1);
end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                                TRepairerThread                               }
{------------------------------------------------------------------------------}
{==============================================================================}

{------------------------------------------------------------------------------}
{   TRepairerThread - Protected methods                                        }
{------------------------------------------------------------------------------}

procedure TRepairerThread.sync_DoProgress;
begin
If Assigned(fOnProgress) then fOnProgress(Self,sync_Progress);
end;

//------------------------------------------------------------------------------

procedure TRepairerThread.ProgressHandler(Sender: TObject; Progress: Single);
begin
sync_Progress := Progress;
If Progress < 0.0 then
  fErrorData := fRepairer.ErrorData;
Synchronize(sync_DoProgress);
end;

//------------------------------------------------------------------------------

procedure TRepairerThread.Execute;
begin
fRepairer.Start;
end;

{------------------------------------------------------------------------------}
{   TRepairerThread - Public methods                                           }
{------------------------------------------------------------------------------}

constructor TRepairerThread.Create(ProcessingSettings: TProcessingSettings; InputFileName: String);
begin
inherited Create(True);
// to ensure thread safety...
UniqueString(ProcessingSettings.RepairData);
UniqueString(InputFileName);
fRepairer := TRepairer.Create(ProcessingSettings,InputFileName);
fRepairer.OnProgress := ProgressHandler;
end;

//------------------------------------------------------------------------------

destructor TRepairerThread.Destroy;
begin
fRepairer.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TRepairerThread.Start;
begin
{$WARN SYMBOL_DEPRECATED OFF}
Resume;
{$WARN SYMBOL_DEPRECATED ON}
end;

//------------------------------------------------------------------------------

procedure TRepairerThread.Pause;
begin
{$WARN SYMBOL_DEPRECATED OFF}
Suspend;
{$WARN SYMBOL_DEPRECATED ON}
end;

//------------------------------------------------------------------------------

procedure TRepairerThread.Stop;
begin
fRepairer.Stop;
end;

//==============================================================================

initialization
{$WARN SYMBOL_PLATFORM OFF}
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT,{%H-}ThreadFormatSettings);
{$WARN SYMBOL_PLATFORM ON}
{$IFDEF zlib_lib_dll}
  ExtractLibrary;
  Initialize;
{$ENDIF}

{$IFDEF zlib_lib_dll}
finalization
  Finalize;
{$ENDIF}

end.
