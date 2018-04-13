{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_ZIP;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes, ProgressTracker,
  DART_Common, DART_ProcessingSettings, DART_Format_ZIP, DART_Repairer;

{===============================================================================
--------------------------------------------------------------------------------
                                TDARTRepairer_ZIP
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_ZIP - progress stages indexing variables
===============================================================================}

var
  DART_PROGSTAGE_IDX_ZIP_EOCDLoading:         Integer = -1;
  DART_PROGSTAGE_IDX_ZIP_CDHeadersLoading:    Integer = -1;
  DART_PROGSTAGE_IDX_ZIP_LocalHeadersLoading: Integer = -1;
  DART_PROGSTAGE_IDX_ZIP_EntriesProgressPrep: Integer = -1;
  DART_PROGSTAGE_IDX_ZIP_EntriesConverting:   Integer = -1;
  DART_PROGSTAGE_IDX_ZIP_EntriesProcessing:   Integer = -1;

  DART_PROGSTAGE_IDX_ZIP_EntryProcessing:     Integer = -1; // not really used
  DART_PROGSTAGE_IDX_ZIP_EntryLoading:        Integer = -1;
  DART_PROGSTAGE_IDX_ZIP_EntryDecompression:  Integer = -1;
  DART_PROGSTAGE_IDX_ZIP_EntrySaving:         Integer = -1;

  PSIDX_Z_EOCDLoading:         Integer = -1;
  PSIDX_Z_CDHeadersLoading:    Integer = -1;
  PSIDX_Z_LocalHeadersLoading: Integer = -1;
  PSIDX_Z_EntriesProgressPrep: Integer = -1;
  PSIDX_Z_EntriesConverting:   Integer = -1;
  PSIDX_Z_EntriesProcessing:   Integer = -1;

  PSIDX_Z_EntryProcessing:     Integer = -1;
  PSIDX_Z_EntryLoading:        Integer = -1;
  PSIDX_Z_EntryDecompression:  Integer = -1;
  PSIDX_Z_EntrySaving:         Integer = -1;

{===============================================================================
    TDARTRepairer_ZIP - class declaration
===============================================================================}
type
  TDARTRepairer_ZIP = class(TDARTRepairer)
  protected
    fProcessingSettings:  TDART_PS_ZIP;
    fArchiveStructure:    TDART_ZIP_ArchiveStructure;
    // initialization methods
    procedure InitializeProcessingSettings; override;
    procedure InitializeData; override;
    procedure InitializeProgress; override;
    // methods for content parsing
    Function IndexOfEntry(const EntryFileName: AnsiString): Integer; override;
    Function GetEntryData(EntryIndex: Integer; out Data: Pointer; out Size: TMemSize): Boolean; override;
    // methods working with known paths
    Function IndexOfKnownPath(const Path: AnsiString): Integer; override;
    Function AddKnownPath(const Path: AnsiString; Directory: Boolean): Integer; override;
    // processing methods
    procedure ArchiveProcessing; override;
    // zip specific routines
    procedure ZIP_LoadEndOfCentralDirectory; virtual;
    procedure ZIP_LoadCentralDirectory; virtual;
    procedure ZIP_LoadLocalHeaders; virtual;
    procedure ZIP_ReconstructLocalHeaders; virtual;
    procedure ZIP_ReconstructCentralDirectoryHeaders; virtual;
    procedure ZIP_ReconstructEndOfCentralDirectory; virtual;
    procedure ZIP_ReconstructFinal; virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
    constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings; CatchExceptions: Boolean);
    Function GetAllKnownPaths(var KnownPaths: TDARTKnownPaths): Integer; override;
    property ArchiveStructure: TDART_ZIP_ArchiveStructure read fArchiveStructure;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                        TDARTRepairer_ZIP_ProcessingBase
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_ZIP_ProcessingBase - class declaration
===============================================================================}

  TDARTRepairer_ZIP_ProcessingBase = class(TDARTRepairer_ZIP)
  protected
    procedure ArchiveProcessing; override;
    procedure ZIP_PrepareEntriesProgress; virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;    
  end;

implementation

uses
  Windows, SysUtils, Classes, StrUtils, CRC32,
  StrRect, MemoryBuffer, ZLibCommon,
  DART_Auxiliary, DART_PathDeconstructor;

{===============================================================================
--------------------------------------------------------------------------------
                                TDARTRepairer_ZIP
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_ZIP - method indexing constants
===============================================================================}

const
  DART_METHOD_ID_ZIP_ARCHPROC = $00000100;
  DART_METHOD_ID_ZIP_GETENTRY = $00000101;
  DART_METHOD_ID_ZIP_ZLEOCD   = $00000110;
  DART_METHOD_ID_ZIP_ZLCD     = $00000111;
  DART_METHOD_ID_ZIP_ZLCDH    = $00000112;
  DART_METHOD_ID_ZIP_ZLLH     = $00000113;
  DART_METHOD_ID_ZIP_ZLLHH    = $00000114;

{===============================================================================
    TDARTRepairer_ZIP - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTRepairer_ZIP - protected methods
-------------------------------------------------------------------------------}

procedure TDARTRepairer_ZIP.InitializeProcessingSettings;
begin
inherited;
fProcessingSettings := fArchiveProcessingSettings.ZIP;
RectifyZIPProcessingSettings(fProcessingSettings);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.InitializeData;
begin
SetLength(fArchiveStructure.Entries.Arr,0);
fArchiveStructure.Entries.Count := 0;
FillChar(fArchiveStructure.EndOfCentralDirectory.BinPart,SizeOf(TDART_ZIP_EndOfCentralDirectoryRecord),0);
fArchiveStructure.EndOfCentralDirectory.Comment := '';
SetLength(fArchiveStructure.KnownPaths.Arr,0);
fArchiveStructure.KnownPaths.Count := 0;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.InitializeProgress;
var
  Quota:  Integer;
begin
inherited;
fProcessingProgNode.BeginUpdate;
try
  Quota := 1000;
  If not fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
    begin
      DART_PROGSTAGE_IDX_ZIP_EOCDLoading := fProcessingProgNode.Add(10);
      Dec(Quota,10);
    end;
  If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
    begin
      DART_PROGSTAGE_IDX_ZIP_CDHeadersLoading := fProcessingProgNode.Add(100);
      Dec(Quota,100);
    end;
  If not fProcessingSettings.LocalHeader.IgnoreLocalHeaders then
    begin
      DART_PROGSTAGE_IDX_ZIP_LocalHeadersLoading := fProcessingProgNode.Add(100);
      Dec(Quota,100);
    end;
  DART_PROGSTAGE_IDX_ZIP_EntriesProgressPrep := fProcessingProgNode.Add(50);
  Dec(Quota,50);
  If fArchiveProcessingSettings.Common.RepairMethod = rmConvert then
    begin
      DART_PROGSTAGE_IDX_ZIP_EntriesConverting := fProcessingProgNode.Add(50);
      Dec(Quota,50);
    end;
  DART_PROGSTAGE_IDX_ZIP_EntriesProcessing := fProcessingProgNode.Add(Quota);
  // assign obtained indices to shorter-named variables  
  PSIDX_Z_EOCDLoading         := DART_PROGSTAGE_IDX_ZIP_EOCDLoading;
  PSIDX_Z_CDHeadersLoading    := DART_PROGSTAGE_IDX_ZIP_CDHeadersLoading;
  PSIDX_Z_LocalHeadersLoading := DART_PROGSTAGE_IDX_ZIP_LocalHeadersLoading;
  PSIDX_Z_EntriesProgressPrep := DART_PROGSTAGE_IDX_ZIP_EntriesProgressPrep;
  PSIDX_Z_EntriesConverting   := DART_PROGSTAGE_IDX_ZIP_EntriesConverting;
  PSIDX_Z_EntriesProcessing   := DART_PROGSTAGE_IDX_ZIP_EntriesProcessing;
  fEntriesProcessingProgNode := fProcessingProgNode.StageObjects[PSIDX_Z_EntriesProcessing];
finally
  fProcessingProgNode.EndUpdate;
end;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_ZIP.IndexOfEntry(const EntryFileName: AnsiString): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  If AnsiSameText(AnsiToStr(EntryFileName),AnsiToStr(fArchiveStructure.Entries.Arr[i].CentralDirectoryHeader.FileName)) then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_ZIP.GetEntryData(EntryIndex: Integer; out Data: Pointer; out Size: TMemSize): Boolean;
begin
Result := False;
If (EntryIndex >= Low(fArchiveStructure.Entries.Arr)) and (EntryIndex < fArchiveStructure.Entries.Count) then
  begin
    with fArchiveStructure.Entries.Arr[EntryIndex] do
      begin
      {
        This method is called only when parsing content of the archive for paths, which,
        for ZIP archive, occurs only when processing it as help archive - therefore we will assume
        the archive is not corrupted and stored compressed size and compression method are
        both correct and do not need to be rectified.
        If archive is corrupted, it should first be repaired and only then used as a help archive.
      }
        // prepare buffer for entry data
        ReallocBufferKeep(fBuffer_Entry,LocalHeader.BinPart.CompressedSize);
        // load entry data
        fInputArchiveStream.Seek(UtilityData.DataOffset,soBeginning);
        ProgressedStreamRead(fInputArchiveStream,fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,DART_PROGSTAGE_INFO_NoProgress);
        If LocalHeader.BinPart.CompressionMethod = DART_ZCM_Store then
          begin
            // entry data are not compressed, copy them out of buffer
            GetMem(Data,LocalHeader.BinPart.CompressedSize);
            Size := TMemSize(LocalHeader.BinPart.CompressedSize);
            Move(fBuffer_Entry.Memory^,Data^,Size);
          end
        else
          begin
            // entry data are compressed, decompress them
            ProgressedDecompressBuffer(fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,
                                       Data,Size,WBITS_RAW,DART_PROGSTAGE_INFO_NoProgress);
          end;
        // if we are here, everything should be fine
        Result := True;        
      end;
  end
else DoError(DART_METHOD_ID_ZIP_GETENTRY,'Entry index (%d) out of bounds.',[EntryIndex]);
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_ZIP.IndexOfKnownPath(const Path: AnsiString): Integer;
var
  PathHash: TCRC32;
  i:        Integer;
begin
Result := -1;
PathHash := StringCRC32(AnsiLowerCase(AnsiToStr(Path)));
For i := Low(fArchiveStructure.KnownPaths.Arr) to Pred(fArchiveStructure.KnownPaths.Count) do
  If fArchiveStructure.KnownPaths.Arr[i].Hash = PathHash then
    If AnsiSameText(AnsiToStr(Path),AnsiToStr(fArchiveStructure.KnownPaths.Arr[i].Path)) then
      begin
        Result := i;
        Break{For i};
      end;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_ZIP.AddKnownPath(const Path: AnsiString; Directory: Boolean): Integer;
begin
Result := IndexOfKnownPath(Path);
If Result < 0 then
  begin
    If fArchiveStructure.KnownPaths.Count >= Length(fArchiveStructure.KnownPaths.Arr) then
      SetLength(fArchiveStructure.KnownPaths.Arr,Length(fArchiveStructure.KnownPaths.Arr) + 1024);
    fArchiveStructure.KnownPaths.Arr[fArchiveStructure.KnownPaths.Count].Path := Path;
    fArchiveStructure.KnownPaths.Arr[fArchiveStructure.KnownPaths.Count].Directory := Directory;
    fArchiveStructure.KnownPaths.Arr[fArchiveStructure.KnownPaths.Count].Hash := StringCRC32(AnsiLowerCase(AnsiToStr(Path)));
    fArchiveStructure.KnownPaths.Arr[fArchiveStructure.KnownPaths.Count].Hash64 := 0;
    Inc(fArchiveStructure.KnownPaths.Count);
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.ArchiveProcessing;
begin
If not fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  begin
    ZIP_LoadEndOfCentralDirectory;
    If not fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries then
      begin
        SetLength(fArchiveStructure.Entries.Arr,fArchiveStructure.EndOfCentralDirectory.BinPart.EntriesOnDisk);
        fArchiveStructure.Entries.Count := Length(fArchiveStructure.Entries.Arr);
      end
  end;
If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
  ZIP_LoadCentralDirectory;
If not fProcessingSettings.LocalHeader.IgnoreLocalHeaders then
  ZIP_LoadLocalHeaders;
ZIP_ReconstructLocalHeaders;
ZIP_ReconstructCentralDirectoryHeaders;
ZIP_ReconstructEndOfCentralDirectory;
ZIP_ReconstructFinal;
If fArchiveStructure.Entries.Count <= 0 then
  DoError(DART_METHOD_ID_ZIP_ARCHPROC,'Input archive does not contain any valid entries.');
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.ZIP_LoadEndOfCentralDirectory;
var
  EOCDPosition: Int64;
begin
DoProgress(fProcessingProgNode,PSIDX_Z_EOCDLoading,0.0);
EOCDPosition := FindSignature(DART_ZIP_EndOfCentralDirectorySignature,DART_PROGSTAGE_INFO_NoProgress,-1,
                              True,fProcessingSettings.EndOfCentralDirectory.LimitSearch);
DoProgress(fProcessingProgNode,PSIDX_Z_EOCDLoading,0.5);
If EOCDPosition >= 0 then
  begin
    fInputArchiveStream.Seek(EOCDPosition,soBeginning);
    If (fInputArchiveStream.Size - fInputArchiveStream.Position) >= SizeOf(TDART_ZIP_EndOfCentralDirectoryRecord) then
      begin
        with fArchiveStructure.EndOfCentralDirectory do
          begin
            fInputArchiveStream.ReadBuffer(BinPart,SizeOf(TDART_ZIP_EndOfCentralDirectoryRecord));
            DoProgress(fProcessingProgNode,PSIDX_Z_EOCDLoading,0.9);
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
                If (fInputArchiveStream.Size - fInputArchiveStream.Position) >= BinPart.CommentLength then
                  begin
                    SetLength(Comment,BinPart.CommentLength);
                    fInputArchiveStream.ReadBuffer(PAnsiChar(Comment)^,BinPart.CommentLength);
                  end
                else DoError(DART_METHOD_ID_ZIP_ZLEOCD,'Not enough data for end of central directory comment.');
              end;
          end;
      end
    else DoError(DART_METHOD_ID_ZIP_ZLEOCD,'Not enough data for end of central directory record.');
  end
else DoError(DART_METHOD_ID_ZIP_ZLEOCD,'End of central directory signature not found in the input stream.');
DoProgress(fProcessingProgNode,PSIDX_Z_EOCDLoading,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.ZIP_LoadCentralDirectory;
var
  WorkingOffset:  Int64;
  i:              Integer;

  // load single header to an entry at index position
  procedure LoadCentralDirectoryHeader(Index: Integer);
  begin
    with fArchiveStructure.Entries.Arr[Index].CentralDirectoryHeader do
      begin
        fInputArchiveStream.ReadBuffer(BinPart,SizeOf(TDART_ZIP_CentralDirectoryFileHeaderRecord));
        // binary part checks
        If fProcessingSettings.CentralDirectory.IgnoreSignature then
          BinPart.Signature := DART_ZIP_CentralDirectoryFileHeaderSignature
        else
          If BinPart.Signature <> DART_ZIP_CentralDirectoryFileHeaderSignature then
            DoError(DART_METHOD_ID_ZIP_ZLCDH,'Bad central directory header signature (0x%.8x) for entry #%d.',[BinPart.Signature,Index]);
        If fProcessingSettings.CentralDirectory.IgnoreVersions then
          begin
            BinPart.VersionMadeBy := DART_ZIP_DefVersionMadeBy;
            BinPart.HostOS := DART_ZIP_DefHostOS;
            BinPart.VersionNeededToExtract := DART_ZIP_DefVersionMadeBy;
            BinPart.OSNeededForExtraction := DART_ZIP_DefHostOS;
          end;
        If fProcessingSettings.CentralDirectory.ClearEncryptionFlags then
          BinPart.GeneralPurposeBitFlag := BinPart.GeneralPurposeBitFlag and not UInt16(DART_ZBF_Encrypted or DART_ZBF_StrongEncryption);
        If not fProcessingSettings.CentralDirectory.IgnoreCompressionMethod then
          begin
            If fProcessingSettings.AssumeCompressionMethod then
              begin
                If BinPart.CompressionMethod <> DART_ZCM_Store then
                  BinPart.CompressionMethod := DART_ZCM_Deflate;
              end
            else
              begin
                If not(BinPart.CompressionMethod in DART_ZIP_SupportedCompressinMethods) then
                  DoError(DART_METHOD_ID_ZIP_ZLCDH,'Unknown compression method (%d) in central directory header for entry #%d.',[BinPart.CompressionMethod,Index]);
              end;
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
          end
        else
          begin
            If fProcessingSettings.CentralDirectory.IgnoreCompressionMethod then
              begin
                If BinPart.CompressedSize <> BinPart.UncompressedSize then
                  begin
                    If BinPart.CompressedSize = 0 then
                      begin
                        BinPart.CompressionMethod := DART_ZCM_Store;
                        BinPart.UncompressedSize := BinPart.CompressedSize
                      end
                    else BinPart.CompressionMethod := DART_ZCM_Deflate;
                  end
                else BinPart.CompressionMethod := DART_ZCM_Store
              end;
          end;
        // load file name
        SetLength(FileName,BinPart.FilenameLength);
        fInputArchiveStream.ReadBuffer(PAnsiChar(FileName)^,BinPart.FileNameLength);
        // load extra field
        If fProcessingSettings.CentralDirectory.IgnoreExtraField then
          begin
            fInputArchiveStream.Seek(BinPart.ExtraFieldLength,soCurrent);
            BinPart.ExtraFieldLength := 0;
          end
        else
          begin
            SetLength(ExtraField,BinPart.ExtraFieldLength);
            fInputArchiveStream.ReadBuffer(PAnsiChar(ExtraField)^,BinPart.ExtraFieldLength);
           end;
        // load file comment
        If fProcessingSettings.CentralDirectory.IgnoreFileComment then
          begin
            fInputArchiveStream.Seek(BinPart.FileCommentLength,soCurrent);
            BinPart.FileCommentLength := 0;
          end
        else
          begin
            SetLength(FileComment,BinPart.FileCommentLength);
            fInputArchiveStream.ReadBuffer(PAnsiChar(FileComment)^,BinPart.FileCommentLength);
          end;
        If fProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit then
          BinPart.DiskNumberStart := 0;  
        // file attributes
        If fProcessingSettings.CentralDirectory.IgnoreInternalFileAttributes then
          BinPart.InternalFileAttributes := 0;
        If fProcessingSettings.CentralDirectory.IgnoreExternalFileAttributes then
          begin
            If Length(ExtractFileName(AnsiReplaceStr(AnsiToStr(FileName),DART_ZIP_PathDelim,PathDelim))) > 0 then
              BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_ARCHIVE
            else
              BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
          end;
        If fProcessingSettings.CentralDirectory.IgnoreLocalHeaderOffset then
          BinPart.RelativeOffsetOfLocalHeader := 0;
      end;  
  end;

begin
DoProgress(fProcessingProgNode,PSIDX_Z_CDHeadersLoading,0.0);
If fProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset or
  fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  begin
    // find where the central directory starts
    WorkingOffset := FindSignature(DART_ZIP_CentralDirectoryFileHeaderSignature,DART_PROGSTAGE_INFO_NoProgress);
    If WorkingOffset >= 0 then
      fArchiveStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset := UInt32(WorkingOffset)
    else
      DoError(DART_METHOD_ID_ZIP_ZLCD,'Start of central directory not found in the input stream.');
  end
else WorkingOffset := Int64(fArchiveStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset);
If fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries or
  fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  begin
    // number of entries is not known, have to search for them
    repeat
      If fArchiveStructure.Entries.Count >= Length(fArchiveStructure.Entries.Arr) then
        SetLength(fArchiveStructure.Entries.Arr,Length(fArchiveStructure.Entries.Arr) + 1024);
      fInputArchiveStream.Seek(WorkingOffset,soBeginning);
      DoProgress(fProcessingProgNode,PSIDX_Z_CDHeadersLoading,
       (WorkingOffset - fArchiveStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset) /
       (fInputArchiveStream.Size - fArchiveStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset));
      LoadCentralDirectoryHeader(fArchiveStructure.Entries.Count);
      WorkingOffset := FindSignature(DART_ZIP_CentralDirectoryFileHeaderSignature,DART_PROGSTAGE_INFO_NoProgress,
                                     fInputArchiveStream.Position,False);
      Inc(fArchiveStructure.Entries.Count);
    until WorkingOffset < 0;
    fArchiveStructure.EndOfCentralDirectory.BinPart.EntriesOnDisk := fArchiveStructure.Entries.Count;
    If fProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit then
      fArchiveStructure.EndOfCentralDirectory.BinPart.Entries := fArchiveStructure.Entries.Count;
  end
else
  begin
    // number of entries is known
    fInputArchiveStream.Seek(WorkingOffset,soBeginning);
    For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
      begin
        LoadCentralDirectoryHeader(i);
        DoProgress(fProcessingProgNode,PSIDX_Z_CDHeadersLoading,(i + 1) / fArchiveStructure.Entries.Count);
      end;
  end;
DoProgress(fProcessingProgNode,PSIDX_Z_CDHeadersLoading,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.ZIP_LoadLocalHeaders;
var
  WorkingOffset:  Int64;
  i:              Integer;

  // load single header to an entry at index position
  procedure LoadLocalHeader(Index: Integer);
  var
    DescriptorOffset:  Int64;
  begin
    with fArchiveStructure.Entries.Arr[Index],fArchiveStructure.Entries.Arr[Index].LocalHeader do
      begin
        fInputArchiveStream.ReadBuffer(BinPart,SizeOf(TDART_ZIP_LocalFileHeaderRecord));
        // binary part checks
        If fProcessingSettings.LocalHeader.IgnoreSignature then
          BinPart.Signature := DART_ZIP_LocalFileHeaderSignature
        else
          If BinPart.Signature <> DART_ZIP_LocalFileHeaderSignature then
            DoError(DART_METHOD_ID_ZIP_ZLLHH,'Bad local header signature (0x%.8x) for entry #%d.',[BinPart.Signature,Index]);
        If fProcessingSettings.LocalHeader.IgnoreVersions then
          begin
            BinPart.VersionNeededToExtract := DART_ZIP_DefVersionMadeBy;
            BinPart.OSNeededForExtraction := DART_ZIP_DefHostOS;
          end;
        If fProcessingSettings.LocalHeader.ClearEncryptionFlags then
          BinPart.GeneralPurposeBitFlag := BinPart.GeneralPurposeBitFlag and not Word(DART_ZBF_Encrypted or DART_ZBF_StrongEncryption);
        If not fProcessingSettings.LocalHeader.IgnoreCompressionMethod then
          begin
            If fProcessingSettings.AssumeCompressionMethod then
              begin
                If BinPart.CompressionMethod <> DART_ZCM_Store then
                  BinPart.CompressionMethod := DART_ZCM_Deflate;
              end
            else
              begin
                If not (BinPart.CompressionMethod in DART_ZIP_SupportedCompressinMethods) then
                  DoError(DART_METHOD_ID_ZIP_ZLLHH,'Unknown compression method (%d) in local header for entry #%d.',[BinPart.CompressionMethod,Index]);
              end;
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
            fInputArchiveStream.Seek(BinPart.FileNameLength,soCurrent);
            BinPart.FileNameLength := CentralDirectoryHeader.BinPart.FileNameLength;
            FileName := CentralDirectoryHeader.FileName;
          end
        else
          begin
            SetLength(FileName,BinPart.FileNameLength);
            fInputArchiveStream.ReadBuffer(PAnsiChar(FileName)^,BinPart.FileNameLength);
          end;
        // extra field
        If fProcessingSettings.LocalHeader.IgnoreExtraField then
          begin
            fInputArchiveStream.Seek(BinPart.ExtraFieldLength,soCurrent);
            BinPart.ExtraFieldLength := 0;
          end
        else
          begin
            SetLength(ExtraField,BinPart.ExtraFieldLength);
            fInputArchiveStream.ReadBuffer(PAnsiChar(ExtraField)^,BinPart.ExtraFieldLength);
          end;
        fArchiveStructure.Entries.Arr[Index].UtilityData.DataOffset := fInputArchiveStream.Position;
        // read data descriptor if present
        If ((BinPart.GeneralPurposeBitFlag and DART_ZBF_DataDescriptor) <> 0) and
          not fProcessingSettings.LocalHeader.IgnoreDataDescriptor then
          begin
            // read data descriptor
            If fProcessingSettings.LocalHeader.IgnoreSizes then
              DescriptorOffset := FindSignature(DART_ZIP_DataDescriptorSignature,DART_PROGSTAGE_INFO_NoProgress,
                                                fInputArchiveStream.Position,False)
            else
              DescriptorOffset := fInputArchiveStream.Position + BinPart.CompressedSize;
            If (DescriptorOffset > 0) and ((DescriptorOffset + SizeOf(TDART_ZIP_DataDescriptorRecord)) <= fInputArchiveStream.Size) then
              begin
                fInputArchiveStream.Seek(DescriptorOffset,soBeginning);
                fInputArchiveStream.ReadBuffer(DataDescriptor,SizeOf(TDART_ZIP_DataDescriptorRecord));
                If fProcessingSettings.LocalHeader.IgnoreSignature then
                  DataDescriptor.Signature := DART_ZIP_DataDescriptorSignature
                else
                  begin
                    If DataDescriptor.Signature <> DART_ZIP_DataDescriptorSignature then
                      DoError(DART_METHOD_ID_ZIP_ZLLHH,'Bad data descriptor signature (0x%.8x) for entry #%d.',[DataDescriptor.Signature,Index])
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
            else DoError(DART_METHOD_ID_ZIP_ZLLHH,'Data descriptor was not found (%d).',[DescriptorOffset]);
          end
        else
          begin
            // do not read data descriptor
            BinPart.GeneralPurposeBitFlag := BinPart.GeneralPurposeBitFlag and not DART_ZBF_DataDescriptor;
            CentralDirectoryHeader.BinPart.GeneralPurposeBitFlag :=
              CentralDirectoryHeader.BinPart.GeneralPurposeBitFlag and not DART_ZBF_DataDescriptor;
          end;
        // following is deffered here for cases where sizes are loaded from data decriptor  
        If not fProcessingSettings.LocalHeader.IgnoreSizes and fProcessingSettings.LocalHeader.IgnoreCompressionMethod then
            begin
              If BinPart.CompressedSize <> BinPart.UncompressedSize then
                begin
                  If BinPart.CompressedSize = 0 then
                    begin
                      BinPart.CompressionMethod := DART_ZCM_Store;
                      BinPart.UncompressedSize := BinPart.CompressedSize
                    end
                  else BinPart.CompressionMethod := DART_ZCM_Deflate;
                end
              else BinPart.CompressionMethod := DART_ZCM_Store
            end;
      end;
  end;

begin
DoProgress(fProcessingProgNode,PSIDX_Z_LocalHeadersLoading,0.0);
If fArchiveStructure.Entries.Count > 0 then
  begin
    // entries are already prepared from central directory
    fInputArchiveStream.Seek(0,soBeginning);
    For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
      with fArchiveStructure.Entries.Arr[i].CentralDirectoryHeader do
        begin
          If fProcessingSettings.CentralDirectory.IgnoreLocalHeaderOffset or
            fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
            begin
              // position of local header for given CD entry is not known,
              // search for next local header from current position
              WorkingOffset := FindSignature(DART_ZIP_LocalFileHeaderSignature,DART_PROGSTAGE_INFO_NoProgress,
                                             fInputArchiveStream.Position,False);
              If WorkingOffset >= 0 then
                begin
                  BinPart.RelativeOffsetOfLocalHeader := UInt32(WorkingOffset);
                  fInputArchiveStream.Seek(WorkingOffset,soBeginning);
                end
              else DoError(DART_METHOD_ID_ZIP_ZLLH,'No local header found for entry #%d.',[i]);
            end
          else fInputArchiveStream.Seek(BinPart.RelativeOffsetOfLocalHeader,soBeginning);
          LoadLocalHeader(i);
          DoProgress(fProcessingProgNode,PSIDX_Z_LocalHeadersLoading,(i + 1) / fArchiveStructure.Entries.Count);
          If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory and
             not AnsiSameText(AnsiToStr(FileName),AnsiToStr(fArchiveStructure.Entries.Arr[i].LocalHeader.FileName)) then
            DoError(DART_METHOD_ID_ZIP_ZLLH,'Mismatch in local and central directory file name for entry #%d ("%s").',[i,AnsiToStr(FileName)]);
        end;
  end
else
  begin
    // no entries are loaded yet, search for local headers and load them
    WorkingOffset := FindSignature(DART_ZIP_LocalFileHeaderSignature,DART_PROGSTAGE_INFO_NoProgress);
    If WorkingOffset >= 0 then
      repeat
        If fArchiveStructure.Entries.Count >= Length(fArchiveStructure.Entries.Arr) then
          SetLength(fArchiveStructure.Entries.Arr,Length(fArchiveStructure.Entries.Arr) + 1024);
        fArchiveStructure.Entries.Arr[fArchiveStructure.Entries.Count].CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader := WorkingOffset;
        fInputArchiveStream.Seek(WorkingOffset,soBeginning);
        DoProgress(fProcessingProgNode,PSIDX_Z_LocalHeadersLoading,WorkingOffset / fInputArchiveStream.Size);
        LoadLocalHeader(fArchiveStructure.Entries.Count);
        WorkingOffset := FindSignature(DART_ZIP_LocalFileHeaderSignature,DART_PROGSTAGE_INFO_NoProgress,
                                       fInputArchiveStream.Position,False);
        Inc(fArchiveStructure.Entries.Count);
      until WorkingOffset < 0;
  end;
DoProgress(fProcessingProgNode,PSIDX_Z_LocalHeadersLoading,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.ZIP_ReconstructLocalHeaders;
var
  i:  Integer;
begin
If fProcessingSettings.LocalHeader.IgnoreLocalHeaders then
  begin
  {
    local headers were not loaded, this means central directory must have been
    loaded, do direct copy of data from cetral directory to local headers
  }
    For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
      with fArchiveStructure.Entries.Arr[i] do
        begin
          LocalHeader.BinPart.Signature := DART_ZIP_LocalFileHeaderSignature;
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
          DataDescriptor.Signature := DART_ZIP_DataDescriptorSignature;
          DataDescriptor.CRC32 := CentralDirectoryHeader.BinPart.CRC32;
          DataDescriptor.CompressedSize := CentralDirectoryHeader.BinPart.CompressedSize;
          DataDescriptor.UncompressedSize := CentralDirectoryHeader.BinPart.UncompressedSize;
          UtilityData.DataOffset := Int64(CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader) +
                                    SizeOf(TDART_ZIP_LocalFileHeaderRecord) +
                                    LocalHeader.BinPart.FileNameLength +
                                    LocalHeader.BinPart.ExtraFieldLength;
          DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0);
        end;
  end
else
  begin
    {
      local headers were loaded, but some fields might have been ignored (they
      are set to default values at this point) - if central directory was not
      ignored, copy values from CD fields to corresponding ignored LH fields,
      but only when they have not been ignored in CD too
    }
    If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
      For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
        with fArchiveStructure.Entries.Arr[i] do
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
            {
              if file name is ignored in LH, it was already set from CD in
              loading (CD cannot be ignored when file name in LH is ignored)

              if extra field is ignored, it is dicarded and not copied from CD
            }
            DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0);
          end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.ZIP_ReconstructCentralDirectoryHeaders;
var
  i:  Integer;
begin
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  with fArchiveStructure.Entries.Arr[i] do
    begin
      {
        If central directory was ignored, it must be initialized and available
        data copied fron local headers (fields not ignored in LH) or
        initialized to default values

        Also, value of fields ignored in CD and not ignored in LH are copied
        from LH

        note - if CD is ignored as a whore, LH cannot be
      }
      If fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or
        (fProcessingSettings.CentralDirectory.IgnoreVersions and not fProcessingSettings.LocalHeader.IgnoreVersions) then
        begin
          CentralDirectoryHeader.BinPart.VersionMadeBy := LocalHeader.BinPart.VersionNeededToExtract;
          CentralDirectoryHeader.BinPart.HostOS := LocalHeader.BinPart.OSNeededForExtraction;
          CentralDirectoryHeader.BinPart.VersionNeededToExtract := LocalHeader.BinPart.VersionNeededToExtract;
          CentralDirectoryHeader.BinPart.OSNeededForExtraction := LocalHeader.BinPart.OSNeededForExtraction;
        end;
      If fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
        CentralDirectoryHeader.BinPart.GeneralPurposeBitFlag := LocalHeader.BinPart.GeneralPurposeBitFlag;
      If fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or
        (fProcessingSettings.CentralDirectory.IgnoreCompressionMethod and not fProcessingSettings.LocalHeader.IgnoreCompressionMethod) then
        CentralDirectoryHeader.BinPart.CompressionMethod := LocalHeader.BinPart.CompressionMethod;
      If fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or
        (fProcessingSettings.CentralDirectory.IgnoreModTime and not fProcessingSettings.LocalHeader.IgnoreModTime) then
        CentralDirectoryHeader.BinPart.LastModFileTime := LocalHeader.BinPart.LastModFileTime;
      If fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or
        (fProcessingSettings.CentralDirectory.IgnoreModDate and not fProcessingSettings.LocalHeader.IgnoreModDate) then
        CentralDirectoryHeader.BinPart.LastModFileDate := LocalHeader.BinPart.LastModFileDate;
      If fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or
        (fProcessingSettings.CentralDirectory.IgnoreCRC32 and not fProcessingSettings.LocalHeader.IgnoreCRC32) then
        CentralDirectoryHeader.BinPart.CRC32 := LocalHeader.BinPart.CRC32;
      If fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or
        (fProcessingSettings.CentralDirectory.IgnoreSizes and not fProcessingSettings.LocalHeader.IgnoreSizes) then
        begin
          CentralDirectoryHeader.BinPart.CompressedSize := LocalHeader.BinPart.CompressedSize;
          CentralDirectoryHeader.BinPart.UncompressedSize := LocalHeader.BinPart.UncompressedSize;
        end;
      // fields that cannot be copied from LH (they are simply not there) or that cannot be ignored
      If fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
        begin
          CentralDirectoryHeader.BinPart.Signature := DART_ZIP_CentralDirectoryFileHeaderSignature;
          CentralDirectoryHeader.BinPart.DiskNumberStart := 0;
          CentralDirectoryHeader.BinPart.InternalFileAttributes := 0;
          If Length(ExtractFileName(AnsiReplaceStr(AnsiToStr(LocalHeader.FileName),DART_ZIP_PathDelim,PathDelim))) > 0 then
            CentralDirectoryHeader.BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_ARCHIVE
          else
            CentralDirectoryHeader.BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
          // if CD was ignored, file name in LH could not have been ignored, copy it  
          CentralDirectoryHeader.BinPart.FileNameLength := LocalHeader.BinPart.FileNameLength;
          CentralDirectoryHeader.FileName := LocalHeader.FileName;
          CentralDirectoryHeader.BinPart.ExtraFieldLength := 0;
          CentralDirectoryHeader.ExtraField := '';
          CentralDirectoryHeader.BinPart.FileCommentLength := 0;
          CentralDirectoryHeader.FileComment := '';
        end;
    end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.ZIP_ReconstructEndOfCentralDirectory;
var
  i:  Integer;
begin
with fArchiveStructure.EndOfCentralDirectory do
  begin
    If fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
      begin
        BinPart.Signature := DART_ZIP_EndOfCentralDirectorySignature;
        BinPart.NumberOfThisDisk := 0;
        BinPart.CentralDirectoryStartDiskNumber := 0;
        BinPart.CommentLength := 0;
        Comment := '';
      end;
    If fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory or
       fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries then
      begin
        BinPart.EntriesOnDisk := fArchiveStructure.Entries.Count;
        BinPart.Entries := fArchiveStructure.Entries.Count;
      end;
    BinPart.CentralDirectorySize := 0;
    For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
      begin
        Inc(BinPart.CentralDirectorySize,SizeOf(TDART_ZIP_CentralDirectoryFileHeaderRecord));
        Inc(BinPart.CentralDirectorySize,fArchiveStructure.Entries.Arr[i].CentralDirectoryHeader.BinPart.FileNameLength);
        Inc(BinPart.CentralDirectorySize,fArchiveStructure.Entries.Arr[i].CentralDirectoryHeader.BinPart.ExtraFieldLength);
        Inc(BinPart.CentralDirectorySize,fArchiveStructure.Entries.Arr[i].CentralDirectoryHeader.BinPart.FileCommentLength);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.ZIP_ReconstructFinal;
var
  i:  Integer;
begin
// finalize reconstruction
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  with fArchiveStructure.Entries.Arr[i] do
    begin
      If not fProcessingSettings.LocalHeader.IgnoreLocalHeaders and
        not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
        begin
          // if both compression methods are ignored, we have to set them to one that was obtained from sizes
          If fProcessingSettings.LocalHeader.IgnoreCompressionMethod and
            fProcessingSettings.CentralDirectory.IgnoreCompressionMethod then
            begin
              If fProcessingSettings.LocalHeader.IgnoreSizes then
                LocalHeader.BinPart.CompressionMethod := CentralDirectoryHeader.BinPart.CompressionMethod
              else
                CentralDirectoryHeader.BinPart.CompressionMethod := LocalHeader.BinPart.CompressionMethod;
            end;
        end;
      // set whether CRC32 needs to be recalculated in further processing
      fArchiveStructure.Entries.Arr[i].UtilityData.NeedsCRC32 :=
        (fProcessingSettings.LocalHeader.IgnoreLocalHeaders or fProcessingSettings.LocalHeader.IgnoreCRC32) and
        (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or fProcessingSettings.CentralDirectory.IgnoreCRC32);
      // set whether sizes needs to be obtained in further processing
      fArchiveStructure.Entries.Arr[i].UtilityData.NeedsSizes :=
        (fProcessingSettings.LocalHeader.IgnoreLocalHeaders or fProcessingSettings.LocalHeader.IgnoreSizes) and
        (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or fProcessingSettings.CentralDirectory.IgnoreSizes);
      // store to known paths
      AddKnownPath(CentralDirectoryHeader.FileName,(CentralDirectoryHeader.BinPart.ExternalFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0);
      DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0);      
    end;
// in case any directory is not explicitly stored, deconstruct all paths and add them as known
with TDARTPathDeconstructor.Create(DART_ZIP_PathDelim) do
try
  For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
    DeconstructPath(fArchiveStructure.Entries.Arr[i].CentralDirectoryHeader.FileName);
  DoProgress(DART_PROGSTAGE_IDX_NoProgress,0.0);
  For i := 0 to Pred(Count) do
    If Length(Nodes[i].FullPath) > 0 then
      AddKnownPath(Nodes[i].FullPath,True);
finally
  Free
end;
end;

{-------------------------------------------------------------------------------
    TDARTRepairer_ZIP - public methods
-------------------------------------------------------------------------------}

class Function TDARTRepairer_ZIP.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  DART_METHOD_ID_ZIP_ARCHPROC:  Result := 'ArchiveProcessing';
  DART_METHOD_ID_ZIP_GETENTRY:  Result := 'GetEntryData(Index)';
  DART_METHOD_ID_ZIP_ZLEOCD:    Result := 'ZIP_LoadEndOfCentralDirectory';
  DART_METHOD_ID_ZIP_ZLCD:      Result := 'ZIP_LoadCentralDirectory';
  DART_METHOD_ID_ZIP_ZLCDH:     Result := 'ZIP_LoadCentralDirectory.LoadCentralDirectoryHeader';
  DART_METHOD_ID_ZIP_ZLLH:      Result := 'ZIP_LoadLocalHeaders';
  DART_METHOD_ID_ZIP_ZLLHH:     Result := 'ZIP_LoadLocalHeaders.LoadLocalHeader';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

//------------------------------------------------------------------------------

constructor TDARTRepairer_ZIP.Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings; CatchExceptions: Boolean);
begin
inherited Create(PauseControlObject,ArchiveProcessingSettings,CatchExceptions);
fExpectedSignature := DART_ZIP_ArchiveSignature;
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_ZIP.GetAllKnownPaths(var KnownPaths: TDARTKnownPaths): Integer;
var
  i:  Integer;
begin
If (KnownPaths.Count + fArchiveStructure.KnownPaths.Count) > Length(KnownPaths.Arr) then
  SetLength(KnownPaths.Arr,KnownPaths.Count + fArchiveStructure.KnownPaths.Count);
Result := 0;
For i := Low(fArchiveStructure.KnownPaths.Arr) to Pred(fArchiveStructure.KnownPaths.Count) do
  If IndexOfKnownPath(fArchiveStructure.KnownPaths.Arr[i].Path,KnownPaths) < 0 then
    begin
      KnownPaths.Arr[KnownPaths.Count] := fArchiveStructure.KnownPaths.Arr[i];
      Inc(Result);
      Inc(KnownPaths.Count);
    end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                        TDARTRepairer_ZIP_ProcessingBase
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_ZIP_ProcessingBase - method indexing constants
===============================================================================}

const
  DART_METHOD_ID_ZIP_PROC_ARCHPROC = 1100;

{===============================================================================
    TDARTRepairer_ZIP_ProcessingBase - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTRepairer_ZIP_ProcessingBase - protected methods
-------------------------------------------------------------------------------}

procedure TDARTRepairer_ZIP_ProcessingBase.ArchiveProcessing;
begin
// check if target <> source
If AnsiSameText(fArchiveProcessingSettings.Common.ArchivePath,fArchiveProcessingSettings.Common.TargetPath) then
  DoError(DART_METHOD_ID_ZIP_PROC_ARCHPROC,'Output is directed into an input archive, cannot proceed.');
inherited;
ZIP_PrepareEntriesProgress;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_ProcessingBase.ZIP_PrepareEntriesProgress;
var
  i,Index:  Integer;
  CurrNode: TProgressTracker;
begin
DoProgress(fProcessingProgNode,PSIDX_Z_EntriesProgressPrep,0.0);
fEntriesProcessingProgNode.BeginUpdate;
try
  For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
    with fArchiveStructure.Entries.Arr[i] do
      begin
        If CentralDirectoryHeader.BinPart.CompressedSize <> 0 then
          Index := fEntriesProcessingProgNode.Add(CentralDirectoryHeader.BinPart.CompressedSize)
        else
          Index := fEntriesProcessingProgNode.Add(1);
        CurrNode := fEntriesProcessingProgNode.StageObjects[Index];
        CurrNode.BeginUpdate;
        try
          If UtilityData.NeedsCRC32 or UtilityData.NeedsSizes then
            begin
              If LocalHeader.BinPart.CompressionMethod = DART_ZCM_Store then
                begin
                  // no decompression
                  DART_PROGSTAGE_IDX_ZIP_EntryLoading       := CurrNode.Add(40);
                  DART_PROGSTAGE_IDX_ZIP_EntryDecompression := CurrNode.Add(20);
                  DART_PROGSTAGE_IDX_ZIP_EntrySaving        := CurrNode.Add(40);
                end
              else
                begin
                  // decompression is needed
                  DART_PROGSTAGE_IDX_ZIP_EntryLoading       := CurrNode.Add(30);
                  DART_PROGSTAGE_IDX_ZIP_EntryDecompression := CurrNode.Add(40);
                  DART_PROGSTAGE_IDX_ZIP_EntrySaving        := CurrNode.Add(30);
                end;
            end
          else
            begin
              // no decompression or other calculation
              DART_PROGSTAGE_IDX_ZIP_EntryLoading       := CurrNode.Add(50);
              DART_PROGSTAGE_IDX_ZIP_EntryDecompression := CurrNode.Add(0);
              DART_PROGSTAGE_IDX_ZIP_EntrySaving        := CurrNode.Add(50);
            end;
          // assign obtained indices to shorter named-variables
          PSIDX_Z_EntryProcessing    := DART_PROGSTAGE_IDX_ZIP_EntryProcessing;
          PSIDX_Z_EntryLoading       := DART_PROGSTAGE_IDX_ZIP_EntryLoading;
          PSIDX_Z_EntryDecompression := DART_PROGSTAGE_IDX_ZIP_EntryDecompression;
          PSIDX_Z_EntrySaving        := DART_PROGSTAGE_IDX_ZIP_EntrySaving;
        finally
          CurrNode.EndUpdate;
        end;
        DoProgress(fProcessingProgNode,PSIDX_Z_EntriesProgressPrep,(i + 1) / fArchiveStructure.Entries.Count);
      end;
finally
  fEntriesProcessingProgNode.EndUpdate;
end;
DoProgress(fProcessingProgNode,PSIDX_Z_EntriesProgressPrep,1.0);
end;

{-------------------------------------------------------------------------------
    TDARTRepairer_ZIP_ProcessingBase - public methods
-------------------------------------------------------------------------------}

class Function TDARTRepairer_ZIP_ProcessingBase.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  DART_METHOD_ID_ZIP_PROC_ARCHPROC: Result := 'ArchiveProcessing';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

end.
