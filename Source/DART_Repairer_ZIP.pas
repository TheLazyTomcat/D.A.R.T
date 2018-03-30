unit DART_Repairer_ZIP;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes,
  DART_Common, DART_ProcessingSettings, DART_Format_ZIP, DART_Repairer;

const
  // progress stages
  DART_PROGSTAGE_ID_ZIP_EOCDLoading         = DART_PROGSTAGE_ID_MAX + 1;
  DART_PROGSTAGE_ID_ZIP_CDHeadersLoading    = DART_PROGSTAGE_ID_MAX + 2;
  DART_PROGSTAGE_ID_ZIP_LocalHeadersLoading = DART_PROGSTAGE_ID_MAX + 3;
  DART_PROGSTAGE_ID_ZIP_EntriesProcessing   = DART_PROGSTAGE_ID_MAX + 4;
  DART_PROGSTAGE_ID_ZIP_EntryProcessing     = DART_PROGSTAGE_ID_MAX + 5;
  DART_PROGSTAGE_ID_ZIP_EntryLoading        = DART_PROGSTAGE_ID_MAX + 6;
  DART_PROGSTAGE_ID_ZIP_EntryDecompression  = DART_PROGSTAGE_ID_MAX + 7;
  DART_PROGSTAGE_ID_ZIP_EntrySaving         = DART_PROGSTAGE_ID_MAX + 8;
  DART_PROGSTAGE_ID_ZIP_Max                 = DART_PROGSTAGE_ID_MAX + 99;

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
    Function GetEntryData(const EntryFileName: AnsiString; out Data: Pointer; out Size: TMemSize): Boolean; override;
    // processing methods
    procedure ArchiveProcessing; override;
    // zip specific routines
    procedure ZIP_LoadEndOfCentralDirectory; virtual;
    procedure ZIP_LoadCentralDirectory; virtual;
    procedure ZIP_LoadLocalHeaders; virtual;
    procedure ZIP_ReconstructLocalHeaders; virtual;
    procedure ZIP_ReconstructCentralDirectoryHeaders; virtual;
    procedure ZIP_ReconstructEndOfCentralDirectory; virtual;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
    constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
    property ArchiveStructure: TDART_ZIP_ArchiveStructure read fArchiveStructure;
  end;

implementation

uses
  Windows, SysUtils, Classes, StrUtils,
  StrRect, MemoryBuffer, ZLibCommon;

const
  DART_METHOD_ID_ZIP_ARCHPROC = 0100;
  DART_METHOD_ID_ZIP_GETENTRY = 0101;
  DART_METHOD_ID_ZIP_ZLEOCD   = 0110;
  DART_METHOD_ID_ZIP_ZLCD     = 0111;
  DART_METHOD_ID_ZIP_ZLCDH    = 0112;
  DART_METHOD_ID_ZIP_ZLLH     = 0113;
  DART_METHOD_ID_ZIP_ZLLHH    = 0114;

procedure TDARTRepairer_ZIP.InitializeProcessingSettings;
begin
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
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.InitializeProgress;
begin
{$message 'implement'}
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_ZIP.IndexOfEntry(const EntryFileName: AnsiString): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  If AnsiSameText(EntryFileName,fArchiveStructure.Entries.Arr[i].CentralDirectoryHeader.FileName) then
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
        for ZIP archive, occurs only when processing it as help file - therefore we will assume
        the archive is not corrupted and stored compressed size and compression method are
        both correct and do not need to be rectified.
        If archive is corrupted, it should first be repaired and only then used as a help file.
      }
        // prepare buffer for entry data
        ReallocBufferKeep(fBuffer_Entry,LocalHeader.BinPart.CompressedSize);
        // load entry data
        fInputArchiveStream.Seek(UtilityData.DataOffset,soBeginning);
        ProgressedStreamRead(fInputArchiveStream,fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,DART_PROGSTAGE_ID_NoProgress);
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
                                       Data,Size,WBITS_RAW,DART_PROGSTAGE_ID_NoProgress);
          end;
        // if we are here, everything should be fine
        Result := True;        
      end;
  end
else DoError(DART_METHOD_ID_ZIP_GETENTRY,'Entry index (%d) out of bounds.',[EntryIndex]);
end;

//------------------------------------------------------------------------------

Function TDARTRepairer_ZIP.GetEntryData(const EntryFileName: AnsiString; out Data: Pointer; out Size: TMemSize): Boolean;
var
  Index:  Integer;
begin
Index := IndexOfEntry(EntryFileName);
If Index >= 0 then
  Result := GetEntryData(Index,Data,Size)
else
  Result := False;
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
ZIP_LoadLocalHeaders;
If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
  ZIP_ReconstructLocalHeaders;
ZIP_ReconstructCentralDirectoryHeaders;
ZIP_ReconstructEndOfCentralDirectory;
If fArchiveStructure.Entries.Count <= 0 then
  DoError(DART_METHOD_ID_ZIP_ARCHPROC,'Input file does not contain any valid entries.');
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.ZIP_LoadEndOfCentralDirectory;
var
  EOCDPosition: Int64;
begin
DoProgress(DART_PROGSTAGE_ID_ZIP_EOCDLoading,0.0);
EOCDPosition := FindSignature(DART_ZIP_EndOfCentralDirectorySignature,-1,True,fProcessingSettings.EndOfCentralDirectory.LimitSearch);
DoProgress(DART_PROGSTAGE_ID_ZIP_EOCDLoading,0.5);
If EOCDPosition >= 0 then
  begin
    fInputArchiveStream.Seek(EOCDPosition,soBeginning);
    If (fInputArchiveStream.Size - fInputArchiveStream.Position) >= SizeOf(TDART_ZIP_EndOfCentralDirectoryRecord) then
      begin
        with fArchiveStructure.EndOfCentralDirectory do
          begin
            fInputArchiveStream.ReadBuffer(BinPart,SizeOf(TDART_ZIP_EndOfCentralDirectoryRecord));
            DoProgress(DART_PROGSTAGE_ID_ZIP_EOCDLoading,0.9);
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
DoProgress(DART_PROGSTAGE_ID_ZIP_EOCDLoading,1.0);
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
        If fProcessingSettings.CentralDirectory.IgnoreCompressionMethod then
          begin
            If BinPart.CompressedSize = BinPart.UncompressedSize then
              BinPart.CompressionMethod := DART_ZCM_Store
            else
              BinPart.CompressionMethod := DART_ZCM_Deflate;
          end
        else
          begin
            If not (BinPart.CompressionMethod in DART_ZIP_SupportedCompressinMethods) and not fProcessingSettings.AssumeCompressionMethods then
              DoError(DART_METHOD_ID_ZIP_ZLCDH,'Unknown compression method (%d) in central directory header for entry #%d.',[BinPart.CompressionMethod,Index]);
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
        fInputArchiveStream.ReadBuffer(PAnsiChar(FileName)^,BinPart.FileNameLength);
        // file attributes must be done here, not sooner, because file name is required
        If fProcessingSettings.CentralDirectory.IgnoreInternalFileAttributes then
          BinPart.InternalFileAttributes := 0;
        If fProcessingSettings.CentralDirectory.IgnoreExternalFileAttributes then
          begin
            If ExtractFileName(AnsiReplaceStr(FileName,DART_ZIP_PathDelim,PathDelim)) <> '' then
              BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_ARCHIVE
            else
              BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
          end;
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
    end;  
  end;

begin
DoProgress(DART_PROGSTAGE_ID_ZIP_CDHeadersLoading,0.0);
If fProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset or
  fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  begin
    // find where the central directory starts
    WorkingOffset := FindSignature(DART_ZIP_CentralDirectoryFileHeaderSignature);
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
      DoProgress(DART_PROGSTAGE_ID_ZIP_CDHeadersLoading,
       (WorkingOffset - fArchiveStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset) /
       (fInputArchiveStream.Size - fArchiveStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset));
      LoadCentralDirectoryHeader(fArchiveStructure.Entries.Count);
      WorkingOffset := FindSignature(DART_ZIP_CentralDirectoryFileHeaderSignature,fInputArchiveStream.Position,False);
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
        DoProgress(DART_PROGSTAGE_ID_ZIP_CDHeadersLoading,(i + 1) / fArchiveStructure.Entries.Count);
      end;
  end;
DoProgress(DART_PROGSTAGE_ID_ZIP_CDHeadersLoading,1.0);
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
        If fProcessingSettings.LocalHeader.IgnoreCompressionMethod then
          begin
            If BinPart.CompressedSize = BinPart.UncompressedSize then
              BinPart.CompressionMethod := DART_ZCM_Store
            else
              BinPart.CompressionMethod := DART_ZCM_Deflate;
          end
        else  
          begin
            If not (BinPart.CompressionMethod in DART_ZIP_SupportedCompressinMethods) and not fProcessingSettings.AssumeCompressionMethods then
              DoError(DART_METHOD_ID_ZIP_ZLLHH,'Unknown compression method (%d) in local header for entry #%d.',[BinPart.CompressionMethod,Index]);
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
            If fProcessingSettings.LocalHeader.IgnoreSizes then
              DescriptorOffset := FindSignature(DART_ZIP_DataDescriptorSignature,fInputArchiveStream.Position,False)
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
            BinPart.GeneralPurposeBitFlag := BinPart.GeneralPurposeBitFlag and not DART_ZBF_DataDescriptor;
            CentralDirectoryHeader.BinPart.GeneralPurposeBitFlag := CentralDirectoryHeader.BinPart.GeneralPurposeBitFlag and not
                                                                    DART_ZBF_DataDescriptor;
          end;
      end;
  end;

  // copy selected data from central directory to local file headers
  procedure CopyCentralToLocal(Index: Integer);
  begin
    with fArchiveStructure.Entries.Arr[Index] do
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
        UtilityData.DataOffset := Int64(CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader) +
                                  SizeOf(TDART_ZIP_LocalFileHeaderRecord) +
                                  LocalHeader.BinPart.FileNameLength +
                                  LocalHeader.BinPart.ExtraFieldLength;
      end;
  end;

begin
DoProgress(DART_PROGSTAGE_ID_ZIP_LocalHeadersLoading,0.0);
If fProcessingSettings.LocalHeader.IgnoreLocalHeaders then
  begin
    // local headers are not loaded from the file, they are instead constructed from data
    // stored in central directory
    For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
      CopyCentralToLocal(i);
  end
else
  begin
    // load local headers from the archive file
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
                  WorkingOffset := FindSignature(DART_ZIP_LocalFileHeaderSignature,fInputArchiveStream.Position,False);
                  If WorkingOffset >= 0 then
                    begin
                      BinPart.RelativeOffsetOfLocalHeader := UInt32(WorkingOffset);
                      fInputArchiveStream.Seek(WorkingOffset,soBeginning);
                    end
                  else DoError(DART_METHOD_ID_ZIP_ZLLH,'No local header found for entry #%d.',[i]);
                end
              else fInputArchiveStream.Seek(BinPart.RelativeOffsetOfLocalHeader,soBeginning);
              LoadLocalHeader(i);
              DoProgress(DART_PROGSTAGE_ID_ZIP_LocalHeadersLoading,(i + 1) / fArchiveStructure.Entries.Count);
              If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
                If not AnsiSameText(FileName,fArchiveStructure.Entries.Arr[i].LocalHeader.FileName) then
                  DoError(DART_METHOD_ID_ZIP_ZLLH,'Mismatch in local and central directory file name for entry #%d (%s; %s).',
                    [i,AnsiToStr(FileName),AnsiToStr(fArchiveStructure.Entries.Arr[i].LocalHeader.FileName)]);
            end;
      end
    else
      begin
        // no entries are loaded yet, search for local headers and load them
        WorkingOffset := FindSignature(DART_ZIP_LocalFileHeaderSignature);
        If WorkingOffset >= 0 then
          repeat
            If fArchiveStructure.Entries.Count > Length(fArchiveStructure.Entries.Arr) then
              SetLength(fArchiveStructure.Entries.Arr,Length(fArchiveStructure.Entries.Arr) + 1024);
            fArchiveStructure.Entries.Arr[fArchiveStructure.Entries.Count].CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader := WorkingOffset;
            fInputArchiveStream.Seek(WorkingOffset,soBeginning);
            DoProgress(DART_PROGSTAGE_ID_ZIP_LocalHeadersLoading,WorkingOffset / fInputArchiveStream.Size);
            LoadLocalHeader(fArchiveStructure.Entries.Count);
            WorkingOffset := FindSignature(DART_ZIP_LocalFileHeaderSignature,fInputArchiveStream.Position,False);
            Inc(fArchiveStructure.Entries.Count);
          until WorkingOffset < 0;
      end;
  end;
DoProgress(DART_PROGSTAGE_ID_ZIP_LocalHeadersLoading,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP.ZIP_ReconstructLocalHeaders;
var
  i:  Integer;
begin
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
      DoProgress(DART_PROGSTAGE_ID_NoProgress,0.0);
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
          CentralDirectoryHeader.BinPart.Signature := DART_ZIP_CentralDirectoryFileHeaderSignature;
          CentralDirectoryHeader.BinPart.DiskNumberStart := 0;
          CentralDirectoryHeader.BinPart.InternalFileAttributes := 0;
          If ExtractFileName(AnsiReplaceStr(LocalHeader.FileName,DART_ZIP_PathDelim,PathDelim)) <> '' then
            CentralDirectoryHeader.BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_ARCHIVE
          else
            CentralDirectoryHeader.BinPart.ExternalFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
          CentralDirectoryHeader.BinPart.FileNameLength := LocalHeader.BinPart.FileNameLength;
          CentralDirectoryHeader.FileName := LocalHeader.FileName;
        end;
      fArchiveStructure.Entries.Arr[i].UtilityData.NeedsCRC32 :=
        (fProcessingSettings.LocalHeader.IgnoreLocalHeaders or fProcessingSettings.LocalHeader.IgnoreCRC32) and
        (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or fProcessingSettings.CentralDirectory.IgnoreCRC32);
      fArchiveStructure.Entries.Arr[i].UtilityData.NeedsSizes :=
        (fProcessingSettings.LocalHeader.IgnoreLocalHeaders or fProcessingSettings.LocalHeader.IgnoreSizes) and
        (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or fProcessingSettings.CentralDirectory.IgnoreSizes);
      DoProgress(DART_PROGSTAGE_ID_NoProgress,0.0);  
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
    If fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory or fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries then
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

//==============================================================================

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

constructor TDARTRepairer_ZIP.Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
inherited Create(PauseControlObject,ArchiveProcessingSettings);
fExpectedSignature := DART_ZIP_FileSignature;
end;

end.
