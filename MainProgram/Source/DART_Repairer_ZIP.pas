{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_ZIP;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes, WinSyncObjs,
  DART_ProcessingSettings, DART_Repairer;

{==============================================================================}
{------------------------------------------------------------------------------}
{                         ZIP structures and constants                         }
{------------------------------------------------------------------------------}
{==============================================================================}

//--- ZIP archive signature ----------------------------------------------------

const
  FileSignature_ZIP = UInt32($04034b50);

//--- Bit flags ----------------------------------------------------------------

  ZBF_Encrypted        = UInt16($0001); // bit 0
  ZBF_DataDescriptor   = UInt16($0008); // bit 3
  ZBF_StrongEncryption = UInt16($0040); // bit 6


//--- Signatures ---------------------------------------------------------------

  ZIP_LocalFileHeaderSignature            = UInt32($04034b50);
  ZIP_DataDescriptorSignature             = UInt32($08074b50);
  ZIP_CentralDirectoryFileHeaderSignature = UInt32($02014b50);
  ZIP_EndOfCentralDirectorySignature      = UInt32($06054b50);

type
//--- Local file header --------------------------------------------------------
  TZIP_LocalFileHeaderRecord = packed record
    Signature:              UInt32;
    VersionNeededToExtract: UInt8;
    OSNeededForExtraction:  UInt8;
    GeneralPurposeBitFlag:  UInt16;
    CompressionMethod:      UInt16;
    LastModFileTime:        UInt16;
    LastModFileDate:        UInt16;
    CRC32:                  UInt32;
    CompressedSize:         UInt32;
    UncompressedSize:       UInt32;
    FileNameLength:         UInt16;
    ExtraFieldLength:       UInt16;
  end;

  TZIP_LocalFileHeader = record
    BinPart:    TZIP_LocalFileHeaderRecord;
    FileName:   AnsiString;
    ExtraField: AnsiString;
  end;

//--- Data descriptor record ---------------------------------------------------

  TZIP_DataDescriptorRecord = packed record
    Signature:        UInt32;
    CRC32:            UInt32;
    CompressedSize:   UInt32;
    UncompressedSize: UInt32;
  end;

//--- Central directory file header --------------------------------------------

  TZIP_CentralDirectoryFileHeaderRecord = packed record
    Signature:                    UInt32;
    VersionMadeBy:                UInt8;
    HostOS:                       UInt8;
    VersionNeededToExtract:       UInt8;
    OSNeededForExtraction:        UInt8;
    GeneralPurposeBitFlag:        UInt16;
    CompressionMethod:            UInt16;
    LastModFileTime:              UInt16;
    LastModFileDate:              UInt16;
    CRC32:                        UInt32;
    CompressedSize:               UInt32;
    UncompressedSize:             UInt32;
    FileNameLength:               UInt16;
    ExtraFieldLength:             UInt16;
    FileCommentLength:            UInt16;
    DiskNumberStart:              UInt16;
    InternalFileAttributes:       UInt16;
    ExternalFileAttributes:       UInt32;
    RelativeOffsetOfLocalHeader:  UInt32;
  end;

  TZIP_CentralDirectoryFileHeader = record
    BinPart:      TZIP_CentralDirectoryFileHeaderRecord;
    FileName:     AnsiString;
    ExtraField:   AnsiString;
    FileComment:  AnsiString;
  end;

//--- End of central directory record ------------------------------------------

  TZIP_EndOfCentralDirectoryRecord = packed record
    Signature:                        UInt32;
    NumberOfThisDisk:                 UInt16;
    CentralDirectoryStartDiskNumber:  UInt16;
    EntriesOnDisk:                    UInt16;
    Entries:                          UInt16;
    CentralDirectorySize:             UInt32;
    CentralDirectoryOffset:           UInt32;
    CommentLength:                    UInt16;
  end;

  TZIP_EndOfCentralDirectory = record
    BinPart:  TZIP_EndOfCentralDirectoryRecord;
    Comment:  AnsiString;
  end;

//--- Utility data -------------------------------------------------------------

  TZIP_UtilityData = record
    OriginalLocalHeaderOffset:  UInt32;   // stores offset of local header in input file while processing it into output
    DataOffset:                 Int64;    // offset of actual entry data from the start of archive
    NeedsCRC32:                 Boolean;  // CRC32 has to be recalculated
    NeedsSizes:                 Boolean;  // actual sizes needs to be obtained
    Erroneous:                  Boolean;  // entry is erroneous but the error was ignored
  end;

//--- Main structure -----------------------------------------------------------

  TZIP_Entry = record
    LocalHeader:            TZIP_LocalFileHeader;
    DataDescriptor:         TZIP_DataDescriptorRecord;
    CentralDirectoryHeader: TZIP_CentralDirectoryFileHeader;
    UtilityData:            TZIP_UtilityData;
  end;

  TZIP_ArchiveStructure = record
    Entries:                array of TZIP_Entry;
    EndOfCentralDirectory:  TZIP_EndOfCentralDirectory;
  end;

//==============================================================================

const
  PROCSTAGEIDX_ZIP_EOCDLoading         = PROCSTAGEIDX_Max + 1;
  PROCSTAGEIDX_ZIP_CDHeadersLoading    = PROCSTAGEIDX_Max + 2;
  PROCSTAGEIDX_ZIP_LocalHeadersLoading = PROCSTAGEIDX_Max + 3;
  PROCSTAGEIDX_ZIP_EntriesProcessing   = PROCSTAGEIDX_Max + 4;
  PROCSTAGEIDX_ZIP_EntryProcessing     = PROCSTAGEIDX_Max + 5;
  PROCSTAGEIDX_ZIP_EntryLoading        = PROCSTAGEIDX_Max + 6;
  PROCSTAGEIDX_ZIP_EntryDecompressing  = PROCSTAGEIDX_Max + 7;
  PROCSTAGEIDX_ZIP_EntrySaving         = PROCSTAGEIDX_Max + 8;
  PROCSTAGEIDX_ZIP_Max                 = PROCSTAGEIDX_ZIP_EntrySaving;


type
  TRepairer_ZIP = class(TRepairer)
  protected
    fProcessingSettings:  TZIP_Settings;
    fArchiveStructure:    TZIP_ArchiveStructure;
    procedure ZIP_PrepareEntryProgressInfo(EntryIndex: Integer); virtual;
    procedure ZIP_LoadEndOfCentralDirectory; virtual;
    procedure ZIP_LoadCentralDirectory; virtual;
    procedure ZIP_LoadLocalHeaders; virtual;
    procedure ZIP_ReconstructLocalHeaders; virtual;
    procedure ZIP_ReconstructCentralDirectoryHeaders; virtual;
    procedure ZIP_ReconstructEndOfCentralDirectory; virtual;
    procedure RectifyFileProcessingSettings; override;
    procedure InitializeData; override;
    procedure InitializeProgress; override;
    procedure ArchiveProcessing; override;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
    constructor Create(FlowControlObject: TEvent; FileProcessingSettings: TFileProcessingSettings; CatchExceptions: Boolean = True);
    property ArchiveStructure: TZIP_ArchiveStructure read fArchiveStructure;
  end;

implementation

uses
  Windows, SysUtils, Classes, StrUtils
{$IF Defined(FPC)}
  , LazUTF8
{$IFEND};

procedure TRepairer_ZIP.ZIP_PrepareEntryProgressInfo(EntryIndex: Integer);
begin
with fArchiveStructure.Entries[EntryIndex] do
  begin
    fProgressStages[PROCSTAGEIDX_ZIP_EntryProcessing].Offset := fProgressStages[PROCSTAGEIDX_ZIP_EntriesProcessing].Offset +
      (UtilityData.OriginalLocalHeaderOffset / fArchiveStream.Size) * fProgressStages[PROCSTAGEIDX_ZIP_EntriesProcessing].Range;
    fProgressStages[PROCSTAGEIDX_ZIP_EntryProcessing].Range := fProgressStages[PROCSTAGEIDX_ZIP_EntriesProcessing].Range *
      ((LocalHeader.BinPart.CompressedSize + SizeOf(TZIP_LocalFileHeaderRecord) + LocalHeader.BinPart.FileNameLength +
        LocalHeader.BinPart.ExtraFieldLength + SizeOf(TZIP_CentralDirectoryFileHeaderRecord) + CentralDirectoryHeader.BinPart.FileNameLength +
        CentralDirectoryHeader.BinPart.ExtraFieldLength + CentralDirectoryHeader.BinPart.FileCommentLength) / fArchiveStream.Size);
  end;
fProgressStages[PROCSTAGEIDX_ZIP_EntryLoading].Offset := fProgressStages[PROCSTAGEIDX_ZIP_EntryProcessing].Offset;
fProgressStages[PROCSTAGEIDX_ZIP_EntryLoading].Range := fProgressStages[PROCSTAGEIDX_ZIP_EntryProcessing].Range * 0.4;
fProgressStages[PROCSTAGEIDX_ZIP_EntryDecompressing].Offset := fProgressStages[PROCSTAGEIDX_ZIP_EntryLoading].Offset +
                                                               fProgressStages[PROCSTAGEIDX_ZIP_EntryLoading].Range;
fProgressStages[PROCSTAGEIDX_ZIP_EntryDecompressing].Range := fProgressStages[PROCSTAGEIDX_ZIP_EntryProcessing].Range * 0.2;
fProgressStages[PROCSTAGEIDX_ZIP_EntrySaving].Offset := fProgressStages[PROCSTAGEIDX_ZIP_EntryDecompressing].Offset +
                                                        fProgressStages[PROCSTAGEIDX_ZIP_EntryDecompressing].Range;
fProgressStages[PROCSTAGEIDX_ZIP_EntrySaving].Range := fProgressStages[PROCSTAGEIDX_ZIP_EntryProcessing].Range * 0.4;
end;

//------------------------------------------------------------------------------

procedure TRepairer_ZIP.ZIP_LoadEndOfCentralDirectory;
var
  EOCDPosition: Int64;
begin
DoProgress(PROCSTAGEIDX_ZIP_EOCDLoading,0.0);
EOCDPosition := FindSignature(ZIP_EndOfCentralDirectorySignature,-1,True,fProcessingSettings.EndOfCentralDirectory.LimitSearch);
DoProgress(PROCSTAGEIDX_ZIP_EOCDLoading,0.5);
If EOCDPosition >= 0 then
  begin
    fArchiveStream.Seek(EOCDPosition,soFromBeginning);
    If (fArchiveStream.Size - fArchiveStream.Position) >= SizeOf(TZIP_EndOfCentralDirectoryRecord) then
      begin
        with fArchiveStructure.EndOfCentralDirectory do
          begin
            fArchiveStream.ReadBuffer(BinPart,SizeOf(TZIP_EndOfCentralDirectoryRecord));
            DoProgress(PROCSTAGEIDX_ZIP_EOCDLoading,0.9);
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
                If (fArchiveStream.Size - fArchiveStream.Position) >= BinPart.CommentLength then
                  begin
                    SetLength(Comment,BinPart.CommentLength);
                    fArchiveStream.ReadBuffer(PAnsiChar(Comment)^,BinPart.CommentLength);
                  end
                else DoError(100,'Not enough data for end of central directory comment.');
              end;
          end;
      end
    else DoError(100,'Not enough data for end of central directory record.');
  end
else DoError(100,'End of central directory signature not found in the input stream.');
DoProgress(PROCSTAGEIDX_ZIP_EOCDLoading,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer_ZIP.ZIP_LoadCentralDirectory;
var
  WorkingOffset:  Int64;
  i:              Integer;

  procedure LoadCentralDirectoryHeader(Index: Integer);
  begin
    with fArchiveStructure.Entries[Index].CentralDirectoryHeader do
      begin
        fArchiveStream.ReadBuffer(BinPart,SizeOf(TZIP_CentralDirectoryFileHeaderRecord));
        // binary part checks
        If fProcessingSettings.CentralDirectory.IgnoreSignature then
          BinPart.Signature := ZIP_CentralDirectoryFileHeaderSignature
        else
          If BinPart.Signature <> ZIP_CentralDirectoryFileHeaderSignature then
            DoError(102,'Bad central directory header signature (0x%.8x) for entry #%d.',[BinPart.Signature,Index]);
        If fProcessingSettings.CentralDirectory.IgnoreVersions then
          begin
            BinPart.VersionMadeBy := 20;
            BinPart.HostOS := 0;
            BinPart.VersionNeededToExtract := 20;
            BinPart.OSNeededForExtraction := 0;
          end;
        If fProcessingSettings.CentralDirectory.ClearEncryptionFlags then
          BinPart.GeneralPurposeBitFlag := BinPart.GeneralPurposeBitFlag and not UInt16(ZBF_Encrypted or ZBF_StrongEncryption);
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
              DoError(102,'Unknown compression method (%d) in central directory header for entry #%d.',[BinPart.CompressionMethod,Index]);
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
        fArchiveStream.ReadBuffer(PAnsiChar(FileName)^,BinPart.FileNameLength);
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
            fArchiveStream.Seek(BinPart.ExtraFieldLength,soFromCurrent);
            BinPart.ExtraFieldLength := 0;
          end
        else
          begin
            SetLength(ExtraField,BinPart.ExtraFieldLength);
            fArchiveStream.ReadBuffer(PAnsiChar(ExtraField)^,BinPart.ExtraFieldLength);
           end;
        // load file comment
        If fProcessingSettings.CentralDirectory.IgnoreFileComment then
          begin
            fArchiveStream.Seek(BinPart.FileCommentLength,soFromCurrent);
            BinPart.FileCommentLength := 0;
          end
        else
          begin
            SetLength(FileComment,BinPart.FileCommentLength);
            fArchiveStream.ReadBuffer(PAnsiChar(FileComment)^,BinPart.FileCommentLength);
          end;
    end;
  end;

begin
DoProgress(PROCSTAGEIDX_ZIP_CDHeadersLoading,0.0);
If fProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset or
  fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  begin
    // find where the central directory starts
    WorkingOffset := FindSignature(ZIP_CentralDirectoryFileHeaderSignature);
    If WorkingOffset >= 0 then
      fArchiveStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset := WorkingOffset
    else
      DoError(101,'Start of central directory not found in the input stream.');
  end
else WorkingOffset := fArchiveStructure.EndOfCentralDirectory.BinPart.CentralDirectoryOffset;
If fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries or
  fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  begin
    // number of entries is not known, have to search for them
    i := 0;
    repeat
      If (i + 1) > Length(fArchiveStructure.Entries) then
        SetLength(fArchiveStructure.Entries,Length(fArchiveStructure.Entries) + 1024);
      fArchiveStream.Seek(WorkingOffset,soFromBeginning);
      with fArchiveStructure.EndOfCentralDirectory.BinPart do
        DoProgress(PROCSTAGEIDX_ZIP_CDHeadersLoading,(WorkingOffset - CentralDirectoryOffset) / (fArchiveStream.Size - CentralDirectoryOffset));
      LoadCentralDirectoryHeader(i);
      WorkingOffset := FindSignature(ZIP_CentralDirectoryFileHeaderSignature,fArchiveStream.Position,False);
      Inc(i);
    until WorkingOffset < 0;
    SetLength(fArchiveStructure.Entries,i);
    fArchiveStructure.EndOfCentralDirectory.BinPart.EntriesOnDisk := i;
    If fProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit then
      fArchiveStructure.EndOfCentralDirectory.BinPart.Entries := i;
  end
else
  begin
    // number of entries is known
    fArchiveStream.Seek(WorkingOffset,soFromBeginning);
    For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
      begin
        LoadCentralDirectoryHeader(i);
        DoProgress(PROCSTAGEIDX_ZIP_CDHeadersLoading,(i + 1) / Length(fArchiveStructure.Entries));
      end;
  end;
DoProgress(PROCSTAGEIDX_ZIP_CDHeadersLoading,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer_ZIP.ZIP_LoadLocalHeaders;
var
  WorkingOffset:  Int64;
  i:              Integer;

  procedure LoadLocalHeader(Index: Integer);
  var
    DescriptorOffset:  Int64;
  begin
    with fArchiveStructure.Entries[Index],fArchiveStructure.Entries[Index].LocalHeader do
      begin
        fArchiveStream.ReadBuffer(BinPart,SizeOf(TZIP_LocalFileHeaderRecord));
        // binary part checks
        If fProcessingSettings.LocalHeader.IgnoreSignature then
          BinPart.Signature := ZIP_LocalFileHeaderSignature
        else
          If BinPart.Signature <> ZIP_LocalFileHeaderSignature then
            DoError(104,'Bad local header signature (0x%.8x) for entry #%d.',[BinPart.Signature,Index]);
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
              DoError(104,'Unknown compression method (%d) in local header for entry #%d.',[BinPart.CompressionMethod,Index]);
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
            fArchiveStream.Seek(BinPart.FileNameLength,soFromCurrent);
            BinPart.FileNameLength := CentralDirectoryHeader.BinPart.FileNameLength;
            FileName := CentralDirectoryHeader.FileName;
          end
        else
          begin
            SetLength(FileName,BinPart.FileNameLength);
            fArchiveStream.ReadBuffer(PAnsiChar(FileName)^,BinPart.FileNameLength);
          end;
        // extra field
        If fProcessingSettings.LocalHeader.IgnoreExtraField then
          begin
            fArchiveStream.Seek(BinPart.ExtraFieldLength,soFromCurrent);
            BinPart.ExtraFieldLength := 0;
          end
        else
          begin
            SetLength(ExtraField,BinPart.ExtraFieldLength);
            fArchiveStream.ReadBuffer(PAnsiChar(ExtraField)^,BinPart.ExtraFieldLength);
          end;
        fArchiveStructure.Entries[Index].UtilityData.DataOffset := fArchiveStream.Position;
        // read data descriptor if present
        If ((BinPart.GeneralPurposeBitFlag and ZBF_DataDescriptor) <> 0) and
          not fProcessingSettings.LocalHeader.IgnoreDataDescriptor then
          begin
            If fProcessingSettings.LocalHeader.IgnoreSizes then
              DescriptorOffset := FindSignature(ZIP_DataDescriptorSignature,fArchiveStream.Position,False)
            else
              DescriptorOffset := fArchiveStream.Position + BinPart.CompressedSize;
            If (DescriptorOffset > 0) and ((DescriptorOffset + SizeOf(TZIP_DataDescriptorRecord)) <= fArchiveStream.Size) then
              begin
                fArchiveStream.Seek(DescriptorOffset,soFromBeginning);
                fArchiveStream.ReadBuffer(DataDescriptor,SizeOf(TZIP_DataDescriptorRecord));
                If fProcessingSettings.LocalHeader.IgnoreSignature then
                  DataDescriptor.Signature := ZIP_DataDescriptorSignature
                else
                  begin
                    If DataDescriptor.Signature <> ZIP_DataDescriptorSignature then
                      DoError(104,'Bad data descriptor signature (0x%.8x) for entry #%d.',[DataDescriptor.Signature,Index])
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
            else DoError(104,'Data descriptor was not found (%d).',[DescriptorOffset]);
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
    with fArchiveStructure.Entries[Index] do
      begin
        LocalHeader.BinPart.Signature := ZIP_LocalFileHeaderSignature;
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
                                  SizeOf(TZIP_LocalFileHeaderRecord) +
                                  LocalHeader.BinPart.FileNameLength +
                                  LocalHeader.BinPart.ExtraFieldLength;
      end;
  end;

begin
DoProgress(PROCSTAGEIDX_ZIP_LocalHeadersLoading,0.0);
If fProcessingSettings.LocalHeader.IgnoreLocalHeaders then
  begin
    // local headers are not loaded from the file, they are instead constructed from data
    // stored in central directory
    For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
      CopyCentralToLocal(i);
  end
else
  begin
    // load local headers from archive file
    If Length(fArchiveStructure.Entries) > 0 then
      begin
        // entries are already prepared from central directory
        fArchiveStream.Seek(0,soFromBeginning);
        For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
          with fArchiveStructure.Entries[i].CentralDirectoryHeader do
            begin
              If fProcessingSettings.CentralDirectory.IgnoreLocalHeaderOffset or
                fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
                begin
                  // position of local header for given CD entry is not known,
                  // search for next local header from current position
                  WorkingOffset := FindSignature(ZIP_LocalFileHeaderSignature,fArchiveStream.Position,False);
                  If WorkingOffset >= 0 then
                    begin
                      BinPart.RelativeOffsetOfLocalHeader := WorkingOffset;
                      fArchiveStream.Seek(WorkingOffset,soFromBeginning);
                    end
                  else DoError(103,'No local header found for entry #%d.',[i]);
                end
              else fArchiveStream.Seek(BinPart.RelativeOffsetOfLocalHeader,soFromBeginning);
              LoadLocalHeader(i);
              DoProgress(PROCSTAGEIDX_ZIP_LocalHeadersLoading,(i + 1) / Length(fArchiveStructure.Entries));
              If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
                If not AnsiSameText(FileName,fArchiveStructure.Entries[i].LocalHeader.FileName) then
                {$IFDEF FPC}
                  DoError(103,'Mismatch in local and central directory file name for entry #%d (%s; %s).',[i,WinCPToUTF8(FileName),WinCPToUTF8(fArchiveStructure.Entries[i].LocalHeader.FileName)]);
                {$ELSE}
                  DoError(103,'Mismatch in local and central directory file name for entry #%d (%s; %s).',[i,FileName,fArchiveStructure.Entries[i].LocalHeader.FileName]);
                {$ENDIF}
            end;
      end
    else
      begin
        // no entries are loaded yet, search for local headers and load them
        i := 0;
        WorkingOffset := FindSignature(ZIP_LocalFileHeaderSignature);
        If WorkingOffset >= 0 then
          repeat
            If (i + 1) > Length(fArchiveStructure.Entries) then
              SetLength(fArchiveStructure.Entries,Length(fArchiveStructure.Entries) + 1024);
            fArchiveStructure.Entries[i].CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader := WorkingOffset;
            fArchiveStream.Seek(WorkingOffset,soFromBeginning);
            DoProgress(PROCSTAGEIDX_ZIP_LocalHeadersLoading,WorkingOffset / fArchiveStream.Size);
            LoadLocalHeader(i);
            WorkingOffset := FindSignature(ZIP_LocalFileHeaderSignature,fArchiveStream.Position,False);
            Inc(i);
          until WorkingOffset < 0;
        SetLength(fArchiveStructure.Entries,i);
      end;
  end;
DoProgress(PROCSTAGEIDX_ZIP_LocalHeadersLoading,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer_ZIP.ZIP_ReconstructLocalHeaders;
var
  i:  Integer;
begin
For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
  with fArchiveStructure.Entries[i] do
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
      DoProgress(PROCSTAGEIDX_NoProgress,0.0);
    end;
end;

//------------------------------------------------------------------------------

procedure TRepairer_ZIP.ZIP_ReconstructCentralDirectoryHeaders;
var
  i:  Integer;
begin
For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
  with fArchiveStructure.Entries[i] do
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
          CentralDirectoryHeader.BinPart.Signature := ZIP_CentralDirectoryFileHeaderSignature;
          CentralDirectoryHeader.BinPart.DiskNumberStart := 0;
          CentralDirectoryHeader.BinPart.InternalFileAttributes := 0;
          CentralDirectoryHeader.BinPart.ExternalFileAttributes := 0;
          CentralDirectoryHeader.BinPart.FileNameLength := LocalHeader.BinPart.FileNameLength;
          CentralDirectoryHeader.FileName := LocalHeader.FileName;
        end;
      fArchiveStructure.Entries[i].UtilityData.NeedsCRC32 :=
        (fProcessingSettings.LocalHeader.IgnoreLocalHeaders or fProcessingSettings.LocalHeader.IgnoreCRC32) and
        (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or fProcessingSettings.CentralDirectory.IgnoreCRC32);
      fArchiveStructure.Entries[i].UtilityData.NeedsSizes :=
        (fProcessingSettings.LocalHeader.IgnoreLocalHeaders or fProcessingSettings.LocalHeader.IgnoreSizes) and
        (fProcessingSettings.CentralDirectory.IgnoreCentralDirectory or fProcessingSettings.CentralDirectory.IgnoreSizes);
      DoProgress(PROCSTAGEIDX_NoProgress,0.0);  
    end;
end;

//------------------------------------------------------------------------------

procedure TRepairer_ZIP.ZIP_ReconstructEndOfCentralDirectory;
var
  i:  Integer;
begin
with fArchiveStructure.EndOfCentralDirectory do
  begin
    If fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
      begin
        BinPart.Signature := ZIP_EndOfCentralDirectorySignature;
        BinPart.NumberOfThisDisk := 0;
        BinPart.CentralDirectoryStartDiskNumber := 0;
        BinPart.CommentLength := 0;
        Comment := '';
      end;
    If fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory or fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries then
      begin
        BinPart.EntriesOnDisk := Length(fArchiveStructure.Entries);
        BinPart.Entries := Length(fArchiveStructure.Entries);
      end;
    BinPart.CentralDirectorySize := 0;
    For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
      begin
        Inc(BinPart.CentralDirectorySize,SizeOf(TZIP_CentralDirectoryFileHeaderRecord));
        Inc(BinPart.CentralDirectorySize,fArchiveStructure.Entries[i].CentralDirectoryHeader.BinPart.FileNameLength);
        Inc(BinPart.CentralDirectorySize,fArchiveStructure.Entries[i].CentralDirectoryHeader.BinPart.ExtraFieldLength);
        Inc(BinPart.CentralDirectorySize,fArchiveStructure.Entries[i].CentralDirectoryHeader.BinPart.FileCommentLength);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TRepairer_ZIP.RectifyFileProcessingSettings;
begin
fProcessingSettings := fFileProcessingSettings.ZIPSettings;
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

procedure TRepairer_ZIP.InitializeData;
begin
SetLength(fArchiveStructure.Entries,0);
FillChar(fArchiveStructure.EndOfCentralDirectory.BinPart,SizeOf(TZIP_EndOfCentralDirectoryRecord),0);
fArchiveStructure.EndOfCentralDirectory.Comment := '';
end;

//------------------------------------------------------------------------------

procedure TRepairer_ZIP.InitializeProgress;
var
  AvailableRange: Single;
begin
inherited;
SetLength(fProgressStages,Succ(PROCSTAGEIDX_ZIP_Max));
AvailableRange := 1.0 - (fProgressStages[PROCSTAGEIDX_Loading].Range + fProgressStages[PROCSTAGEIDX_Saving].Range);
// set progress info for loading of EOCD
fProgressStages[PROCSTAGEIDX_ZIP_EOCDLoading].Offset := fProgressStages[PROCSTAGEIDX_Loading].Range;
If not fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  fProgressStages[PROCSTAGEIDX_ZIP_EOCDLoading].Range := 0.01 * AvailableRange;
// ... loading of central directory
fProgressStages[PROCSTAGEIDX_ZIP_CDHeadersLoading].Offset := fProgressStages[PROCSTAGEIDX_ZIP_EOCDLoading].Offset +
                                                             fProgressStages[PROCSTAGEIDX_ZIP_EOCDLoading].Range;
If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
  fProgressStages[PROCSTAGEIDX_ZIP_CDHeadersLoading].Range := 0.1 * AvailableRange;
// ... loading of local headers
fProgressStages[PROCSTAGEIDX_ZIP_LocalHeadersLoading].Offset := fProgressStages[PROCSTAGEIDX_ZIP_CDHeadersLoading].Offset +
                                                                fProgressStages[PROCSTAGEIDX_ZIP_CDHeadersLoading].Range;
If not fProcessingSettings.LocalHeader.IgnoreLocalHeaders then
  fProgressStages[PROCSTAGEIDX_ZIP_LocalHeadersLoading].Range := 0.1 * AvailableRange;
// ... processing of entries
fProgressStages[PROCSTAGEIDX_ZIP_EntriesProcessing].Offset := fProgressStages[PROCSTAGEIDX_ZIP_LocalHeadersLoading].Offset +
                                                              fProgressStages[PROCSTAGEIDX_ZIP_LocalHeadersLoading].Range;
fProgressStages[PROCSTAGEIDX_ZIP_EntriesProcessing].Range := AvailableRange -
                                                             (fProgressStages[PROCSTAGEIDX_ZIP_EOCDLoading].Range +
                                                              fProgressStages[PROCSTAGEIDX_ZIP_CDHeadersLoading].Range +
                                                              fProgressStages[PROCSTAGEIDX_ZIP_LocalHeadersLoading].Range);
end;

//------------------------------------------------------------------------------

procedure TRepairer_ZIP.ArchiveProcessing;
begin
If not fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory then
  begin
    ZIP_LoadEndOfCentralDirectory;
    If not fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries then
      SetLength(fArchiveStructure.Entries,fArchiveStructure.EndOfCentralDirectory.BinPart.EntriesOnDisk);
  end;
If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
  ZIP_LoadCentralDirectory;
ZIP_LoadLocalHeaders;
If not fProcessingSettings.CentralDirectory.IgnoreCentralDirectory then
  ZIP_ReconstructLocalHeaders;
ZIP_ReconstructCentralDirectoryHeaders;
ZIP_ReconstructEndOfCentralDirectory;
If Length(fArchiveStructure.Entries) <= 0 then
  DoError(105,'Input file does not contain any valid entries.');
end;

//==============================================================================

class Function TRepairer_ZIP.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  100:  Result := 'ZIP_LoadEndOfCentralDirectory';
  101:  Result := 'ZIP_LoadCentralDirectory';
  102:  Result := 'ZIP_LoadCentralDirectory.LoadCentralDirectoryHeader';
  103:  Result := 'ZIP_LoadLocalHeaders';
  104:  Result := 'ZIP_LoadLocalHeaders.LoadLocalHeader';
  105:  Result := 'ArchiveProcessing';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

//------------------------------------------------------------------------------

constructor TRepairer_ZIP.Create(FlowControlObject: TEvent; FileProcessingSettings: TFileProcessingSettings; CatchExceptions: Boolean = True);
begin
inherited Create(FlowControlObject,FileProcessingSettings,CatchExceptions);
fExpectedSignature := FileSignature_ZIP;
end;

end.
