unit DART_Format_ZIP;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes, CRC32;

{===============================================================================
--------------------------------------------------------------------------------
                          ZIP structures and constants
--------------------------------------------------------------------------------
===============================================================================}

//--- ZIP archive signature ----------------------------------------------------

const
  DART_ZIP_FileSignature = UInt32($04034b50);

//--- Signatures ---------------------------------------------------------------

  DART_ZIP_LocalFileHeaderSignature            = UInt32($04034b50);
  DART_ZIP_DataDescriptorSignature             = UInt32($08074b50);
  DART_ZIP_CentralDirectoryFileHeaderSignature = UInt32($02014b50);
  DART_ZIP_EndOfCentralDirectorySignature      = UInt32($06054b50);

type
//--- Local file header --------------------------------------------------------

  TDART_ZIP_LocalFileHeaderRecord = packed record
    Signature:              UInt32;
    VersionNeededToExtract: UInt8;
    OSNeededForExtraction:  UInt8;
    GeneralPurposeBitFlag:  UInt16;
    CompressionMethod:      UInt16;
    LastModFileTime:        UInt16;
    LastModFileDate:        UInt16;
    CRC32:                  TCRC32;
    CompressedSize:         UInt32;
    UncompressedSize:       UInt32;
    FileNameLength:         UInt16;
    ExtraFieldLength:       UInt16;
  end;

  TDART_ZIP_LocalFileHeader = record
    BinPart:    TDART_ZIP_LocalFileHeaderRecord;
    FileName:   AnsiString;
    ExtraField: AnsiString;
  end;

//--- Data descriptor record ---------------------------------------------------

  TDART_ZIP_DataDescriptorRecord = packed record
    Signature:        UInt32;
    CRC32:            TCRC32;
    CompressedSize:   UInt32;
    UncompressedSize: UInt32;
  end;

//--- Central directory file header --------------------------------------------

  TDART_ZIP_CentralDirectoryFileHeaderRecord = packed record
    Signature:                    UInt32;
    VersionMadeBy:                UInt8;
    HostOS:                       UInt8;
    VersionNeededToExtract:       UInt8;
    OSNeededForExtraction:        UInt8;
    GeneralPurposeBitFlag:        UInt16;
    CompressionMethod:            UInt16;
    LastModFileTime:              UInt16;
    LastModFileDate:              UInt16;
    CRC32:                        TCRC32;
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

  TDART_ZIP_CentralDirectoryFileHeader = record
    BinPart:      TDART_ZIP_CentralDirectoryFileHeaderRecord;
    FileName:     AnsiString;
    ExtraField:   AnsiString;
    FileComment:  AnsiString;
  end;

//--- End of central directory record ------------------------------------------

  TDART_ZIP_EndOfCentralDirectoryRecord = packed record
    Signature:                        UInt32;
    NumberOfThisDisk:                 UInt16;
    CentralDirectoryStartDiskNumber:  UInt16;
    EntriesOnDisk:                    UInt16;
    Entries:                          UInt16;
    CentralDirectorySize:             UInt32;
    CentralDirectoryOffset:           UInt32;
    CommentLength:                    UInt16;
  end;

  TDART_ZIP_EndOfCentralDirectory = record
    BinPart:  TDART_ZIP_EndOfCentralDirectoryRecord;
    Comment:  AnsiString;
  end;

//--- Entry utility data -------------------------------------------------------

  TDART_ZIP_EntryUtilityData = record
    DataOffset:                 Int64;    // offset of actual entry data from the start of archive
    NeedsCRC32:                 Boolean;  // CRC32 has to be recalculated
    NeedsSizes:                 Boolean;  // actual sizes needs to be obtained
    OriginalLocalHeaderOffset:  UInt32;   // stores original offset of local header in input file    
    Erroneous:                  Boolean;  // entry is erroneous but the error was ignored
  end;

//--- Main structure -----------------------------------------------------------

  TDART_ZIP_Entry = record
    LocalHeader:            TDART_ZIP_LocalFileHeader;
    DataDescriptor:         TDART_ZIP_DataDescriptorRecord;
    CentralDirectoryHeader: TDART_ZIP_CentralDirectoryFileHeader;
    UtilityData:            TDART_ZIP_EntryUtilityData;
  end;

  TDART_ZIP_Entries = record
    Arr:    array of TDART_ZIP_Entry;
    Count:  Integer;
  end;

  TDART_ZIP_ArchiveStructure = record
    Entries:                TDART_ZIP_Entries;
    EndOfCentralDirectory:  TDART_ZIP_EndOfCentralDirectory;
  end;

//--- Bit flags ----------------------------------------------------------------

const
  DART_ZBF_Encrypted        = UInt16($0001);  // bit 0
  DART_ZBF_DataDescriptor   = UInt16($0008);  // bit 3
  DART_ZBF_StrongEncryption = UInt16($0040);  // bit 6

//--- Compression method numbers -----------------------------------------------

  DART_ZCM_Store   = 0; // no compression
  DART_ZCM_Deflate = 8; // deflate

  DART_ZIP_SupportedCompressinMethods = [DART_ZCM_Store,DART_ZCM_Deflate];  // store, deflate  

//--- Other ZIP constants ------------------------------------------------------

  DART_ZIP_PathDelim = '/';

  DART_ZIP_DefVersionMadeBy = 20; // 2.0
  DART_ZIP_DefHostOS        = 0;  // DOS / Windows


implementation

end.
