{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Format_ZIP;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes, CRC32;

{==============================================================================}
{------------------------------------------------------------------------------}
{                         ZIP structures and constants                         }
{------------------------------------------------------------------------------}
{==============================================================================}

//--- ZIP archive signature ----------------------------------------------------

const
  ZIP_FileSignature = UInt32($04034b50);

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
    CRC32:                  TCRC32;
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
    CRC32:            TCRC32;
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

//--- Other ZIP constants ------------------------------------------------------

const
  ZIP_PathDelim = '/';  

implementation

end.
