unit DART_Format_SCS;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes, CRC32;

{===============================================================================
--------------------------------------------------------------------------------
                          SCS# structures and constants
--------------------------------------------------------------------------------
===============================================================================}

//--- SCS# archive signature ---------------------------------------------------

const
  DART_SCS_FileSignature = UInt32($23534353); // SCS#

//--- Archive header record ----------------------------------------------------

type
  TDART_SCS_ArchiveHeader = packed record
    Signature:      UInt32;
    Unknown:        UInt32;
    HashType:       UInt32;
    EntryCount:     UInt32;
    EntriesOffset:  UInt64;
    UnknownOffset:  UInt64;
  end;

//--- Entry record (table item) ------------------------------------------------

  TDART_SCS_EntryRecord = packed record
    FileNameHash:     UInt64;
    DataOffset:       UInt64;
    Flags:            UInt32;
    CRC32:            TCRC32;
    UncompressedSize: UInt32;
    CompressedSize:   UInt32;
  end;

//--- Utility data -------------------------------------------------------------

  TDART_SCS_EntryUtilityData = record
    //Resolved:           Boolean;
    //Erroneous:          Boolean;
    //SubEntries:         array of AnsiString;
    //OriginalDataOffset: UInt64;
  end;

  TDART_SCS_UtilityData = record
    //DataBytes:      UInt64;
  end;

//--- Main structure -----------------------------------------------------------

  TDART_SCS_Entry = record
    Bin:          TDART_SCS_EntryRecord;
    FileName:     AnsiString;
    UtilityData:  TDART_SCS_EntryUtilityData;
  end;

  TDART_SCS_Entries = record
    Arr:    array of TDART_SCS_Entry;
    Count:  Integer;
  end;

  TDART_SCS_KnownPathItem = record
    Path: AnsiString;
    Hash: TCRC32;
  end;

  TDART_SCS_KnownPaths = record
    Arr:    array of TDART_SCS_KnownPathItem;
    Count:  Integer;
  end;

  TDART_SCS_ArchiveStructure = record
    ArchiveHeader:  TDART_SCS_ArchiveHeader;
    Entries:        TDART_SCS_Entries;
    KnownPaths:     TDART_SCS_KnownPaths;
    UtilityData:    TDART_SCS_UtilityData;
  end;

//--- Known hash algorithms ----------------------------------------------------

const
  DART_SCS_HASH_City = UInt32($59544943);   // CITY

//--- Entry bit flags ----------------------------------------------------------

  DART_SCS_FLAG_Directory  = $00000001;
  DART_SCS_FLAG_Compressed = $00000002;
  DART_SCS_FLAG_Unknown    = $00000004;   // seems to be always set

//--- Default offsets ----------------------------------------------------------

  DART_SCS_DefaultEntryTableOffset = UInt64($0000000000001000);

//--- Path constants, predefined paths -----------------------------------------

  DART_SCS_PATHS_Root = '';

  DART_SCS_PATHS_Predefined: array[0..21] of AnsiString = (
  {
    modifications stuff
  }
    'manifest.sii','mod_description.txt',
    'sample_mod.jpg','mod_icon.jpg','icon.jpg',
  {
    untracked files from ETS2 base.scs
  }
    'version.txt','autoexec.cfg',
  {
    some folders from ETS2
  }
    'automat','def','effect','locale','map','material','model','model2',
    'prefab','prefab2','sound','system','ui','unit','vehicle');

//--- Other SCS# constants -----------------------------------------------------

  DART_SCS_PathDelim = '/';

implementation

end.
