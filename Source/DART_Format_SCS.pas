{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Format_SCS;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes, CRC32,
  DART_Common;

{===============================================================================
--------------------------------------------------------------------------------
                          SCS# structures and constants
--------------------------------------------------------------------------------
===============================================================================}

//--- SCS# archive signature ---------------------------------------------------

const
  DART_SCS_ArchiveSignature = UInt32($23534353); // SCS#

//--- Archive header record ----------------------------------------------------

type
  TDART_SCS_ArchiveHeader = packed record
    Signature:        UInt32;
    Unknown:          UInt32; // allways 1
    HashType:         UInt32;
    EntryCount:       UInt32;
    EntryTableOffset: UInt64;
    UnknownOffset:    UInt64;
  end;

//--- Entry record (table item) ------------------------------------------------

  TDART_SCS_EntryRecord = packed record
    Hash:             TDARTHash64;
    DataOffset:       UInt64;
    Flags:            UInt32;
    CRC32:            TCRC32;
    UncompressedSize: UInt32;
    CompressedSize:   UInt32;
  end;

//--- Utility data -------------------------------------------------------------

  TDART_SCS_EntryUtilityData = record
    Resolved:     Boolean;
    Erroneous:    Boolean;
    DirContent:   array of AnsiString;
    DataOffset:   UInt64;
    Index:        Integer;
  end;

  TDART_SCS_UtilityData = record
    UnresolvedCount:  Integer;
  end;

//--- Main structure -----------------------------------------------------------

  TDART_SCS_Entry = record
    BinPart:      TDART_SCS_EntryRecord;
    FileName:     AnsiString;
    UtilityData:  TDART_SCS_EntryUtilityData;
  end;

  TDART_SCS_Entries = record
    Arr:    array of TDART_SCS_Entry;
    Count:  Integer;
  end;

  TDART_SCS_ArchiveStructure = record
    ArchiveHeader:  TDART_SCS_ArchiveHeader;
    Entries:        TDART_SCS_Entries;
    KnownPaths:     TDARTKnownPaths;
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
  DART_SCS_DefaultUnknownOffset    = UInt64($0000000000000000); // usually starts at $80

//--- Path constants, predefined paths -----------------------------------------

  DART_SCS_PATHS_Root = '';

  DART_SCS_PATHS_PredefinedDirs: array[0..14] of AnsiString = (
    {
    some folders from ETS2
  }
    'automat','def','effect','locale','map','material','model','model2',
    'prefab','prefab2','sound','system','ui','unit','vehicle');

  DART_SCS_PATHS_PredefinedFiles: array[0..6] of AnsiString = (
  {
    modifications stuff
  }
    'manifest.sii','mod_description.txt',
    'sample_mod.jpg','mod_icon.jpg','icon.jpg',
  {
    untracked files from ETS2 base.scs
  }
    'version.txt','autoexec.cfg');


//--- Other SCS# constants -----------------------------------------------------

  DART_SCS_DefaultExt = '.scs';

  DART_SCS_PathDelim = '/';

  DART_SCS_DirMark = '*';

{
  Maximum size of a directory entry that will not be compressed, in bytes.
  If the entry is larger, it will be compressed in the output, otherwise it will
  be stored with no compression.

  When set to 0, all directory entries will be compressed.
}
  DART_SCS_MaxUncompDirEntrySize = 32;

implementation

end.
