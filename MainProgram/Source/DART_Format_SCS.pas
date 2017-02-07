{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Format_SCS;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes, CRC32;

{==============================================================================}
{------------------------------------------------------------------------------}
{                         SCS# structures and constants                        }
{------------------------------------------------------------------------------}
{==============================================================================}

//--- SCS# archive signature ---------------------------------------------------

const
  SCS_FileSignature = UInt32($23534353);  // SCS#

//--- Known hash algorithms ----------------------------------------------------

  SCS_HASH_City = UInt32($59544943);  // CITY

//--- Entry bit flags ----------------------------------------------------------

  SCS_FLAG_Directory  = $00000001;
  SCS_FLAG_Compressed = $00000002;
  SCS_FLAG_Unknown    = $00000004;  // seems to be always set

//--- Default offsets ----------------------------------------------------------

  SCS_DefaultEntryTableOffset = UInt64($0000000000001000);

type
//--- Archive header record ----------------------------------------------------

  TSCS_ArchiveHeader = packed record
    Signature:      UInt32;
    Unknown:        UInt32;
    HashType:       UInt32;
    Entries:        UInt32;
    EntriesOffset:  UInt64;
    UnknownOffset:  UInt64;
  end;

//--- Entry record (table item) ------------------------------------------------

  TSCS_EntryRecord = packed record
    Hash:             UInt64;
    DataOffset:       UInt64;
    Flags:            UInt32;
    CRC32:            TCRC32;
    UncompressedSize: UInt32;
    CompressedSize:   UInt32;
  end;

//--- Utility data -------------------------------------------------------------

  TSCS_UtilityData = record
    Resolved:           Boolean;
    Erroneous:          Boolean;
    SubEntries:         array of AnsiString;
    OriginalDataOffset: UInt64;
  end;

//--- Main structure -----------------------------------------------------------

  TSCS_Entry = record
    Bin:          TSCS_EntryRecord;
    FileName:     AnsiString;
    UtilityData:  TSCS_UtilityData;
  end;

  TSCS_KnownPathItem = record
    Path: AnsiString;
    Hash: TCRC32;
  end;

  TSCS_KnownPaths = record
    Paths:  array of TSCS_KnownPathItem;
    Count:  Integer;
  end;

  TSCS_ArchiveStructure = record
    ArchiveHeader:  TSCS_ArchiveHeader;
    Entries:        array of TSCS_Entry;
    KnownPaths:     TSCS_KnownPaths;
    DataBytes:      UInt64;
  end;

//--- Path constants, predefined paths -----------------------------------------

const
  SCS_PathDelim = '/';
  
  SCS_RootPath = '';

  SCS_PredefinedPaths: array[0..20] of AnsiString = (
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
    'automat','def','effect','map','material','model','model2',
    'prefab','prefab2','sound','system','ui','unit','vehicle');

implementation

end.
