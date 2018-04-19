{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_ProcessingSettings_Presets;

{$INCLUDE DART_defs.inc}

interface

uses
  DART_ProcessingSettings;

{===============================================================================
--------------------------------------------------------------------------------
                        SCS# processing settings presets
--------------------------------------------------------------------------------
===============================================================================}

type
  TDARTSCSPreset = record
    PresetName: String;
    PresetData: TDART_PS_SCS;
  end;

const
  DART_PS_SCS_Presets: array[0..3] of TDARTSCSPreset = (
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   (PresetName: 'SCS# Preset - Archive slightly damaged (default)';
    PresetData: (
      EntryTabInMem:                True;     
      Entry: (
        IgnoreCRC32:                False;
        IgnoreCompressionFlag:      False;
        IgnoreDictionaryID:         False);
      PathResolve:(
        AssumeCityHash:             False;
        UsePredefinedPaths:         True;
        ExtractedUnresolvedEntries: False;
        CustomPaths:                nil;
        HelpArchives:               nil;
        ContentParsing: (
          ParseContent:             False;
          ParseEverything:          False;
          ParseHelpArchives:        False;
          ParseEverythingInHlpArch: False;
          PrintableASCIIOnly:       True;
          LimitedCharacterSet:      True;
          BinaryThreshold:          0.0;
          MinPathLength:            2);
        BruteForce: (
          ActivateBruteForce:         False;
          Multithreaded:              True;
          UseKnownPaths:              False;
          PrintableASCIIOnly:         True;
          LimitedCharSet:             True;
          PathLengthLimit:            32)))),
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   (PresetName: 'SCS# Preset - Archive moderately damaged';
    PresetData: (
      EntryTabInMem:                True;
      Entry: (
        IgnoreCRC32:                True;
        IgnoreCompressionFlag:      True;
        IgnoreDictionaryID:         True);
      PathResolve:(
        AssumeCityHash:             True;
        UsePredefinedPaths:         True;
        ExtractedUnresolvedEntries: False;
        CustomPaths:                nil;
        HelpArchives:               nil;
        ContentParsing: (
          ParseContent:             False;
          ParseEverything:          False;
          ParseHelpArchives:        False;
          ParseEverythingInHlpArch: False;
          PrintableASCIIOnly:       True;
          LimitedCharacterSet:      True;
          BinaryThreshold:          0.0;
          MinPathLength:            2);
        BruteForce: (
          ActivateBruteForce:         False;
          Multithreaded:              True;
          UseKnownPaths:              False;
          PrintableASCIIOnly:         True;
          LimitedCharSet:             True;
          PathLengthLimit:            32)))),
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   (PresetName: 'SCS# Preset - Archive severely damaged - parse content';
    PresetData: (
      EntryTabInMem:                True;
      Entry: (
        IgnoreCRC32:                True;
        IgnoreCompressionFlag:      True;
        IgnoreDictionaryID:         True);
      PathResolve:(
        AssumeCityHash:             True;
        UsePredefinedPaths:         True;
        ExtractedUnresolvedEntries: False;
        CustomPaths:                nil;
        HelpArchives:               nil;
        ContentParsing: (
          ParseContent:             True;
          ParseEverything:          False;
          ParseHelpArchives:        False;
          ParseEverythingInHlpArch: False;
          PrintableASCIIOnly:       True;
          LimitedCharacterSet:      True;
          BinaryThreshold:          0.0;
          MinPathLength:            2);
        BruteForce: (
          ActivateBruteForce:         False;
          Multithreaded:              True;
          UseKnownPaths:              False;
          PrintableASCIIOnly:         True;
          LimitedCharSet:             True;
          PathLengthLimit:            32)))),
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   (PresetName: 'SCS# Preset - Full retard mode';
    PresetData: (
      EntryTabInMem:                True;
      Entry: (
        IgnoreCRC32:                True;
        IgnoreCompressionFlag:      True;
        IgnoreDictionaryID:         True);
      PathResolve:(
        AssumeCityHash:             True;
        UsePredefinedPaths:         True;
        ExtractedUnresolvedEntries: False;
        CustomPaths:                nil;
        HelpArchives:               nil;
        ContentParsing: (
          ParseContent:             True;
          ParseEverything:          True;
          ParseHelpArchives:        True;
          ParseEverythingInHlpArch: True;
          PrintableASCIIOnly:       False;
          LimitedCharacterSet:      False; 
          BinaryThreshold:          0.05;
          MinPathLength:            1);
        BruteForce: (
          ActivateBruteForce:         True;
          Multithreaded:              True;
          UseKnownPaths:              False;
          PrintableASCIIOnly:         True;
          LimitedCharSet:             False;
          PathLengthLimit:            32)))));

{===============================================================================
--------------------------------------------------------------------------------
                        ZIP processing settings presets                         
--------------------------------------------------------------------------------
===============================================================================}

type
  TDARTZIPPreset = record
    PresetName: String;
    PresetData: TDART_PS_ZIP; 
  end;

const
  DART_PS_ZIP_Presets: array[0..5] of TDARTZIPPreset = (
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   (PresetName: 'ZIP Preset - Archive slightly damaged';
    PresetData: (
      AssumeCompressionMethod:  False;
      EndOfCentralDirectory: (
        IgnoreEndOfCentralDirectory:  False;
        IgnoreDiskSplit:              True;
        IgnoreNumberOfEntries:        False;
        IgnoreCentralDirectoryOffset: False;
        IgnoreComment:                False;
        LimitSearch:                  False);
      CentralDirectory: (
        IgnoreCentralDirectory:       False;
        IgnoreSignature:              False;
        IgnoreVersions:               False;
        ClearEncryptionFlags:         True;
        IgnoreCompressionMethod:      False;
        IgnoreModTime:                False;
        IgnoreModDate:                False;
        IgnoreCRC32:                  False;
        IgnoreSizes:                  False;
        IgnoreInternalFileAttributes: False;
        IgnoreExternalFileAttributes: False;
        IgnoreLocalHeaderOffset:      False;
        IgnoreExtraField:             False;
        IgnoreFileComment:            False);
      LocalHeader: (
        IgnoreLocalHeaders:           False;
        IgnoreSignature:              False;
        IgnoreVersions:               False;
        ClearEncryptionFlags:         True;
        IgnoreCompressionMethod:      False;
        IgnoreModTime:                False;
        IgnoreModDate:                False;
        IgnoreCRC32:                  False;
        IgnoreSizes:                  False;
        IgnoreFileName:               False;
        IgnoreExtraField:             False;
        IgnoreDataDescriptor:         False))),
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   (PresetName: 'ZIP Preset - Archive moderately damaged';
    PresetData: (
      AssumeCompressionMethod:  False;
      EndOfCentralDirectory: (
        IgnoreEndOfCentralDirectory:  False;
        IgnoreDiskSplit:              True;
        IgnoreNumberOfEntries:        False;
        IgnoreCentralDirectoryOffset: False;
        IgnoreComment:                True;
        LimitSearch:                  True);
      CentralDirectory: (
        IgnoreCentralDirectory:       False;
        IgnoreSignature:              False;
        IgnoreVersions:               True;
        ClearEncryptionFlags:         True;
        IgnoreCompressionMethod:      False;
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
        IgnoreSignature:              False;
        IgnoreVersions:               True;
        ClearEncryptionFlags:         True;
        IgnoreCompressionMethod:      False;
        IgnoreModTime:                False;
        IgnoreModDate:                False;
        IgnoreCRC32:                  True;
        IgnoreSizes:                  False;
        IgnoreFileName:               False;
        IgnoreExtraField:             True;
        IgnoreDataDescriptor:         False))),
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   (PresetName: 'ZIP Preset - Archive severely damaged (default)';
    PresetData: (
      AssumeCompressionMethod:  False;
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
        IgnoreDataDescriptor:         False))),
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   (PresetName: 'ZIP Preset - Archive extremely damaged';
    PresetData: (
      AssumeCompressionMethod:  True;
      EndOfCentralDirectory: (
        IgnoreEndOfCentralDirectory:  False;
        IgnoreDiskSplit:              True;
        IgnoreNumberOfEntries:        True;
        IgnoreCentralDirectoryOffset: True;
        IgnoreComment:                True;
        LimitSearch:                  True);
      CentralDirectory: (
        IgnoreCentralDirectory:       False;
        IgnoreSignature:              True;
        IgnoreVersions:               True;
        ClearEncryptionFlags:         True;
        IgnoreCompressionMethod:      True;
        IgnoreModTime:                True;
        IgnoreModDate:                True;
        IgnoreCRC32:                  True;
        IgnoreSizes:                  False;
        IgnoreInternalFileAttributes: True;
        IgnoreExternalFileAttributes: True;
        IgnoreLocalHeaderOffset:      True;
        IgnoreExtraField:             True;
        IgnoreFileComment:            True);
      LocalHeader: (
        IgnoreLocalHeaders:           False;
        IgnoreSignature:              True;
        IgnoreVersions:               True;
        ClearEncryptionFlags:         True;
        IgnoreCompressionMethod:      True;
        IgnoreModTime:                True;
        IgnoreModDate:                True;
        IgnoreCRC32:                  True;
        IgnoreSizes:                  True;
        IgnoreFileName:               True;
        IgnoreExtraField:             True;
        IgnoreDataDescriptor:         True))),
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   (PresetName: 'ZIP Preset - Full retard mode (with central directory)';
    PresetData: (
      AssumeCompressionMethod:  True;
      EndOfCentralDirectory: (
        IgnoreEndOfCentralDirectory:  True;
        IgnoreDiskSplit:              True;
        IgnoreNumberOfEntries:        True;
        IgnoreCentralDirectoryOffset: True;
        IgnoreComment:                True;
        LimitSearch:                  True);
      CentralDirectory: (
        IgnoreCentralDirectory:       False;
        IgnoreSignature:              False;
        IgnoreVersions:               True;
        ClearEncryptionFlags:         True;
        IgnoreCompressionMethod:      True;
        IgnoreModTime:                True;
        IgnoreModDate:                True;
        IgnoreCRC32:                  True;
        IgnoreSizes:                  False;
        IgnoreInternalFileAttributes: True;
        IgnoreExternalFileAttributes: True;
        IgnoreLocalHeaderOffset:      False;
        IgnoreExtraField:             True;
        IgnoreFileComment:            True);
      LocalHeader: (
        IgnoreLocalHeaders:           True;
        IgnoreSignature:              True;
        IgnoreVersions:               True;
        ClearEncryptionFlags:         True;
        IgnoreCompressionMethod:      True;
        IgnoreModTime:                True;
        IgnoreModDate:                True;
        IgnoreCRC32:                  True;
        IgnoreSizes:                  True;
        IgnoreFileName:               True;
        IgnoreExtraField:             True;
        IgnoreDataDescriptor:         True))),
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   (PresetName: 'ZIP Preset - Full retard mode (with local headers)';
    PresetData: (
      AssumeCompressionMethod:  True;
      EndOfCentralDirectory: (
        IgnoreEndOfCentralDirectory:  True;
        IgnoreDiskSplit:              True;
        IgnoreNumberOfEntries:        True;
        IgnoreCentralDirectoryOffset: True;
        IgnoreComment:                True;
        LimitSearch:                  True);
      CentralDirectory: (
        IgnoreCentralDirectory:       True;
        IgnoreSignature:              True;
        IgnoreVersions:               True;
        ClearEncryptionFlags:         True;
        IgnoreCompressionMethod:      True;
        IgnoreModTime:                True;
        IgnoreModDate:                True;
        IgnoreCRC32:                  True;
        IgnoreSizes:                  True;
        IgnoreInternalFileAttributes: True;
        IgnoreExternalFileAttributes: True;
        IgnoreLocalHeaderOffset:      True;
        IgnoreExtraField:             True;
        IgnoreFileComment:            True);
      LocalHeader: (
        IgnoreLocalHeaders:           False;
        IgnoreSignature:              False;
        IgnoreVersions:               True;
        ClearEncryptionFlags:         True;
        IgnoreCompressionMethod:      True;
        IgnoreModTime:                True;
        IgnoreModDate:                True;
        IgnoreCRC32:                  True;
        IgnoreSizes:                  False;
        IgnoreFileName:               False;
        IgnoreExtraField:             True;
        IgnoreDataDescriptor:         True))));

implementation

end.
