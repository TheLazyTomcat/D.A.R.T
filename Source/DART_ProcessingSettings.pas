{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_ProcessingSettings;

{$INCLUDE DART_defs.inc}

interface

type
  TDARTKnownArchiveTypes = (katUnknown,katSCS,katZIP);
  TDARTArchiveType       = (atUnknown,atSCS_sig,atSCS_frc,atZIP_sig,atZIP_frc,atZIP_dft);
  TDARTRepairMethod      = (rmUnknown,rmRebuild,rmExtract,rmConvert);

const
  DART_KnownArchiveTypeStrings: array[TDARTKnownArchiveTypes] of String =
    ('Unknown','SCS#','ZIP');

  DART_ArchiveTypeStrings: array[TDARTArchiveType] of String =
    ('Unknown','SCS#','SCS# (forced)','ZIP','ZIP (forced)','ZIP (defaulted)');

  DART_RepairMethodStrings: array[TDARTRepairMethod] of String =
    ('Unknown','Rebuild archive','Extract archive','Convert archive to %s');

//--- Common processing settings -----------------------------------------------

type
  TDART_PS_Common = record
    ArchivePath:            String;
    OriginalArchiveType:    TDARTArchiveType;
    SelectedArchiveType:    TDARTArchiveType;
    RepairMethod:           TDARTRepairMethod;
    ConvertTo:              TDARTKnownArchiveTypes;
    TargetPath:             String;
    IgnoreArchiveSignature: Boolean;
    InMemoryProcessing:     Boolean;
    IgnoreErroneousEntries: Boolean;
  end;

  TDART_PS_Auxiliaty = record
    InMemoryProcessingAllowed:  Boolean;  // depends on available memory and size of input archive
  end;

//--- ZIP-specific processing settings -----------------------------------------

  TDART_PS_ZIP_EndOfCentralDirectory = record
    IgnoreEndOfCentralDirectory:    Boolean;
    IgnoreDiskSplit:                Boolean;
    IgnoreNumberOfEntries:          Boolean;
    IgnoreCentralDirectoryOffset:   Boolean;
    IgnoreComment:                  Boolean;
    LimitSearch:                    Boolean;
  end;

  TDART_PS_ZIP_CentralDirectory = record
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

  TDART_PS_ZIP_LocalHeader = record
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

  TDART_PS_ZIP = record
    AssumeCompressionMethod:  Boolean;
    EndOfCentralDirectory:    TDART_PS_ZIP_EndOfCentralDirectory;
    CentralDirectory:         TDART_PS_ZIP_CentralDirectory;
    LocalHeader:              TDART_PS_ZIP_LocalHeader;
  end;

//--- SCS#-specific processing settings ----------------------------------------

  TDART_PS_SCS_EntrySettings = record
    IgnoreCRC32:            Boolean;
    IgnoreCompressionFlag:  Boolean;
    IgnoreDictionaryID:     Boolean;
  end;

  TDART_PS_SCS_PathResolveSettings = record
    AssumeCityHash:             Boolean;
    UsePredefinedPaths:         Boolean;
    ExtractedUnresolvedEntries: Boolean;
    CustomPaths:                array of AnsiString;
    HelpArchives:               array of String;
    // temporary fields, will be expanded as the functions are implemented
    ParseContent: Boolean;
    BruteForce:   Boolean;
  end;

  TDART_PS_SCS = record
    Entry:          TDART_PS_SCS_EntrySettings;
    EntryTabInMem:  Boolean;
    PathResolve:    TDART_PS_SCS_PathResolveSettings;
  end;

//--- Main structure -----------------------------------------------------------

  TDARTArchiveProcessingSettings = record
    Common:       TDART_PS_Common;
    ZIP:          TDART_PS_ZIP;
    SCS:          TDART_PS_SCS;
    Auxiliary:    TDART_PS_Auxiliaty;    
  end;

//==============================================================================

//--- Default archive processing settings --------------------------------------

const
  DART_DefaultArchiveProcessingSettings: TDARTArchiveProcessingSettings = (
    Common: (
      ArchivePath:            '';
      OriginalArchiveType:    atUnknown;
      SelectedArchiveType:    atUnknown;
      RepairMethod:           rmRebuild;
      ConvertTo:              katUnknown;
      TargetPath:             '';
      IgnoreArchiveSignature: True;
      InMemoryProcessing:     False;
      IgnoreErroneousEntries: False);
    ZIP: (
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
        IgnoreDataDescriptor:         False));
    SCS:  (
      Entry: (
        IgnoreCRC32:                False;
        IgnoreCompressionFlag:      False;
        IgnoreDictionaryID:         False);
      EntryTabInMem:                True;  
      PathResolve:(
        AssumeCityHash:             False;
        UsePredefinedPaths:         True;
        ExtractedUnresolvedEntries: False;
        CustomPaths:                nil;
        HelpArchives:               nil;
        ParseContent:               False;
        BruteForce:                 False));
    Auxiliary: (
      InMemoryProcessingAllowed: False));

//==============================================================================

//--- Processing settings functions --------------------------------------------

procedure RectifyArchiveProcessingSettings(var APS: TDARTArchiveProcessingSettings);

procedure RectifyZIPProcessingSettings(var ZIP_PS: TDART_PS_ZIP);
procedure RectifySCSProcessingSettings(var {%H-}SCS_PS: TDART_PS_SCS);

procedure EnsureThreadSafety(var APS: TDARTArchiveProcessingSettings); overload;

implementation

procedure RectifyArchiveProcessingSettings(var APS: TDARTArchiveProcessingSettings);
begin
If not APS.Auxiliary.InMemoryProcessingAllowed then
  APS.Common.InMemoryProcessing := False;
end;

//------------------------------------------------------------------------------

procedure RectifyZIPProcessingSettings(var ZIP_PS: TDART_PS_ZIP);
begin
{
  Central directory and local headers cannot be both ignored, the data must be
  obtained somewhere. Central directory has more data, so it is preferred.
}
If ZIP_PS.LocalHeader.IgnoreLocalHeaders and ZIP_PS.CentralDirectory.IgnoreCentralDirectory then
  ZIP_PS.CentralDirectory.IgnoreCentralDirectory := False;
{
  When local headers are ignored, data in them are constructed from central
  directory. Especially field IgnoreLocalHeaderOffset is important, since it
  is used to calcualte offset of actual data, which is important in further
  processing.

  If either central directory or IgnoreLocalHeaderOffset is ignored, this
  construction cannot happen because IgnoreLocalHeaderOffset is not properly
  initialized.

  Therefore, if any of the two mentioned is ignored, local headers must not be
  ignored and must be loaded.
}
If ZIP_PS.CentralDirectory.IgnoreCentralDirectory or
   ZIP_PS.CentralDirectory.IgnoreLocalHeaderOffset then
  ZIP_PS.LocalHeader.IgnoreLocalHeaders := False;
{
  For proper processing of entries, it is important to somehow obtain sizes
  of each entry (both compressed and uncompressed sizes).

  It is possible to get these values indirectly as long as the compression
  method is known (compressed size can be guessed from data, uncompressed size
  can be obtained by actually decompressing the data, or, for uncompressed
  entries, it is the same as compressed size).

  This means that at least one of the following four values has be obtained
  from the archive, and must not be ignored: central directory sizes or
  compression method, local header sizes or compresson method.

  Also, if central directory is ignored, it means entry file name must be
  obtained form local header, and therefore that field cannot be ignored.
}
If ZIP_PS.LocalHeader.IgnoreLocalHeaders then
  begin
    // local headers are ignored
    If ZIP_PS.CentralDirectory.IgnoreCompressionMethod then
      ZIP_PS.CentralDirectory.IgnoreSizes := False;
    If ZIP_PS.CentralDirectory.IgnoreSizes then
      ZIP_PS.CentralDirectory.IgnoreCompressionMethod := False;
  end
else If ZIP_PS.CentralDirectory.IgnoreCentralDirectory then
  begin
    // cenral directory ignored (local headers not ignored)
    If ZIP_PS.LocalHeader.IgnoreCompressionMethod then
      ZIP_PS.LocalHeader.IgnoreSizes := False;
    If ZIP_PS.LocalHeader.IgnoreSizes then
      ZIP_PS.LocalHeader.IgnoreCompressionMethod := False;
    ZIP_PS.LocalHeader.IgnoreFileName := False;
  end
else
  begin
    // both central directory and local headers not ignored
    If ZIP_PS.CentralDirectory.IgnoreCompressionMethod and ZIP_PS.CentralDirectory.IgnoreSizes and
      ZIP_PS.LocalHeader.IgnoreCompressionMethod and ZIP_PS.LocalHeader.IgnoreSizes then
      ZIP_PS.CentralDirectory.IgnoreCompressionMethod := False;
  end;
end;

//------------------------------------------------------------------------------

procedure RectifySCSProcessingSettings(var SCS_PS: TDART_PS_SCS);
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure EnsureThreadSafety(var APS: TDARTArchiveProcessingSettings);
var
  i:  Integer;
begin
UniqueString(APS.Common.ArchivePath);
UniqueString(APS.Common.TargetPath);
SetLength(APS.SCS.PathResolve.CustomPaths,Length(APS.SCS.PathResolve.CustomPaths));
SetLength(APS.SCS.PathResolve.HelpArchives,Length(APS.SCS.PathResolve.HelpArchives));
For i := Low(APS.SCS.PathResolve.CustomPaths) to High(APS.SCS.PathResolve.CustomPaths) do
  UniqueString(APS.SCS.PathResolve.CustomPaths[i]);
For i := Low(APS.SCS.PathResolve.HelpArchives) to High(APS.SCS.PathResolve.HelpArchives) do
  UniqueString(APS.SCS.PathResolve.HelpArchives[i]);
end;

end.
