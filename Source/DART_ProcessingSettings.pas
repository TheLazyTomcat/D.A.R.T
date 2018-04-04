unit DART_ProcessingSettings;

{$INCLUDE DART_defs.inc}

interface

type
  TDARTArchiveType = (atUnknown,atSCS_sig,atSCS_frc,atZIP_sig,atZIP_frc,atZIP_dft,
                      atSCS = atSCS_sig,atZIP = atZIP_sig);
  TDARTRepairMethod = (rmUnknown,rmRebuild,rmExtract,rmConvert);

const
  DART_ArchiveTypeStrings: array[TDARTArchiveType] of String =
    ('Unknown','SCS#','SCS# (forced)','ZIP','ZIP (forced)','ZIP (defaulted)');

  DART_RepairMethodStrings: array[TDARTRepairMethod] of String =
    ('Unknown','Rebuild archive','Extract archive','Convert archive');

//--- Common processing settings -----------------------------------------------

type
  TDART_PS_Common = record
    ArchivePath:            String;
    OriginalArchiveType:    TDARTArchiveType;
    SelectedArchiveType:    TDARTArchiveType;
    RepairMethod:           TDARTRepairMethod;
    ConvertTo:              TDARTArchiveType;
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
    AssumeCompressionMethods: Boolean;
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
    // temporary fields...
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
      ConvertTo:              atUnknown;
      TargetPath:             '';
      IgnoreArchiveSignature: True;
      InMemoryProcessing:     False;
      IgnoreErroneousEntries: False);
    ZIP: (
      AssumeCompressionMethods: False;
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
procedure RectifySCSProcessingSettings(var SCS_PS: TDART_PS_SCS);

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
If ZIP_PS.CentralDirectory.IgnoreCentralDirectory or ZIP_PS.CentralDirectory.IgnoreLocalHeaderOffset then
  ZIP_PS.LocalHeader.IgnoreLocalHeaders := False;
If ZIP_PS.CentralDirectory.IgnoreCentralDirectory then
  begin
    If ZIP_PS.LocalHeader.IgnoreCompressionMethod then
      ZIP_PS.LocalHeader.IgnoreSizes := False;
    If ZIP_PS.LocalHeader.IgnoreSizes then
      ZIP_PS.LocalHeader.IgnoreCompressionMethod := False;
    ZIP_PS.LocalHeader.IgnoreFileName := False;
  end
else
  begin
    If ZIP_PS.CentralDirectory.IgnoreCompressionMethod then
      ZIP_PS.CentralDirectory.IgnoreSizes := False;
    If ZIP_PS.CentralDirectory.IgnoreSizes then
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
