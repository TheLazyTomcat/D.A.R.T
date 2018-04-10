{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_ProcessingSettings;

{$INCLUDE DART_defs.inc}

interface

type
  TDARTKnownArchiveType = (katUnknown,katSCS,katZIP);
  TDARTArchiveType      = (atUnknown,atSCS_sig,atSCS_frc,atZIP_sig,atZIP_frc,atZIP_dft);
  TDARTRepairMethod     = (rmUnknown,rmRebuild,rmExtract,rmConvert);

const
  DART_KnownArchiveTypeStrings: array[TDARTKnownArchiveType] of String =
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
    ConvertTo:              TDARTKnownArchiveType;
    TargetPath:             String;
    IgnoreArchiveSignature: Boolean;
    InMemoryProcessing:     Boolean;
    IgnoreErroneousEntries: Boolean;
  end;

  TDART_PS_Auxiliaty = record
    InMemoryProcessingAllowed:  Boolean;  // depends on available memory and size of input archive
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
    EntryTabInMem:  Boolean;
    Entry:          TDART_PS_SCS_EntrySettings;
    PathResolve:    TDART_PS_SCS_PathResolveSettings;
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

//--- Main structure -----------------------------------------------------------

  TDARTArchiveProcessingSettings = record
    Common:       TDART_PS_Common;
    SCS:          TDART_PS_SCS;
    ZIP:          TDART_PS_ZIP;
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
    SCS:  (
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
        ParseContent:               False;
        BruteForce:                 False));      
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
    Auxiliary: (
      InMemoryProcessingAllowed: False));

//==============================================================================

//--- Processing settings functions --------------------------------------------

procedure RectifyArchiveProcessingSettings(var APS: TDARTArchiveProcessingSettings; RectifyInnerStructs: Boolean = False);

procedure RectifyZIPProcessingSettings(var ZIP_PS: TDART_PS_ZIP);
procedure RectifySCSProcessingSettings(var {%H-}SCS_PS: TDART_PS_SCS);

procedure EnsureThreadSafety(var APS: TDARTArchiveProcessingSettings); overload;

//--- Processing settings to/from INI files ------------------------------------

procedure SaveToIniFile(const IniFileName: String; const APS: TDARTArchiveProcessingSettings);

procedure LoadFromIniFile(const IniFileName: String; var APS: TDARTArchiveProcessingSettings);

implementation

uses
  SysUtils, IniFiles, TypInfo;

procedure RectifyArchiveProcessingSettings(var APS: TDARTArchiveProcessingSettings; RectifyInnerStructs: Boolean = False);
begin
If not APS.Auxiliary.InMemoryProcessingAllowed then
  APS.Common.InMemoryProcessing := False;
If RectifyInnerStructs then
  begin
    RectifyZIPProcessingSettings(APS.ZIP);
    RectifySCSProcessingSettings(APS.SCS);
  end;
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

//==============================================================================

procedure SaveToIniFile(const IniFileName: String; const APS: TDARTArchiveProcessingSettings);
var
  i:  Integer;
begin
with TIniFile.Create(IniFileName) do
try
  // common settings - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  with APS.Common do
    begin
      WriteString('Common','ArchivePath',ArchivePath);
      WriteString('Common','OriginalArchiveType',GetEnumName(TypeInfo(TDARTArchiveType),Ord(OriginalArchiveType)));
      WriteString('Common','SelectedArchiveType',GetEnumName(TypeInfo(TDARTArchiveType),Ord(SelectedArchiveType)));
      WriteString('Common','RepairMethod',GetEnumName(TypeInfo(TDARTRepairMethod),Ord(RepairMethod)));
      WriteString('Common','ConvertTo',GetEnumName(TypeInfo(TDARTKnownArchiveType),Ord(ConvertTo)));
      WriteString('Common','TargetPath',TargetPath);
      WriteBool('Common','IgnoreArchiveSignature',IgnoreArchiveSignature);
      WriteBool('Common','InMemoryProcessing',InMemoryProcessing);
      WriteBool('Common','IgnoreErroneousEntries',IgnoreErroneousEntries);
    end;

  // SCS settings  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  WriteBool('SCS','EntryTabInMem',APS.SCS.EntryTabInMem);

  WriteBool('SCS_Entry','IgnoreCRC32',APS.SCS.Entry.IgnoreCRC32);
  WriteBool('SCS_Entry','IgnoreCompressionFlag',APS.SCS.Entry.IgnoreCompressionFlag);
  WriteBool('SCS_Entry','IgnoreDictionaryID',APS.SCS.Entry.IgnoreDictionaryID);

  WriteBool('SCS_PathResolve','AssumeCityHash',APS.SCS.PathResolve.AssumeCityHash);
  WriteBool('SCS_PathResolve','UsePredefinedPaths',APS.SCS.PathResolve.UsePredefinedPaths);
  WriteBool('SCS_PathResolve','ExtractedUnresolvedEntries',APS.SCS.PathResolve.ExtractedUnresolvedEntries);
  WriteInteger('SCS_PathResolve','CustomPaths',Length(APS.SCS.PathResolve.CustomPaths));
  For i := Low(APS.SCS.PathResolve.CustomPaths) to High(APS.SCS.PathResolve.CustomPaths) do
    WriteString('SCS_PathResolve',Format('CustomPaths[%d]',[i]),APS.SCS.PathResolve.CustomPaths[i]);
  WriteInteger('SCS_PathResolve','HelpArchives',Length(APS.SCS.PathResolve.HelpArchives));
  For i := Low(APS.SCS.PathResolve.HelpArchives) to High(APS.SCS.PathResolve.HelpArchives) do
    WriteString('SCS_PathResolve',Format('HelpArchives[%d]',[i]),APS.SCS.PathResolve.HelpArchives[i]);

  // ZIP settings  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  WriteBool('ZIP','AssumeCompressionMethod',APS.ZIP.AssumeCompressionMethod);

  with APS.ZIP.EndOfCentralDirectory do
    begin
      WriteBool('ZIP_EndOfCentralDirectory','IgnoreEndOfCentralDirectory',IgnoreEndOfCentralDirectory);
      WriteBool('ZIP_EndOfCentralDirectory','IgnoreDiskSplit',IgnoreDiskSplit);
      WriteBool('ZIP_EndOfCentralDirectory','IgnoreNumberOfEntries',IgnoreNumberOfEntries);
      WriteBool('ZIP_EndOfCentralDirectory','IgnoreCentralDirectoryOffset',IgnoreCentralDirectoryOffset);
      WriteBool('ZIP_EndOfCentralDirectory','IgnoreComment',IgnoreComment);
      WriteBool('ZIP_EndOfCentralDirectory','LimitSearch',LimitSearch);
    end;

  with APS.ZIP.CentralDirectory do
    begin
      WriteBool('ZIP_CentralDirectory','IgnoreCentralDirectory',IgnoreCentralDirectory);
      WriteBool('ZIP_CentralDirectory','IgnoreSignature',IgnoreSignature);
      WriteBool('ZIP_CentralDirectory','IgnoreVersions',IgnoreVersions);
      WriteBool('ZIP_CentralDirectory','ClearEncryptionFlags',ClearEncryptionFlags);
      WriteBool('ZIP_CentralDirectory','IgnoreCompressionMethod',IgnoreCompressionMethod);
      WriteBool('ZIP_CentralDirectory','IgnoreModTime',IgnoreModTime);
      WriteBool('ZIP_CentralDirectory','IgnoreModDate',IgnoreModDate);
      WriteBool('ZIP_CentralDirectory','IgnoreCRC32',IgnoreCRC32);
      WriteBool('ZIP_CentralDirectory','IgnoreSizes',IgnoreSizes);
      WriteBool('ZIP_CentralDirectory','IgnoreInternalFileAttributes',IgnoreInternalFileAttributes);
      WriteBool('ZIP_CentralDirectory','IgnoreExternalFileAttributes',IgnoreExternalFileAttributes);
      WriteBool('ZIP_CentralDirectory','IgnoreLocalHeaderOffset',IgnoreLocalHeaderOffset);
      WriteBool('ZIP_CentralDirectory','IgnoreExtraField',IgnoreExtraField);
      WriteBool('ZIP_CentralDirectory','IgnoreFileComment',IgnoreFileComment);
    end;

  with APS.ZIP.LocalHeader do
    begin
      WriteBool('ZIP_LocalHeader','IgnoreLocalHeaders',IgnoreLocalHeaders);
      WriteBool('ZIP_LocalHeader','IgnoreSignature',IgnoreSignature);
      WriteBool('ZIP_LocalHeader','IgnoreVersions',IgnoreVersions);
      WriteBool('ZIP_LocalHeader','ClearEncryptionFlags',ClearEncryptionFlags);
      WriteBool('ZIP_LocalHeader','IgnoreCompressionMethod',IgnoreCompressionMethod);
      WriteBool('ZIP_LocalHeader','IgnoreModTime',IgnoreModTime);
      WriteBool('ZIP_LocalHeader','IgnoreModDate',IgnoreModDate);
      WriteBool('ZIP_LocalHeader','IgnoreCRC32',IgnoreCRC32);
      WriteBool('ZIP_LocalHeader','IgnoreSizes',IgnoreSizes);
      WriteBool('ZIP_LocalHeader','IgnoreFileName',IgnoreFileName);
      WriteBool('ZIP_LocalHeader','IgnoreExtraField',IgnoreExtraField);
      WriteBool('ZIP_LocalHeader','IgnoreDataDescriptor',IgnoreDataDescriptor);
    end;

  // auxiliary settings is not saved - - - - - - - - - - - - - - - - - - - - - -
finally
  Free;
end;
end;

//------------------------------------------------------------------------------

procedure LoadFromIniFile(const IniFileName: String; var APS: TDARTArchiveProcessingSettings);
var
  TempArchiveType:    TDARTArchiveType;
  TempRepairMethod:   TDARTRepairMethod;
  TempKnownArchType:  TDARTKnownArchiveType;
  i:                  Integer;
begin
with TIniFile.Create(IniFileName) do
try
  // common settings - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  with APS.Common do
    begin
      ArchivePath := ReadString('Common','ArchivePath',ArchivePath);
      TempArchiveType := TDARTArchiveType(GetEnumValue(TypeInfo(TDARTArchiveType),ReadString('Common','OriginalArchiveType','')));
      If (TempArchiveType >= Low(TDARTArchiveType)) and (TempArchiveType <= High(TDARTArchiveType)) then
        OriginalArchiveType := TempArchiveType;
      TempArchiveType := TDARTArchiveType(GetEnumValue(TypeInfo(TDARTArchiveType),ReadString('Common','SelectedArchiveType','')));
      If (TempArchiveType >= Low(TDARTArchiveType)) and (TempArchiveType <= High(TDARTArchiveType)) then
        SelectedArchiveType := TempArchiveType;
      TempRepairMethod := TDARTRepairMethod(GetEnumValue(TypeInfo(TDARTRepairMethod),ReadString('Common','RepairMethod','')));
      If (TempRepairMethod >= Low(TDARTRepairMethod)) and (TempRepairMethod <= High(TDARTRepairMethod)) then
        RepairMethod := TempRepairMethod;
      TempKnownArchType := TDARTKnownArchiveType(GetEnumValue(TypeInfo(TDARTKnownArchiveType),ReadString('Common','ConvertTo','')));
      If (TempKnownArchType >= Low(TDARTKnownArchiveType)) and (TempKnownArchType <= High(TDARTKnownArchiveType)) then
        ConvertTo := TempKnownArchType;
      TargetPath := ReadString('Common','TargetPath',TargetPath);
      IgnoreArchiveSignature := ReadBool('Common','IgnoreArchiveSignature',IgnoreArchiveSignature);
      InMemoryProcessing := ReadBool('Common','InMemoryProcessing',InMemoryProcessing);
      IgnoreErroneousEntries := ReadBool('Common','IgnoreErroneousEntries',IgnoreErroneousEntries);
    end;

  // SCS settings  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  APS.SCS.EntryTabInMem := ReadBool('SCS','EntryTabInMem',APS.SCS.EntryTabInMem);

  APS.SCS.Entry.IgnoreCRC32 :=  ReadBool('SCS_Entry','IgnoreCRC32',APS.SCS.Entry.IgnoreCRC32);
  APS.SCS.Entry.IgnoreCompressionFlag := ReadBool('SCS_Entry','IgnoreCompressionFlag',APS.SCS.Entry.IgnoreCompressionFlag);
  APS.SCS.Entry.IgnoreDictionaryID := ReadBool('SCS_Entry','IgnoreDictionaryID',APS.SCS.Entry.IgnoreDictionaryID);

  APS.SCS.PathResolve.AssumeCityHash := ReadBool('SCS_PathResolve','AssumeCityHash',APS.SCS.PathResolve.AssumeCityHash);
  APS.SCS.PathResolve.UsePredefinedPaths := ReadBool('SCS_PathResolve','UsePredefinedPaths',APS.SCS.PathResolve.UsePredefinedPaths);
  APS.SCS.PathResolve.ExtractedUnresolvedEntries := ReadBool('SCS_PathResolve','ExtractedUnresolvedEntries',APS.SCS.PathResolve.ExtractedUnresolvedEntries);
  SetLength(APS.SCS.PathResolve.CustomPaths,ReadInteger('SCS_PathResolve','CustomPaths',0));
  For i := Low(APS.SCS.PathResolve.CustomPaths) to High(APS.SCS.PathResolve.CustomPaths) do
    APS.SCS.PathResolve.CustomPaths[i] := ReadString('SCS_PathResolve',Format('CustomPaths[%d]',[i]),'');
  SetLength(APS.SCS.PathResolve.HelpArchives,ReadInteger('SCS_PathResolve','HelpArchives',0));
  For i := Low(APS.SCS.PathResolve.HelpArchives) to High(APS.SCS.PathResolve.HelpArchives) do
    APS.SCS.PathResolve.HelpArchives[i] := ReadString('SCS_PathResolve',Format('HelpArchives[%d]',[i]),'');

  // ZIP settings  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  APS.ZIP.AssumeCompressionMethod := ReadBool('ZIP','AssumeCompressionMethod',APS.ZIP.AssumeCompressionMethod);

  with APS.ZIP.EndOfCentralDirectory do
    begin
      IgnoreEndOfCentralDirectory := ReadBool('ZIP_EndOfCentralDirectory','IgnoreEndOfCentralDirectory',IgnoreEndOfCentralDirectory);
      IgnoreDiskSplit := ReadBool('ZIP_EndOfCentralDirectory','IgnoreDiskSplit',IgnoreDiskSplit);
      IgnoreNumberOfEntries := ReadBool('ZIP_EndOfCentralDirectory','IgnoreNumberOfEntries',IgnoreNumberOfEntries);
      IgnoreCentralDirectoryOffset := ReadBool('ZIP_EndOfCentralDirectory','IgnoreCentralDirectoryOffset',IgnoreCentralDirectoryOffset);
      IgnoreComment := ReadBool('ZIP_EndOfCentralDirectory','IgnoreComment',IgnoreComment);
      LimitSearch := ReadBool('ZIP_EndOfCentralDirectory','LimitSearch',LimitSearch);
    end;

  with APS.ZIP.CentralDirectory do
    begin
      IgnoreCentralDirectory := ReadBool('ZIP_CentralDirectory','IgnoreCentralDirectory',IgnoreCentralDirectory);
      IgnoreSignature := ReadBool('ZIP_CentralDirectory','IgnoreSignature',IgnoreSignature);
      IgnoreVersions := ReadBool('ZIP_CentralDirectory','IgnoreVersions',IgnoreVersions);
      ClearEncryptionFlags := ReadBool('ZIP_CentralDirectory','ClearEncryptionFlags',ClearEncryptionFlags);
      IgnoreCompressionMethod := ReadBool('ZIP_CentralDirectory','IgnoreCompressionMethod',IgnoreCompressionMethod);
      IgnoreModTime := ReadBool('ZIP_CentralDirectory','IgnoreModTime',IgnoreModTime);
      IgnoreModDate := ReadBool('ZIP_CentralDirectory','IgnoreModDate',IgnoreModDate);
      IgnoreCRC32 := ReadBool('ZIP_CentralDirectory','IgnoreCRC32',IgnoreCRC32);
      IgnoreSizes := ReadBool('ZIP_CentralDirectory','IgnoreSizes',IgnoreSizes);
      IgnoreInternalFileAttributes := ReadBool('ZIP_CentralDirectory','IgnoreInternalFileAttributes',IgnoreInternalFileAttributes);
      IgnoreExternalFileAttributes := ReadBool('ZIP_CentralDirectory','IgnoreExternalFileAttributes',IgnoreExternalFileAttributes);
      IgnoreLocalHeaderOffset := ReadBool('ZIP_CentralDirectory','IgnoreLocalHeaderOffset',IgnoreLocalHeaderOffset);
      IgnoreExtraField := ReadBool('ZIP_CentralDirectory','IgnoreExtraField',IgnoreExtraField);
      IgnoreFileComment := ReadBool('ZIP_CentralDirectory','IgnoreFileComment',IgnoreFileComment);
    end;

  with APS.ZIP.LocalHeader do
    begin
      IgnoreLocalHeaders := ReadBool('ZIP_LocalHeader','IgnoreLocalHeaders',IgnoreLocalHeaders);
      IgnoreSignature := ReadBool('ZIP_LocalHeader','IgnoreSignature',IgnoreSignature);
      IgnoreVersions := ReadBool('ZIP_LocalHeader','IgnoreVersions',IgnoreVersions);
      ClearEncryptionFlags := ReadBool('ZIP_LocalHeader','ClearEncryptionFlags',ClearEncryptionFlags);
      IgnoreCompressionMethod := ReadBool('ZIP_LocalHeader','IgnoreCompressionMethod',IgnoreCompressionMethod);
      IgnoreModTime := ReadBool('ZIP_LocalHeader','IgnoreModTime',IgnoreModTime);
      IgnoreModDate := ReadBool('ZIP_LocalHeader','IgnoreModDate',IgnoreModDate);
      IgnoreCRC32 := ReadBool('ZIP_LocalHeader','IgnoreCRC32',IgnoreCRC32);
      IgnoreSizes := ReadBool('ZIP_LocalHeader','IgnoreSizes',IgnoreSizes);
      IgnoreFileName := ReadBool('ZIP_LocalHeader','IgnoreFileName',IgnoreFileName);
      IgnoreExtraField := ReadBool('ZIP_LocalHeader','IgnoreExtraField',IgnoreExtraField);
      IgnoreDataDescriptor := ReadBool('ZIP_LocalHeader','IgnoreDataDescriptor',IgnoreDataDescriptor);
    end;

  // auxiliary settings is not saved - - - - - - - - - - - - - - - - - - - - - -
finally
  Free;
end;
end;

end.
