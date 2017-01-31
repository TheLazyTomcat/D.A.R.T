{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_SCS_Rebuild;

{$INCLUDE DART_defs.inc}

interface

uses
  DART_Repairer_SCS;

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TRepairer_SCS_Rebuild                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TRepairer_SCS_Rebuild - class declaration                                  }
{==============================================================================}
type
  TRepairer_SCS_Rebuild = class(TRepairer_SCS)
  protected
    procedure SCS_DiscardDirectories; virtual;
    procedure SCS_ReconstructDirectories; virtual;
    procedure SCS_RebuildArchive; virtual;
    procedure ArchiveProcessing; override;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;      
  end;

implementation

uses
  SysUtils, Classes,
  BitOps, CRC32,
  DART_MemoryBuffer, DART_PathDeconstructor, DART_Format_SCS, DART_Repairer
{$IFDEF FPC_NonUnicode_NoUTF8RTL}
  , LazFileUtils
{$ENDIF};

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TRepairer_SCS_Rebuild                            }
{------------------------------------------------------------------------------}
{==============================================================================}

const
(*
  Maximum size of a directory entry that will not be compressed, in bytes.
  If the entry is larger, it will be compressed in the output, otherwise it will
  be stored with no compression.

  When set to 0, all directory entries will be compressed.
*)
  DirEntryUncompMaxSize = 32;

{==============================================================================}
{   TRepairer_SCS_Rebuild - class implementation                               }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TRepairer_SCS_Rebuild - protected methods                                  }
{------------------------------------------------------------------------------}

procedure TRepairer_SCS_Rebuild.SCS_DiscardDirectories;
var
  EntryCount: Integer;
  i:          Integer;
begin
// removes all resolved directory entries - they will be reconstructed from file names
EntryCount := Low(fArchiveStructure.Entries);
For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
  with fArchiveStructure.Entries[i] do
  begin
    If not(GetFlagState(Bin.Flags,SCS_FLAG_Directory) and UtilityData.Resolved) then
      begin
        If EntryCount <> i then
          fArchiveStructure.Entries[EntryCount] := fArchiveStructure.Entries[i];
        Inc(EntryCount);
      end;
    DoProgress(PROCSTAGEIDX_NoProgress,0.0);
  end;
SetLength(fArchiveStructure.Entries,EntryCount);
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS_Rebuild.SCS_ReconstructDirectories;
var
  PathDeconstructor:  TPathDeconstructor;
  i,j:                Integer;
begin
PathDeconstructor := TPathDeconstructor.Create;
try
  // deconstruct path of all resolved entries
  For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
    begin
      If fArchiveStructure.Entries[i].UtilityData.Resolved then
        PathDeconstructor.DeconstructFilePath(fArchiveStructure.Entries[i].FileName);
      // this is the slowest part of path reconstruction, so progress is done here
      DoProgress(PROCSTAGEIDX_SCS_PathsLoading_DirsRect,(i + 1) / Length(fArchiveStructure.Entries));
    end;
  PathDeconstructor.Sort;
  // store deconstructed paths into entries
  SetLength(fArchiveStructure.Entries,Length(fArchiveStructure.Entries) + PathDeconstructor.Count);
  For i := 0 to Pred(PathDeconstructor.Count) do
    with fArchiveStructure.Entries[High(fArchiveStructure.Entries) - i] do
      begin
        FileName := PathDeconstructor[i].FullPath;
        Bin.Hash := SCS_EntryFileNameHash(FileName);
        Bin.Flags := SCS_FLAG_Unknown or SCS_FLAG_Directory;
        // other bin fields will be filled when the item is written
        UtilityData.Resolved := True;
        UtilityData.Erroneous := False;
        SetLength(UtilityData.SubEntries,PathDeconstructor[i].SubNodeCount +
                                         PathDeconstructor[i].FileCount);
        For j := 0 to Pred(PathDeconstructor[i].SubNodeCount) do
          UtilityData.SubEntries[j] := '*' + PathDeconstructor[i].SubNodes[j].Name;
        For j := 0 to Pred(PathDeconstructor[i].FileCount) do
          UtilityData.SubEntries[PathDeconstructor[i].SubNodeCount + j] := PathDeconstructor[i].Files[j];
        DoProgress(PROCSTAGEIDX_NoProgress,0.0);
      end;
finally
  PathDeconstructor.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS_Rebuild.SCS_RebuildArchive;
var
  ProcessedBytes:       UInt64;
  DataOffset:           Int64;
  RebuildArchiveStream: TStream;
  i,j:                  Integer;
  CompressedBuff:       Pointer;
  CompressedSize:       Integer;
  DecompressedBuff:     Pointer;
  DecompressedSize:     Integer;
  DirEntryStr:          AnsiString;

  // manages unresolved entries extraction
  procedure ExtractUnresolvedEntry(EntryIdx: Integer; Data: Pointer; DataSize: Integer);
  var
    EntryExtractFile: TFileStream;
    EntryFileName:    String;
  begin
    with fArchiveStructure.Entries[EntryIdx] do
      If not UtilityData.Resolved then
        begin
          If fProcessingSettings.PathResolve.ExtractedUnresolvedEntries then
            begin
              // raw data will be saved to a file
              DoWarning(Format('File name of entry #%d (0x%.16x) could not be resolved, extracting entry data.',[i,Bin.Hash,Bin.UncompressedSize]));
              EntryFileName := IncludeTrailingPathDelimiter(ExtractFilePath(fFileProcessingSettings.Common.TargetPath) + 'unresolved_' +
                               ChangeFileExt(ExtractFileName(fFileProcessingSettings.Common.TargetPath),'')) + Format('%s(%.16x)',[SCS_HashName,Bin.Hash]);
              If GetFlagState(Bin.Flags,SCS_FLAG_Directory) then
                EntryFileName := EntryFileName + 'D'
              else
                EntryFileName := EntryFileName + 'F';
            {$IFDEF FPC_NonUnicode_NoUTF8RTL}
              ForceDirectoriesUTF8(ExtractFileDir(EntryFileName));
            {$ELSE}
              ForceDirectories(ExtractFileDir(EntryFileName));
            {$ENDIF}
              EntryExtractFile := CreateFileStream(EntryFileName,fmCreate or fmShareDenyWrite);
              try
                EntryExtractFile.WriteBuffer(Data^,DataSize);
              finally
                EntryExtractFile.Free;
              end;
            end
          // data will not be extracted...
          else DoWarning(Format('File name of entry #%d (0x%.16x) could not be resolved.',[i,Bin.Hash]));
        end;
  end;

begin
DoProgress(PROCSTAGEIDX_SCS_EntriesProcessing,0.0);
// create directory for the rebuild file
{$IFDEF FPC_NonUnicode_NoUTF8RTL}
ForceDirectoriesUTF8(ExtractFileDir(fFileProcessingSettings.Common.TargetPath));
{$ELSE}
ForceDirectories(ExtractFileDir(fFileProcessingSettings.Common.TargetPath));
{$ENDIF}
// create output stream
If fFileProcessingSettings.Common.InMemoryProcessing then
  RebuildArchiveStream := TMemoryStream.Create
else
  RebuildArchiveStream := CreateFileStream(fFileProcessingSettings.Common.TargetPath,fmCreate or fmShareDenyWrite);
try
  // prepare output stream
  If fFileProcessingSettings.Common.InMemoryProcessing then
    RebuildArchiveStream.Size := Trunc(fArchiveStream.Size * 1.1);
  RebuildArchiveStream.Seek(0,soFromBeginning);   
  ProcessedBytes := 0;
  // set stream position at the start of data, header and entries table will be
  // written later (for now fill the area with zeroes)
  RebuildArchiveStream.Seek(0,soFromBeginning);
  DataOffset := SCS_DefaultEntryTableOffset + Length(fArchiveStructure.Entries) * SizeOf(TSCS_EntryRecord);
  DART_MemoryBuffer.ReallocateMemoryBuffer(fCED_Buffer,DataOffset);
  FillChar(fCED_Buffer.Memory^,DataOffset,0);
  RebuildArchiveStream.WriteBuffer(fCED_Buffer.Memory^,DataOffset);
  // traverse entries and process them...
  For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
    with fArchiveStructure.Entries[i] do
      try
        // calculate entry processing progress info
        SCS_PrepareEntryProgressInfo(i,ProcessedBytes);
        DoProgress(PROCSTAGEIDX_SCS_EntryProcessing,0.0);
        If GetFlagState(Bin.Flags,SCS_FLAG_Directory) and UtilityData.Resolved then
          begin
            // entry is a resolved directory
            // build the entry data
            DirEntryStr := '';
            For j := Low(UtilityData.SubEntries) to High(UtilityData.SubEntries) do
              If j < High(UtilityData.SubEntries) then
                DirEntryStr := DirEntryStr + UtilityData.SubEntries[j] + #10{LF}
              else
                DirEntryStr := DirEntryStr + UtilityData.SubEntries[j];
            // fill missing info
            Bin.DataOffset := UInt64(RebuildArchiveStream.Position);
            Bin.CRC32 := AnsiStringCRC32(DirEntryStr);
            Bin.UncompressedSize := Length(DirEntryStr);
            If Bin.UncompressedSize > DirEntryUncompMaxSize then
              begin
                ProgressedCompressBuffer(PChar(DirEntryStr),Length(DirEntryStr),
                  CompressedBuff,CompressedSize,PROCSTAGEIDX_NoProgress,
                {$IFDEF FPC}
                  {$IFDEF Unicode}UTF8Decode(FileName),{$ELSE}FileName,{$ENDIF}
                {$ELSE}String(UTF8ToAnsi(FileName)),{$ENDIF}WINDOWBITS_ZLib);
                try
                  Bin.CompressedSize := CompressedSize;
                  SetFlagValue(Bin.Flags,SCS_FLAG_Compressed);
                  RebuildArchiveStream.WriteBuffer(CompressedBuff^,CompressedSize);
                finally
                  FreeMem(CompressedBuff,CompressedSize);
                end;
              end
            else
              begin
                Bin.CompressedSize := Length(DirEntryStr);
                ResetFlagValue(Bin.Flags,SCS_FLAG_Compressed);
                RebuildArchiveStream.WriteBuffer(PChar(DirEntryStr)^,Length(DirEntryStr));
              end;
            DoProgress(PROCSTAGEIDX_NoProgress,0.0);
          end
        else
          begin
            // entry is a file or unresolved directory
            // store original data offset
            UtilityData.OriginalDataOffset := Bin.DataOffset;
            // prepare buffer that will hold compressed data
            ReallocateMemoryBuffer(fCED_Buffer,Bin.CompressedSize);
            // load compressed data from input file
            fArchiveStream.Seek(Bin.DataOffset,soFromBeginning);
            ProgressedStreamRead(fArchiveStream,fCED_Buffer.Memory,Bin.CompressedSize,PROCSTAGEIDX_SCS_EntryLoading);
            If ((Bin.UncompressedSize <> 0) and SameCRC32(Bin.CRC32,0)) or not UtilityData.Resolved then
              begin
                // a new CRC32 checksum is required or the entry is not resolved (might need extraction)
                If GetFlagState(Bin.Flags,SCS_FLAG_Compressed) then
                  begin
                    // data needs to be decompressed for CRC32 calculation
                    ProgressedDecompressBuffer(fCED_Buffer.Memory,Bin.CompressedSize,
                      DecompressedBuff,DecompressedSize,PROCSTAGEIDX_SCS_EntryDecompressing,
                    {$IFDEF FPC}
                      {$IFDEF Unicode}UTF8Decode(FileName),{$ELSE}FileName,{$ENDIF}
                    {$ELSE}String(UTF8ToAnsi(FileName)),{$ENDIF}WINDOWBITS_ZLib);
                    try
                      ExtractUnresolvedEntry(i,DecompressedBuff,DecompressedSize);
                      If (Bin.UncompressedSize <> 0) and SameCRC32(Bin.CRC32,0) then
                        Bin.CRC32 := BufferCRC32(DecompressedBuff^,DecompressedSize);
                    finally
                      FreeMem(DecompressedBuff,DecompressedSize);
                    end;
                  end
                else
                  begin
                    ExtractUnresolvedEntry(i,fCED_Buffer.Memory,Bin.CompressedSize);
                    If (Bin.UncompressedSize <> 0) and SameCRC32(Bin.CRC32,0) then
                      Bin.CRC32 := BufferCRC32(fCED_Buffer.Memory^,Bin.CompressedSize);
                  end;
              end;
            // data offset is set to current position in output stream
            Bin.DataOffset := UInt64(RebuildArchiveStream.Position);
            // bin part does not need any more changes, save the data to output
            ProgressedStreamWrite(RebuildArchiveStream,fCED_Buffer.Memory,Bin.CompressedSize,PROCSTAGEIDX_SCS_EntrySaving);
            Inc(ProcessedBytes,Bin.CompressedSize);
          end;
        DoProgress(PROCSTAGEIDX_SCS_EntryProcessing,1.0);
      except
        on E: Exception do
          begin
            UtilityData.Erroneous := True;
            If fFileProcessingSettings.Common.IgnoreErroneousEntries and not fTerminating then
              begin
                Resume;
                DoWarning(Format('%s: %s',[E.ClassName,E.Message]));
              end
            else raise;
          end;
      end;
  RebuildArchiveStream.Size := RebuildArchiveStream.Position;
  // fill archive header and write it
  with fArchiveStructure.ArchiveHeader do
    begin
      Signature := FileSignature_SCS;
      Unknown := 1;
      HashType := SCS_HASH_City;
      Entries := 0;
      For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
        If not fArchiveStructure.Entries[i].UtilityData.Erroneous then Inc(Entries);
      EntriesOffset := SCS_DefaultEntryTableOffset;
      UnknownOffset := 0;
    end;
  RebuildArchiveStream.Seek(0,soFromBeginning);
  RebuildArchiveStream.WriteBuffer(fArchiveStructure.ArchiveHeader,SizeOf(TSCS_ArchiveHeader));
  // write entry table (erroneous entries are discarded)
  RebuildArchiveStream.Seek(fArchiveStructure.ArchiveHeader.EntriesOffset,soFromBeginning);
  For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
    with fArchiveStructure.Entries[i] do
      begin
        If not UtilityData.Erroneous then
          RebuildArchiveStream.WriteBuffer(Bin,SizeOf(TSCS_EntryRecord));
        DoProgress(PROCSTAGEIDX_NoProgress,0.0);
      end;
  // finalize
  DoProgress(PROCSTAGEIDX_SCS_EntriesProcessing,1.0);
  If fFileProcessingSettings.Common.InMemoryProcessing then
    ProgressedSaveFile(fFileProcessingSettings.Common.TargetPath,RebuildArchiveStream,PROCSTAGEIDX_Saving);
finally
  RebuildArchiveStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS_Rebuild.ArchiveProcessing;
begin
// check if target <> source
If AnsiSameText(fFileProcessingSettings.Common.FilePath,fFileProcessingSettings.Common.TargetPath) then
  DoError(200,'Output is directed into an input file, cannot proceed.');
inherited;
// reconstruct all directory entries from file names
fEntriesSorted := False;
SCS_DiscardDirectories;
SCS_ReconstructDirectories;
SCS_SortEntries;
DoProgress(PROCSTAGEIDX_SCS_PathsLoading,1.0);
SCS_RebuildArchive;
end;

{------------------------------------------------------------------------------}
{   TRepairer_SCS_Rebuild - public methods                                     }
{------------------------------------------------------------------------------}

class Function TRepairer_SCS_Rebuild.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  200:  Result := 'ArchiveProcessing';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

end.
