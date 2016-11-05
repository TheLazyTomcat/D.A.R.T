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
  BitOps,
  DART_MemoryBuffer, DART_Repairer;

procedure TRepairer_SCS_Rebuild.SCS_DiscardDirectories;
var
  i,j:        Integer;
  EntryCount: Integer;
begin
// removes all resolved directory entries - they will be reconstructed from file names
EntryCount := Length(fArchiveStructure.Entries);
For i := High(fArchiveStructure.Entries) downto Low(fArchiveStructure.Entries) do
  with fArchiveStructure.Entries[i] do
    begin
      If GetFlagState(Bin.Flags,SCS_FLAG_Directory) and UtilityData.Resolved then
        begin
          For j := i to (EntryCount - 2) do
            fArchiveStructure.Entries[j] := fArchiveStructure.Entries[j + 1];
          Dec(EntryCount);
        end;
      DoProgress(PROCSTAGEIDX_NoProgress,0.0);   
    end;
SetLength(fArchiveStructure.Entries,EntryCount);
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS_Rebuild.SCS_ReconstructDirectories;
begin
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS_Rebuild.SCS_RebuildArchive;
var
  ProcessedBytes:       UInt64;
  DataOffset:           Int64;
  RebuildArchiveStream: TStream;
  i:                    Integer;
begin
DoProgress(PROCSTAGEIDX_SCS_EntriesProcessing,0.0);
// reconstruct all directory entries from file names
SCS_DiscardDirectories;
SCS_ReconstructDirectories;
SCS_SortEntries;
// create directory for the rebuild file
{$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
ForceDirectoriesUTF8(ExtractFileDir(fFileProcessingSettings.Common.TargetPath));
{$ELSE}
ForceDirectories(ExtractFileDir(fFileProcessingSettings.Common.TargetPath));
{$IFEND}
// create output stream
If fFileProcessingSettings.Common.InMemoryProcessing then
  RebuildArchiveStream := TMemoryStream.Create
else
  RebuildArchiveStream := CreateFileStream(fFileProcessingSettings.Common.TargetPath,fmCreate or fmShareDenyWrite);
try
  // prepare output stream
  If fFileProcessingSettings.Common.InMemoryProcessing then
    RebuildArchiveStream.Size := Trunc(fArchiveStream.Size * 1.1);
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
        {$message 'implement'}
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
  {$message 'implement'}
  // finalize
  RebuildArchiveStream.Size := RebuildArchiveStream.Position;
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
SCS_RebuildArchive;
end;

//==============================================================================

class Function TRepairer_SCS_Rebuild.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  200:  Result := 'ArchiveProcessing';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

end.
