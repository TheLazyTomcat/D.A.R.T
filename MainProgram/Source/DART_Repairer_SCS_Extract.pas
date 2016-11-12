{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_SCS_Extract;

{$INCLUDE DART_defs.inc}

interface

uses
  DART_Repairer_SCS;

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TRepairer_SCS_Extract                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TRepairer_SCS_Extract - class declaration                                  }
{==============================================================================}
type
  TRepairer_SCS_Extract = class(TRepairer_SCS)
  protected
    procedure SCS_ExtractArchive; virtual;
    procedure ArchiveProcessing; override;
  end;

implementation

uses
  SysUtils, Classes, StrUtils,
  BitOps,
  DART_MemoryBuffer, DART_Repairer
{$IFDEF FPC_NonUnicode_NoUTF8RTL}
  , LazFileUtils
{$ENDIF};

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TRepairer_SCS_Extract                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TRepairer_SCS_Extract - class implementation                               }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TRepairer_SCS_Extract - protected methods                                  }
{------------------------------------------------------------------------------}

procedure TRepairer_SCS_Extract.SCS_ExtractArchive;
var

  ProcessedBytes:     UInt64;
  i:                  Integer;
  FullEntryFileName:  String;
  ForceExtract:       Boolean;
  EntryFileStream:    TFileStream;
  DecompressedBuff:   Pointer;
  DecompressedSize:   Integer;
begin
DoProgress(PROCSTAGEIDX_SCS_EntriesProcessing,0.0);
ProcessedBytes := 0;
// traverse all entries and extract them                    
For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
  with fArchiveStructure.Entries[i] do
    try
      // calculate entry processing progress info
      SCS_PrepareEntryProgressInfo(i,ProcessedBytes);
      Inc(ProcessedBytes,Bin.CompressedSize);
      DoProgress(PROCSTAGEIDX_SCS_EntryProcessing,0.0);
      ForceExtract := False;
      If UtilityData.Resolved then
        begin
          // file name of the entry was fully resolved, construct full path for entry output file
          FullEntryFileName := IncludeTrailingPathDelimiter(fFileProcessingSettings.Common.TargetPath) +
                             {$IFDEF FPC}
                               {$IFDEF Unicode}
                               AnsiReplaceStr(UTF8Decode(FileName),SCS_PathDelim,'\');
                               {$ELSE}
                               AnsiReplaceStr(FileName,SCS_PathDelim,'\');
                               {$ENDIF}
                             {$ELSE}
                               AnsiReplaceStr(String(UTF8ToAnsi(FileName)),SCS_PathDelim,'\');
                             {$ENDIF}
        end
      else
        begin
          // entry file name was not resolved...
          If fProcessingSettings.PathResolve.ExtractedUnresolvedEntries then
            begin
              // data will be saved to a special file, its name consits only of the hash
              FullEntryFileName := IncludeTrailingPathDelimiter(fFileProcessingSettings.Common.TargetPath) +
                                   Format('_unresolved_\%s(%.16x)',[SCS_HashName,Bin.Hash]);
              If GetFlagState(Bin.Flags,SCS_FLAG_Directory) then
                FullEntryFileName := FullEntryFileName + 'D'
              else
                FullEntryFileName := FullEntryFileName + 'F';
              DoWarning(Format('File name of entry #%d (0x%.16x) could not be resolved, extracting entry data.',[i,Bin.Hash]));
              ForceExtract := True;
            end
          else
            begin
              // data will not be saved...
              DoWarning(Format('File name of entry #%d (0x%.16x) could not be resolved, it will be skipped.',[i,Bin.Hash]));
              DoProgress(PROCSTAGEIDX_SCS_EntryProcessing,1.0);
              // ...skip to the next entry
              Continue{For i};
            end;
        end;
      // create necessary directory structure for the entry output file
    {$IFDEF FPC_NonUnicode_NoUTF8RTL}
      If not DirectoryExistsUTF8(ExtractFileDir(FullEntryFileName)) then
        ForceDirectoriesUTF8(ExtractFileDir(FullEntryFileName));
    {$ELSE}
      If not DirectoryExists(ExtractFileDir(FullEntryFileName)) then
        ForceDirectories(ExtractFileDir(FullEntryFileName));
    {$ENDIF}
      If not GetFlagState(Bin.Flags,SCS_FLAG_Directory) or ForceExtract then
        begin
          // processed entry is a file or extraction is forced (unresolved entry), extract it
          // if entry is a resolved directory then just continue to the next entry
          EntryFileStream := CreateFileStream(FullEntryFileName,fmCreate or fmShareDenyWrite);
          try
            // prepare buffer for compressed data
            ReallocateMemoryBuffer(fCED_Buffer,Bin.CompressedSize);
            // read (compressed) data from the archive
            fArchiveStream.Seek(Bin.DataOffset,soFromBeginning);
            ProgressedStreamRead(fARchiveStream,fCED_Buffer.Memory,Bin.CompressedSize,PROCSTAGEIDX_SCS_EntryLoading);
            // process input data according to compression flag
            If GetFlagState(Bin.Flags,SCS_FLAG_Compressed) then
              begin
                // entry data are compressed using ZLib (full zlib stream with header)
                ProgressedDecompressBuffer(fCED_Buffer.Memory,Bin.CompressedSize,
                  DecompressedBuff,DecompressedSize,PROCSTAGEIDX_SCS_EntryDecompressing,
                {$IFDEF FPC}
                  {$IFDEF Unicode}UTF8Decode(FileName),{$ELSE}FileName,{$ENDIF}
                {$ELSE}String(UTF8ToAnsi(FileName)),{$ENDIF}WINDOWBITS_ZLib);
                try
                  // write decompressed data into entry output file
                  ProgressedStreamWrite(EntryFileStream,DecompressedBuff,DecompressedSize,PROCSTAGEIDX_SCS_EntrySaving);
                finally
                  FreeMem(DecompressedBuff,DecompressedSize);
                end;
              end
            // entry data are not compressed, write data directly to entry output
            else ProgressedStreamWrite(EntryFileStream,fCED_Buffer.Memory,Bin.CompressedSize,PROCSTAGEIDX_SCS_EntrySaving);
            EntryFileStream.Size := EntryFileStream.Position;
            // make progress
            DoProgress(PROCSTAGEIDX_SCS_EntryProcessing,1.0);
          finally
            EntryFileStream.Free;
          end;
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
DoProgress(PROCSTAGEIDX_SCS_EntriesProcessing,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer_SCS_Extract.ArchiveProcessing;
begin
inherited;
DoProgress(PROCSTAGEIDX_SCS_PathsLoading,1.0);
SCS_ExtractArchive;
end;

end.
