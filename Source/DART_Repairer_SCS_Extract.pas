unit DART_Repairer_SCS_Extract;

{$INCLUDE DART_defs.inc}

interface

uses
  DART_Repairer_SCS;

type
  TDARTRepairer_SCS_Extract = class(TDARTRepairer_SCS_ProcessingBase)
  protected
    procedure ArchiveProcessing; override;
    // scs-extraction specific methods
    procedure SCS_ExtractArchive; virtual;
    procedure SCS_ExtractEntry(Index: Integer); virtual;
  end;

implementation

uses
  SysUtils, Classes, StrUtils,
  AuxTypes, StrRect, BitOps, MemoryBuffer, ZLibCommon,
  DART_Auxiliary, DART_Common, DART_Format_SCS;

procedure TDARTRepairer_SCS_Extract.ArchiveProcessing;
begin
inherited;
SCS_ExtractArchive;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS_Extract.SCS_ExtractArchive;
var
  i:  Integer;
begin
DoProgress(fProcessingProgNode,PSIDX_C_EntriesProcessing,0.0);
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  SCS_ExtractEntry(i);
DoProgress(fProcessingProgNode,PSIDX_C_EntriesProcessing,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_SCS_Extract.SCS_ExtractEntry(Index: Integer);
var
  FullEntryFileName:  String;
  EntryFileStream:    TFileStream;
  DecompressedBuff:   Pointer;
  DecompressedSize:   TMemSize;
begin
with fArchiveStructure.Entries.Arr[Index] do
try
  // prepare progress
  fEntryProcessingProgNode := fEntriesProcessingProgNode.StageObjects[Index];
  DoProgress(fEntriesProcessingProgNode,Index,0.0);

  If not UtilityData.Resolved then
    begin
       // entry file name was not resolved...
      If fProcessingSettings.PathResolve.ExtractedUnresolvedEntries then
        begin
          // prepare buffer for compressed data
          ReallocBufferKeep(fBuffer_Entry,BinPart.CompressedSize);
          // read (compressed) data from the archive
          fInputArchiveStream.Seek(BinPart.DataOffset,soBeginning);
          ProgressedStreamRead(fInputArchiveStream,fBuffer_Entry.Memory,BinPart.CompressedSize,
                               DARTProgressStageInfo(fEntryProcessingProgNode,PSIDX_C_EntryLoading));
          // process input data according to compression flag
          If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed) then
            begin
              // decompress data
              ProgressedDecompressBuffer(fBuffer_Entry.Memory,BinPart.CompressedSize,DecompressedBuff,DecompressedSize,
                WBITS_ZLIB,DARTProgressStageInfo(fEntryProcessingProgNode,PSIDX_C_EntryDecompression));
              try
                // write decompressed data
                SCS_SaveEntryAsUnresolved(Index,DecompressedBuff,DecompressedSize,
                  DARTProgressStageInfo(fEntryProcessingProgNode,PSIDX_C_EntrySaving));
              finally
                FreeMem(DecompressedBuff,DecompressedSize);
              end;
            end
          // entry data are not compressed, write data directly
          else SCS_SaveEntryAsUnresolved(Index,fBuffer_Entry.Memory,BinPart.CompressedSize,
                 DARTProgressStageInfo(fEntryProcessingProgNode,PSIDX_C_EntrySaving));
        end
      else DoWarning(Format('File name of entry #%d (0x%.16x) could not be resolved, it will be skipped.',[Index,BinPart.Hash]));
      // make final progress...
      DoProgress(fEntriesProcessingProgNode,Index,1.0);
      // ...and skip to the next entry
      Exit;
    end
  // file name of the entry was fully resolved, construct full path for entry output file
  else FullEntryFileName := IncludeTrailingPathDelimiter(fArchiveProcessingSettings.Common.TargetPath) +
                            AnsiReplaceStr(AnsiToStr(FileName),DART_SCS_PathDelim,PathDelim);

  // create necessary directory structure for the entry output file
  If not DART_DirectoryExists(ExtractFileDir(FullEntryFileName)) then
    DART_ForceDirectories(ExtractFileDir(FullEntryFileName));

  If not GetFlagState(BinPart.Flags,DART_SCS_FLAG_Directory) then
    begin
      // processed entry is a file or extraction is forced (unresolved entry), extract it
      // if entry is a resolved directory then just continue to the next entry
      EntryFileStream := TFileStream.Create(StrToRTL(FullEntryFileName),fmCreate or fmShareDenyWrite);
      try
        // prepare buffer for compressed data
        ReallocBufferKeep(fBuffer_Entry,BinPart.CompressedSize);
        // read (compressed) data from the archive
        fInputArchiveStream.Seek(BinPart.DataOffset,soBeginning);
        ProgressedStreamRead(fInputArchiveStream,fBuffer_Entry.Memory,BinPart.CompressedSize,
                             DARTProgressStageInfo(fEntryProcessingProgNode,PSIDX_C_EntryLoading));
        // process input data according to compression flag
        If GetFlagState(BinPart.Flags,DART_SCS_FLAG_Compressed) then
          begin
            // entry data are compressed using ZLib (full zlib stream with header)
            ProgressedDecompressBuffer(fBuffer_Entry.Memory,BinPart.CompressedSize,DecompressedBuff,DecompressedSize,
              WBITS_ZLIB,DARTProgressStageInfo(fEntryProcessingProgNode,PSIDX_C_EntryDecompression));
            try
              // write decompressed data into entry output file
              ProgressedStreamWrite(EntryFileStream,DecompressedBuff,DecompressedSize,
                DARTProgressStageInfo(fEntryProcessingProgNode,PSIDX_C_EntrySaving));
            finally
              FreeMem(DecompressedBuff,DecompressedSize);
            end;
          end
        // entry data are not compressed, write data directly to entry output
        else ProgressedStreamWrite(EntryFileStream,fBuffer_Entry.Memory,BinPart.CompressedSize,
               DARTProgressStageInfo(fEntryProcessingProgNode,PSIDX_C_EntrySaving));
        // finalize
        EntryFileStream.Size := EntryFileStream.Position;
      finally
        EntryFileStream.Free;
      end;
    end;
    
  DoProgress(fEntriesProcessingProgNode,Index,1.0);
except
  on E: Exception do
    begin
      UtilityData.Erroneous := True;
      If fArchiveProcessingSettings.Common.IgnoreErroneousEntries and not fTerminating then
        begin
          Terminated := False;
          DoWarning(Format('%s: %s',[E.ClassName,E.Message]));
        end
      else raise;
    end;
end;
end;

end.
