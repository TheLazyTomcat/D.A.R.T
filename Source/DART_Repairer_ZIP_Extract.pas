{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_ZIP_Extract;

{$INCLUDE DART_defs.inc}

interface

uses
  DART_Repairer_ZIP;

{===============================================================================
--------------------------------------------------------------------------------
                           TDARTRepairer_ZIP_Extract
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_ZIP_Extract - class declaration
===============================================================================}
type
  TDARTRepairer_ZIP_Extract = class(TDARTRepairer_ZIP_ProcessingBase)
  protected
    procedure ArchiveProcessing; override;
    // zip-extraction specific methods
    procedure ZIP_ExtractArchive; virtual;
    procedure ZIP_ExtractEntry(Index: Integer); virtual;
    procedure ZIP_WriteEntryFileTime(const FileName: String; LastModTime, LastModDate: Word; Directory: Boolean = False);
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;     
  end;

implementation

uses
  Windows, SysUtils, Classes, StrUtils,
  AuxTypes, StrRect, MemoryBuffer, ZLibCommon,
  DART_Auxiliary, DART_Common, DART_Format_ZIP, DART_Repairer;

{===============================================================================
--------------------------------------------------------------------------------
                           TDARTRepairer_ZIP_Extract
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_ZIP_Extract - method indexing constants
===============================================================================}

const
  DART_METHOD_ID_ZIP_EXT_SETFLTM = $00020100;

{===============================================================================
    TDARTRepairer_ZIP_Extract - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTRepairer_ZIP_Extract - protected methods
-------------------------------------------------------------------------------}

procedure TDARTRepairer_ZIP_Extract.ArchiveProcessing;
begin
inherited;
ZIP_ExtractArchive;
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Extract.ZIP_ExtractArchive;
var
  i:  Integer;
begin
DoProgress(fProcessingProgNode,PSIDX_Z_EntriesProcessing,0.0);
For i := Low(fArchiveStructure.Entries.Arr) to Pred(fArchiveStructure.Entries.Count) do
  ZIP_ExtractEntry(i);
DoProgress(fProcessingProgNode,PSIDX_Z_EntriesProcessing,1.0);
end;

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Extract.ZIP_ExtractEntry(Index: Integer);
var
  FullEntryFileName:  String;
  EntryFileStream:    TFileStream;
  TempOffset:         Int64;
  DecompressedBuff:   Pointer;
  DecompressedSize:   TMemSize;
begin
with fArchiveStructure.Entries.Arr[Index] do
try
  // construct full path for entry output file
  FullEntryFileName := IncludeTrailingPathDelimiter(fArchiveProcessingSettings.Common.TargetPath) +
                       AnsiReplaceStr(AnsiToStr(LocalHeader.FileName),DART_ZIP_PathDelim,PathDelim);
  // create necessary directory structure for the entry output file
  If not DART_DirectoryExists(ExtractFileDir(FullEntryFileName)) then
    DART_ForceDirectories(ExtractFileDir(FullEntryFileName));

  // set file times for created directory
  ZIP_WriteEntryFileTime(ExtractFileDir(FullEntryFileName),LocalHeader.BinPart.LastModFileTime,LocalHeader.BinPart.LastModFileDate,True);

  // continue only if the entry is an actual file
  If (Length(ExtractFileName(FullEntryFileName)) > 0) and ((CentralDirectoryHeader.BinPart.ExternalFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0) then
    begin
      // entry contains actual file, not a directory -> process the data
      // create file stream where the entry data will be written
      EntryFileStream := TFileStream.Create(StrToRTL(FullEntryFileName),fmCreate or fmShareDenyWrite);
      try
        // determine size of the entry data, so it can be read from input archive
        If UtilityData.NeedsSizes then
          begin
            // need to obtain sizes of entry - getting compressed size here
            If Index < Pred(fArchiveStructure.Entries.Count) then
              // inner entry (ie. not a last one), comp. size is calculated as difference
              // between data start (obtained earlier) and start of local header of next entry
              LocalHeader.BinPart.CompressedSize := UInt32(Int64(fArchiveStructure.Entries.Arr[Index + 1].CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader) - UtilityData.DataOffset)
            else
              begin
                // last entry, find CD or EOCD
                TempOffset := FindSignature(DART_ZIP_CentralDirectoryFileHeaderSignature,DART_PROGSTAGE_INFO_NoProgress);
                If TempOffset < 0 then
                  TempOffset := FindSignature(DART_ZIP_EndOfCentralDirectorySignature,DART_PROGSTAGE_INFO_NoProgress);
                If TempOffset >= UtilityData.DataOffset then
                  // CD or EOCD found, let's assume it marks end of entry data
                  LocalHeader.BinPart.CompressedSize := UInt32(TempOffset - UtilityData.DataOffset)
                else
                  // CD or EOCD not found, assuming entry data are continuing to EOF
                  LocalHeader.BinPart.CompressedSize := UInt32(fInputArchiveStream.Size - UtilityData.DataOffset);
              end;
          // correct for 0-size entries
          If LocalHeader.BinPart.CompressedSize = 0 then
           begin
              LocalHeader.BinPart.CompressionMethod := DART_ZCM_Store;
              CentralDirectoryHeader.BinPart.CompressionMethod := DART_ZCM_Store;
              LocalHeader.BinPart.UncompressedSize := 0;
              DataDescriptor.UncompressedSize := 0;
              CentralDirectoryHeader.BinPart.UncompressedSize := 0;
            end;
          end;

        // store offset of entry local header it has in input (needed for progress)
        UtilityData.OriginalLocalHeaderOffset := CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader;
        // prepare progress
        fEntryProcessingProgNode := fEntriesProcessingProgNode.StageObjects[Index];
        DoProgress(fEntriesProcessingProgNode,Index,0.0);
        // prepare buffer for entry data
        ReallocBufferKeep(fBuffer_Entry,LocalHeader.BinPart.CompressedSize);
        // read data from input archive
        fInputArchiveStream.Seek(UtilityData.DataOffset,soBeginning);
        ProgressedStreamRead(fInputArchiveStream,fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,
                             ProgressStageInfo(fEntryProcessingProgNode,PSIDX_Z_EntryLoading));

        // process input data according to compression method
        case LocalHeader.BinPart.CompressionMethod of
          DART_ZCM_Deflate:
            begin
              // decompress data
              ProgressedDecompressBuffer(fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,DecompressedBuff,DecompressedSize,
                                         WBITS_RAW,ProgressStageInfo(fEntryProcessingProgNode,PSIDX_Z_EntryDecompression));
              try
                // write decompressed data into entry output archive
                ProgressedStreamWrite(EntryFileStream,DecompressedBuff,DecompressedSize,
                                      ProgressStageInfo(fEntryProcessingProgNode,PSIDX_Z_EntrySaving));
              finally
                FreeMem(DecompressedBuff,DecompressedSize);
              end;
            end;
        else
          // no compression, write input data directly to output
          ProgressedStreamWrite(EntryFileStream,fBuffer_Entry.Memory,LocalHeader.BinPart.CompressedSize,
                                ProgressStageInfo(fEntryProcessingProgNode,PSIDX_Z_EntrySaving));
        end;

        // finalize
        EntryFileStream.Size := EntryFileStream.Position;
        // write file time
        ZIP_WriteEntryFileTime(FullEntryFileName,LocalHeader.BinPart.LastModFileTime,LocalHeader.BinPart.LastModFileDate);
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

//------------------------------------------------------------------------------

procedure TDARTRepairer_ZIP_Extract.ZIP_WriteEntryFileTime(const FileName: String; LastModTime, LastModDate: Word; Directory: Boolean = False);
var
  FileHandle: THandle;
  FileTime:   TFileTime;
  Flags:      DWORD;
begin
If Directory then
  Flags := FILE_ATTRIBUTE_NORMAL or FILE_FLAG_BACKUP_SEMANTICS
else
  Flags := FILE_ATTRIBUTE_NORMAL;
FileHandle := CreateFile(PChar(StrToWin(FileName)),FILE_WRITE_ATTRIBUTES,FILE_SHARE_READ or FILE_SHARE_WRITE,nil,OPEN_EXISTING,Flags,0);
If FileHandle <> INVALID_HANDLE_VALUE then
  try
    If DosDateTimeToFileTime(LastModDate,LastModTime,{%H-}FileTime) then
      begin
        If LocalFileTimeToFileTime(FileTime,FileTime) then
          begin
            If not SetFileTime(FileHandle,nil,nil,@FileTime) then
              DoError(DART_METHOD_ID_ZIP_EXT_SETFLTM,'Cannot write file time (0x%.8x).',[GetLastError]);
          end
        else DoError(DART_METHOD_ID_ZIP_EXT_SETFLTM,'Cannot convert local file time to file time (0x%.8x).',[GetLastError]);
      end
    else DoError(DART_METHOD_ID_ZIP_EXT_SETFLTM,'Cannot convert DOS time to file time (0x%.8x).',[GetLastError]);
  finally
    CloseHandle(FileHandle);
  end
else DoError(DART_METHOD_ID_ZIP_EXT_SETFLTM,'Cannot open file "%s" (0x%.8x).',[FileName,GetLastError]);
end;

{-------------------------------------------------------------------------------
    TDARTRepairer_ZIP_Extract - public methods
-------------------------------------------------------------------------------}

class Function TDARTRepairer_ZIP_Extract.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  DART_METHOD_ID_ZIP_EXT_SETFLTM: Result := 'ZIP_SetEntryFileTime';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

end.
