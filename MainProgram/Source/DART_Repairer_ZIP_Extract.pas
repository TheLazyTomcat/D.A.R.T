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

type
  TRepairer_ZIP_Extract = class(TRepairer_ZIP)
  protected
    procedure ZIP_ExtractArchive; virtual;
    procedure ArchiveProcessing; override;
  public
    class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;    
  end;

implementation

uses
  Windows, SysUtils, Classes, StrUtils,
  AuxTypes,
  DART_MemoryBuffer, DART_Repairer;

{$IF not declared(FILE_WRITE_ATTRIBUTES)}
const
  FILE_WRITE_ATTRIBUTES = 256;
{$IFEND}

//------------------------------------------------------------------------------

procedure TRepairer_ZIP_Extract.ZIP_ExtractArchive;
var
  i:                  Integer;
  FullEntryFileName:  String;
  EntryFileStream:    TFileStream;
  TempOffset:         Int64;
  DecompressedBuff:   Pointer;
  DecompressedSize:   Integer;

  procedure WriteFileTime(const FileName: String; LastModTime, LastModDate: Word; Directory: Boolean = False);
  var
    FileHandle: THandle;
    FileTime:   TFileTime;
    Flags:      DWORD;
  begin
    If Directory then
      Flags := FILE_ATTRIBUTE_NORMAL or FILE_FLAG_BACKUP_SEMANTICS
    else
      Flags := FILE_ATTRIBUTE_NORMAL;
  {$IF Defined(FPC) and not Defined(Unicode)}
    FileHandle := CreateFile(PChar(UTF8ToWinCP(FileName)),FILE_WRITE_ATTRIBUTES,FILE_SHARE_READ or FILE_SHARE_WRITE,nil,OPEN_EXISTING,Flags,0);
  {$ELSE}
    FileHandle := CreateFile(PChar(FileName),FILE_WRITE_ATTRIBUTES,FILE_SHARE_READ or FILE_SHARE_WRITE,nil,OPEN_EXISTING,Flags,0);
  {$IFEND}
    If FileHandle <> INVALID_HANDLE_VALUE then
      try
        If DosDateTimeToFileTime(LastModDate,LastModTime,{%H-}FileTime) then
          begin
            If LocalFileTimeToFileTime(FileTime,FileTime) then
              begin
                If not SetFileTime(FileHandle,nil,nil,@FileTime) then
                  DoError(300,'Cannot write file time (0x%.8x).',[GetLastError]);
              end
            else DoError(300,'Cannot convert local file time to file time (0x%.8x).',[GetLastError]);
          end
        else DoError(300,'Cannot convert DOS time to file time (0x%.8x).',[GetLastError]);
      finally
        CloseHandle(FileHandle);
      end
    else DoError(300,'Cannot open file "%s" (0x%.8x).',[FileName,GetLastError]);
  end;

begin
DoProgress(PROCSTAGEIDX_ZIP_EntriesProcessing,0.0);
For i := Low(fArchiveStructure.Entries) to High(fArchiveStructure.Entries) do
  with fArchiveStructure.Entries[i] do
    try
      // construct full path for entry output file
      FullEntryFileName := IncludeTrailingPathDelimiter(fFileProcessingSettings.Common.TargetPath) +
                         {$IFDEF FPC}
                           AnsiReplaceStr(WinCPToUTF8(LocalHeader.FileName),'/','\');
                         {$ELSE}
                           AnsiReplaceStr(LocalHeader.FileName,'/','\');
                         {$ENDIF}
      // create necessary directory structure for the entry output file
    {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
      If not DirectoryExistsUTF8(ExtractFileDir(FullEntryFileName)) then
        ForceDirectoriesUTF8(ExtractFileDir(FullEntryFileName));
    {$ELSE}
      If not DirectoryExists(ExtractFileDir(FullEntryFileName)) then
        ForceDirectories(ExtractFileDir(FullEntryFileName));
    {$IFEND}
      // set file times for created directory
      WriteFileTime(ExtractFileDir(FullEntryFileName),LocalHeader.BinPart.LastModFileTime,LocalHeader.BinPart.LastModFileDate,True);
      If (ExtractFileName(FullEntryFileName) <> '') and ((CentralDirectoryHeader.BinPart.ExternalFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0) then
        begin
          // entry contains actual file, not a directory -> process the data
          // create file stream where the entry data will be written
          EntryFileStream := CreateFileStream(FullEntryFileName,fmCreate or fmShareDenyWrite);
          try
            If UtilityData.NeedsSizes then
              begin
                // need to obtain sizes of entry - getting compressed size here
                If i < High(fArchiveStructure.Entries) then
                  // inner entry (ie. not a last one), comp. size is calculated as difference
                  // between data start (obtained earlier) and start of local header of next entry
                  LocalHeader.BinPart.CompressedSize := UInt32((Int64(fArchiveStructure.Entries[i + 1].CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader) - UtilityData.DataOffset))
                else
                  begin
                    // last entry, find CD or EOCD
                    TempOffset := FindSignature(ZIP_CentralDirectoryFileHeaderSignature);
                    If TempOffset < 0 then
                      TempOffset := FindSignature(ZIP_EndOfCentralDirectorySignature);
                    If TempOffset >= UtilityData.DataOffset then
                      // CD or EOCD found, let's assume it marks end of entry data
                      LocalHeader.BinPart.CompressedSize := UInt32(TempOffset - UtilityData.DataOffset)
                    else
                      // CD or EOCD not found, assuming entry data are continuing to EOF
                      LocalHeader.BinPart.CompressedSize := UInt32(fArchiveStream.Size - UtilityData.DataOffset);
                  end;
              end;
            If fProcessingSettings.AssumeCompressionMethods then
              begin
                If (LocalHeader.BinPart.CompressedSize > 0) and (LocalHeader.BinPart.CompressionMethod <> 0) then
                  // compressed entry has non-zero size and stored compression method
                  // differs from 0 (store) =>  assuming compression method 8 (deflate)
                  LocalHeader.BinPart.CompressionMethod := 8
                else
                  LocalHeader.BinPart.CompressionMethod := 0; // store (no compression)
              end;
            // store offset of entry LH it has in input (needed for progress)
            UtilityData.OriginalLocalHeaderOffset := CentralDirectoryHeader.BinPart.RelativeOffsetOfLocalHeader;
            // calculating progress info for processing of current entry
            ZIP_PrepareEntryProgressInfo(i);
            DoProgress(PROCSTAGEIDX_ZIP_EntryProcessing,0.0);
            // prepare buffer for compressed data
            ReallocateMemoryBuffer(fCED_Buffer,LocalHeader.BinPart.CompressedSize);
            // read data from input archive
            fArchiveStream.Seek(UtilityData.DataOffset,soFromBeginning);
            ProgressedStreamRead(fArchiveStream,fCED_Buffer.Memory,LocalHeader.BinPart.CompressedSize,PROCSTAGEIDX_ZIP_EntryLoading);
            // process input data according to compression method
            case LocalHeader.BinPart.CompressionMethod of
              8:  begin // deflate
                    // decompressing data
                    ProgressedDecompressBuffer(fCED_Buffer.Memory,LocalHeader.BinPart.CompressedSize,
                      DecompressedBuff,DecompressedSize,PROCSTAGEIDX_ZIP_EntryDecompressing,
                      {$IFDEF FPC}WinCPToUTF8(CentralDirectoryHeader.FileName),
                      {$ELSE}CentralDirectoryHeader.FileName,{$ENDIF}WINDOWBITS_Raw);
                    try
                      // write decompressed data into entry output file
                      ProgressedStreamWrite(EntryFileStream,DecompressedBuff,DecompressedSize,PROCSTAGEIDX_ZIP_EntrySaving);
                    finally
                      FreeMem(DecompressedBuff,DecompressedSize);
                    end;
                  end;
            else
              // no compression, write input data directly to output
              ProgressedStreamWrite(EntryFileStream,fCED_Buffer.Memory,LocalHeader.BinPart.CompressedSize,PROCSTAGEIDX_ZIP_EntrySaving);
            end;
            EntryFileStream.Size := EntryFileStream.Position;
            // write file times and make progress
            WriteFileTime(FullEntryFileName,LocalHeader.BinPart.LastModFileTime,LocalHeader.BinPart.LastModFileDate);
            DoProgress(PROCSTAGEIDX_ZIP_EntryProcessing,1.0);
          finally
            EntryFileStream.Free;
          end;
        end;  
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
DoProgress(PROCSTAGEIDX_ZIP_EntriesProcessing,1.0);
end;

//------------------------------------------------------------------------------

procedure TRepairer_ZIP_Extract.ArchiveProcessing;
begin
inherited;
ZIP_ExtractArchive;
end;

//==============================================================================

class Function TRepairer_ZIP_Extract.GetMethodNameFromIndex(MethodIndex: Integer): String;
begin
case MethodIndex of
  300:  Result := 'ZIP_ExtractInputFile.WriteFileTime';
else
  Result := inherited GetMethodNameFromIndex(MethodIndex);
end;
end;

end.
