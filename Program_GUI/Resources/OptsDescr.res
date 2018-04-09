        ��  ��                  -B  0   ��
 O P T S D E S C R       0         #1
Full path of the processed input archive file.
#2
Type of the input archive file - this value affects how the archive will be processed. At this moment, support for two types is implemented - ZIP archives and SCS# archives.

When a file is selected for processing, the program determines its type by looking at the file signature (first four bytes), but this can be overridden by manually forcing the file type. If the file has an unknown signature, the type defaults to ZIP.
#3
When activated, initial type of the file (determined from a file signature) will be overridden to a type selected to the right of this checkbox.
#4
Archive type to be forced (select one).
#5
Input archive will be completely rebuild into another archive of the same type.
#6
All entries from input archive will be extracted into a selected target folder.
#7
Input archive will be converted into another archive of selected type.
#8
Type of archive to which the input archive will be converted.

If you select the same type as has the input archive, the conversion will be silently changed to rebuilding.
#9
- Rebuild or conversion is selected -

File where the rebuild or converted output will be written.

You can select a file with standard dialog when you click on the button to the right of this editbox. You can also write/edit the path manually.

WARNING - do not set output back into the input file!


- Extraction is selected -

Folder where the content of the input file will be extracted.

You can select the folder with standard dialog when you click on the button to the right of this editbox, or can write/edit the path manually.
#10
When NOT active, the program will, at the start of processing, load first four bytes of the input file (if there are at least four bytes) and check if they match with expected header signature (for a given file type). If they do not match or there is not enough data, then the processing will be terminated with an error.
#11
When this option is activated, the input file is completely loaded into memory where it is then processed, and output (when rebuilding or conversion is selected) is also build in the memory and then saved as a whole to the actual output file.

This can be used eg. when input archive contains extremely large number of very small files. Loading and saving of such archive the normal way could take very long time.

The input file must be smaller than or equal to 25% of total physical memory available to the process, otherwise this option is not available.

Note that 32bit builds are limited to 2GiB of memory, irrespective of actual size of memory installed in your computer, meaning this option is limited to input files up to 512MiB in such builds.
#12
When active, errors raised while individual archive entries are processed are ignored and processing continues on the next entry.

Takes effect only in the data processing, errors raised while archive metadata (headers) are loaded or processed still cause processing termination.

WARNING - use this option with extreme caution and only if you are 100% sure what are you doing.
#101
If compression method loaded from the archive is set to store, then it is left unchanged, if it is set to any other value, it is changed to deflate.

This option has effect only when "Ignore compression method" is not selected (affects central directory and local headers separately).
#201
When active, the end of central directory record is not searched for and loaded from the input file.

All values of individual fields are reconstructed from other data loaded later from the archive or are filled with default values (zero, empty string).
#202
Stored values of end of central directory record fields "NumberOfThisDisk" and "CentralDirectoryStartDiskNumber" are ignored and set to 0. Field "Entries" is set to be equal to value stored in field "EntriesOnDisk".
#203
Values stored in end of central directory record fields "EntriesOnDisk" and "Entries" are ignored. Actual number of entries is obtained from a length of array of entries later in the loading.
#204
When NOT active, a central directory is expected to start on an offset stored in end of central directory record field "CentralDirectoryOffset".

When active, this offset is ignored and instead is set to an offset of first appearance of central directory header signature (0x02014b50). If this signature is not found in the input stream, then an error is raised.
#205
When active, the archive comment stored in the end of central directory is ignored and set to an empty string.
#206
Before the end of central directory record can be loaded, it must be found in the input stream. It is searched for from the back, iterating over individual bytes, everytime checking for an end of central directory signature. To speed this up, iterating is done in-memory over buffers roughly 1MiB large.

Activating this option will cause that only one buffer is searched for the signature, meaning the record must be at most 1MiB distant from the end of file, otherwise such file is assumed to be corrupted.

If you deactivate it, the whole file will be searched, which may take very long time if the file is large and does not contain end of central directory record, but may be needed in case the file is damaged by inserting large amount of bogus data at its end.
#301
If active, the whole central directory structure is ignored (ie. is not loaded) and all data stored in there are reconstructed from local headers.

Local headers cannot be ignored when this option is active.
#302
When active, the "Signature" field of loaded central directory header is not checked to match expected signature (0x02014b50).

An error is raised in case the loaded signature does not match and this option is not active.

It has no effect when "Ignore central directory offset" for end of central directory is active - the central directory is searched for in the stream using the signature, so it must match.
#303
Central directory header fields "VersionMadeBy", "HostOS", "VersionNeededToExtract" and "OSNeededForExtraction" are ignored and set to following values:

  VersionMadeBy ............. 0x20
  HostOS .................... 0x00
  VersionNeededToExtract .... 0x20
  OSNeededForExtraction ..... 0x00

When local headers are loaded and versions stored in them are not ignored, then central directory fields "VersionNeededToExtract" and "OSNeededForExtraction" are set to values stored in corresponding local header fields, field "VersionMadeBy" is set to the same value as "VersionNeededToExtract" and "HostOS" is set to the same value as "OSNeededForExtraction".
#304
The first and seventh bit (ie. bit 0 and 6, "encrypted" and "strong encryption" flags) of "GeneralPurposeBitFlags" field are cleared.
#305
Compression method stored in the header is ignored.

If local headers are loaded and compression method stored in them is not ignored, then central directory header compression method is set to the same value as is stored in corresponding local header.

This option is not allowed to be active if one of the following two combinations is true - local headers are ignored and sizes in central directory headers are ignored, or sizes in central directory headers are ignored and both sizes and compression method in local headers are ignored
#306
Last modification time of an entry is ignored. It is set to actual system time instead.

If local headers are loaded and time stored in there is not ignored, then a value stored in local header is copied into central directory header.
#307
Last modification date of an entry is ignored. It is set to actual system date instead.

If local headers are loaded and date stored in there is not ignored, then a value stored in local header is copied into central directory header.
#308
CRC32 checksum stored in central directory header is ignored and is presumed to be invalid.

If corresponding local header is NOT loaded or CRC32 stored in it is also ignored, then the program will decompress stored entry data, calculate new checksum and then stores it back in the central directory header.

If local header is loaded and the checksum stored in there is not ignored, then that value is used instead.
#309
Compressed size and uncompressed size stored in the central directory header are ignored.

If corresponding local header is loaded and sizes stored in it are not ignored, then they are copied into the central directory header. Otherwise a complex operations are performed to obtain compressed size (see source code for details) and uncompressed size is obtained by decompressing the data or by copying the compressed size in case of compression method 0.

This option is not allowed to be active if one of the following two combinations is true - local headers are ignored and compression method in central directory headers is ignored, or compression method in central directory headers is ignored and both sizes and compression method in local headers are ignored
#310
Internal file attributes stored in the header are ignored and are set to 0.
#311
External file attributes stored in the header are ignored.
Instead, if a file name (that is, a file without a path) can be extracted from a stored entry file path, then attributes are set to FILE_ATTRIBUTE_ARCHIVE (0x00000020), otherwise they are set to FILE_ATTRIBUTE_DIRECTORY (0x00000010).
#312
Offset where a corresponding local header can be found is ignored.

Local headers must be loaded if this option is set (they cannot be ignored). The loading is done this way:

  - for each entry loaded from central directory
    a local header is searched for in the input
    stream so that...
  - for first central directory entry the first
    found local header is used, for second central
    directory entry the second found local header
    is used, and so on

This means that the local headers must have valid signatures (0x04034b50) and they must be in the same order as in the central directory. It also means there must be at least the same amount of local headers as central directory headers in the file.
#313
Extra field stored in the central directory header is ignored (data are discarded).
#314
Entry file comment stored in the central directory header is ignored.
#401
When active, the local headers are completely ignored (not loaded from the input stream). They are instead reconstructed using data from central directory.

This option cannot be active when central directory is ignored or when local header offset stored in central directory header is ignored.
#402
When active, the "Signature" field of loaded local header is not checked to match expected signature (0x04034b50).

An error is raised in case the loaded signature does not match and this option is not active.

It also affects loading of data descriptor the same way.

Has no effect when "Ignore local header offset" is active for central directory as individual local headers are searched for by the signature, so it must match.
#403
Local header fields "VersionNeededToExtract" and "OSNeededForExtraction" are ignored and set to following values:

  VersionNeededToExtract .... 0x20
  OSNeededForExtraction ..... 0x00

When central directory headers are loaded and versions stored in them are not ignored, then values are copied from corresponding fields in central directory headers.
#404
The first and seventh bit (ie. bit 0 and 6, "encrypted" and "strong encryption" flags) of "GeneralPurposeBitFlags" field are cleared.
#405
Compression method stored in the header is ignored.

If central directory headers are loaded and compression method stored in them is not ignored, then local header compression method is set to the same values as is stored in corresponding central directory header.

This option is not allowed to be active if one of the following two combinations is true - central directory headers are ignored and sizes in local headers are ignored, or sizes in local headers are ignored and both sizes and compression method in central directory headers are ignored
#406
Last modification time of entry is ignored. It is set to an actual system time instead.

If central directory headers are loaded and time stored in there is not ignored, then a value stored in central directory header is copied into local header.
#407
Last modification date of entry is ignored. It is set to an actual system date instead.

If central directory headers are loaded and date stored in there is not ignored, then a value stored in central directory header is copied into local header.
#408
CRC32 checksum stored in local header is ignored and is presumed to be invalid.

If corresponding central directory header is not loaded or CRC32 stored in it is ignored, then the program will decompress stored entry data, calculate new checksum and then stores it back in the local header.

If central directory header is loaded and the checksum stored in there is not ignored, then this value is used instead.
#409
Compressed size and uncompressed size stored in the header are ignored.

If corresponding central directory header is loaded and sizes stored in it are not ignored, then they are copied into the local header.

This option is not allowed to be active if one of the following two combinations is true - central directory headers are ignored and compression method in local header is ignored, or compression method in local header is ignored and both sizes and compression method in central directory header are ignored.
#410
When active, the file name stored in local header is ignored and file name stored in corresponding central directory header is used instead.

Cannot be active when central directory is ignored.
#411
Extra field stored in the central directory header is ignored (data are discarded).
#412
When active, the data descriptor, if indicated to be present by appropriate flag, is ignored and not loaded from the input stream. The data descriptor flag (bit 3) in general purpose bit flags is cleared.

When not active and when data descriptor is indicated to be present, then data descriptor is read from the input stream - this reading is affected by individual settings for local header (see source code for details).
#1001
When selected, entire entry table is loaded into memory and processed there. Also, when saving entry table, it is first constructed in memory and then saved as a whole. 

This can speed-up reading and saving of SCS# archives with very large number of entries.

Has no effect when in-memory archive processing is active.
#1101
Stored CRC32 checksum is discarded.
Instead, a new CRC32 is calculated as the entry data are processed and saved into output.

Has meaning only when rebuilding or conversion is selected.
#1102
Compression flag in entry metadata is ignored.

Actual state of compression is determined from stored sizes. If compressed and uncompressed size matches, then compression flag is cleared, if they are different, then compression flag is set.
#1103
Dictionary ID in zlib stream is ignored if present. When this option is not active and dictionary ID is encountered, and error is raised.
 
This option has meaning only for conversion from SCS# archive to ZIP archive.
#1201
Algorithm used for hashing entry file name is assumed to be CityHash.

Also field "HashType" in archive header is ignored and set to CITY (0x59544943).
#1202
A set of predefined paths (hardcoded in the program) is used when resolving entry file names.
#1203
Data of entries whose file names could not be resolved will be extracted.
Names of individual files to which the entries are extracted are constructed from stored hashes.

- Rebuilding, converting -

The files are stored in the same folder as the output file, into subfolder "unresolved_filename", where filename is the name of output file without the extension.

Note - when this option is active during rebuilding, unresolved entries are also copied into output file.


- Extracting -

Files are stored into the output folder, subfolder "_unresolved_".
#1204
Set of paths that will be used to resolve entries file names.

One path on each line. Path delimiter is forward slash (/). Paths must not contain leading and/or trailing delimited.
#1205
Set of files whose file/directory structure (entries they contain) will be loaded and used in resolving entries file names in currently processed archive.

Note that when resolving these help files, all previously resolved paths are used, so ordering does matter.

One file on each line, full absolute paths. Only SCS# and ZIP files are allowed.

Also note that processing of these files is not included into progress, so when very large file is loaded, the program might seemingly stop - just give it a time.   