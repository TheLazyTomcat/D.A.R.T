        ��  ��                  b  0   ��
 S E T T D E S C R       0         #1
Name of the processed input file (without a path).
#2
Processed file will be completely rebuild into an output file.
#3
All entries from input file will be extracted into a selected folder.
#4
- Rebuild file selected -

File where the rebuild output will be written. 
You can select a file with standard dialog when you click on the button to the right of this editbox. 
You can also write/edit the path manually. 

WARNING - do not set output back into the input file!


- Extract archive selected- 

Folder where the content of the input file will be extracted.
You can select the folder with standard dialog when you click on the button to the right of this editbox, or can write/edit the path manually.
#6
When NOT checked, the program will, at the start of processing, load first four bytes of the input file (if there are four bytes) and check if they match with ZIP local header signature (0x04034b50). 
If they do not match or there is not enough data, then the processing will be terminated with an error.
#7
If compressed size of an entry is larger than zero and compressed method is not zero, then actual compression method is assumed to be 8 (deflate), otherwise compresion method is assumed to be 0 (no compression/store).

Also affects loading of headers (both central directory and local) when "Ignore compression method" is not active. If stored compression method is not 0 or 8 and this setting is not active, then and error is raised during header loading.
#8
When this setting is activated, the input file is completely loaded into memory where it is then processed, and output (when rebuilding is selected) is also build in the memory and then saved as a whole to the actual output file.

This can be used eg. when input archive contains extremely large number of very small files. Loading and saving such archive the normal way could take very long time.

The input file must be smaller then or equal to 25% of total physical memory available to the process, otherwise this setting is not available. 
Note that 32bit builds are limited to 2GiB of memory, irrespective of actual size of memory installed in your computer, meaning input file are limited to 512MiB.
#9
Available only when archive extraction is selected as processing method.

When active, errors raised while individual entries are proceessed are ignored and processing continues on the next entry. Takes effect only in the data processing, errors raised while the headers are loaded or processed still cause processing termination.

WARNING - use this option with extreme caution and only if you are 100% sure what are you doing. 
#20
When selected, the end of central directory record is not searched for and loaded from the input file. All values of individual fields are reconstructed from other data loaded later from the archive or are filled with default values (zero, empty string).
#21
Stored values of EOCD record fields NumberOfThisDisk and CentralDirectoryStartDiskNumber are ignored and set to 0. Field Entries is set to be equal to value stored in field EntriesOnDisk.  
#22
Values stored in EOCD record fields EntriesOnDisk and Entries are ignored. Actual number of entries is obtained from a length of array of entries later in the loading.
#23
When NOT active, a central directory is expected to start on an offset stored in EOCD record field CentralDirectoryOffset. 
When active, this offset is ignored and instead is set to an offset of first appearance of central directory header signature (0x02014b50). If this signature is not found, an error is raised.
#24
When active, the ZIP file comment stored in the input archive is ignored and set to an empty string.
#25
Before the end of central directory record can be loaded, it must be found in the input stream. It is done from the back, iterating over individual bytes, everytime checking for an EOCD signature. To speed this up, iterating is done in-memory over buffers rougfly 1MiB large. 

Activating this setting will cause that only one buffer is searched for the signature, meaning the record must be at most 1MiB distant from the end of file, otherwise such file is assumed to be corrupted. 
If you deactivate it, the whole file will be searched, which may take very long time if the file is large and does not contain EOCD record, but may be needed in case the file is damaged by inserting large amount of bogus data at its end.  