================================================================================

                  D.A.R.T - Damaged Archives Repair Tool 1.1.x

================================================================================

Index
--------------------------------------------------
Content of this document divided into individual parts, with line numbers at
which each part starts.

  Index ...................................................   7
  Description .............................................  27
  Installation ............................................  47
  How to use the program ..................................  57
  Workarounds for common processing errors ................  82
  Changelog ............................................... 175
  Known issues ............................................ 205
  Source code ............................................. 212
  Licensing ............................................... 221
  Authors, contacts ....................................... 229
  Links ................................................... 235
  Copyright ............................................... 241



Description
--------------------------------------------------
This tool is designed to repair ZIP archives used as a storage for modifications
created for SCS Software truck games (for example Euro Truck Simulator,
18 Wheels of Steel, etc.).
Virtually all mods created for those games are distributed in one or more SCS
files (that is, a file with SCS extension), where such file is actually a ZIP
archive. The problem is, many modification files are, for various reasons,
damaged in a way that prevents normal user to decompress their content or even
open them with common archive tools (WinZip, WinRAR, 7zip, ...), but the game
is generally still able to load them. Such damaged file, if it could be opened
at all, will be in most cases identified by many tools as locked. This program
is able to repair and/or extract content of vast majority of those damaged
files.

NOTE - There are SCS files that are part of the game itself, but they are not
       ZIP files and this program cannot work with them.



Installation
--------------------------------------------------
There is no need to install this program, just put it anywhere on your disk
where you have access rights and that should be enough.
There are three identical builds of this program - L32 (Lazarus 32bit),
L64 (Lazarus 64bit) and D32 (Delphi 32bit). It is up to you what build will you
use, there is no functional difference or limitation.



How to use the program
--------------------------------------------------
This program was created to be as simple as possible, yet offering some settings
for advanced users. General use would be as follows:

  - run the program
  - right-click to the files list (large white rectangle in the upper part of
    the main window)...
  - ...a popup menu will appear, select "Add files..."
  - in opened dialog, select file or files you want to repair and confirm your
    selection
  - [optional] right-click on a particular file and in opened menu select
    "File processing settings..."; a new window will appear; there, you can
    set individual settings that will be used when the file will be processed
  - click on "Start processing" button and wait for the end of processing
  - done

WARNING - It is possible for the program to finish processing successfully
          despite the fact that repairing was unsuccessful. In such case, select
          "Ignore end of central directory" and "Ignore central directory"
          settings for this file and try process it again. If that does not help
          either, ask in a forum mentioned in "Links" section.



Workarounds for common processing errors
--------------------------------------------------
Not every damaged archive can be repaired using default processing settings.
In such case, a processing error is almost always raised - you can use this
error and following list and try to repair the file with described changes in
processing setings.
This list is not in any way complete. If you cannot repair the file even with
described settings, you should seek help in a forum mentioned further.
Each item in the list is a specific error text accompanied with one or more
suggestions what to do to eliminate the error in next processing (note that the
suggestions are independent of each other, meaning you can use only one for a
particular error, not all of them; also, some suggested settings are already
part of default settings and they are mentioned only for the sake of
completeness)


  "Bad file signature (0xXXXXXXXX)."
    - make sure you have selected proper file
    - activate [General settings] > [Ignore file signature]

  "File is too small to contain valid signature (X bytes)."
    - make sure you have selected proper file (this error means the file is
      smaller than four bytes)
    - activate [General settings] > [Ignore file signature]

  "Not enough data for end of central directory comment."
    - activate [End of central direcotry] > [Ignore comment]
    - activate [End of central direcotry] > [Ignore end of central directory]

  "Not enough data for end of central directory record."
    - activate [End of central direcotry] > [Ignore end of central directory]

  "End of central directory signature not found in the input stream."
    - make sure you have selected proper file
    - deactivate [End of central direcotry] > [Limit search to one buffer]
    - activate [End of central direcotry] > [Ignore end of central directory]

  "Bad central directory header signature (0xXXXXXXXX) for entry #Y."
    - activate [Central directory headers] > [Ignore header signature]
    - activate [Central directory headers] > [Ignore central directory]

  "Unknown compression method (X) in central directory header for entry #Y."
    - activate [Central directory headers] > [Ignore compression method]
    - activate [General settings] > [Assume compression method]

  "Start of central directory not found in the input stream."
    - deactivate [End of central direcotry] > [Ignore central directory offset]

  "Bad local header signature (0xXXXXXXXX) for entry #Y."
    - activate [Local headers] > [Ignore header signature]
    - activate [Local headers] > [Ignore local headers]

  "Unknown compression method (X) in local header for entry #Y."
    - activate [Local headers] > [Ignore compression method]
    - activate [General settings] > [Assume compression method]

  "Bad data descriptor signature (0xXXXXXXXX) for entry #Y."
    - activate [Local headers] > [Ignore header signature]
    - activate [Local headers] > [Ignore data descriptor]

  "Data descriptor was not found (X)."
    - activate [Local headers] > [Ignore data descriptor]

  "No local header found for entry #X."
    - activate [Central directory headers] > [Ignore local header offset]
    - activate [Central directory headers] > [Ignore central directory]

  "Mismatch in local and central directory file name for entry #X (...; ...)."
    - activate [Local headers] > [Ignore file name]
    - activate [Local headers] > [Ignore local headers]
    - activate [Central directory headers] > [Ignore central directory]

  "zlib: ... (entry "...")"
    - activate [General settings] > [Ignore processing errors]

  "Output is directed into an input file, cannot proceed."
    - make sure you have not directed rebuild back into the source file

  "Input file does not contain any valid entries."
    - make sure the processed file contains any sensible data and is in fact
      a ZIP archive

  "Input file does not contain any data."
    - make sure you have selected proper file (this error indicates that the
      file is empty)

  "Processing terminated. Data can be in inconsistent state."
    - you have prematurely ended the processing, do not do it if you don't
      really need to
    - run the processing again and let it finish



Changelog
--------------------------------------------------
List of changes between individual versions of this program.

D.A.R.T 1.0.1 -> D.A.R.T 1.1.0
  - processing errors can now be ignored in archive rebuil mode (used to be
    available only when extracting archive)
  - added an option to log ignored processing errors
  - error text (in error window) is now displayed in memo instead of static text
  - overall percentage is shown in main window title bar and on taskbar button,
    overall progress is displayed in taskbar button (Windows 7+)
  - corrected bug where extraction could not be stopped by the user when
    ignoring of processing errors was activated
  - large number of corrections in Lazarus/FPC builds to ensure proper handling
    of non-ascii characters in file paths
  - other small changes and corrections


SCS Unlocker 2.0.0 -> D.A.R.T 1.0.1 (project was renamed)
  - project renamed to D.A.R.T - Damaged Archives Repair Tool
  - date and time of stored file's last change is now written when archive is
    being extracted
  - limited life of preallocated buffers (lowers memory usage when processing
    is not running)
  - central directory sizes and compression method cannot be both ignored when
    local headers are ignored
  - other small changes and corrections



Known issues
--------------------------------------------------
This section lists all known issues and bugs that made their way to the final
release.



Source code
--------------------------------------------------
You can get copy of full source code on either of the following git repository:

https://bitbucket.org/ncs-sniper/d.a.r.t
https://github.com/ncs-sniper/D.A.R.T



Licensing
--------------------------------------------------
This program is licensed under the terms of Mozilla Public License Version 2.0.
You can find full text of this license in file license.txt or on web page
https://www.mozilla.org/MPL/2.0/.



Authors, contacts
--------------------------------------------------
František Milt, frantisek.milt@gmail.com



Links
--------------------------------------------------
Forum thread: http://forum.scssoft.com/viewtopic.php?f=41&t=192120



Copyright
--------------------------------------------------
©2015-2016 František Milt, all rights reserved