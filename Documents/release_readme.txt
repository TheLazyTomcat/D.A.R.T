================================================================================

                               SCS Unlocker 2.0.0

================================================================================

Index
------------------------------
Content of this document divided into individual parts, with line numbers at
which each part starts.

  Index ...................................................   7
  Disclaimer ..............................................  26
  Description .............................................  39
  Installation ............................................  59
  How to use the program ..................................  69
  Known issues ............................................  88
  Source code .............................................  95
  Licensing ............................................... 104
  Authors, contacts ....................................... 112
  Links ................................................... 118
  Copyright ............................................... 124



Disclaimer
------------------------------
Although this program has "SCS" in its name, SCS Software is not responsible
for it in any way. It is entirely work of author(s) listed below. If you have
problems or questions regarding this program, please do NOT contact
SCS Software's support, use contacts from section "Authors, contacts".
Also, you, as a user, are responsible for everything you will do with this piece
of software. Original intention of this program is to give users a simple way of
unlocking modifications for the sake of learning or maintenance, not to steal
others work.



Description
------------------------------
This tool is designed to unlock, or better said repair, SCS archives used as a
storage for modifications created for SCS Software truck games (for example
Euro Truck Simulator, 18 Wheels of Steel, etc.).
Virtually all mods created for those games are distributed in one or more SCS
files (that is, a file with SCS extension), where such file is actually a zip
archive. The problem is, many mod authors decided, for various reasons, to
intentionally damage those files in a way that prevents normal user to
decompress their content or even open them with common archive tools (WinZip,
WinRAR, 7zip, ...), but the game can still load them. Such damaged file, if it
could be opened at all, will be identified by most tools as locked. This program
is able to repair and/or extract content of vast majority of those "locked"
files.

NOTE - There are SCS files that are part of the game itself, but they are not
       zip files and this program cannot work with them.



Installation
------------------------------
There is no need to install this program, just put it anywhere on your disk
where you have access rights and that should be enough.
There are three identical builds of this program - L32 (Lazarus 32bit),
L64 (Lazarus 64bit) and D32 (Delphi 32bit). It is up to you what build will you
use, there is no functional difference or limitation.



How to use the program
------------------------------
This program was created to be as simple as possible, yet offering some settings
for advanced users. General use would be as follows:

  - run the program
  - right-click to the files list (large white rectangle in the upper part of
    the main window)...
  - ...a popup menu will appear, select "Add files..."
  - in opened dialog, select file or files you want to unlock and confirm your
    selection
  - [optional] right-click on a particular file and in opened menu select
    "File processing settings..."; a new window will appear; there, you can
    set individual settings that will be used when the file will be processed
  - click on "Start processing" button and wait for the end of processing
  - done



Known issues
------------------------------
This section lists all known issues and bugs that made their way to the final
release.



Source code
------------------------------
You can get copy of full source code on either of the following git repository:

https://bitbucket.org/ncs-sniper/scs_unlocker
https://github.com/ncs-sniper/SCS_Unlocker



Licensing
------------------------------
This program is licensed under the terms of Mozilla Public License Version 2.0.
You can find full text of this license in file license.txt or on web page
https://www.mozilla.org/MPL/2.0/.



Authors, contacts
------------------------------
František Milt, frantisek.milt@gmail.com



Links
------------------------------
Forum thread:



Copyright
------------------------------
©2015 František Milt, all rights reserved