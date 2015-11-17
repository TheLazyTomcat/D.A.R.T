@echo off

if exist ..\Release rd ..\Release /s /q

mkdir ..\Release

copy ..\Documents\release_readme.txt ..\Release\readme.txt
copy ..\Documents\release_license.txt ..\Release\license.txt

copy ..\MainProgram\Delphi\Release\win_x86\DART.exe "..\Release\DART[D32].exe"

copy ..\MainProgram\Lazarus\Release\win_x86\DART.exe "..\Release\DART[L32].exe"

copy ..\MainProgram\Lazarus\Release\win_x64\DART.exe "..\Release\DART[L64].exe"