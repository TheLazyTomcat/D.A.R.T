@echo off

if exist ..\Release rd ..\Release /s /q

mkdir ..\Release

copy ..\Documents\release_readme.txt ..\Release\readme.txt
copy ..\Documents\release_license.txt ..\Release\license.txt

copy ..\MainProgram\Delphi\Release\win_x86\SCS_Unlocker.exe "..\Release\SCS Unlocker[D32].exe"

copy ..\MainProgram\Lazarus\Release\win_x86\SCS_Unlocker.exe "..\Release\SCS Unlocker[L32].exe"

copy ..\MainProgram\Lazarus\Release\win_x64\SCS_Unlocker.exe "..\Release\SCS Unlocker[L64].exe"