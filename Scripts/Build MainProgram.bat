@echo off

pushd .

cd ..\MainProgram\Resources
call "Build zlib (windres).bat"
call "Build SettDescr (windres).bat"
call "Build SettDescr (brcc32).bat"

cd ..\Delphi
dcc32.exe -Q -B DART.dpr

cd ..\Lazarus
lazbuild -B --bm=Release_win_x86 DART.lpi
lazbuild -B --bm=Release_win_x64 DART.lpi
lazbuild -B --bm=Debug_win_x86 DART.lpi
lazbuild -B --bm=Debug_win_x64 DART.lpi

popd