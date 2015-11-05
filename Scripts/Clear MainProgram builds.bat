@echo off

del ..\MainProgram\Delphi\Release\win_x86\DART.exe /S /Q 
 
del ..\MainProgram\Lazarus\Release\win_x86\DART.exe /S /Q
del ..\MainProgram\Lazarus\Release\win_x86\zlib1.dll /S /Q  

del ..\MainProgram\Lazarus\Release\win_x64\DART.exe /S /Q 
del ..\MainProgram\Lazarus\Release\win_x64\zlib1.dll /S /Q  

del ..\MainProgram\Lazarus\Debug\win_x86\DART.exe /S /Q
del ..\MainProgram\Lazarus\Debug\win_x86\zlib1.dll /S /Q    

del ..\MainProgram\Lazarus\Debug\win_x64\DART.exe /S /Q
del ..\MainProgram\Lazarus\Debug\win_x64\zlib1.dll /S /Q