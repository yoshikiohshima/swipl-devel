@echo off
rem this assumes that jpl.dll is somewhere on your PATH
call ..\env.bat
echo JPL demo: Family

java -classpath "..\..\..\jpl.jar;." Family

pause

