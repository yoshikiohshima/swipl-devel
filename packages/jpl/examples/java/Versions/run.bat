@echo off
call ..\env.bat

if not exist Versions.class (
  echo  Compiling Versions.class
  javac Versions.class
)

java Versions.java

pause
