@echo off
call ..\env.bat

if not exist FamilyMT.class (
  echo  Compiling FamilyMT.class
  javac FamilyMT.class
)

java FamilyMT.java

pause
