@echo off
call ..\env.bat

if not exist Zahed.class (
  echo  Compiling Zahed.class
  javac Zahed.class
)

java Zahed.java

pause
