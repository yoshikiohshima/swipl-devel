@echo off
call ..\env.bat

if not exist Test.class (
  echo  Compiling Test.class
  javac Test.class
)

java Test.java

pause
