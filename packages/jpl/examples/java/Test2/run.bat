@echo off
call ..\env.bat

if not exist Test2.class (
  echo  Compiling Test2.class
  javac Test2.class
)

java Test2.java

pause
