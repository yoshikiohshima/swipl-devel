@echo off
call ..\env.bat

if not exist Family.class (
  echo  Compiling Family.class
  javac Family.class
)

java Family.java

pause
