@echo off
call ..\env.bat

if not exist Exceptions.class (
  echo  Compiling Exceptions.class
  javac Exceptions.class
)

java Exceptions.java

pause
