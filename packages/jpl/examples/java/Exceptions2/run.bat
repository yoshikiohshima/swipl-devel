@echo off
call ..\env.bat

if not exist Exceptions2.class (
  echo  Compiling Exceptions2.class
  javac Exceptions2.class
)

java Exceptions2.java

pause
