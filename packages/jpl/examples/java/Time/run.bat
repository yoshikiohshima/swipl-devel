@echo off
call ..\env.bat

if not exist Time.class (
  echo  Compiling Time.class
  javac Time.class
)

java Time.java

pause
