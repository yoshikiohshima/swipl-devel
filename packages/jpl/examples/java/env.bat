@rem Set variables needed by the examples
@echo off

set SWI_PROLOG_HOME=C:\Program Files\pl
set JAVA_HOME=C:\j2sdk1.4.2_04
set PATH=%PATH%;%SWI_PROLOG_HOME%\bin;%JAVA_HOME%\bin

rem Find classpath for jpl.jar.  First case holds if we are in the source
rem tree.

if exist ..\..\..\jpl.jar (
  set CLASSPATH=.;..\..\..\jpl.jar
) else (
  set CLASSPATH=.;%SWI_PROLOG_HOME%\lib\jpl.jar
)

