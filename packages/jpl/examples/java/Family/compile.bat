@rem this assumes that jpl.dll is somewhere on your PATH
@rem and that jpl.pl is in your Prolog library
call ..\env.bat

javac -deprecation Family.java
@pause

