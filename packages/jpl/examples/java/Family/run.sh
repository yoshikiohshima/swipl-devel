echo JPL demo: Family

JPL=`(cd ../../.. && pwd)`

export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$JPL"

java -classpath "$JPL/jpl.jar:." Family

