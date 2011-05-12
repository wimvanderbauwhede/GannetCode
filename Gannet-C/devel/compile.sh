#!/bin/sh
#rm -Rf GannetC
#rm -f *.class
#java -jar  /opt/sablecc-3.2/lib/sablecc.jar $*
sablecc $*
javac -classpath . Main.java

