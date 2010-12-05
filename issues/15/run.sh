#!/bin/sh -x
javac -g Teste.java
# This is the line reported by the user.  It does not work under
# Windows/cygwin with java 1.6.0_22
#java  -classpath . -Xdebug -agentlib:jdwp=transport=dt_socket,server=y,address.0.0:6001 Teste

# This line should be similar.
java  -classpath . -Xdebug -agentlib:jdwp=transport=dt_socket,server=y,address=0.0:6001 Teste

# EOF