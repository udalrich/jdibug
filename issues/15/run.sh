#!/bin/sh
javac -g Teste.java
java  -classpath . -Xdebug -agentlib:jdwp=transport=dt_socket,server=y,address.0.0:6001 Teste
