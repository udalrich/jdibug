#!/bin/sh -x
#
# What steps will reproduce the problem?
# 1. Download JdibugTest.java
# 2. run  javac JdibugTest.java
# 3. run java -Xdebug -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=y JdibugTest
# 4. Open JdibugTest.java in emacs.

# 5. Set a breakpoint at doStuff() using jdibug-toggle-breakpoint
# 6. Connect to the running process using jdibug-connect - The main thread is correctly shown in the Frame browser.
# 7. Resume execution using jdibug-resume.

# What is the expected output? What do you see instead?

# Expect execution to suspend at the breakpoint set at step 5. Instead, execution continues until main() exits.

javac JdibugTest.java

java -Xdebug -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=y JdibugTest &

emacs -Q --load jdibug-setup.el --load jdibug-config.el JdibugTest.java
