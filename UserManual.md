# This page is no longer maintained #
Please see the latest [official user manual](http://jdibug.googlecode.com/svn/trunk/jdibug.html).


JDIbug User Manual


1 Rationale


> One word, locals browser. As I started using JDE, there are two debuggers that are available, which are jdb and jdebug.

> jdb is too simple to be used as a debugger, I find myself doing `locals' repeatedly while stepping through the codes, and the output of `locals' are not sorted! Its worse when I want to inspect the attributes of particular objects.

> jdebug is too slow and buggy for my liking, stepping through the source files takes forever.

> I then started using jswat, which is very user friendly. But it doesn't integrate with Emacs.

> So I created JDIbug.

2 Installation


> JDIbug is hosted at googlecode, at `http://code.google.com/p/jdibug/'

> Download all the package file and untar them into somewhere within your `load-path'. For example, I keep all my emacs modules under my `~/emacs/site', so the jdibug `.el'  files will go into ~/emacs/site/jdibug-0.1'.

> Then add the path into your `load-path', and `require' it, for example, I have the following in my `.emacs' file:
```
     (add-to-list 'load-path (expand-file-name "~/emacs/site/jdibug-0.1"))
     (require 'jdibug)
```

> That's it, you are good to go.

3 Configuration


> Although JDIbug current does not rely on any data from JDE, a lot of things are made very simple when using JDIbug with JDE. So please install JDE if you have not yet done it.

> There is only one parameter that you need to configure before you can start using JDIbug in version 0.3, which is the `jdibug-connect-hosts' parameter.  This is a list of strings of the form "hostname:port".  "hostname" specifies the hostname of the debuggee process that you want to debug. "port" specifies the port to connect to.

> If you are using JDEE, you will also want to customize jde-run-option-debug, as shown below.

http://jdibug.googlecode.com/files/jde-run-option-debug.PNG

> Ensure that you use the same port number when customizing `jdibug-connect-hosts' and `jde-run-option-debug'.


> If you are not using JDEE, you will want to add the following lines the your java command.  (These values work with the Sun JVM.  Other JVM's may need different parameters.)
```
-Xdebug -Xrunjdwp:transport=dt_socket,address=6001,server=y,suspend=y
```


> If you have multiple projects set up using JDE's `prj.el' file, you can set the two parameters inside the `prj.el' file so that different projects can be connected using different host/ports by JDIbug. Having said that, JDIbug currently _does not_ support more than one debugging session per emacs session.

4 Connecting


> After configuring JDIbug, use the key sequence `C-c C-c C-c' in a jde-mode buffer to connect to the debuggee process. By default, the frame will be splitted into windows each displaying different buffers.

> With the java source file on the top left, the locals browser on the top right, the stack browser on the bottom left, and the breakpoints list on the bottom right.

> To disconnect from the debuggee, use the key sequence `C-c C-c C-d'.  This will allow the debuggee to continue running.  To disconnect and kill the debuggee, use M-x jdibug-exit-jvm.

5 Breakpoints


> To toggle the breakpoint on the current line of java source, use the key sequence `C-c C-c C-b' in the java source buffer. This toggles the breakpoints on the current line between, disabled, enabled, and no breakpoint. The "disabled" status is just an easy way of remembering that line as a line of interest but you do not want the debugger to stop there.

> If the line of code is not loaded when the breakpoint is requested, it will be marked as pending. When the class is finally loaded, the breakpoint will be installed.

6 Stepping


> When the debugger is suspended, you can use the following key sequences to step through the program.

> C-c C-c C-n to step over.
> C-c C-c C-i to step into.
> C-c C-c C-o to step out.

> As these keys are used very often, I bind them to function keys for faster access. I have the following in my .emacs file:
```
     (define-key jde-mode-map [f8]   'jdibug-step-over)
     (define-key jde-mode-map [M-f8] 'jdibug-step-into)
     (define-key jde-mode-map [f7]   'jdibug-step-out)
     (define-key jde-mode-map [M-f7] 'jdibug-resume)
```
> When you are done, you can use the key sequence `C-c C-c C-r' to resume the debugger process. Or if you have added the snipplet above, you can just `M-f7' for that.

7 Locals Browser


> The locals browser uses the emacs tree-mode to display the local variables. Each of the variables can be expanded to view the member variables by clicking on the expand icon.

> When the cursor is over any element in the tree, you can press the `s' key to invoke the `toString' method on the variable under the cursor. The result will be printed in the echo-area.

> One extra feature that is very useful in the Locals Browser is that it reduces the number of clicks that you have to do to inspect a variable. By default, a number of java objects will be displayed with their string representation.  For example Boolean, Number, StringBuffer etc. For descendants of Collection and Map, there will be extra information displayed about the number of items that are within the Collection of Map.

> If you like the above feature, there's more! You can define custom displayer functions for your own java objects, for example, if you have a class `com.foo.Dog' which have a getter called `getName', and you want to see the name of the dog displayed, you can just add the following line into your `.emacs' (after the `(require 'jdibug)' line) and viola, you will see the name of the dog instead of the `Dog' class name.
```
     (add-to-list 'jdi-value-custom-set-strings '("com.foo.Dog" "getName"))
```

> When expanding a Collection or Map, the locals browser will be displaying the contents as an array instead of having you to click one or two more times to see the contents within the Collection or Map.

8 Frames Browser


> The frames buffer is a simple list of all the frames from the `main' up to the current function. It provides a simple caller/callee stack of the current breakpoint. If the frame points to a location which can be corresponded to a line of code in the source path, the frame will be clickable, and the source code buffer will be updated to show the file and line number. The locals browser will also be updated to show the locals of that particular frame.

> Please note that the current execution point does not change when you switch between the frames, so if you do `jdibug-step-over', the stepping will go back to the original execution point.

9 Breakpoints Buffer


> The breakpoints buffer shows a list of the current active and disabled breakpoints. Clicking on the breakpoints will show the line of code of the breakpoint in the source code buffer.

10 Window Layout


> The default four buffers layout is what I like the most on my screen.  If you don't like it, you just have to create your own functions that create the window layout of your liking, and then add this in your `.emacs' file:
```
     (remove-hook 'jdibug-connected-hook 'jdibug-debug-view-1)
     (remove-hook 'jdibug-detached-hook  'jdibug-undebug-view)
```

> There is an alternate view (jdibug-debug-view) that can be used instead.  Or you can proceed to add your equivalent functions in the above hooks. You can refer to the two functions `jdibug-debug-view' and `jdibug-undebug-view' for a start.