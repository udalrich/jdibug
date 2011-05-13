package com.jdibug;

/**
 * Exception for testing breaking on exception
 *
 * Created: Sun Jan 02 13:38:55 2011
 *
 * @author <a href="mailto:udalrich.schermer@gmail.com"></a>
 * @version 1.0
 */
public class JdibugException extends RuntimeException
{
	 public JdibugException(String cause)
	 {
		  super(cause);
	 }
}

