package com.jdibug;

/**
 * Describe class ExcMain here.
 *
 *
 * Created: Sun Jan 02 18:11:59 2011
 *
 * @author <a href="mailto:udalrich@TROY"></a>
 * @version 1.0
 */
public class ExcMain {

	 /**
	  * Creates a new <code>ExcMain</code> instance.
	  *
	  */
	 public ExcMain() {

	 }

	 public static void main(String[] args)
	 {
		  new Thread(new Runnable() {
					 @Override public void run()
					 {
						  throw new JdibugFooException("uncaught");
					 }
				}).start();
		  try {
				Thread.sleep(5000L);
		  } catch (Exception exc) {
				exc.printStackTrace();
		  }

	 }
}
