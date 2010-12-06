package com.jdibug; // Generated package name

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.ArrayList;

public class Main
{
    public static void main(String[] args)
    {
        Stuff stuff = new Stuff();
        stuff.x = 7;

        System.out.println("Created stuff");

        int twoAsInt = 2;

        float f = 1.2f;
        int intVar = 3;
        double dblVar = 3.4;
        System.out.println("Created dbl Var");

        float[] floatArray = new float[10];
        floatArray[1] =-3.4f;
        floatArray[0] = Float.NaN;
        floatArray[3] = Float.POSITIVE_INFINITY;
        floatArray[4] = Float.NEGATIVE_INFINITY;
        floatArray[2] = f*f;
        floatArray[5] = 0f;
        floatArray[7] = (float) Math.pow(2, -130); // subnormal
        System.out.println(Arrays.toString(floatArray));

        int[] intArr = new int[] { 0, 32, -5432, Integer.MAX_VALUE,Integer.MIN_VALUE };

        List<String> list = Arrays.asList("foo", "bar");

		// Large array for testing display of sub-arrays
		Object[] largeArray = new Object[20000];
		largeArray[17] = floatArray;
		largeArray[173] = list;


        System.out.println(list);

        Main main = new Main();
        main.submitJobs();
		System.out.println("submitJobs returned");
        Gui gui = new Gui();
        gui.drawStuff();

		System.out.println("Main.main finished");
    }

    private void submitJobs()
    {
        ExecutorService service = Executors.newFixedThreadPool(2);
        List<Future<?>> results = new ArrayList<Future<?>>();

		Collection<Runnable> tasks = new ArrayList<Runnable>();
		for (int index = 0; index < 10; ++index)
		{
			tasks.add(new Runnable()
				{
					@Override
					public void run()
					{
						doStuff();
					}
				});
		}

		for (Runnable runnable: tasks)
		{
            results.add(service.submit(runnable));
		}

        for (Future<?> result: results)
        {
            try
            {
                result.get();
            } catch (Exception exc)
            {
                exc.printStackTrace();
            }
        }
    }

    private void doStuff()
    {
        float x = 3;
        float y = 5;
        double[] array = new double[] { 3.4, -4, 5.4, Double.NaN,
                                        Double.POSITIVE_INFINITY,
                                        Double.NEGATIVE_INFINITY,
                                        0 };

        for (int index = 0; index < 100; ++index)
        {
            y = y*x - y;
            x = x*y - x;
            System.out.println("x=" + x + ", y=" + y);
        }

    }

    private static class Stuff
    {
        int x;
        double y;
    }
}
