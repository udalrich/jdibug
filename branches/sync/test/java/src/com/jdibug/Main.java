package com.jdibug; // Generated package name

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.ArrayList;

public class Main
{
    public static void main(String[] args)
    {
        float f = 1.2f;
        int intVar = 3;
        double dblVar = 3.4;
        float[] array = new float[10];
        array[1] =-3.4f;
        array[0] = Float.NaN;
        array[3] = Float.POSITIVE_INFINITY;
        array[4] = Float.NEGATIVE_INFINITY;
        array[2] = f*f;
        array[5] = 0f;
        array[7] = (float) Math.pow(2, -130); // subnormal
        System.out.println(Arrays.toString(array));

        int[] intArr = new int[] { 0, 32, -5432, Integer.MAX_VALUE,Integer.MIN_VALUE };

        List<String> list = Arrays.asList("foo", "bar");

        System.out.println(list);

        Main main = new Main();
        main.submitJobs();

        Gui gui = new Gui();
        gui.drawStuff();
    }

    private void submitJobs()
    {
        ExecutorService service = Executors.newFixedThreadPool(2);
        List<Future<?>> results = new ArrayList<Future<?>>();

         for (int index = 0; index < 10; ++index)
        {
            results.add(service.submit(new Runnable()
                {
                    @Override
                    public void run()
                    {
                        doStuff();
                    }
                }));
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
}
