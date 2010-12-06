package com.jdibug; // Generated package name

import javax.swing.JFrame;
import java.awt.event.WindowEvent;
import java.awt.event.WindowAdapter;
import javax.swing.JLabel;

public class Gui
{
    public void drawStuff()
    {
        final JFrame frame = new JFrame("JDIbug testing");
        frame.getContentPane().add(new JLabel("a label"));
        frame.addWindowListener(new WindowAdapter()
            {
                @Override
                public void windowClosing(WindowEvent evt)
                {
                    synchronized(frame)
                    {
                        frame.notify();
                    }
                }
            });

        synchronized (frame)
        {
            frame.pack();
            frame.setVisible(true);
            try
            {
                frame.wait();
            } catch (InterruptedException exc) {
                exc.printStackTrace();
            }

        }

    }
}