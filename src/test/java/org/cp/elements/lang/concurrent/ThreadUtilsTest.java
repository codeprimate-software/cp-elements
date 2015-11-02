/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang.concurrent;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * The ThreadUtilsTest class is a test suite of test cases testing the contract and functionality of the 
 * ThreadUtils class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.concurrent.ThreadUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ThreadUtilsTest {

  protected static void sleep(final long milliseconds) {
    try {
      Thread.sleep(milliseconds);
    }
    catch (InterruptedException ignore) {
    }
  }

  @Before
  public void setup() {
    Thread.interrupted(); // clear the interrupt status of the current Thread
  }

  @Test
  public void testGetName() {
    assertEquals("t1", ThreadUtils.getName(new Thread("t1")));
  }

  @Test
  public void testGetNameForNullThread() {
    assertNull(ThreadUtils.getName(null));
  }

  @Test
  public void testJoin() {
    final boolean[] array = { false };
    final int expectedWait = 500;
    final long t0 = System.currentTimeMillis();

    final Runnable testThreadRunnable = () -> {
      array[0] = true;
      sleep(expectedWait);
    };

    assertFalse(array[0]);

    final Thread testThread = new Thread(testThreadRunnable, "Test Thread");
    testThread.setDaemon(false);
    testThread.start();

    assertTrue(ThreadUtils.join(testThread, expectedWait, 0));

    final long t1 = System.currentTimeMillis();

    assertFalse(Thread.interrupted());
    assertTrue(array[0]);
    assertTrue((t1 - t0) >= expectedWait);
  }

  @Test
  public void testJoinInterrupted() throws Throwable {
    TestFramework.runOnce(new JoinInterruptedMultithreadedTestCase());
  }

  //@Test
  //@Ignore
  @SuppressWarnings("unused")
  public void testJoinInterruptedDeprecated() {
    final Thread mainThread = Thread.currentThread();

    final Runnable testThreadRunnable = mainThread::interrupt;

    final Thread testThread = new Thread(testThreadRunnable, "Test Thread");
    testThread.setDaemon(false);
    testThread.start();

    assertFalse(ThreadUtils.join(testThread, 500, 0));
    assertTrue(mainThread.isInterrupted());
  }

  @Test
  public void testPause() {
    final long expectedWait = 500;
    final long t0 = System.currentTimeMillis();

    assertTrue(ThreadUtils.pause(expectedWait, 0));

    final long t1 = System.currentTimeMillis();

    assertTrue((t1 - t0) >= expectedWait);
  }

  @SuppressWarnings("unused")
  protected static final class JoinInterruptedMultithreadedTestCase extends MultithreadedTestCase {

    private Thread interruptingThread;
    private Thread joiningThread;

    public void thread1() {
      assertTick(0);

      interruptingThread = Thread.currentThread();
      interruptingThread.setName("Interrupting Thread");

      waitForTick(2);
      assertNotNull(joiningThread);

      joiningThread.interrupt();

      waitForTick(3);
    }

    public void thread2() {
      waitForTick(1);

      joiningThread = Thread.currentThread();
      joiningThread.setName("Joining Thread");

      assertNotNull(interruptingThread);
      assertFalse(ThreadUtils.join(interruptingThread, 0, 0));
      assertTrue(joiningThread.isInterrupted());
    }

    @Override
    public void finish() {
      assertFalse(interruptingThread.isAlive());
      assertFalse(joiningThread.isAlive());
    }
  }

}
