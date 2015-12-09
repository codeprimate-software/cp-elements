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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
  public void getContextClassLoaderOfNonNullThread() {
    ClassLoader mockClassLoader = mock(ClassLoader.class);
    Thread mockThread = mock(Thread.class);

    when(mockThread.getContextClassLoader()).thenReturn(mockClassLoader);

    assertThat(ThreadUtils.getContextClassLoader(mockThread), is(equalTo(mockClassLoader)));

    verify(mockThread, times(1)).getContextClassLoader();
  }

  @Test
  public void getContextClassLoaderOfNullThread() {
    assertThat(ThreadUtils.getContextClassLoader(null), is(ThreadUtils.class.getClassLoader()));
  }

  @Test
  public void getIdOfNonNullThread() {
    Thread mockThread = mock(Thread.class);

    when(mockThread.getId()).thenReturn(1l);

    assertThat(ThreadUtils.getId(mockThread), is(equalTo(1l)));

    verify(mockThread, times(1)).getId();
  }

  @Test
  public void getIdOfNullThread() {
    assertThat(ThreadUtils.getId(null), is(equalTo(0l)));
  }

  @Test
  public void getNameOfNonNullThread() {
    assertThat(ThreadUtils.getName(new Thread("test")), is(equalTo("test")));
  }

  @Test
  public void getNameOfNullThread() {
    assertThat(ThreadUtils.getName(null), is(nullValue()));
  }

  @Test
  public void getPriorityOfNonNullThread() {
    Thread thread = new Thread("test");
    thread.setPriority(1);

    assertThat(ThreadUtils.getPriority(thread), is(equalTo(1)));
  }

  @Test
  public void getPriorityOfNullThread() {
    assertThat(ThreadUtils.getPriority(null), is(equalTo(0)));
  }

  @Test
  public void getStackTraceOfMNonNullThread() {
    StackTraceElement[] expectedStackTrace = Thread.currentThread().getStackTrace();
    Thread mockThread = mock(Thread.class);

    when(mockThread.getStackTrace()).thenReturn(expectedStackTrace);

    assertThat(ThreadUtils.getStackTrace(mockThread), is(equalTo(expectedStackTrace)));

    verify(mockThread, times(1)).getStackTrace();
  }

  @Test
  public void getStackTraceOfNullThread() {
    StackTraceElement[] stackTrace = ThreadUtils.getStackTrace(null);

    assertThat(stackTrace, is(notNullValue()));
    assertThat(stackTrace.length, is(equalTo(0)));
  }

  @Test
  public void getStateOfNonNullThread() {
    Thread mockThread = mock(Thread.class);

    when(mockThread.getState()).thenReturn(Thread.State.RUNNABLE);

    assertThat(ThreadUtils.getState(mockThread), is(equalTo(Thread.State.RUNNABLE)));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void getStateOfNullThread() {
    assertThat(ThreadUtils.getState(null), is(nullValue()));
  }

  @Test
  public void getThreadGroupOfNonNullThread() {
    ThreadGroup expectedThreadGroup = new ThreadGroup("test");
    Thread thread = new Thread(expectedThreadGroup, "test");

    assertThat(ThreadUtils.getThreadGroup(thread), is(equalTo(expectedThreadGroup)));
  }

  @Test
  public void getThreadGroupOfNullThread() {
    assertThat(ThreadUtils.getThreadGroup(null), is(nullValue()));
  }

  @Test
  public void join() {
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
  public void joinInterrupted() throws Throwable {
    TestFramework.runOnce(new JoinInterruptedMultithreadedTestCase());
  }

  //@Test
  //@Ignore
  @SuppressWarnings("unused")
  public void joinInterruptedDeprecated() {
    Thread mainThread = Thread.currentThread();

    Runnable testRunnable = mainThread::interrupt;

    Thread testThread = new Thread(testRunnable, "Test Thread");

    testThread.setDaemon(false);
    testThread.start();

    assertFalse(ThreadUtils.join(testThread, 500, 0));
    assertTrue(mainThread.isInterrupted());
  }

  @Test
  public void pause() {
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
