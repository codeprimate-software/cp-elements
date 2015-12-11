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

import static org.cp.elements.lang.concurrent.ThreadUtils.waitFor;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.number.OrderingComparisons.greaterThanOrEqualTo;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.cp.elements.lang.Condition;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * The ThreadUtilsTest class is a test suite of test cases testing the contract and functionality of the 
 * ThreadUtils class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.concurrent.ThreadUtils
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ThreadUtilsTest {

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  @Mock
  private Thread mockThread;

  protected static void sleep(final long milliseconds) {
    try {
      Thread.sleep(milliseconds);
    }
    catch (InterruptedException ignore) {
    }
  }

  @Before
  public void setup() {
    // clear the interrupt status of the current Thread
    Thread.interrupted();
  }

  @Test
  public void isAliveWithNull() {
    assertThat(ThreadUtils.isAlive(null), is(false));
  }

  @Test
  public void isBlockedWithBlockedThread() {
    when(mockThread.getState()).thenReturn(Thread.State.BLOCKED);
    assertThat(ThreadUtils.isBlocked(mockThread), is(true));
    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isBlockedWithRunnableThread() {
    when(mockThread.getState()).thenReturn(Thread.State.RUNNABLE);
    assertThat(ThreadUtils.isBlocked(mockThread), is(false));
    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isBlockedWithNull() {
    assertThat(ThreadUtils.isBlocked(null), is(false));
  }

  @Test
  public void isDaemonWithDaemonThread() {
    Thread thread = new Thread("testDaemonThread");
    thread.setDaemon(true);

    assertThat(ThreadUtils.isDaemon(thread), is(true));
  }

  @Test
  public void isDaemonWithNonDaemonThread() {
    Thread thread = new Thread("testNonDaemonThread");
    thread.setDaemon(false);

    assertThat(ThreadUtils.isDaemon(thread), is(false));
  }

  @Test
  public void isDaemonWithNull() {
    assertThat(ThreadUtils.isDaemon(null), is(false));
  }

  @Test
  public void isNonDaemonWithNonDaemonThread() {
    Thread thread = new Thread("testNonDaemonThread");
    thread.setDaemon(false);

    assertThat(ThreadUtils.isNonDaemon(thread), is(true));
  }

  @Test
  public void isNonDaemonWithDaemonThread() {
    Thread thread = new Thread("testDaemonThread");
    thread.setDaemon(true);

    assertThat(ThreadUtils.isNonDaemon(thread), is(false));
  }

  @Test
  public void isNonDaemonWithNull() {
    assertThat(ThreadUtils.isNonDaemon(null), is(false));
  }

  @Test
  public void isNewWithNewThread() {
    when(mockThread.getState()).thenReturn(Thread.State.NEW);
    assertThat(ThreadUtils.isNew(mockThread), is(true));
    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isNewWithTerminatedThread() {
    when(mockThread.getState()).thenReturn(Thread.State.TERMINATED);
    assertThat(ThreadUtils.isNew(mockThread), is(false));
    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isNewWithNull() {
    assertThat(ThreadUtils.isNew(null), is(false));
  }

  @Test
  public void isRunnableWithRunnableThread() {
    when(mockThread.getState()).thenReturn(Thread.State.RUNNABLE);
    assertThat(ThreadUtils.isRunnable(mockThread), is(true));
    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isRunnableWithBlockedThread() {
    when(mockThread.getState()).thenReturn(Thread.State.BLOCKED);
    assertThat(ThreadUtils.isRunnable(mockThread), is(false));
    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isRunnableWithNull() {
    assertThat(ThreadUtils.isRunnable(null), is(false));
  }

  @Test
  public void isTerminatedWithTerminatedThread() {
    when(mockThread.getState()).thenReturn(Thread.State.TERMINATED);
    assertThat(ThreadUtils.isTerminated(mockThread), is(true));
    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isTerminatedWithNewThread() {
    when(mockThread.getState()).thenReturn(Thread.State.NEW);
    assertThat(ThreadUtils.isTerminated(mockThread), is(false));
    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isTerminatedWithNull() {
    assertThat(ThreadUtils.isTerminated(null), is(false));
  }

  @Test
  public void isTimedWaitingWithTimedWaitingThread() {
    when(mockThread.getState()).thenReturn(Thread.State.TIMED_WAITING);
    assertThat(ThreadUtils.isTimedWaiting(mockThread), is(true));
    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isTimedWaitingWithWaitingThread() {
    when(mockThread.getState()).thenReturn(Thread.State.WAITING);
    assertThat(ThreadUtils.isTimedWaiting(mockThread), is(false));
    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isTimeWaitingWithNull() {
    assertThat(ThreadUtils.isTimedWaiting(null), is(false));
  }

  @Test
  public void isWaitingWithWaitingThread() {
    when(mockThread.getState()).thenReturn(Thread.State.WAITING);
    assertThat(ThreadUtils.isWaiting(mockThread), is(true));
    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isWaitingWithTimedWaitingThread() {
    when(mockThread.getState()).thenReturn(Thread.State.TIMED_WAITING);
    assertThat(ThreadUtils.isWaiting(mockThread), is(false));
    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isWaitingWithNull() {
    assertThat(ThreadUtils.isWaiting(null), is(false));
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
  public void getContextClassLoaderWithNull() {
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
  public void getIdWithNull() {
    assertThat(ThreadUtils.getId(null), is(equalTo(0l)));
  }

  @Test
  public void getNameOfNonNullThread() {
    assertThat(ThreadUtils.getName(new Thread("test")), is(equalTo("test")));
  }

  @Test
  public void getNameWithNull() {
    assertThat(ThreadUtils.getName(null), is(nullValue()));
  }

  @Test
  public void getPriorityOfNonNullThread() {
    Thread thread = new Thread("test");
    thread.setPriority(1);

    assertThat(ThreadUtils.getPriority(thread), is(equalTo(1)));
  }

  @Test
  public void getPriorityWithNull() {
    assertThat(ThreadUtils.getPriority(null), is(equalTo(0)));
  }

  @Test
  public void getStackTraceOfNonNullThread() {
    StackTraceElement[] expectedStackTrace = Thread.currentThread().getStackTrace();
    Thread mockThread = mock(Thread.class);

    when(mockThread.getStackTrace()).thenReturn(expectedStackTrace);

    assertThat(ThreadUtils.getStackTrace(mockThread), is(equalTo(expectedStackTrace)));

    verify(mockThread, times(1)).getStackTrace();
  }

  @Test
  public void getStackTraceWithNull() {
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
  public void getStateWithNull() {
    assertThat(ThreadUtils.getState(null), is(nullValue()));
  }

  @Test
  public void getThreadGroupOfNonNullThread() {
    ThreadGroup expectedThreadGroup = new ThreadGroup("test");
    Thread thread = new Thread(expectedThreadGroup, "test");

    assertThat(ThreadUtils.getThreadGroup(thread), is(equalTo(expectedThreadGroup)));
  }

  @Test
  public void getThreadGroupWithNull() {
    assertThat(ThreadUtils.getThreadGroup(null), is(nullValue()));
  }

  @Test
  public void interruptInterruptsThread() {
    ThreadUtils.interrupt(mockThread);
    verify(mockThread, times(1)).interrupt();
  }

  @Test
  public void interruptWithNull() {
    ThreadUtils.interrupt(null);
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

  @Test
  @Ignore
  @Deprecated
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

  @Test
  @Ignore
  public void waitForDuration() {
    final long timeout = (System.currentTimeMillis() + 500);

    assertThat(waitFor(500, TimeUnit.MILLISECONDS).checkEvery(100, TimeUnit.MILLISECONDS).run(), is(true));
    assertThat(System.currentTimeMillis(), is(greaterThanOrEqualTo(timeout)));
  }

  @Test
  public void waitForPassesConditionImmediately() {
    AtomicInteger count = new AtomicInteger(0);

    Condition countCondition = () ->  (count.incrementAndGet() > 0);

    assertThat(count.get(), is(equalTo(0)));
    assertThat(waitFor(5, TimeUnit.SECONDS).checkEvery(1, TimeUnit.SECONDS).on(countCondition), is(true));
    assertThat(count.get(), is(equalTo(2)));
  }

  @Test
  public void waitForWithInvalidDuration() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("duration (-500) must be greater than 0");

    waitFor(-500);
  }

  @Test
  public void waitForWithIntervalGreaterThanDuration() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("interval (2 SECONDS) must be greater than 0 and less than equal to duration (500 MILLISECONDS)");

    waitFor(500).checkEvery(2, TimeUnit.SECONDS);
  }

  @Test
  public void waitForWithNegativeInterval() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("interval (-1 SECONDS) must be greater than 0 and less than equal to duration (500 MILLISECONDS)");

    waitFor(500).checkEvery(-1, TimeUnit.SECONDS);
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
