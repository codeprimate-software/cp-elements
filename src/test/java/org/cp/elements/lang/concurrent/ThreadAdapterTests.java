/*
 * Copyright 2011-Present Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.cp.elements.lang.concurrent;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit tests for {@link ThreadAdapter}.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ThreadAdapterTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Rule
  public Timeout globalTimeout = Timeout.seconds(5);

  @Mock
  private Thread mockThread;

  @Test
  public void constructThreadAdapterWithCurrentThread() {

    ThreadAdapter threadAdapter = new ThreadAdapter();

    assertThat(threadAdapter, is(notNullValue()));
    assertThat(threadAdapter.getDelegate(), is(equalTo(Thread.currentThread())));
  }

  @Test
  public void constructThreadAdapterWithRunnable() {

    Runnable mockRunnable = mock(Runnable.class);

    ThreadAdapter threadAdapter = new ThreadAdapter(mockRunnable);

    assertThat(threadAdapter, is(notNullValue()));
    assertThat(threadAdapter.getDelegate(), is(notNullValue()));
    assertThat(threadAdapter.getDelegate(), is(not(equalTo(Thread.currentThread()))));

    threadAdapter.getDelegate().run();

    verify(mockRunnable, times(1)).run();
  }

  @Test
  public void constructThreadAdapterWithThread() {

    ThreadAdapter threadAdapter = new ThreadAdapter(mockThread);

    assertThat(threadAdapter, is(notNullValue()));
    assertThat(threadAdapter.getDelegate(), is(sameInstance(mockThread)));
  }

  @Test
  public void constructThreadAdapterWithNull() {

    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The delegate Thread must not be null");

    new ThreadAdapter(null);
  }

  @Test
  public void isAliveReturnsFalseForNonStartedThread() {
    assertThat(new ThreadAdapter(mockThread).isAlive(), is(false));
  }

  @Test
  public void isBlockedReturnsTrueForBlockedThread() {

    when(mockThread.getState()).thenReturn(Thread.State.BLOCKED);

    assertThat(new ThreadAdapter(mockThread).isBlocked(), is(true));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isBlockedReturnsFalseForNonBlockedThread() {

    when(mockThread.getState()).thenReturn(Thread.State.RUNNABLE);

    assertThat(new ThreadAdapter(mockThread).isBlocked(), is(false));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isDaemonReturnsTrueForDaemonThread() {

    Thread testThread = new Thread("test");

    testThread.setDaemon(true);

    Thread spyThread = spy(testThread);

    assertThat(new ThreadAdapter(spyThread).isDaemon(), is(true));

    verify(spyThread, times(1)).isDaemon();
  }

  @Test
  public void isDaemonReturnsFalseForNonDaemonThread() {

    Thread testThread = new Thread("test");

    testThread.setDaemon(false);

    Thread spyThread = spy(testThread);

    assertThat(new ThreadAdapter(spyThread).isDaemon(), is(false));

    verify(spyThread, times(1)).isDaemon();
  }

  @Test
  public void isNonDaemonReturnsTrueForNonDaemonThread() {

    Thread testThread = new Thread("test");

    testThread.setDaemon(false);

    Thread spyThread = spy(testThread);

    assertThat(new ThreadAdapter(spyThread).isNonDaemon(), is(true));

    verify(spyThread, times(1)).isDaemon();
  }

  @Test
  public void isNonDaemonReturnsFalseForDaemonThread() {

    Thread testThread = new Thread("test");

    testThread.setDaemon(true);

    Thread spyThread = spy(testThread);

    assertThat(new ThreadAdapter(spyThread).isNonDaemon(), is(false));

    verify(spyThread, times(1)).isDaemon();
  }

  @Test
  public void isInterruptedReturnsTrueForInterruptedThread() {

    when(mockThread.isInterrupted()).thenReturn(true);

    assertThat(new ThreadAdapter(mockThread).isInterrupted(), is(true));

    verify(mockThread, times(1)).isInterrupted();
  }

  @Test
  public void isInterruptedReturnsFalseForUninterruptedThread() {

    when(mockThread.isInterrupted()).thenReturn(false);

    assertThat(new ThreadAdapter(mockThread).isInterrupted(), is(false));

    verify(mockThread, times(1)).isInterrupted();
  }

  @Test
  public void isNewReturnsTrueForNewThread() {

    when(mockThread.getState()).thenReturn(Thread.State.NEW);

    assertThat(new ThreadAdapter(mockThread).isNew(), is(true));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isNewReturnsFalseForOldThread() {

    when(mockThread.getState()).thenReturn(Thread.State.TERMINATED);

    assertThat(new ThreadAdapter(mockThread).isNew(), is(false));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isRunnableReturnsTrueForRunnableThread() {

    when(mockThread.getState()).thenReturn(Thread.State.RUNNABLE);

    assertThat(new ThreadAdapter(mockThread).isRunnable(), is(true));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isRunnableReturnsFalseForNonRunnableThread() {

    when(mockThread.getState()).thenReturn(Thread.State.WAITING);

    assertThat(new ThreadAdapter(mockThread).isRunnable(), is(false));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isTerminatedReturnsTrueForTerminatedThread() {

    when(mockThread.getState()).thenReturn(Thread.State.TERMINATED);

    assertThat(new ThreadAdapter(mockThread).isTerminated(), is(true));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isTerminatedReturnsFalseForNonTerminatedThread() {

    when(mockThread.getState()).thenReturn(Thread.State.NEW);

    assertThat(new ThreadAdapter(mockThread).isTerminated(), is(false));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isTimedWaitingReturnsTrueForTimedWaitingThread() {

    when(mockThread.getState()).thenReturn(Thread.State.TIMED_WAITING);

    assertThat(new ThreadAdapter(mockThread).isTimedWaiting(), is(true));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isTimedWaitingReturnsFalseForNonTimedWaitingThread() {

    when(mockThread.getState()).thenReturn(Thread.State.RUNNABLE);

    assertThat(new ThreadAdapter(mockThread).isTimedWaiting(), is(false));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isWaitingReturnsTrueForWaitingThread() {

    when(mockThread.getState()).thenReturn(Thread.State.WAITING);

    assertThat(new ThreadAdapter(mockThread).isWaiting(), is(true));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void isWaitingReturnsFalseForNonWaitingThread() {

    when(mockThread.getState()).thenReturn(Thread.State.RUNNABLE);

    assertThat(new ThreadAdapter(mockThread).isWaiting(), is(false));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void setContextClassLoader() {

    ClassLoader expectedClassLoader = Thread.currentThread().getContextClassLoader();

    ThreadAdapter expectedThreadAdapter = new ThreadAdapter(mockThread);
    ThreadAdapter actualThreadAdatper = expectedThreadAdapter.setContextClassLoader(expectedClassLoader);

    assertThat(expectedClassLoader, is(notNullValue()));
    assertThat(actualThreadAdatper, is(sameInstance(expectedThreadAdapter)));

    verify(mockThread, times(1)).setContextClassLoader(same(expectedClassLoader));
  }

  @Test
  public void getContextClassLoader() {

    ClassLoader expectedClassLoader = Thread.currentThread().getContextClassLoader();

    when(mockThread.getContextClassLoader()).thenReturn(expectedClassLoader);

    assertThat(new ThreadAdapter(mockThread).getContextClassLoader(), is(sameInstance(expectedClassLoader)));

    verify(mockThread, times(1)).getContextClassLoader();
  }

  @Test
  public void setDaemon() {

    ThreadAdapter expectedThreadAdapter = new ThreadAdapter(mockThread);
    ThreadAdapter actualThreadAdapter = expectedThreadAdapter.setDaemon(true);

    assertThat(expectedThreadAdapter, is(notNullValue()));
    assertThat(actualThreadAdapter, is(sameInstance(expectedThreadAdapter)));

    verify(mockThread, times(1)).setDaemon(eq(true));
  }

  @Test
  public void getId() {

    when(mockThread.getId()).thenReturn(1L);

    assertThat(new ThreadAdapter(mockThread).getId(), is(equalTo(1L)));

    verify(mockThread, times(1)).getId();
  }

  @Test
  public void setName() {

    Thread spyThread = spy(new Thread("test"));

    ThreadAdapter expectedThreadAdapter = new ThreadAdapter(spyThread);
    ThreadAdapter actualThreadAdapter = expectedThreadAdapter.setName("test");

    assertThat(expectedThreadAdapter, is(notNullValue()));
    assertThat(actualThreadAdapter, is(sameInstance(expectedThreadAdapter)));

    verify(spyThread, times(1)).setName("test");
  }

  @Test
  public void getName() {

    Thread spyThread = spy(new Thread("test"));

    assertThat(new ThreadAdapter(spyThread).getName(), is(equalTo("test")));

    verify(spyThread, times(1)).getName();
  }

  @Test
  public void setPriority() {

    Thread spyThread = spy(new Thread("test"));

    ThreadAdapter expectedThreadAdapter = new ThreadAdapter(spyThread);
    ThreadAdapter actualThreadAdapter = expectedThreadAdapter.setPriority(1);

    assertThat(expectedThreadAdapter, is(notNullValue()));
    assertThat(actualThreadAdapter, is(sameInstance(expectedThreadAdapter)));

    verify(spyThread, times(1)).setPriority(1);
  }

  @Test
  public void getPriority() {

    Thread testThread = new Thread("test");

    testThread.setPriority(10);

    Thread spyThread = spy(testThread);

    assertThat(new ThreadAdapter(spyThread).getPriority(), is(equalTo(10)));

    verify(spyThread, times(1)).getPriority();
  }

  @Test
  public void getStackTrace() {

    StackTraceElement[] expectedStackTrace = Thread.currentThread().getStackTrace();

    when(mockThread.getStackTrace()).thenReturn(expectedStackTrace);

    assertThat(new ThreadAdapter(mockThread).getStackTrace(), is(equalTo(expectedStackTrace)));

    verify(mockThread, times(1)).getStackTrace();
  }

  @Test
  public void getState() {

    when(mockThread.getState()).thenReturn(Thread.State.RUNNABLE);

    assertThat(new ThreadAdapter(mockThread).getState(), is(equalTo(Thread.State.RUNNABLE)));

    verify(mockThread, times(1)).getState();
  }

  @Test
  public void getThreadGroup() {

    ThreadGroup testThreadGroup = new ThreadGroup("test");

    Thread spyThread = spy(new Thread(testThreadGroup, "test"));

    assertThat(new ThreadAdapter(spyThread).getThreadGroup(), is(equalTo(testThreadGroup)));

    verify(spyThread, times(1)).getThreadGroup();
  }

  @Test
  public void setUncaughtExceptionHandler() {

    Thread.UncaughtExceptionHandler mockUncaughtExceptionHandler = mock(Thread.UncaughtExceptionHandler.class);

    ThreadAdapter expectedThreadAdapter = new ThreadAdapter(mockThread);
    ThreadAdapter actualThreadAdapter = expectedThreadAdapter.setUncaughtExceptionHandler(mockUncaughtExceptionHandler);

    assertThat(expectedThreadAdapter, is(notNullValue()));
    assertThat(actualThreadAdapter, is(sameInstance(expectedThreadAdapter)));

    verify(mockThread, times(1)).setUncaughtExceptionHandler(eq(mockUncaughtExceptionHandler));
  }

  @Test
  public void getUncaughtExceptionHandler() {

    Thread.UncaughtExceptionHandler mockUncaughtExceptionHandler = mock(Thread.UncaughtExceptionHandler.class);

    when(mockThread.getUncaughtExceptionHandler()).thenReturn(mockUncaughtExceptionHandler);

    assertThat(new ThreadAdapter(mockThread).getUncaughtExceptionHandler(), is(equalTo(mockUncaughtExceptionHandler)));

    verify(mockThread, times(1)).getUncaughtExceptionHandler();
  }

  @Test
  public void checkAccessSucceeds() {

    Thread spyThread = spy(new Thread("test"));

    new ThreadAdapter(spyThread).checkAccess();

    verify(spyThread, times(1)).checkAccess();
  }

  @Test
  public void dumpStackWasCalled() {

    PrintStream systemErr = System.err;

    try {
      ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
      PrintStream errorStream = new PrintStream(outputStream);

      System.setErr(errorStream);

      new ThreadAdapter(mockThread).dumpStack();

      errorStream.flush();

      byte[] errorStreamBytes = outputStream.toByteArray();
      String stackTrace = new String(errorStreamBytes);

      //System.out.println(stackTrace);

      assertThat(stackTrace, containsString("java.lang.Exception: Stack trace"));
      assertThat(stackTrace, containsString("at java.lang.Thread.dumpStack"));
    }
    finally {
      System.setErr(systemErr);
    }
  }

  @Test
  public void interrupt() {

    new ThreadAdapter(mockThread).interrupt();

    verify(mockThread, times(1)).interrupt();
  }

  @Test
  public void join() throws Exception {

    AtomicBoolean runCalled = new AtomicBoolean(false);

    CountDownLatch latch = new CountDownLatch(1);

    ThreadAdapter joinedThread = new ThreadAdapter(() -> {
      try {
        latch.await();
      }
      catch (InterruptedException ignore) {
      }
      finally {
        runCalled.compareAndSet(false, true);
      }
    });

    joinedThread.setDaemon(true);
    joinedThread.setName("Joined Thread");
    joinedThread.setPriority(Thread.NORM_PRIORITY);
    joinedThread.start();
    latch.countDown();
    joinedThread.join();

    assertThat(joinedThread.isTerminated(), is(true));
    assertThat(runCalled.get(), is(true));
  }

  @Test
  public void joinRespondsToInterruption() throws Throwable {
    TestFramework.runOnce(new JoinInterruptionMultithreadedTestCase());
  }

  @Test
  public void joinWithMilliseconds() throws Exception {

    AtomicBoolean runCalled = new AtomicBoolean(false);
    AtomicBoolean runFinished = new AtomicBoolean(false);

    CountDownLatch latch = new CountDownLatch(1);

    ThreadAdapter joinedThread = new ThreadAdapter(() -> {
      try {
        runCalled.compareAndSet(false, true);
        latch.await();
      }
      catch (InterruptedException ignore) {
      }
      finally {
        runFinished.compareAndSet(false, true);
      }
    });

    try {
      joinedThread.setDaemon(true);
      joinedThread.setName("Joined Thread");
      joinedThread.setPriority(Thread.NORM_PRIORITY);
      joinedThread.start();
      joinedThread.join(200L);

      assertThat(joinedThread.isTerminated(), is(false));
      assertThat(runCalled.get(), is(true));
      assertThat(runFinished.get(), is(false));
    }
    finally {
      latch.countDown();
      joinedThread.join();

      assertThat(joinedThread.isTerminated(), is(true));
      assertThat(runFinished.get(), is(true));
    }
  }

  @Test
  public void joinWithMillisecondsRespondsToInterruption() throws Throwable {
    TestFramework.runOnce(new JoinInterruptionMultithreadedTestCase(TimeUnit.SECONDS.toMillis(5)));
  }

  @Test
  public void joinWithMillisecondsAndNanoseconds() throws Exception {

    AtomicBoolean runCalled = new AtomicBoolean(false);
    AtomicBoolean runFinished = new AtomicBoolean(false);

    CountDownLatch latch = new CountDownLatch(1);

    ThreadAdapter joinedThread = new ThreadAdapter(() -> {
      try {
        runCalled.compareAndSet(false, true);
        latch.await();
      }
      catch (InterruptedException ignore) {
      }
      finally {
        runFinished.compareAndSet(false, true);
      }
    });

    try {
      joinedThread.setDaemon(true);
      joinedThread.setName("Joined Thread");
      joinedThread.setPriority(Thread.NORM_PRIORITY);
      joinedThread.start();
      joinedThread.join(200L, 500);

      assertThat(joinedThread.isTerminated(), is(false));
      assertThat(runCalled.get(), is(true));
      assertThat(runFinished.get(), is(false));
    }
    finally {
      latch.countDown();
      joinedThread.join();

      assertThat(joinedThread.isTerminated(), is(true));
      assertThat(runFinished.get(), is(true));
    }
  }

  @Test
  public void joinWithMillisecondsAndNanosecondsRespondsToInterruption() throws Throwable {
    TestFramework.runOnce(new JoinInterruptionMultithreadedTestCase(TimeUnit.SECONDS.toMillis(5), 500));
  }

  @Test
  public void run() {

    new ThreadAdapter(mockThread).run();

    verify(mockThread, times(1)).run();
  }

  @Test
  public void start() {

    new ThreadAdapter(mockThread).start();

    verify(mockThread, times(1)).start();
  }

  @Test
  public void toStringIsCorrect() {

    ThreadGroup testThreadGroup = new ThreadGroup("ToStringTestThreadGroup");

    Thread testThread = spy(new Thread(testThreadGroup, () -> {}, "ToStringTestThread"));

    testThread.setDaemon(true);
    testThread.setPriority(9);

    when(testThread.getState()).thenReturn(Thread.State.RUNNABLE);

    ThreadAdapter testThreadAdapter = new ThreadAdapter(testThread);

    assertThat(testThreadAdapter.getDelegate(), is(sameInstance(testThread)));
    assertThat(testThreadAdapter.toString(), is(equalTo(String.format(
      "{ @type = %s, id = %d, name = ToStringTestThread, daemon = true, group = %s, priority = 9, state = RUNNABLE }",
        testThreadAdapter.getClass().getName(), testThread.getId(), testThreadGroup))));
  }

  @SuppressWarnings("unused")
  protected static final class JoinInterruptionMultithreadedTestCase extends MultithreadedTestCase {

    private final AtomicBoolean interrupted = new AtomicBoolean(false);

    private final Integer nanoseconds;

    private final Long milliseconds;

    private volatile ThreadAdapter joiningThread;
    private volatile ThreadAdapter interruptingThread;

    public JoinInterruptionMultithreadedTestCase() {
      this(null, null);
    }

    JoinInterruptionMultithreadedTestCase(Long milliseconds) {
      this(milliseconds, null);
    }

    JoinInterruptionMultithreadedTestCase(Long milliseconds, Integer nanoseconds) {
      this.milliseconds = milliseconds;
      this.nanoseconds = nanoseconds;
    }

    Thread.State getExpectedJoiningThreadState() {
      return (milliseconds != null ? Thread.State.TIMED_WAITING : Thread.State.WAITING);
    }

    public void thread1() {
      assertTick(0);

      interruptingThread = new ThreadAdapter(Thread.currentThread()).setName("Interrupting Thread");

      waitForTick(2);

      assertThat(joiningThread.getState(), is(equalTo(getExpectedJoiningThreadState())));
      assertThat(interrupted.get(), is(false));

      joiningThread.interrupt();

      waitForTick(3);

      assertThat(interrupted.get(), is(true));
      assertThat(joiningThread.isTerminated(), is(true));
    }

    public void thread2() {
      waitForTick(1);

      joiningThread = new ThreadAdapter(Thread.currentThread()).setName("Joining Thread");

      assertThat(interruptingThread, is(notNullValue()));

      try {
        if (milliseconds != null && nanoseconds != null) {
          interruptingThread.join(milliseconds, nanoseconds);
        }
        else if (milliseconds != null) {
          interruptingThread.join(milliseconds);
        }
        else {
          interruptingThread.join();
        }
      }
      catch (InterruptedException e) {
        interrupted.compareAndSet(false, true);
      }
    }
  }
}
