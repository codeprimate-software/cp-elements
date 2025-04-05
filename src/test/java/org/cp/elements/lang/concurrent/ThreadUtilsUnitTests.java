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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Supplier;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.lang.Condition;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit Tests for {@link ThreadUtils}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.junit.jupiter.api.extension.ExtendWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.jupiter.MockitoExtension
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @see org.cp.elements.lang.concurrent.ThreadUtils
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
@SuppressWarnings("all")
@ExtendWith(MockitoExtension.class)
class ThreadUtilsUnitTests {

  @Mock
  private Thread mockThread;

  private static void sleep(long milliseconds) {

    try {
      Thread.sleep(milliseconds);
    }
    catch (InterruptedException ignore) { }
  }

  @AfterAll
  public static void tearDown() {
    Thread.interrupted();
  }

  @BeforeEach
  public void setup() {
    Thread.interrupted();
  }

  @Test
  void isAliveWithNullThreadIsNullSafe() {
    assertThat(ThreadUtils.isAlive(null)).isFalse();
  }

  @Test
  void isBlockedWithBlockedThread() {

    doReturn(Thread.State.BLOCKED).when(this.mockThread).getState();

    assertThat(ThreadUtils.isBlocked(this.mockThread)).isTrue();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isBlockedWithRunnableThread() {

    doReturn(Thread.State.RUNNABLE).when(this.mockThread).getState();

    assertThat(ThreadUtils.isBlocked(this.mockThread)).isFalse();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isBlockedWithNullThread() {
    assertThat(ThreadUtils.isBlocked(null)).isFalse();
  }

  @Test
  void isDaemonWithDaemonThread() {

    Thread thread = new Thread("testDaemonThread");

    thread.setDaemon(true);

    assertThat(ThreadUtils.isDaemon(thread)).isTrue();
  }

  @Test
  void isDaemonWithNonDaemonThread() {

    Thread thread = new Thread("testNonDaemonThread");

    thread.setDaemon(false);

    assertThat(ThreadUtils.isDaemon(thread)).isFalse();
  }

  @Test
  void isDaemonWithNullThread() {
    assertThat(ThreadUtils.isDaemon(null)).isFalse();
  }

  @Test
  void isNonDaemonWithNonDaemonThread() {

    Thread thread = new Thread("testNonDaemonThread");

    thread.setDaemon(false);

    assertThat(ThreadUtils.isNonDaemon(thread)).isTrue();
  }

  @Test
  void isNonDaemonWithDaemonThread() {

    Thread thread = new Thread("testDaemonThread");

    thread.setDaemon(true);

    assertThat(ThreadUtils.isNonDaemon(thread)).isFalse();
  }

  @Test
  void isNonDaemonWithNullThread() {
    assertThat(ThreadUtils.isNonDaemon(null)).isFalse();
  }

  @Test
  void isUserWithDaemonThread() {

    Thread thread = new Thread("testDaemonThread");

    thread.setDaemon(true);

    assertThat(ThreadUtils.isUserThread(thread)).isFalse();
  }

  @Test
  void isUserWithNonDaemonThread() {

    Thread thread = new Thread("testNonDaemonThread");

    thread.setDaemon(false);

    assertThat(ThreadUtils.isUserThread(thread)).isTrue();
  }

  @Test
  void isUserWithNullThread() {
    assertThat(ThreadUtils.isUserThread(null)).isFalse();
  }

  @Test
  void isInterruptedWithInterruptedThread() {

    doReturn(true).when(this.mockThread).isInterrupted();

    assertThat(ThreadUtils.isInterrupted(this.mockThread)).isTrue();

    verify(this.mockThread, times(1)).isInterrupted();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isInterruptedWithNonInterruptedThread() {

    doReturn(false).when(this.mockThread).isInterrupted();

    assertThat(ThreadUtils.isInterrupted(this.mockThread)).isFalse();

    verify(this.mockThread, times(1)).isInterrupted();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isInterruptedWithNullThread() {
    assertThat(ThreadUtils.isInterrupted(null)).isFalse();
  }

  private void testThreadInNonDesiredState(Thread.State desiredThreadState,
      Function<Thread, Boolean> threadStateTestFunction) {

    for (Thread.State threadState : Thread.State.values()) {
      if (!threadState.equals(desiredThreadState)) {
        doReturn(threadState).when(this.mockThread).getState();
        assertThat(threadStateTestFunction.apply(this.mockThread)).isFalse();
      }
    }

    verify(this.mockThread, times(Thread.State.values().length - 1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isNewWithNewThread() {

    doReturn(Thread.State.NEW).when(this.mockThread).getState();

    assertThat(ThreadUtils.isNew(this.mockThread)).isTrue();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isNewWithTerminatedThread() {

    doReturn(Thread.State.TERMINATED).when(this.mockThread).getState();

    assertThat(ThreadUtils.isNew(this.mockThread)).isFalse();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isNewWithNonNewThread() {
    testThreadInNonDesiredState(Thread.State.NEW, ThreadUtils::isNew);
  }

  @Test
  void isNewWithNullThread() {
    assertThat(ThreadUtils.isNew(null)).isFalse();
  }

  @Test
  void isRunnableWithRunnableThread() {

    doReturn(Thread.State.RUNNABLE).when(this.mockThread).getState();

    assertThat(ThreadUtils.isRunnable(this.mockThread)).isTrue();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isRunnableWithBlockedThread() {

    doReturn(Thread.State.BLOCKED).when(this.mockThread).getState();

    assertThat(ThreadUtils.isRunnable(this.mockThread)).isFalse();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isRunnableWithNonRunnableThread() {
    testThreadInNonDesiredState(Thread.State.RUNNABLE, ThreadUtils::isRunnable);
  }

  @Test
  void isRunnableWithNullThread() {
    assertThat(ThreadUtils.isRunnable(null)).isFalse();
  }

  @Test
  void isTerminatedWithTerminatedThread() {

    doReturn(Thread.State.TERMINATED).when(this.mockThread).getState();

    assertThat(ThreadUtils.isTerminated(this.mockThread)).isTrue();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isTerminatedWithNewThread() {

    doReturn(Thread.State.NEW).when(this.mockThread).getState();

    assertThat(ThreadUtils.isTerminated(this.mockThread)).isFalse();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isTerminatedWithNonTerminatedThread() {
    testThreadInNonDesiredState(Thread.State.TERMINATED, ThreadUtils::isTerminated);
  }

  @Test
  void isTerminatedWithNullThread() {
    assertThat(ThreadUtils.isTerminated(null)).isFalse();
  }

  @Test
  void isTimedWaitingWithTimedWaitingThread() {

    doReturn(Thread.State.TIMED_WAITING).when(this.mockThread).getState();

    assertThat(ThreadUtils.isTimedWaiting(this.mockThread)).isTrue();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isTimedWaitingWithWaitingThread() {

    doReturn(Thread.State.WAITING).when(this.mockThread).getState();

    assertThat(ThreadUtils.isTimedWaiting(this.mockThread)).isFalse();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isTimedWaitingWithNonTimedWaitingThread() {
    testThreadInNonDesiredState(Thread.State.TIMED_WAITING, ThreadUtils::isTimedWaiting);
  }

  @Test
  void isTimedWaitingWithNullThread() {
    assertThat(ThreadUtils.isTimedWaiting(null)).isFalse();
  }

  @Test
  void isWaitingWithWaitingThread() {

    doReturn(Thread.State.WAITING).when(this.mockThread).getState();

    assertThat(ThreadUtils.isWaiting(this.mockThread)).isTrue();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isWaitingWithTimedWaitingThread() {

    doReturn(Thread.State.TIMED_WAITING).when(this.mockThread).getState();

    assertThat(ThreadUtils.isWaiting(this.mockThread)).isFalse();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void isWaitingWithNonWaitingThread() {
    testThreadInNonDesiredState(Thread.State.WAITING, ThreadUtils::isWaiting);
  }

  @Test
  void isWaitingWithNullThread() {
    assertThat(ThreadUtils.isWaiting(null)).isFalse();
  }

  @Test
  void getContextClassLoaderFromMockThread() {

    ClassLoader mockClassLoader = mock(ClassLoader.class);

    doReturn(mockClassLoader).when(this.mockThread).getContextClassLoader();

    assertThat(ThreadUtils.getContextClassLoader(this.mockThread)).isEqualTo(mockClassLoader);

    verify(this.mockThread, times(1)).getContextClassLoader();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void getContextClassLoaderFromNonNullThread() {

    Thread thread = new Thread();

    ClassLoader threadContextClassLoader = thread.getContextClassLoader();

    assertThat(threadContextClassLoader).isNotNull();
    assertThat(ThreadUtils.getContextClassLoader(thread)).isEqualTo(threadContextClassLoader);
  }

  @Test
  void getContextClassLoaderFromNullThread() {
    assertThat(ThreadUtils.getContextClassLoader(null)).isEqualTo(ThreadUtils.class.getClassLoader());
  }

  @Test
  void getNameFromNonNullThread() {
    assertThat(ThreadUtils.getName(new Thread("test"))).isEqualTo("test");
  }

  @Test
  void getNameFromNullThread() {
    assertThat(ThreadUtils.getName(null)).isNull();
  }

  @Test
  void getPriorityFromNonNullThread() {

    Thread thread = new Thread("test");

    thread.setPriority(4);

    assertThat(ThreadUtils.getPriority(thread)).isEqualTo(4);
  }

  @Test
  void getPriorityFromNullThread() {
    assertThat(ThreadUtils.getPriority(null)).isEqualTo(Thread.NORM_PRIORITY);
  }

  @Test
  void getStackTraceFromNonNullThread() {

    StackTraceElement[] expectedStackTrace = Thread.currentThread().getStackTrace();

    doReturn(expectedStackTrace).when(this.mockThread).getStackTrace();

    assertThat(ThreadUtils.getStackTrace(this.mockThread)).isEqualTo(expectedStackTrace);

    verify(this.mockThread, times(1)).getStackTrace();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void getStackTraceFromNullThread() {

    StackTraceElement[] stackTrace = ThreadUtils.getStackTrace(null);

    assertThat(stackTrace).isNotNull();
    assertThat(stackTrace).isEmpty();
  }

  @Test
  void getStateFromMockThread() {

    doReturn(Thread.State.RUNNABLE).when(this.mockThread).getState();

    assertThat(ThreadUtils.getState(this.mockThread)).isEqualTo(Thread.State.RUNNABLE);

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void getStateFromNonNullThread() {
    assertThat(ThreadUtils.getState(new Thread())).isEqualTo(Thread.State.NEW);
  }

  @Test
  void getStateFromNullThread() {
    assertThat(ThreadUtils.getState(null)).isNull();
  }

  @Test
  void getThreadGroupFromNonNullThread() {

    ThreadGroup expectedThreadGroup = new ThreadGroup("TestGroup");

    Thread thread = new Thread(expectedThreadGroup, "TestThread");

    assertThat(ThreadUtils.getThreadGroup(thread)).isEqualTo(expectedThreadGroup);
  }

  @Test
  void getThreadGroupFromNullThread() {
    assertThat(ThreadUtils.getThreadGroup(null)).isNull();
  }

  @Test
  void dumpStackWasCalled() {

    PrintStream systemErrorStream = System.err;

    try {

      ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

      PrintStream errorStream = new PrintStream(outputStream);

      Thread currentThread = Thread.currentThread();

      System.setErr(errorStream);
      currentThread.setName("DumpStackTestThread");

      // Dump the current call stack to the errorStream
      ThreadUtils.dumpStack("TestTag");

      errorStream.flush();

      byte[] errorStreamBytes = outputStream.toByteArray();

      String stackTrace = new String(errorStreamBytes);

      assertThat(stackTrace).contains("[TESTTAG] DumpStackTestThread Thread".formatted(currentThread.getId()));

      assertThat(stackTrace).contains("java.lang.Exception: Stack trace");
      assertThat(stackTrace).containsPattern("at .*java.lang.Thread.dumpStack(.*)");
      assertThat(stackTrace).contains("at " + ThreadUtils.class.getName() + ".dumpStack");
      assertThat(stackTrace).contains("at " + ThreadUtilsUnitTests.class.getName() + ".dumpStackWasCalled");
    }
    finally {
      System.setErr(systemErrorStream);
    }
  }

  @Test
  void interruptInterruptsThread() {

    ThreadUtils.interrupt(this.mockThread);

    verify(this.mockThread, times(1)).interrupt();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  void interruptNullThread() {
    ThreadUtils.interrupt(null);
  }

  @Test
  void join() {

    final long expectedWait = 500L;

    AtomicBoolean condition = new AtomicBoolean(false);

    Runnable testRunnable = () -> {
      condition.set(true);
      sleep(expectedWait);
    };

    Thread testThread = new Thread(testRunnable, "Test Thread");

    testThread.setDaemon(false);
    testThread.setPriority(Thread.MAX_PRIORITY);
    testThread.start();

    assertThat(condition.get()).isFalse();

    long t0 = System.currentTimeMillis();

    assertThat(ThreadUtils.join(testThread, expectedWait, 0)).isTrue();

    long t1 = System.currentTimeMillis();

    assertThat(Thread.interrupted()).isFalse();
    assertThat(condition.get()).isTrue();
    assertThat((t1 - t0)).isGreaterThanOrEqualTo(expectedWait);
  }

  @Test
  void joinInterrupted() throws Throwable {
    TestFramework.runOnce(new JoinInterruptedMultithreadedTestCase());
  }

  @Test
  void joinMainThreadInterruptingTestThread() {

    AtomicBoolean joinSuccess = new AtomicBoolean(true);
    AtomicBoolean ran = new AtomicBoolean(false);

    Thread mainThread = Thread.currentThread();

    Runnable testRunnable = () -> {
      ran.set(true);
      joinSuccess.set(ThreadUtils.join(mainThread, TimeUnit.SECONDS.toMillis(10), 0));
    };

    Thread testThread = new Thread(testRunnable, "Test Thread");

    testThread.setDaemon(false);
    testThread.start();
    testThread.interrupt();

    assertThat(testThread.isInterrupted()).isTrue();
    assertThat(ThreadUtils.join(testThread, 500L, 0)).isTrue();

    assertThat(ran.get()).isTrue();
    assertThat(joinSuccess.get()).isFalse();
    //assertThat(testThread.isInterrupted()).isTrue(); // TODO: Why false?
    assertThat(testThread.isAlive()).isFalse();
  }

  @Test
  void joinNullThread() {

    long t0 = System.currentTimeMillis();
    boolean success = ThreadUtils.join(null, 1000, 1000);
    long t1 = System.currentTimeMillis();

    assertThat(success).isFalse();
    assertThat(t1 - t0).isLessThan(1000L);
  }

  @Test
  void runAtomicallyUsingRunnableIsCorrect() {

    AtomicBoolean run = new AtomicBoolean(false);

    Object lock = new Object();

    Runnable operation = () -> {
      assertThat(Thread.currentThread().holdsLock(lock));
      run.set(true);
    };

    ThreadUtils.runAtomically(lock, operation);

    assertThat(run.get()).isTrue();
  }

  @Test
  void runAtomicallyUsingRunnableWithoutLockIsCorrect() {

    AtomicBoolean run = new AtomicBoolean(false);

    Runnable operation = () -> run.set(true);

    ThreadUtils.runAtomically(null, operation);

    assertThat(run.get()).isTrue();
  }

  @Test
  void runAtomicallyUsingRunnableWithNullOperation() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ThreadUtils.runAtomically(null, (Runnable) null))
      .withMessage("Operation to run is required")
      .withNoCause();
  }

  @Test
  void runAtomicallyUsingSupplierIsCorrect() {

    Object lock = new Object();

    Supplier<String> operation = () -> {
      assertThat(Thread.currentThread().holdsLock(lock));
      return "test";
    };

    assertThat(ThreadUtils.runAtomically(lock, operation)).isEqualTo("test");
  }

  @Test
  void runAtomicallyUsingSupplierWithoutLockIsCorrect() {

    Supplier<String> operation = () -> "mock";

    assertThat(ThreadUtils.runAtomically(null, operation)).isEqualTo("mock");
  }

  @Test
  void runAtomicallyUsingSupplierWithNullOperation() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ThreadUtils.runAtomically(null, (Supplier<?>) null))
      .withMessage("Operation to run is required")
      .withNoCause();
  }

  @Test
  void sleep() {

    long expectedWait = 500L;
    long t0 = System.currentTimeMillis();

    boolean success = ThreadUtils.sleep(expectedWait, 0);

    long t1 = System.currentTimeMillis();

    assertThat(success).isTrue();
    assertThat(t1 - t0).isGreaterThanOrEqualTo(expectedWait);
  }

  @Test
  void sleepInterrupted() throws Throwable {
    TestFramework.runOnce(new SleepInterruptedMultithreadedTestCase());
  }

  @Test
  void waitForDuration() {

    long timeout = System.currentTimeMillis() + 500L;

    assertThat(ThreadUtils.waitFor(500L).checkEvery(100L).run()).isTrue();
    assertThat(System.currentTimeMillis()).isGreaterThanOrEqualTo(timeout);
  }

  @Test
  void waitForPassesConditionImmediately() {

    AtomicInteger count = new AtomicInteger(0);

    Condition countCondition = () ->  count.incrementAndGet() > 0;

    assertThat(count.get()).isEqualTo(0);

    boolean success = ThreadUtils.waitFor(5L, TimeUnit.SECONDS)
      .checkEvery(1L, TimeUnit.SECONDS)
      .on(countCondition);

    assertThat(success).isTrue();
    assertThat(count.get()).isEqualTo(2);
  }

  @Test
  void waitForInterrupted() throws Throwable {
    TestFramework.runOnce(new WaitForInterruptedMultithreadedTestCase());
  }

  @Test
  void waitForWithInvalidDuration() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ThreadUtils.waitFor(-500L))
      .withMessage("Duration [-500] must be greater than 0")
      .withNoCause();
  }

  @Test
  void waitForWithIntervalGreaterThanDuration() {

    assertThatIllegalArgumentException()
      .isThrownBy(() ->ThreadUtils.waitFor(500L).checkEvery(2L, TimeUnit.SECONDS))
      .withMessage("Interval [2 SECONDS] must be greater than 0 and less than equal to duration [500 MILLISECONDS]")
      .withNoCause();
  }

  @Test
  void waitForWithNegativeInterval() {

    assertThatIllegalArgumentException()
      .isThrownBy(() ->ThreadUtils.waitFor(500L).checkEvery(-1L, TimeUnit.SECONDS))
      .withMessage("Interval [-1 SECONDS] must be greater than 0 and less than equal to duration [500 MILLISECONDS]")
      .withNoCause();
  }

  @SuppressWarnings("unused")
  protected static final class JoinInterruptedMultithreadedTestCase extends MultithreadedTestCase {

    private volatile Thread interruptingThread;
    private volatile Thread joiningThread;

    void thread1() throws InterruptedException {

      assertTick(0);

      this.interruptingThread = Thread.currentThread();
      this.interruptingThread.setName("Interrupting Thread");

      waitForTick(2);
      assertTick(2);

      assertThat(this.joiningThread).isNotNull();

      this.joiningThread.interrupt();
      this.joiningThread.join();

      assertThat(this.interruptingThread.isInterrupted()).isFalse();
    }

    public void thread2() {

      assertTick(0);

      this.joiningThread = Thread.currentThread();
      this.joiningThread.setName("Joining Thread");

      waitForTick(1);
      assertTick(1);

      assertThat(this.interruptingThread).isNotNull();
      assertThat(ThreadUtils.join(this.interruptingThread, TimeUnit.SECONDS.toMillis(5), 0)).isFalse();
      assertThat(this.joiningThread.isInterrupted()).isTrue();
    }

    @Override
    public void finish() {

      assertThat(this.interruptingThread.isAlive()).isFalse();
      assertThat(this.joiningThread.isAlive()).isFalse();
    }
  }

  @SuppressWarnings("unused")
  protected static final class SleepInterruptedMultithreadedTestCase extends MultithreadedTestCase {

    private Thread interruptingThread;
    private Thread sleepingThread;

    public void thread1() {

      assertTick(0);

      interruptingThread = Thread.currentThread();
      interruptingThread.setName("Interrupting Thread");

      waitForTick(2);

      assertThat(sleepingThread).isNotNull();

      sleepingThread.interrupt();
    }

    public void thread2() {

      waitForTick(1);

      sleepingThread = Thread.currentThread();
      sleepingThread.setName("Sleeping Thread");

      assertThat(interruptingThread).isNotNull();
      assertThat(ThreadUtils.sleep(TimeUnit.SECONDS.toMillis(5), 0)).isFalse();
      assertThat(sleepingThread.isInterrupted()).isTrue();
    }

    @Override
    public void finish() {

      assertThat(interruptingThread.isAlive()).isFalse();
      assertThat(sleepingThread.isAlive()).isFalse();
    }
  }

  @SuppressWarnings("unused")
  protected static final class WaitForInterruptedMultithreadedTestCase extends MultithreadedTestCase {

    private Thread interruptingThread;
    private Thread waitingThread;

    public void thread1() {

      assertTick(0);

      interruptingThread = Thread.currentThread();
      interruptingThread.setName("Interrupting Thread");

      waitForTick(2);

      assertThat(waitingThread).isNotNull();

      waitingThread.interrupt();
    }

    public void thread2() {

      waitForTick(1);

      waitingThread = Thread.currentThread();
      waitingThread.setName("Waiting Thread");

      AtomicInteger counter = new AtomicInteger(0);

      Condition countCondition = () -> counter.incrementAndGet() > 2;

      assertThat(interruptingThread).isNotNull();

      boolean success = ThreadUtils.waitFor(500, TimeUnit.MILLISECONDS)
        .checkEvery(100, TimeUnit.MILLISECONDS)
        .on(countCondition);

      assertThat(success).isFalse();
      assertThat(waitingThread.isInterrupted()).isTrue();
      assertThat(counter.get()).isEqualTo(2);
    }

    @Override
    public void finish() {

      assertThat(interruptingThread.isAlive()).isFalse();
      assertThat(waitingThread.isAlive()).isFalse();
    }
  }
}
