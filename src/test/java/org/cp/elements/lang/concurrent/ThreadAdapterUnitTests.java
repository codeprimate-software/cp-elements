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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Function;

import org.cp.elements.test.TestUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit Tests for {@link ThreadAdapter}.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.Timeout
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ThreadAdapterUnitTests {

  @Rule
  public Timeout globalTimeout = Timeout.seconds(5);

  @Mock
  private Runnable mockRunnable;

  @Mock
  private Thread mockThread;

  private Thread newThread(String name) {
    return newThread(name, Function.identity());
  }

  private Thread newThread(String name, Function<Thread, Thread> function) {

    Thread thread = new Thread(this.mockRunnable, name);

    thread = function.apply(thread);

    return spy(thread);
  }

  private <T> Function<T, T> toFunction(Consumer<T> consumer) {

    return target -> {
      consumer.accept(target);
      return target;
    };
  }

  @Test
  public void constructThreadAdapterWithCurrentThread() {

    ThreadAdapter threadAdapter = new ThreadAdapter();

    assertThat(threadAdapter).isNotNull();
    assertThat(threadAdapter.getDelegate()).isEqualTo(Thread.currentThread());
  }

  @Test
  @SuppressWarnings("all")
  public void constructThreadAdapterWithRunnable() {

    ThreadAdapter threadAdapter = new ThreadAdapter(this.mockRunnable);

    assertThat(threadAdapter).isNotNull();
    assertThat(threadAdapter.getDelegate()).isNotNull();
    assertThat(threadAdapter.getDelegate()).isNotEqualTo(Thread.currentThread());

    threadAdapter.getDelegate().run();

    verify(this.mockRunnable, times(1)).run();
    verifyNoMoreInteractions(this.mockRunnable);
  }

  @Test
  public void constructThreadAdapterWithThread() {

    ThreadAdapter threadAdapter = new ThreadAdapter(this.mockThread);

    assertThat(threadAdapter).isNotNull();
    assertThat(threadAdapter.getDelegate()).isEqualTo(this.mockThread);

    verifyNoInteractions(this.mockThread);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructThreadAdapterWithNull() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> new ThreadAdapter(null),
      () -> "Delegate Thread is required");
  }

  @Test
  public void fromCurrentThread() {

    ThreadAdapter currentThreadAdapter = ThreadAdapter.currentThread();

    assertThat(currentThreadAdapter).isNotNull();
    assertThat(currentThreadAdapter.getDelegate()).isEqualTo(Thread.currentThread());
  }

  @Test
  public void fromMockThread() {

    ThreadAdapter mockThreadAdapter = ThreadAdapter.from(this.mockThread);

    assertThat(mockThreadAdapter).isNotNull();
    assertThat(mockThreadAdapter.getDelegate()).isEqualTo(this.mockThread);

    verifyNoInteractions(this.mockThread);
  }

  @Test(expected = IllegalArgumentException.class)
  public void fromNullThread() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> ThreadAdapter.from(null),
      () -> "Delegate Thread is required");
  }

  @Test
  public void isAliveReturnsFalseForNonStartedThread() {
    assertThat(new ThreadAdapter(this.mockThread).isAlive()).isFalse();
  }

  @Test
  public void isBlockedWithBlockedThread() {

    doReturn(Thread.State.BLOCKED).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).isBlocked()).isTrue();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void isBlockedWithNonBlockedThread() {

    doReturn(Thread.State.RUNNABLE).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).isBlocked()).isFalse();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  @SuppressWarnings("all")
  public void isDaemonWithDaemonThread() {

    Thread thread = newThread("isDaemonWithDaemonThreadTest", toFunction(it -> it.setDaemon(true)));

    assertThat(new ThreadAdapter(thread).isDaemon()).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void isDaemonWithNonDaemonThread() {

    Thread thread = newThread("isDaemonWithNonDaemonThreadTest", toFunction(it -> it.setDaemon(false)));

    assertThat(new ThreadAdapter(thread).isDaemon()).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isNonDaemonWithDaemonThread() {

    Thread thread = newThread("isNonDaemonWithDaemonThreadTest", toFunction(it -> it.setDaemon(true)));

    assertThat(new ThreadAdapter(thread).isNonDaemon()).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isNonDaemonWithNonDaemonThread() {

    Thread thread = newThread("isNonDaemonWithNonDaemonThreadTest", toFunction(it -> it.setDaemon(false)));

    assertThat(new ThreadAdapter(thread).isNonDaemon()).isTrue();
  }

  @Test
  public void isUserThreadWithDaemonThread() {

    Thread thread = newThread("isUserThreadWithDaemonThreadTest", toFunction(it -> it.setDaemon(true)));

    assertThat(new ThreadAdapter(thread).isUser()).isFalse();
  }

  @Test
  public void isUserThreadWithNonDaemonThread() {

    Thread thread = newThread("isUserThreadWithDaemonThreadTest", toFunction(it -> it.setDaemon(false)));

    assertThat(new ThreadAdapter(thread).isUser()).isTrue();
  }

  @Test
  public void isInterruptedWithInterruptedThread() {

    doReturn(true).when(this.mockThread).isInterrupted();

    assertThat(new ThreadAdapter(this.mockThread).isInterrupted()).isTrue();

    verify(this.mockThread, times(1)).isInterrupted();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void isInterruptedWithUninterruptedThread() {

    doReturn(false).when(this.mockThread).isInterrupted();

    assertThat(new ThreadAdapter(this.mockThread).isInterrupted()).isFalse();

    verify(this.mockThread, times(1)).isInterrupted();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void isNewWithNewThread() {

    doReturn(Thread.State.NEW).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).isNew()).isTrue();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void isNewWithOldThread() {

    doReturn(Thread.State.TERMINATED).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).isNew()).isFalse();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void isRunnableWithRunnableThread() {

    doReturn(Thread.State.RUNNABLE).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).isRunnable()).isTrue();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void isRunnableWithNonRunnableThread() {

    doReturn(Thread.State.WAITING).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).isRunnable()).isFalse();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void isTerminatedWithTerminatedThread() {

    doReturn(Thread.State.TERMINATED).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).isTerminated()).isTrue();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void isTerminatedWithNonTerminatedThread() {

    doReturn(Thread.State.NEW).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).isTerminated()).isFalse();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void isTimedWaitingWithTimedWaitingThread() {

    doReturn(Thread.State.TIMED_WAITING).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).isTimedWaiting()).isTrue();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void isTimedWaitingWithNonTimedWaitingThread() {

    doReturn(Thread.State.RUNNABLE).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).isTimedWaiting()).isFalse();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void isWaitingWithWaitingThread() {

    doReturn(Thread.State.WAITING).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).isWaiting()).isTrue();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void isWaitingWithNonWaitingThread() {

    doReturn(Thread.State.RUNNABLE).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).isWaiting()).isFalse();

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void setContextClassLoader() {

    ClassLoader mockClassLoader = mock(ClassLoader.class);

    ThreadAdapter expectedThreadAdapter = new ThreadAdapter(this.mockThread);

    assertThat(expectedThreadAdapter.setContextClassLoader(mockClassLoader)).isSameAs(expectedThreadAdapter);

    verify(this.mockThread, times(1)).setContextClassLoader(same(mockClassLoader));
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void setContextClassLoaderToNullIsNullSafe() {

    ClassLoader mockClassLoader = mock(ClassLoader.class);

    AtomicReference<ClassLoader> classLoaderRef = new AtomicReference<>(mockClassLoader);

    doAnswer(invocation -> classLoaderRef.get()).when(this.mockThread).getContextClassLoader();
    doAnswer(invocation -> { classLoaderRef.set(invocation.getArgument(0, ClassLoader.class)); return null; })
      .when(this.mockThread).setContextClassLoader(any());

    assertThat(this.mockThread.getContextClassLoader()).isEqualTo(mockClassLoader);

    ThreadAdapter expectedThreadAdapter = new ThreadAdapter(this.mockThread);

    assertThat(expectedThreadAdapter.setContextClassLoader(null)).isSameAs(expectedThreadAdapter);
    assertThat(this.mockThread.getContextClassLoader()).isNull();

    verify(this.mockThread, times(1)).setContextClassLoader(isNull());
    verify(this.mockThread, times(2)).getContextClassLoader();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void getContextClassLoader() {

    ClassLoader mockClassLoader = mock(ClassLoader.class);

    doReturn(mockClassLoader).when(this.mockThread).getContextClassLoader();

    assertThat(new ThreadAdapter(this.mockThread).getContextClassLoader()).isEqualTo(mockClassLoader);

    verify(this.mockThread, times(1)).getContextClassLoader();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void getContextClassLoaderIsNull() {

    assertThat(new ThreadAdapter(this.mockThread).getContextClassLoader()).isNull();

    verify(this.mockThread, times(1)).getContextClassLoader();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void setDaemon() {

    ThreadAdapter expectedThreadAdapter = new ThreadAdapter(this.mockThread);

    assertThat(expectedThreadAdapter.setDaemon(true)).isSameAs(expectedThreadAdapter);
    assertThat(this.mockThread.isDaemon()).isTrue();
  }

  @Test
  public void getId() {

    doReturn(1L).when(this.mockThread).getId();

    assertThat(new ThreadAdapter(this.mockThread).getId()).isEqualTo(1L);

    verify(this.mockThread, times(1)).getId();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void setName() {

    Thread thread = newThread("test");

    ThreadAdapter expectedThreadAdapter = new ThreadAdapter(thread);

    assertThat(expectedThreadAdapter.setName("setNameTest")).isSameAs(expectedThreadAdapter);
    assertThat(thread.getName()).isEqualTo("setNameTest");
  }

  @SuppressWarnings("all")
  private void testSetNameToIllegalValue(String name) {

    try {
      new ThreadAdapter(this.mockThread).setName(name);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Name [%s] is required", name);
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      // Cannot make this verification since Thread.setName(:String) is final!
      //verify(this.mockThread, never()).setName(any());
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void setNameToBlank() {
    testSetNameToIllegalValue("  ");
  }

  @Test(expected = IllegalArgumentException.class)
  public void setNameToEmpty() {
    testSetNameToIllegalValue("");
  }

  @Test(expected = IllegalArgumentException.class)
  public void setNameToNull() {
    testSetNameToIllegalValue(null);
  }

  @Test
  public void getName() {

    Thread thread = newThread("getNameTest");

    assertThat(new ThreadAdapter(thread).getName()).isEqualTo("getNameTest");
  }

  @Test
  public void setPriority() {

    Thread thread = newThread("test");

    ThreadAdapter expectedThreadAdapter = new ThreadAdapter(thread);

    assertThat(expectedThreadAdapter.setPriority(2)).isSameAs(expectedThreadAdapter);
    assertThat(thread.getPriority()).isEqualTo(2);
  }

  @Test
  public void getPriority() {

    Thread thread = newThread("test", toFunction(it -> it.setPriority(8)));

    assertThat(new ThreadAdapter(thread).getPriority()).isEqualTo(8);
  }

  @Test
  public void getStackTrace() {

    StackTraceElement[] expectedStackTrace = Thread.currentThread().getStackTrace();

    doReturn(expectedStackTrace).when(this.mockThread).getStackTrace();

    assertThat(new ThreadAdapter(this.mockThread).getStackTrace()).isEqualTo(expectedStackTrace);

    verify(this.mockThread, times(1)).getStackTrace();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void getState() {

    doReturn(Thread.State.RUNNABLE).when(this.mockThread).getState();

    assertThat(new ThreadAdapter(this.mockThread).getState()).isEqualTo(Thread.State.RUNNABLE);

    verify(this.mockThread, times(1)).getState();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void getThreadGroup() {

    ThreadGroup testThreadGroup = new ThreadGroup("test");

    Thread testThread = new Thread(testThreadGroup, this.mockRunnable, "mock");

    assertThat(new ThreadAdapter(testThread).getThreadGroup()).isEqualTo(testThreadGroup);
  }

  @Test
  public void getThreadGroupIsEqualToParentThreadGroup() {

    Thread testThread = new Thread(null, this.mockRunnable, "test");

    assertThat(new ThreadAdapter(testThread).getThreadGroup()).isEqualTo(Thread.currentThread().getThreadGroup());
  }

  @Test
  public void setUncaughtExceptionHandler() {

    Thread.UncaughtExceptionHandler mockUncaughtExceptionHandler = mock(Thread.UncaughtExceptionHandler.class);

    ThreadAdapter expectedThreadAdapter = new ThreadAdapter(this.mockThread);

    assertThat(expectedThreadAdapter.setUncaughtExceptionHandler(mockUncaughtExceptionHandler))
      .isSameAs(expectedThreadAdapter);

    verify(this.mockThread, times(1))
      .setUncaughtExceptionHandler(eq(mockUncaughtExceptionHandler));
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void setUncaughtExceptionHandlerToNullIsNullSafe() {

    Thread.UncaughtExceptionHandler mockUncaughtExceptionHandler = mock(Thread.UncaughtExceptionHandler.class);

    Thread testThread = newThread("setUncaughtExceptionHandlerToNullIsNullSafeTest",
      toFunction(it -> it.setUncaughtExceptionHandler(mockUncaughtExceptionHandler)));

    assertThat(testThread).isNotNull();
    assertThat(testThread.getName()).isEqualTo("setUncaughtExceptionHandlerToNullIsNullSafeTest");
    assertThat(testThread.getUncaughtExceptionHandler()).isEqualTo(mockUncaughtExceptionHandler);

    ThreadAdapter expectedThreadAdapter = new ThreadAdapter(testThread);

    assertThat(expectedThreadAdapter).isNotNull();
    assertThat(expectedThreadAdapter.getDelegate()).isEqualTo(testThread);
    assertThat(expectedThreadAdapter.setUncaughtExceptionHandler(null)).isEqualTo(expectedThreadAdapter);
    assertThat(testThread.getUncaughtExceptionHandler()).isNotEqualTo(mockUncaughtExceptionHandler);

    verify(testThread, times(1)).setUncaughtExceptionHandler(isNull());
    verify(testThread, times(2)).getUncaughtExceptionHandler();
    verifyNoMoreInteractions(testThread);
  }

  @Test
  public void getUncaughtExceptionHandler() {

    Thread.UncaughtExceptionHandler mockUncaughtExceptionHandler = mock(Thread.UncaughtExceptionHandler.class);

    doReturn(mockUncaughtExceptionHandler).when(this.mockThread).getUncaughtExceptionHandler();

    assertThat(new ThreadAdapter(this.mockThread).getUncaughtExceptionHandler()).isEqualTo(mockUncaughtExceptionHandler);

    verify(this.mockThread, times(1)).getUncaughtExceptionHandler();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void checkAccessSucceeds() {
    new ThreadAdapter(this.mockThread).checkAccess();
  }

  @Test
  public void dumpStackWasCalled() {

    PrintStream systemErr = System.err;

    try {
      ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
      PrintStream errorStream = new PrintStream(outputStream);

      System.setErr(errorStream);

      new ThreadAdapter(this.mockThread).dumpStack();

      errorStream.flush();

      String stackTrace = outputStream.toString();

      //System.out.println(stackTrace);

      assertThat(stackTrace).contains("java.lang.Exception: Stack trace");
      assertThat(stackTrace).containsPattern("at .*java.lang.Thread.dumpStack(.*)");
    }
    finally {
      System.setErr(systemErr);
    }
  }

  @Test
  public void interrupt() {

    new ThreadAdapter(this.mockThread).interrupt();

    verify(this.mockThread, times(1)).interrupt();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void join() throws Exception {

    AtomicBoolean runCalled = new AtomicBoolean(false);

    CountDownLatch latch = new CountDownLatch(1);

    Runnable task = () -> {
      try {
        latch.await();
      }
      catch (InterruptedException ignore) {
      }
      finally {
        runCalled.compareAndSet(false, true);
      }
    };

    ThreadAdapter joinedThread = new ThreadAdapter(task);

    joinedThread.setDaemon(true);
    joinedThread.setName("Joined Thread");
    joinedThread.setPriority(Thread.NORM_PRIORITY);
    joinedThread.start();
    latch.countDown();
    joinedThread.join();

    assertThat(joinedThread.isTerminated()).isTrue();
    assertThat(runCalled.get()).isTrue();
  }

  @Test
  public void joinRespondsToInterruption() throws Throwable {
    TestFramework.runOnce(new JoinInterruptionMultithreadedTestCase());
  }

  @Test
  public void joinWithTimeout() throws Exception {

    AtomicBoolean runCalled = new AtomicBoolean(false);
    AtomicBoolean runFinished = new AtomicBoolean(false);

    CountDownLatch latch = new CountDownLatch(1);

    Runnable task = () -> {
      try {
        runCalled.compareAndSet(false, true);
        latch.await();
      }
      catch (InterruptedException ignore) { }
      finally {
        runFinished.compareAndSet(false, true);
      }
    };

    ThreadAdapter joinedThread = new ThreadAdapter(task);

    try {

      joinedThread.setDaemon(true);
      joinedThread.setName("Joined Thread");
      joinedThread.setPriority(Thread.NORM_PRIORITY);
      joinedThread.start();
      joinedThread.join(200L);

      assertThat(joinedThread.isTerminated()).isFalse();
      assertThat(runCalled.get()).isTrue();
      assertThat(runFinished.get()).isFalse();
    }
    finally {

      latch.countDown();
      joinedThread.join();

      assertThat(joinedThread.isTerminated()).isTrue();
      assertThat(runFinished.get()).isTrue();
    }
  }

  @Test
  public void joinWithTimeoutRespondsToInterruption() throws Throwable {
    TestFramework.runOnce(new JoinInterruptionMultithreadedTestCase(TimeUnit.SECONDS.toMillis(5)));
  }

  @Test
  public void joinWithTimeoutUsingMillisecondsAndNanoseconds() throws Exception {

    AtomicBoolean runCalled = new AtomicBoolean(false);
    AtomicBoolean runFinished = new AtomicBoolean(false);

    CountDownLatch latch = new CountDownLatch(1);

    Runnable task = () -> {
      try {
        runCalled.compareAndSet(false, true);
        latch.await();
      }
      catch (InterruptedException ignore) { }
      finally {
        runFinished.compareAndSet(false, true);
      }
    };

    ThreadAdapter joinedThread = new ThreadAdapter(task);

    try {

      joinedThread.setDaemon(true);
      joinedThread.setName("Joined Thread");
      joinedThread.setPriority(Thread.NORM_PRIORITY);
      joinedThread.start();
      joinedThread.join(200L, 500);

      assertThat(joinedThread.isTerminated()).isFalse();
      assertThat(runCalled.get()).isTrue();
      assertThat(runFinished.get()).isFalse();
    }
    finally {

      latch.countDown();
      joinedThread.join();

      assertThat(joinedThread.isTerminated()).isTrue();
      assertThat(runFinished.get()).isTrue();
    }
  }

  @Test
  public void joinWithTimeoutUsingMillisecondsAndNanosecondsRespondsToInterruption() throws Throwable {
    TestFramework.runOnce(new JoinInterruptionMultithreadedTestCase(TimeUnit.SECONDS.toMillis(5), 500));
  }

  @Test
  @SuppressWarnings("all")
  public void run() {

    ThreadAdapter.from(this.mockThread).run();

    verify(this.mockThread, times(1)).run();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void start() {

    ThreadAdapter.from(this.mockThread).start();

    verify(this.mockThread, times(1)).start();
    verifyNoMoreInteractions(this.mockThread);
  }

  @Test
  public void toStringIsCorrect() {

    ThreadGroup testThreadGroup = new ThreadGroup("ToStringTestThreadGroup");

    Thread testThread = spy(new Thread(testThreadGroup, this.mockRunnable, "ToStringTest"));

    testThread.setDaemon(true);
    testThread.setPriority(9);

    doReturn(123L).when(testThread).getId();
    doReturn(Thread.State.RUNNABLE).when(testThread).getState();

    ThreadAdapter testThreadAdapter = new ThreadAdapter(testThread);

    assertThat(testThreadAdapter).isNotNull();
    assertThat(testThreadAdapter.getDelegate()).isEqualTo(testThread);

    assertThat(testThreadAdapter.toString()).isEqualTo(String.format(
      "{ @type = %s, id = 123, name = ToStringTest, daemon = true, group = %s, priority = 9, state = RUNNABLE }",
      testThreadAdapter.getClass().getName(), testThreadGroup));
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
      return milliseconds != null ? Thread.State.TIMED_WAITING : Thread.State.WAITING;
    }

    public void thread1() {

      assertTick(0);

      this.interruptingThread = new ThreadAdapter(Thread.currentThread()).setName("Interrupting Thread");

      waitForTick(2);

      assertThat(this.joiningThread.getState()).isEqualTo(getExpectedJoiningThreadState());
      assertThat(this.interrupted.get()).isFalse();

      this.joiningThread.interrupt();

      waitForTick(3);

      assertThat(this.interrupted.get()).isTrue();
      assertThat(this.joiningThread.isTerminated()).isTrue();
    }

    public void thread2() {

      waitForTick(1);

      this.joiningThread = new ThreadAdapter(Thread.currentThread()).setName("Joining Thread");

      assertThat(this.interruptingThread).isNotNull();

      try {
        if (this.milliseconds != null && this.nanoseconds != null) {
          this.interruptingThread.join(this.milliseconds, this.nanoseconds);
        }
        else if (this.milliseconds != null) {
          this.interruptingThread.join(this.milliseconds);
        }
        else {
          this.interruptingThread.join();
        }
      }
      catch (InterruptedException ignore) {
        this.interrupted.compareAndSet(false, true);
      }
    }
  }
}
