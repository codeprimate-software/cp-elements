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
import java.lang.Thread.UncaughtExceptionHandler;
import java.util.Arrays;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Function;

import org.junit.Before;
import org.junit.Rule;
import org.junit.jupiter.api.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;

import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit Tests for {@link ThreadAdapter}.
 *
 * @author John J. Blum
 * @see java.lang.Thread
 * @see org.junit.Rule
 * @see org.junit.jupiter.api.Test
 * @see org.junit.rules.Timeout
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @see org.cp.elements.lang.concurrent.ThreadAdapter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("unused")
public class ThreadAdapterUnitTests {

  @Rule
  public Timeout globalTimeout = Timeout.seconds(5);

  @Mock
  private Runnable mockRunnable;

  @Mock
  private Thread mockThread;

  @Before
  public void resetMocks() {
    Mockito.reset(this.mockThread, this.mockRunnable);
  }

  private @NotNull Runnable mockRunnable() {
    return this.mockRunnable;
  }

  private @NotNull Thread mockThread() {
    return this.mockThread;
  }

  private @NotNull Thread newThread(@NotNull String name) {
    return newThread(name, Function.identity());
  }

  private @NotNull Thread newThread(@NotNull String name, @NotNull Function<Thread, Thread> function) {

    Thread thread = new Thread(mockRunnable(), name);

    thread = function.apply(thread);

    return thread;
  }

  private @NotNull Thread newThreadSpy(@NotNull String name) {
    return newThread(name, Mockito::spy);
  }

  private @NotNull Thread newThreadSpy(@NotNull String name, @NotNull Function<Thread, Thread> function) {
    return newThread(name, function.andThen(Mockito::spy));
  }

  @Test
  public void newJavaThread() {

    ClassLoader mockClassLoader = mock(ClassLoader.class);

    Runnable mockRunnable = mock(Runnable.class);

    UncaughtExceptionHandler mockUncaughtExceptionHandler = mock(UncaughtExceptionHandler.class);

    Thread thread = new Thread(mockRunnable, "TestThread");

    thread.setDaemon(true);
    thread.setPriority(Thread.MAX_PRIORITY);
    thread.setUncaughtExceptionHandler(mockUncaughtExceptionHandler);
    thread.setContextClassLoader(mockClassLoader);

    assertThat(thread).isNotNull();
    assertThat(thread.isDaemon()).isTrue();
    assertThat(thread.getContextClassLoader()).isEqualTo(mockClassLoader);
    assertThat(thread.getName()).isEqualTo("TestThread");
    assertThat(thread.getPriority()).isEqualTo(Thread.MAX_PRIORITY);
    assertThat(thread.getUncaughtExceptionHandler()).isEqualTo(mockUncaughtExceptionHandler);

    verifyNoInteractions(mockClassLoader, mockRunnable, mockUncaughtExceptionHandler);
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

    Runnable mockRunnable = mockRunnable();
    ThreadAdapter threadAdapter = new ThreadAdapter(mockRunnable);

    assertThat(threadAdapter).isNotNull();
    assertThat(threadAdapter.getDelegate()).isNotNull();
    assertThat(threadAdapter.getDelegate()).isNotEqualTo(Thread.currentThread());

    threadAdapter.getDelegate().run();

    verify(mockRunnable, times(1)).run();
    verifyNoMoreInteractions(mockRunnable);
  }

  @Test
  public void constructThreadAdapterWithThread() {

    Thread mockThread = mockThread();
    ThreadAdapter threadAdapter = new ThreadAdapter(mockThread);

    assertThat(threadAdapter).isNotNull();
    assertThat(threadAdapter.getDelegate()).isEqualTo(mockThread);

    verifyNoInteractions(mockThread);
  }

  @Test
  public void constructThreadAdapterWithNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ThreadAdapter(null))
      .withMessage("Delegate Thread is required")
      .withNoCause();
  }

  @Test
  public void fromCurrentThread() {

    ThreadAdapter currentThreadAdapter = ThreadAdapter.currentThread();

    assertThat(currentThreadAdapter).isNotNull();
    assertThat(currentThreadAdapter.getDelegate()).isEqualTo(Thread.currentThread());
  }

  @Test
  public void fromMockThread() {

    Thread mockThread = mockThread();
    ThreadAdapter mockThreadAdapter = ThreadAdapter.from(mockThread);

    assertThat(mockThreadAdapter).isNotNull();
    assertThat(mockThreadAdapter.getDelegate()).isEqualTo(mockThread);

    verifyNoInteractions(mockThread);
  }

  @Test
  public void fromNullThread() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ThreadAdapter.from(null))
      .withMessage("Delegate Thread is required")
      .withNoCause();
  }

  @Test
  public void isAliveReturnsFalseForNonStartedThread() {
    assertThat(new ThreadAdapter(mockThread()).isAlive()).isFalse();
  }

  @Test
  public void isBlockedWithBlockedThread() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.BLOCKED).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).isBlocked()).isTrue();

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void isBlockedWithNonBlockedThread() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.RUNNABLE).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).isBlocked()).isFalse();

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  @SuppressWarnings("all")
  public void isDaemonWithDaemonThread() {

    Consumer<Thread> consumer = it -> it.setDaemon(true);

    Thread thread = newThreadSpy("isDaemonWithDaemonThreadTest", FunctionUtils.toFunction(consumer));

    assertThat(new ThreadAdapter(thread).isDaemon()).isTrue();

    verify(thread, times(1)).isDaemon();
  }

  @Test
  @SuppressWarnings("all")
  public void isDaemonWithNonDaemonThread() {

    Consumer<Thread> consumer = it -> it.setDaemon(false);

    Thread thread = newThreadSpy("isDaemonWithNonDaemonThreadTest", FunctionUtils.toFunction(consumer));

    assertThat(new ThreadAdapter(thread).isDaemon()).isFalse();

    verify(thread, times(1)).isDaemon();
  }

  @Test
  @SuppressWarnings("all")
  public void isNonDaemonWithDaemonThread() {

    Consumer<Thread> consumer = it -> it.setDaemon(true);

    Thread thread = newThreadSpy("isNonDaemonWithDaemonThreadTest", FunctionUtils.toFunction(consumer));

    assertThat(new ThreadAdapter(thread).isNonDaemon()).isFalse();

    verify(thread, times(1)).isDaemon();
  }

  @Test
  @SuppressWarnings("all")
  public void isNonDaemonWithNonDaemonThread() {

    Consumer<Thread> consumer = it -> it.setDaemon(false);

    Thread thread = newThreadSpy("isNonDaemonWithNonDaemonThreadTest", FunctionUtils.toFunction(consumer));

    assertThat(new ThreadAdapter(thread).isNonDaemon()).isTrue();

    verify(thread, times(1)).isDaemon();
  }

  @Test
  public void isUserThreadWithDaemonThread() {

    Consumer<Thread> consumer = it -> it.setDaemon(true);

    Thread thread = newThreadSpy("isUserThreadWithDaemonThreadTest", FunctionUtils.toFunction(consumer));

    assertThat(new ThreadAdapter(thread).isUser()).isFalse();

    verify(thread, times(1)).isDaemon();
  }

  @Test
  public void isUserThreadWithNonDaemonThread() {

    Consumer<Thread> consumer = it -> it.setDaemon(false);

    Thread thread = newThreadSpy("isUserThreadWithDaemonThreadTest", FunctionUtils.toFunction(consumer));

    assertThat(new ThreadAdapter(thread).isUser()).isTrue();

    verify(thread, times(1)).isDaemon();
  }

  @Test
  public void isInterruptedWithInterruptedThread() {

    Thread mockThread = mockThread();

    doReturn(true).when(mockThread).isInterrupted();

    assertThat(new ThreadAdapter(mockThread).isInterrupted()).isTrue();

    verify(mockThread, times(1)).isInterrupted();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void isInterruptedWithUninterruptedThread() {

    Thread mockThread = mockThread();

    doReturn(false).when(mockThread).isInterrupted();

    assertThat(new ThreadAdapter(mockThread).isInterrupted()).isFalse();

    verify(mockThread, times(1)).isInterrupted();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void isNewWithNewThread() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.NEW).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).isNew()).isTrue();

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void isNewWithOldThread() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.TERMINATED).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).isNew()).isFalse();

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void isRunnableWithRunnableThread() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.RUNNABLE).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).isRunnable()).isTrue();

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void isRunnableWithNonRunnableThread() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.WAITING).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).isRunnable()).isFalse();

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void isTerminatedWithTerminatedThread() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.TERMINATED).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).isTerminated()).isTrue();

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void isTerminatedWithNonTerminatedThread() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.NEW).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).isTerminated()).isFalse();

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void isTimedWaitingWithTimedWaitingThread() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.TIMED_WAITING).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).isTimedWaiting()).isTrue();

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void isTimedWaitingWithNonTimedWaitingThread() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.RUNNABLE).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).isTimedWaiting()).isFalse();

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void isWaitingWithWaitingThread() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.WAITING).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).isWaiting()).isTrue();

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void isWaitingWithNonWaitingThread() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.RUNNABLE).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).isWaiting()).isFalse();

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void setContextClassLoader() {

    ClassLoader mockClassLoader = mock(ClassLoader.class);

    Thread mockThread = mockThread();

    ThreadAdapter threadAdapter = new ThreadAdapter(mockThread);

    assertThat(threadAdapter.setContextClassLoader(mockClassLoader)).isSameAs(threadAdapter);

    verify(mockThread, times(1)).setContextClassLoader(same(mockClassLoader));
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void setContextClassLoaderToNullIsNullSafe() {

    Thread mockThread = mockThread();

    ClassLoader mockClassLoader = mock(ClassLoader.class);

    AtomicReference<ClassLoader> classLoaderRef = new AtomicReference<>(mockClassLoader);

    doAnswer(invocation -> classLoaderRef.get()).when(mockThread).getContextClassLoader();
    doAnswer(invocation -> { classLoaderRef.set(invocation.getArgument(0, ClassLoader.class)); return null; })
      .when(mockThread).setContextClassLoader(any());

    assertThat(mockThread.getContextClassLoader()).isEqualTo(mockClassLoader);

    ThreadAdapter threadAdapter = new ThreadAdapter(mockThread);

    assertThat(threadAdapter.setContextClassLoader(null)).isSameAs(threadAdapter);
    assertThat(mockThread.getContextClassLoader()).isNull();

    verify(mockThread, times(1)).setContextClassLoader(isNull());
    verify(mockThread, times(2)).getContextClassLoader();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void getContextClassLoader() {

    ClassLoader mockClassLoader = mock(ClassLoader.class);

    Thread mockThread = mockThread();

    doReturn(mockClassLoader).when(mockThread).getContextClassLoader();

    assertThat(new ThreadAdapter(mockThread).getContextClassLoader()).isEqualTo(mockClassLoader);

    verify(mockThread, times(1)).getContextClassLoader();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void getContextClassLoaderIsNull() {

    Thread mockThread = mockThread();

    assertThat(new ThreadAdapter(mockThread).getContextClassLoader()).isNull();

    verify(mockThread, times(1)).getContextClassLoader();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void setDaemon() {

    Thread thread = newThreadSpy("setDaemonTest");
    ThreadAdapter threadAdapter = new ThreadAdapter(thread);

    assertThat(threadAdapter.setDaemon(true)).isSameAs(threadAdapter);
    assertThat(threadAdapter.isDaemon()).isTrue();
  }

  @Test
  public void getId() {

    Thread mockThread = mockThread();

    doReturn(1L).when(mockThread).getId();

    assertThat(new ThreadAdapter(mockThread).getId()).isEqualTo(1L);

    verify(mockThread, times(1)).getId();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void setName() {

    Thread thread = newThreadSpy("test");
    ThreadAdapter threadAdapter = new ThreadAdapter(thread);

    assertThat(threadAdapter.setName("setNameTest")).isSameAs(threadAdapter);
    assertThat(thread.getName()).isEqualTo("setNameTest");
  }

  @SuppressWarnings("all")
  private void testSetNameToIllegalValue(@Nullable String name) {

    try {
      new ThreadAdapter(mockThread()).setName(name);
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

  @Test
  public void setNameToBlank() {

    Arrays.asList("", "  ", null).forEach(name ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> new ThreadAdapter(mockThread()).setName(name))
        .withMessage("Name [%s] is required")
        .withNoCause());
  }

  @Test
  public void getName() {
    assertThat(new ThreadAdapter(newThreadSpy("getNameTest")).getName()).isEqualTo("getNameTest");
  }

  @Test
  public void setPriority() {

    Thread thread = newThreadSpy("setPriorityTest");
    ThreadAdapter threadAdapter = new ThreadAdapter(thread);

    assertThat(threadAdapter.setPriority(2)).isSameAs(threadAdapter);
    assertThat(thread.getPriority()).isEqualTo(2);
  }

  @Test
  public void getPriority() {

    Consumer<Thread> consumer = it -> it.setPriority(8);

    Thread thread = newThreadSpy("getPriorityTest", FunctionUtils.toFunction(consumer));

    assertThat(new ThreadAdapter(thread).getPriority()).isEqualTo(8);
  }

  @Test
  public void getStackTrace() {

    StackTraceElement[] expectedStackTrace = Thread.currentThread().getStackTrace();

    Thread mockThread = mockThread();

    doReturn(expectedStackTrace).when(mockThread).getStackTrace();

    assertThat(new ThreadAdapter(mockThread).getStackTrace()).isEqualTo(expectedStackTrace);

    verify(mockThread, times(1)).getStackTrace();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void getState() {

    Thread mockThread = mockThread();

    doReturn(Thread.State.RUNNABLE).when(mockThread).getState();

    assertThat(new ThreadAdapter(mockThread).getState()).isEqualTo(Thread.State.RUNNABLE);

    verify(mockThread, times(1)).getState();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void getThreadGroup() {

    ThreadGroup testThreadGroup = new ThreadGroup("test");

    Thread testThread = new Thread(testThreadGroup, mockRunnable(), "mock");

    assertThat(new ThreadAdapter(testThread).getThreadGroup()).isEqualTo(testThreadGroup);
  }

  @Test
  public void getThreadGroupIsEqualToParentThreadGroup() {

    Thread testThread = new Thread(null, mockRunnable(), "test");

    assertThat(new ThreadAdapter(testThread).getThreadGroup()).isEqualTo(Thread.currentThread().getThreadGroup());
  }

  @Test
  public void setUncaughtExceptionHandler() {

    Thread mockThread = mockThread();

    Thread.UncaughtExceptionHandler mockUncaughtExceptionHandler = mock(Thread.UncaughtExceptionHandler.class);

    ThreadAdapter threadAdapter = new ThreadAdapter(mockThread);

    assertThat(threadAdapter.setUncaughtExceptionHandler(mockUncaughtExceptionHandler)).isSameAs(threadAdapter);

    verify(mockThread, times(1)).setUncaughtExceptionHandler(eq(mockUncaughtExceptionHandler));
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void setUncaughtExceptionHandlerToNullIsNullSafe() {

    Thread.UncaughtExceptionHandler mockUncaughtExceptionHandler = mock(Thread.UncaughtExceptionHandler.class);

    Consumer<Thread> consumer = it -> it.setUncaughtExceptionHandler(mockUncaughtExceptionHandler);

    Thread testThread = newThreadSpy("setUncaughtExceptionHandlerToNullIsNullSafeTest",
      FunctionUtils.toFunction(consumer));

    assertThat(testThread).isNotNull();
    assertThat(testThread.getName()).isEqualTo("setUncaughtExceptionHandlerToNullIsNullSafeTest");
    assertThat(testThread.getUncaughtExceptionHandler()).isEqualTo(mockUncaughtExceptionHandler);

    ThreadAdapter threadAdapter = new ThreadAdapter(testThread);

    assertThat(threadAdapter).isNotNull();
    assertThat(threadAdapter.getDelegate()).isEqualTo(testThread);
    assertThat(threadAdapter.setUncaughtExceptionHandler(null)).isEqualTo(threadAdapter);
    assertThat(testThread.getUncaughtExceptionHandler()).isNotEqualTo(mockUncaughtExceptionHandler);

    verify(testThread, times(1)).setUncaughtExceptionHandler(isNull());
    verify(testThread, times(2)).getUncaughtExceptionHandler();
  }

  @Test
  public void getUncaughtExceptionHandler() {

    Thread mockThread = mockThread();

    Thread.UncaughtExceptionHandler mockUncaughtExceptionHandler = mock(Thread.UncaughtExceptionHandler.class);

    doReturn(mockUncaughtExceptionHandler).when(mockThread).getUncaughtExceptionHandler();

    assertThat(new ThreadAdapter(mockThread).getUncaughtExceptionHandler()).isEqualTo(mockUncaughtExceptionHandler);

    verify(mockThread, times(1)).getUncaughtExceptionHandler();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void checkAccessSucceeds() {
    new ThreadAdapter(mockThread()).checkAccess();
  }

  @Test
  public void dumpStackWasCalled() {

    PrintStream systemErr = System.err;

    try {
      ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
      PrintStream errorStream = new PrintStream(outputStream);

      System.setErr(errorStream);

      new ThreadAdapter(mockThread()).dumpStack();

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

    Thread mockThread = mockThread();

    new ThreadAdapter(mockThread).interrupt();

    verify(mockThread, times(1)).interrupt();
    verifyNoMoreInteractions(mockThread);
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

    Thread mockThread = mockThread();
    ThreadAdapter.from(mockThread).run();

    verify(mockThread, times(1)).run();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void start() {

    Thread mockThread = mockThread();
    ThreadAdapter.from(mockThread).start();

    verify(mockThread, times(1)).start();
    verifyNoMoreInteractions(mockThread);
  }

  @Test
  public void toStringIsCorrect() {

    ThreadGroup testThreadGroup = new ThreadGroup("ToStringTestThreadGroup");

    Thread testThread = spy(new Thread(testThreadGroup, mockRunnable(), "ToStringTest"));

    testThread.setDaemon(true);
    testThread.setPriority(9);

    doReturn(123L).when(testThread).getId();
    doReturn(Thread.State.RUNNABLE).when(testThread).getState();

    ThreadAdapter testThreadAdapter = new ThreadAdapter(testThread);

    assertThat(testThreadAdapter).isNotNull();
    assertThat(testThreadAdapter.getDelegate()).isEqualTo(testThread);

    assertThat(testThreadAdapter.toString())
      .isEqualTo("{ @type = %s, id = 123, name = ToStringTest, daemon = true, group = %s, priority = 9, state = RUNNABLE }",
        testThreadAdapter.getClass().getName(), testThreadGroup);
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
