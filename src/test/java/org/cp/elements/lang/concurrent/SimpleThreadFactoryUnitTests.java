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
import static org.cp.elements.lang.concurrent.SimpleThreadFactory.SimpleUncaughtExceptionHandler;
import static org.cp.elements.lang.concurrent.ThreadUtils.waitFor;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.cp.elements.lang.ThrowableUtils;
import org.cp.elements.test.TestUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link SimpleThreadFactory}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.lang.concurrent.SimpleThreadFactory
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class SimpleThreadFactoryUnitTests {

  protected static final int EXPECTED_COUNT = 10_000;

  @Mock
  private Runnable mockRunnable;

  @Test
  public void constructNewThreadFactory() {
    assertThat(SimpleThreadFactory.newThreadFactory()).isInstanceOf(SimpleThreadFactory.class);
  }

  @Test
  public void generatedThreadIdsAreUnique() {

    SimpleThreadFactory threadFactory = SimpleThreadFactory.newThreadFactory();

    Set<String> threadIds = new HashSet<>(EXPECTED_COUNT);

    for (int count = 0; count < EXPECTED_COUNT; count++) {
      String threadId = threadFactory.generateThreadId();
      assertThat(threadId).isNotEmpty();
      threadIds.add(threadId);
    }

    assertThat(threadIds.size()).isEqualTo(EXPECTED_COUNT);
  }

  @Test
  public void generatedThreadNamesAreUnique() {

    SimpleThreadFactory threadFactory = SimpleThreadFactory.newThreadFactory();

    Set<String> threadNames = new HashSet<>(EXPECTED_COUNT);

    for (int count = 0; count < EXPECTED_COUNT; count++) {
      String threadName = threadFactory.generateThreadName();
      threadNames.add(threadName);
      assertThat(threadName).matches(SimpleThreadFactory.class.getName() + "\\.THREAD-[-\\w]+");
    }

    assertThat(threadNames.size()).isEqualTo(EXPECTED_COUNT);
  }

  @Test
  public void constructsNewThreadWithRunnableTask() {

    Thread thread = SimpleThreadFactory.newThreadFactory().newThread(this.mockRunnable);

    assertThat(thread).isNotNull();
    assertThat(thread.getName()).matches(SimpleThreadFactory.class.getName() + "\\.THREAD-[-\\w]+");
    assertThat(thread.getContextClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
    assertThat(thread.isDaemon()).isTrue();
    assertThat(thread.getPriority()).isEqualTo(Thread.NORM_PRIORITY);
    assertThat(thread.getThreadGroup()).isEqualTo(SimpleThreadFactory.DEFAULT_THREAD_GROUP);
    assertThat(thread.getUncaughtExceptionHandler())
      .isEqualTo(SimpleThreadFactory.SimpleUncaughtExceptionHandler.INSTANCE);
  }

  @Test
  public void constructsNewNamedThreadWithRunnableTaskInThreadGroup() {

    ThreadGroup testThreadGroup = new ThreadGroup("TestThreadGroup");

    Thread thread = SimpleThreadFactory.newThreadFactory()
      .in(testThreadGroup)
      .newThread("TestThread", this.mockRunnable);

    assertThat(thread).isNotNull();
    assertThat(thread.getName()).isEqualTo("TestThread");
    assertThat(thread.getContextClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
    assertThat(thread.isDaemon()).isTrue();
    assertThat(thread.getPriority()).isEqualTo(Thread.NORM_PRIORITY);
    assertThat(thread.getThreadGroup()).isEqualTo(testThreadGroup);
    assertThat(thread.getUncaughtExceptionHandler())
      .isEqualTo(SimpleThreadFactory.SimpleUncaughtExceptionHandler.INSTANCE);
  }

  @Test
  public void constructsNewCustomThreadWithRunnableTaskInThreadGroup() {

    ClassLoader mockClassLoader = mock(ClassLoader.class);

    ThreadGroup testThreadGroup = new ThreadGroup("TestThreadGroup");

    Thread.UncaughtExceptionHandler mockUncaughtExceptionHandler = mock(Thread.UncaughtExceptionHandler.class);

    Thread thread = SimpleThreadFactory.newThreadFactory()
      .asUserThread()
      .handleUncaughtExceptionsWith(mockUncaughtExceptionHandler)
      .in(testThreadGroup)
      .resolveTypesWith(mockClassLoader)
      .withMaxPriority()
      .newThread("CustomThread", this.mockRunnable);

    assertThat(thread).isNotNull();
    assertThat(thread.getName()).isEqualTo("CustomThread");
    assertThat(thread.getContextClassLoader()).isEqualTo(mockClassLoader);
    assertThat(thread.isDaemon()).isFalse();
    assertThat(thread.getPriority()).isEqualTo(Thread.MAX_PRIORITY);
    assertThat(thread.getThreadGroup()).isEqualTo(testThreadGroup);
    assertThat(thread.getUncaughtExceptionHandler()).isEqualTo(mockUncaughtExceptionHandler);

    verifyNoInteractions(mockClassLoader, mockUncaughtExceptionHandler);
  }

  @Test(expected = IllegalArgumentException.class)
  public void newThreadWithBlankName() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> SimpleThreadFactory.newThreadFactory().newThread("  ", this.mockRunnable),
        () -> "Name [  ] is required");
  }

  @Test(expected = IllegalArgumentException.class)
  public void newThreadWithEmptyName() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> SimpleThreadFactory.newThreadFactory().newThread("", this.mockRunnable),
        () -> "Name [] is required");
  }

  @Test(expected = IllegalArgumentException.class)
  public void newThreadWithNullName() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> SimpleThreadFactory.newThreadFactory().newThread(null, this.mockRunnable),
        () -> "Name [null] is required");
  }

  @Test(expected = IllegalArgumentException.class)
  public void newThreadWithNullRunnable() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> SimpleThreadFactory.newThreadFactory().newThread(null),
        () -> "Runnable task is required");
  }

  @Test
  public void newThreadRunsRunnableTask() {

    AtomicBoolean ran = new AtomicBoolean(false);

    Runnable task = () -> ran.set(true);

    Thread thread = SimpleThreadFactory.newThreadFactory().newThread("TestThread", task);

    assertThat(thread).isNotNull();
    assertThat(thread.getName()).isEqualTo("TestThread");
    assertThat(ran.get()).isFalse();

    thread.start();

    waitFor(500, TimeUnit.MILLISECONDS)
      .checkEvery(100, TimeUnit.MILLISECONDS)
      .on(ran::get);

    assertThat(ran.get()).isTrue();
  }

  @Test
  public void newDaemonThreadIsDaemon() {

    assertThat(SimpleThreadFactory.newThreadFactory()
      .asDaemonThread()
      .newThread(this.mockRunnable)
      .isDaemon()).isTrue();
  }

  @Test
  public void newUserThreadIsNotDaemon() {

    assertThat(SimpleThreadFactory.newThreadFactory()
      .asUserThread()
      .newThread(this.mockRunnable)
      .isDaemon()).isFalse();
  }

  @Test
  public void newThreadHandlesUncaughtExceptionsWithMockUncaughtExceptionHandler() {

    Thread.UncaughtExceptionHandler mockUncaughtExceptionHandler = mock(Thread.UncaughtExceptionHandler.class);

    assertThat(SimpleThreadFactory.newThreadFactory()
      .handleUncaughtExceptionsWith(mockUncaughtExceptionHandler)
      .newThread(this.mockRunnable)
      .getUncaughtExceptionHandler()).isEqualTo(mockUncaughtExceptionHandler);

    verifyNoInteractions(mockUncaughtExceptionHandler);
  }

  @Test
  public void newThreadHandlesUncaughtExceptionsWithSimpleUncaughtExceptionHandler() {

    assertThat(SimpleThreadFactory.newThreadFactory()
      .handleUncaughtExceptionsWith(SimpleThreadFactory.SimpleUncaughtExceptionHandler.INSTANCE)
      .newThread(this.mockRunnable)
      .getUncaughtExceptionHandler()).isEqualTo(SimpleThreadFactory.SimpleUncaughtExceptionHandler.INSTANCE);
  }

  @Test
  public void newThreadSetToNullUncaughtExceptionHandler() {

    Thread.UncaughtExceptionHandler uncaughtExceptionHandler = SimpleThreadFactory.newThreadFactory()
      .handleUncaughtExceptionsWith(null)
      .newThread(this.mockRunnable)
      .getUncaughtExceptionHandler();

    assertThat(uncaughtExceptionHandler).isNotNull();
    assertThat(uncaughtExceptionHandler).isEqualTo(SimpleThreadFactory.SimpleUncaughtExceptionHandler.INSTANCE);
  }

  @Test
  public void newThreadResolvingTypesWithCurrentThreadContextClassLoader() {

    assertThat(SimpleThreadFactory.newThreadFactory().newThread(this.mockRunnable)
      .getContextClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
  }

  @Test
  public void newThreadResolvingTypesWithMockClassLoader() {

    ClassLoader mockClassLoader = mock(ClassLoader.class);

    assertThat(SimpleThreadFactory.newThreadFactory()
      .resolveTypesWith(mockClassLoader)
      .newThread(this.mockRunnable)
      .getContextClassLoader()).isEqualTo(mockClassLoader);

    verifyNoInteractions(mockClassLoader);
  }

  @Test
  public void newThreadSetToNullContextClassLoader() {

    assertThat(SimpleThreadFactory.newThreadFactory()
      .resolveTypesWith(null)
      .newThread(this.mockRunnable)
      .getContextClassLoader()).isNotNull();
  }

  @Test
  public void newThreadWithCustomPriority() {

    assertThat(SimpleThreadFactory.newThreadFactory()
      .withPriority(4)
      .newThread(this.mockRunnable)
      .getPriority()).isEqualTo(4);
  }

  @Test
  public void newThreadWithMaximumPriority() {

    assertThat(SimpleThreadFactory.newThreadFactory()
      .withMaxPriority()
      .newThread(this.mockRunnable)
      .getPriority()).isEqualTo(Thread.MAX_PRIORITY);
  }

  @Test
  public void newThreadWithMinimumPriority() {

    assertThat(SimpleThreadFactory.newThreadFactory()
      .withMinPriority()
      .newThread(this.mockRunnable)
      .getPriority()).isEqualTo(Thread.MIN_PRIORITY);
  }

  @Test
  public void newThreadWithNormalPriority() {

    assertThat(SimpleThreadFactory.newThreadFactory()
      .withNormalPriority()
      .newThread(this.mockRunnable)
      .getPriority()).isEqualTo(Thread.NORM_PRIORITY);
  }

  @Test
  public void simpleUncaughtExceptionHandlerHandlesUncaughtException() {

    SimpleUncaughtExceptionHandler uncaughtExceptionHandler = spy(SimpleUncaughtExceptionHandler.INSTANCE);

    RuntimeException exception = new RuntimeException();

    Logger mockLogger = mock(Logger.class);

    doReturn(mockLogger).when(uncaughtExceptionHandler).getLogger();
    doReturn(true).when(mockLogger).isLoggable(any(Level.class));

    Thread.currentThread().setName("simpleUncaughtExceptionHandlerHandlesUncaughtExceptionTest");

    uncaughtExceptionHandler.uncaughtException(Thread.currentThread(), exception);

    String expectedLogMessage = String.format("An unhandled error [java.lang.RuntimeException] was thrown"
        + " by Thread [simpleUncaughtExceptionHandlerHandlesUncaughtExceptionTest] with ID [%d]",
      Thread.currentThread().getId());

    verify(uncaughtExceptionHandler, times(1))
      .uncaughtException(eq(Thread.currentThread()), eq(exception));
    verify(uncaughtExceptionHandler, times(1)).getLogger();
    verify(mockLogger, times(1)).warning(expectedLogMessage);
    verify(mockLogger, times(1)).isLoggable(eq(Level.FINE));
    verify(mockLogger, times(1)).fine(ThrowableUtils.getStackTrace(exception));

    verifyNoMoreInteractions(uncaughtExceptionHandler, mockLogger);
  }

  @Test
  public void simpleUncaughtExceptionHandlerOnlyLogsWarning() {

    SimpleUncaughtExceptionHandler uncaughtExceptionHandler = spy(SimpleUncaughtExceptionHandler.INSTANCE);

    Error error = new Error();

    Logger mockLogger = mock(Logger.class);

    doReturn(mockLogger).when(uncaughtExceptionHandler).getLogger();
    doReturn(false).when(mockLogger).isLoggable(any(Level.class));

    Thread.currentThread().setName("simpleUncaughtExceptionHandlerOnlyLogsWarningTest");

    uncaughtExceptionHandler.uncaughtException(Thread.currentThread(), error);

    String expectedLogMessage = String.format("An unhandled error [java.lang.Error] was thrown"
        + " by Thread [simpleUncaughtExceptionHandlerOnlyLogsWarningTest] with ID [%d]",
      Thread.currentThread().getId());

    verify(uncaughtExceptionHandler, times(1))
      .uncaughtException(eq(Thread.currentThread()), eq(error));
    verify(uncaughtExceptionHandler, times(1)).getLogger();
    verify(mockLogger, times(1)).warning(expectedLogMessage);
    verify(mockLogger, times(1)).isLoggable(eq(Level.FINE));
    verify(mockLogger, never()).fine(anyString());

    verifyNoMoreInteractions(uncaughtExceptionHandler, mockLogger);
  }
}
