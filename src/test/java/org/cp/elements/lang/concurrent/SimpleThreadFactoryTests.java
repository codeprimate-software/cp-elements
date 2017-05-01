/*
 * Copyright 2016 Author or Authors.
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
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.cp.elements.lang.ThrowableUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link SimpleThreadFactory}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.lang.concurrent.SimpleThreadFactory
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class SimpleThreadFactoryTests {

  protected static final int EXPECTED_COUNT = 10000;

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Mock
  private Runnable mockRunnable;

  @Test
  public void constructNewThreadFactoryIsSuccessful() {
    assertThat(SimpleThreadFactory.newThreadFactory()).isInstanceOf(SimpleThreadFactory.class);
  }

  @Test
  public void generatedThreadIdentifiersAreUnique() {
    SimpleThreadFactory threadFactory = SimpleThreadFactory.newThreadFactory();
    Set<String> threadIds = new HashSet<>(EXPECTED_COUNT);

    for (int count = 0; count < EXPECTED_COUNT; count++) {
      threadIds.add(threadFactory.generatedThreadId());
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
      assertThat(threadName).startsWith(String.format("%s.THREAD-", SimpleThreadFactory.class.getName()));
    }

    assertThat(threadNames.size()).isEqualTo(EXPECTED_COUNT);
  }

  @Test
  public void constructsNewThreadWithRunnableTask() {
    SimpleThreadFactory threadFactory = SimpleThreadFactory.newThreadFactory();

    Thread thread = threadFactory.newThread(mockRunnable);

    assertThat(thread).isNotNull();
    assertThat(thread.getName()).startsWith(String.format("%s.THREAD-", SimpleThreadFactory.class.getName()));
    assertThat(thread.getContextClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
    assertThat(thread.isDaemon()).isTrue();
    assertThat(thread.getPriority()).isEqualTo(Thread.NORM_PRIORITY);
    assertThat(thread.getThreadGroup()).isEqualTo(SimpleThreadFactory.DEFAULT_THREAD_GROUP);
    assertThat(thread.getUncaughtExceptionHandler()).isEqualTo(
      SimpleThreadFactory.SimpleUncaughtExceptionHandler.INSTANCE);
  }

  @Test
  public void constructsNewNamedThreadWithRunnableTaskInThreadGroup() {
    ThreadGroup testThreadGroup = new ThreadGroup("TestThreadGroup");

    Thread thread = SimpleThreadFactory.newThreadFactory().in(testThreadGroup)
      .newThread("TestThread", mockRunnable);

    assertThat(thread).isNotNull();
    assertThat(thread.getName()).isEqualTo("TestThread");
    assertThat(thread.getContextClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
    assertThat(thread.isDaemon()).isTrue();
    assertThat(thread.getPriority()).isEqualTo(Thread.NORM_PRIORITY);
    assertThat(thread.getThreadGroup()).isEqualTo(testThreadGroup);
    assertThat(thread.getUncaughtExceptionHandler()).isEqualTo(
      SimpleThreadFactory.SimpleUncaughtExceptionHandler.INSTANCE);
  }

  @Test
  public void constructsNewCustomizedThreadWithRunnableTaskInThreadGroup() {
    ThreadGroup testThreadGroup = new ThreadGroup("TestThreadGroup");

    Thread.UncaughtExceptionHandler mockUncaughtExceptionHandler = mock(Thread.UncaughtExceptionHandler.class);

    Thread thread = SimpleThreadFactory.newThreadFactory()
      .as(false)
      .in(testThreadGroup)
      .using(ClassLoader.getSystemClassLoader())
      .using(mockUncaughtExceptionHandler)
      .with(Thread.MAX_PRIORITY)
      .newThread("TestThread", mockRunnable);

    assertThat(thread).isNotNull();
    assertThat(thread.getName()).isEqualTo("TestThread");
    assertThat(thread.getContextClassLoader()).isEqualTo(ClassLoader.getSystemClassLoader());
    assertThat(thread.isDaemon()).isFalse();
    assertThat(thread.getPriority()).isEqualTo(Thread.MAX_PRIORITY);
    assertThat(thread.getThreadGroup()).isEqualTo(testThreadGroup);
    assertThat(thread.getUncaughtExceptionHandler()).isEqualTo(mockUncaughtExceptionHandler);
  }

  @Test
  public void newThreadWithEmptyName() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Thread name must be specified");

    SimpleThreadFactory.newThreadFactory().newThread("  ", mockRunnable);
  }

  @Test
  public void newThreadWithNullName() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Thread name must be specified");

    SimpleThreadFactory.newThreadFactory().newThread(null, mockRunnable);
  }

  @Test
  @SuppressWarnings("all")
  public void newThreadWithNullRunnable() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Thread task must not be null");

    SimpleThreadFactory.newThreadFactory().newThread(null);
  }

  @Test
  public void newThreadRunsRunnableTask() {
    AtomicBoolean ran = new AtomicBoolean(false);

    Thread thread = SimpleThreadFactory.newThreadFactory().newThread("TestThread", () -> { ran.set(true); });

    thread.start();
    waitFor(500, TimeUnit.MILLISECONDS).checkEvery(100, TimeUnit.MILLISECONDS).on(ran::get);

    assertThat(ran.get()).isTrue();
  }

  @Test
  public void simpleUncaughtExceptionHandlerHandlesUncaughtException() {
    SimpleUncaughtExceptionHandler uncaughtExceptionHandler = spy(SimpleUncaughtExceptionHandler.INSTANCE);
    RuntimeException exception = new RuntimeException();
    Logger mockLogger = mock(Logger.class);

    when(uncaughtExceptionHandler.getLogger()).thenReturn(mockLogger);
    when(mockLogger.isLoggable(any(Level.class))).thenReturn(true);

    Thread.currentThread().setName("simpleUncaughtExceptionHandlerHandlesUncaughtExceptionTest");
    uncaughtExceptionHandler.uncaughtException(Thread.currentThread(), exception);

    verify(mockLogger, times(1)).warning(String.format(
      "An unhandled error [java.lang.RuntimeException] was thrown by Thread [simpleUncaughtExceptionHandlerHandlesUncaughtExceptionTest] having ID [%d]",
        Thread.currentThread().getId()));

    verify(mockLogger, times(1)).isLoggable(eq(Level.FINE));

    verify(mockLogger, times(1)).fine(ThrowableUtils.getStackTrace(exception));
  }

  @Test
  public void simpleUncaughtExceptionHandlerOnlyLogsWarning() {
    SimpleUncaughtExceptionHandler uncaughtExceptionHandler = spy(SimpleUncaughtExceptionHandler.INSTANCE);
    RuntimeException exception = new RuntimeException();
    Logger mockLogger = mock(Logger.class);

    when(uncaughtExceptionHandler.getLogger()).thenReturn(mockLogger);
    when(mockLogger.isLoggable(any(Level.class))).thenReturn(false);

    Thread.currentThread().setName("simpleUncaughtExceptionHandlerOnlyLogsWarning");
    uncaughtExceptionHandler.uncaughtException(Thread.currentThread(), exception);

    verify(mockLogger, times(1)).warning(String.format(
      "An unhandled error [java.lang.RuntimeException] was thrown by Thread [simpleUncaughtExceptionHandlerOnlyLogsWarning] having ID [%d]",
        Thread.currentThread().getId()));

    verify(mockLogger, times(1)).isLoggable(eq(Level.FINE));

    verify(mockLogger, never()).fine(anyString());
  }
}
