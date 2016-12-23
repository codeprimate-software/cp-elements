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
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;

import java.util.HashSet;
import java.util.Set;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * Unit tests for {@link SimpleThreadFactory}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
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
    assertThat(thread.getPriority()).isEqualTo(Thread.MAX_PRIORITY);
    assertThat(thread.getThreadGroup()).isEqualTo(testThreadGroup);
    assertThat(thread.getUncaughtExceptionHandler()).isEqualTo(mockUncaughtExceptionHandler);
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
  public void newThreadWithNullName() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Thread name must be specified");

    SimpleThreadFactory.newThreadFactory().newThread(null, mockRunnable);
  }

  @Test
  public void newThreadWithEmptyName() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Thread name must be specified");

    SimpleThreadFactory.newThreadFactory().newThread("  ", mockRunnable);
  }
}
