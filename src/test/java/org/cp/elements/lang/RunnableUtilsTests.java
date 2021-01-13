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

package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

import org.assertj.core.api.Assertions;
import org.cp.elements.lang.concurrent.ThreadUtils;
import org.junit.Test;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit tests for {@link RunnableUtils}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.RunnableUtils
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 1.0.0
 */
public class RunnableUtilsTests {

  private void testRunWithSleepUsingIllegalMilliseconds(RunnableUtilsInvoker invoker, long milliseconds) {

    AtomicBoolean ran = new AtomicBoolean(false);

    try {
      invoker.invoke(milliseconds, () -> ran.set(true));
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Milliseconds [%d] must be greater than 0", milliseconds);
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(ran.get()).isFalse();
    }
  }

  private void testRunWithSleepThenReturnValueUsingIllegalMilliseconds(RunnableUtilsReturningInvoker invoker,
      long milliseconds) {

    AtomicBoolean ran = new AtomicBoolean(false);

    try {
      invoker.invoke(milliseconds, () -> {
        ran.set(true);
        return "test";
      });
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Milliseconds [%d] must be greater than 0", milliseconds);
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(ran.get()).isFalse();
    }
  }

  @Test
  public void runWithSleep() {

    AtomicBoolean ran = new AtomicBoolean(false);

    long t0 = System.currentTimeMillis();

    assertThat(RunnableUtils.runWithSleep(50L, () -> ran.set(true))).isTrue();
    assertThat(System.currentTimeMillis() - t0).isGreaterThanOrEqualTo(50L);
    assertThat(ran.get()).isTrue();
  }

  @Test(expected = IllegalArgumentException.class)
  public void runWithSleepUsingNegativeMilliseconds() {
    testRunWithSleepUsingIllegalMilliseconds(RunnableUtils::runWithSleep, -1L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void runWithSleepUsingZeroMilliseconds() {
    testRunWithSleepUsingIllegalMilliseconds(RunnableUtils::runWithSleep, 0L);
  }

  @Test
  public void runWithSleepThrowOnInterrupt() {

    AtomicBoolean ran = new AtomicBoolean(false);

    long t0 = System.currentTimeMillis();

    assertThat(RunnableUtils.runWithSleepThrowOnInterrupt(50L, () -> ran.set(true))).isTrue();
    assertThat(System.currentTimeMillis() - t0).isGreaterThanOrEqualTo(50L);
    assertThat(ran.get()).isTrue();
  }

  @Test
  public void runWithSleepThrowOnInterruptThrowsSleepDeprivedException() throws Throwable {
    TestFramework.runOnce(new RunWithSleepThrowOnInterruptThrowsSleepDeprivedException());
  }

  @Test(expected = IllegalArgumentException.class)
  public void runWithSleepThrowOnInterruptUsingNegativeMilliseconds() {
    testRunWithSleepUsingIllegalMilliseconds(RunnableUtils::runWithSleepThrowOnInterrupt, -1L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void runWithSleepThrowOnInterruptUsingZeroMilliseconds() {
    testRunWithSleepUsingIllegalMilliseconds(RunnableUtils::runWithSleepThrowOnInterrupt, 0L);
  }

  @Test
  public void runWithSleepUninterrupted() {

    AtomicBoolean ran = new AtomicBoolean(false);

    long t0 = System.currentTimeMillis();

    assertThat(RunnableUtils.runWithSleepUninterrupted(50L, () -> ran.set(true))).isTrue();
    assertThat(System.currentTimeMillis() - t0).isGreaterThanOrEqualTo(50L);
    assertThat(ran.get()).isTrue();
  }

  @Test
  public void runWithSleepUninterruptedHandlesInterruptedThread() throws Throwable {
    TestFramework.runOnce(new RunWithSleepUninterruptedHandlesInterruptedThread());
  }

  @Test(expected = IllegalArgumentException.class)
  public void runWithSleepUninterruptedUsingNegativeMilliseconds() {
    testRunWithSleepUsingIllegalMilliseconds(RunnableUtils::runWithSleepUninterrupted, -1L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void runWithSleepUninterruptedUsingZeroMilliseconds() {
    testRunWithSleepUsingIllegalMilliseconds(RunnableUtils::runWithSleepUninterrupted, 0L);
  }

  @Test
  public void runWithSleepThenReturnValue() {

    long t0 = System.currentTimeMillis();

    assertThat(RunnableUtils.runWithSleepThenReturnValue(50L, () -> "test")).isEqualTo("test");
    assertThat(System.currentTimeMillis() - t0).isGreaterThanOrEqualTo(50L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void runWithSleepThenReturnValueWithNegativeMilliseconds() {
    testRunWithSleepThenReturnValueUsingIllegalMilliseconds(RunnableUtils::runWithSleepThenReturnValue, -1L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void runWithSleepThenReturnValueWithZeroMilliseconds() {
    testRunWithSleepThenReturnValueUsingIllegalMilliseconds(RunnableUtils::runWithSleepThenReturnValue, -0L);
  }

  @Test
  public void runWithSleepThenReturnValueThrowOnInterrupt() {

    long t0 = System.currentTimeMillis();

    assertThat(RunnableUtils.runWithSleepThenReturnValueThrowOnInterrupt(50L, () -> "test"))
      .isEqualTo("test");

    assertThat(System.currentTimeMillis() - t0).isGreaterThanOrEqualTo(50L);
  }

  @Test
  public void runWithSleepThenReturnValueThrowOnInterruptThrowsSleepDeprivedException() throws Throwable {
    TestFramework.runOnce(new RunWithSleepThenReturnValueThrowOnInterruptThrowsSleepDeprivedException());
  }

  @Test(expected = IllegalArgumentException.class)
  public void runWithSleepThenReturnValueThrowOnInterruptWithNegativeMilliseconds() {
    testRunWithSleepThenReturnValueUsingIllegalMilliseconds(RunnableUtils::runWithSleepThenReturnValueThrowOnInterrupt,
      -1L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void runWithSleepThenReturnValueThrowOnInterruptWithZeroMilliseconds() {
    testRunWithSleepThenReturnValueUsingIllegalMilliseconds(RunnableUtils::runWithSleepThenReturnValueThrowOnInterrupt,
      -0L);
  }

  @Test
  public void runWithSleepThenReturnValueUninterrupted() {

    long t0 = System.currentTimeMillis();

    assertThat(RunnableUtils.runWithSleepThenReturnValueUninterrupted(50L, () -> "test")).isEqualTo("test");
    assertThat(System.currentTimeMillis() - t0).isGreaterThanOrEqualTo(50L);
  }

  @Test
  public void runWithSleepThenReturnValueUninterruptedHandlesInterruptedThread() throws Throwable {
    TestFramework.runOnce(new RunWithSleepThenReturnValueUninterruptedHandlesInterruptedThread());
  }

  @Test(expected = IllegalArgumentException.class)
  public void runWithSleepThenReturnValueUninterruptedWithNegativeMilliseconds() {
    testRunWithSleepThenReturnValueUsingIllegalMilliseconds(RunnableUtils::runWithSleepThenReturnValueUninterrupted,
      -1L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void runWithSleepThenReturnValueUninterruptedWithZeroMilliseconds() {
    testRunWithSleepThenReturnValueUsingIllegalMilliseconds(RunnableUtils::runWithSleepThenReturnValueUninterrupted,
      -0L);
  }

  @Test
  public void timedRunIsCorrect() {
    assertThat(RunnableUtils.timedRun(() -> ThreadUtils.sleep(100L, 0)).orElse(0L))
      .isGreaterThanOrEqualTo(100L);
  }

  @SuppressWarnings("unused")
  private static final class RunWithSleepThrowOnInterruptThrowsSleepDeprivedException extends MultithreadedTestCase {

    private volatile Thread sleepingThread;

    public void thread1() {

      assertTick(0);

      this.sleepingThread = Thread.currentThread();
      this.sleepingThread.setName("Sleeping Thread");

      AtomicBoolean ran = new AtomicBoolean(false);

      try {
        RunnableUtils.runWithSleepThrowOnInterrupt(2000, () -> ran.set(true));
        Assertions.fail("Current Thread was not interrupted");
      }
      catch (RunnableUtils.SleepDeprivedException expected) {
        assertThat(expected).hasMessage("Failed to wait for [2000] millisecond(s)");
        assertThat(expected).hasNoCause();
      }
      finally {
        assertThat(ran.get()).isTrue();
        assertThat(Thread.currentThread().isInterrupted()).isTrue();
      }
    }

    public void thread2() {

      Thread.currentThread().setName("Interrupting Thread");

      waitForTick(1);

      Optional.ofNullable(this.sleepingThread).ifPresent(Thread::interrupt);
    }
  }

  @SuppressWarnings("unused")
  private static final class RunWithSleepUninterruptedHandlesInterruptedThread extends MultithreadedTestCase {

    private volatile Thread sleepingThread;

    public void thread1() {

      assertTick(0);

      this.sleepingThread = Thread.currentThread();
      this.sleepingThread.setName("Sleeping Thread");

      AtomicBoolean ran = new AtomicBoolean(false);

      long t0 = System.currentTimeMillis();

      try {
        assertThat(RunnableUtils.runWithSleepUninterrupted(200L, () -> ran.set(true))).isFalse();
        assertThat(System.currentTimeMillis() - t0).isGreaterThanOrEqualTo(200L);
      }
      catch (Throwable cause) {
        Assertions.fail("Current Thread was interrupted and the interrupt was not properly handled", cause);
      }
      finally {
        assertThat(ran.get()).isTrue();
        assertThat(Thread.currentThread().isInterrupted()).isTrue();
      }
    }

    public void thread2() {

      Thread.currentThread().setName("Interrupting Thread");

      waitForTick(1);

      Optional.ofNullable(this.sleepingThread).ifPresent(Thread::interrupt);
    }
  }

  @SuppressWarnings("unused")
  private static final class RunWithSleepThenReturnValueThrowOnInterruptThrowsSleepDeprivedException
      extends MultithreadedTestCase {

    private volatile Thread sleepingThread;

    public void thread1() {

      assertTick(0);

      this.sleepingThread = Thread.currentThread();
      this.sleepingThread.setName("Sleeping Thread");

      try {
        RunnableUtils.runWithSleepThenReturnValueThrowOnInterrupt(2000, () -> "test");
        Assertions.fail("Current Thread was not interrupted");
      }
      catch (RunnableUtils.SleepDeprivedException expected) {
        assertThat(expected).hasMessage("Failed to wait for [2000] millisecond(s)");
        assertThat(expected).hasNoCause();
      }
      finally {
        assertThat(Thread.currentThread().isInterrupted()).isTrue();
      }
    }

    public void thread2() {

      Thread.currentThread().setName("Interrupting Thread");

      waitForTick(1);

      Optional.ofNullable(this.sleepingThread).ifPresent(Thread::interrupt);
    }
  }

  @SuppressWarnings("unused")
  private static final class RunWithSleepThenReturnValueUninterruptedHandlesInterruptedThread
      extends MultithreadedTestCase {

    private volatile Thread sleepingThread;

    public void thread1() {

      assertTick(0);

      this.sleepingThread = Thread.currentThread();
      this.sleepingThread.setName("Sleeping Thread");

      AtomicBoolean ran = new AtomicBoolean(false);

      long t0 = System.currentTimeMillis();

      try {
        assertThat(RunnableUtils.runWithSleepThenReturnValueUninterrupted(200L, () -> {
          ran.set(true);
          return "test";
        })).isEqualTo("test");

        assertThat(System.currentTimeMillis() - t0).isGreaterThanOrEqualTo(200L);
      }
      catch (Throwable cause) {
        Assertions.fail("Current Thread was interrupted and the interrupt was not properly handled", cause);
      }
      finally {
        assertThat(ran.get()).isTrue();
        assertThat(Thread.currentThread().isInterrupted()).isTrue();
      }
    }

    public void thread2() {

      Thread.currentThread().setName("Interrupting Thread");

      waitForTick(1);

      Optional.ofNullable(this.sleepingThread).ifPresent(Thread::interrupt);
    }
  }

  @FunctionalInterface
  interface RunnableUtilsInvoker {
    void invoke(long milliseconds, Runnable runnable);
  }

  @FunctionalInterface
  interface RunnableUtilsReturningInvoker {
    void invoke(long milliseconds, RunnableUtils.ReturningRunnable<?> runnable);
  }
}
