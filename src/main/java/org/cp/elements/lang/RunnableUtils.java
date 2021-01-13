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

import java.util.Optional;

import org.cp.elements.lang.concurrent.ThreadUtils;

/**
 * The {@link RunnableUtils} class is a utility class for working with {@link Runnable} objects.
 *
 * @author John Blum
 * @see java.lang.Runnable
 * @see org.cp.elements.lang.concurrent.ThreadUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class RunnableUtils {

  public static final Runnable NOOP_RUNNABLE = () -> {};

  /**
   * Runs the given {@link Runnable} object and then causes the current, calling {@link Thread} to sleep
   * for the given number of milliseconds.
   *
   * This utility method can be used to simulate a long running, expensive operation.
   *
   * @param milliseconds a long value with the number of milliseconds for the current {@link Thread} to sleep.
   * @param runnable {@link Runnable} object to run; must not be {@literal null}.
   * @return a boolean value indicating whether the {@link Runnable} ran successfully
   * and whether the current {@link Thread} slept for the given number of milliseconds.
   * @throws IllegalArgumentException if milliseconds is less than equal to 0.
   * @see org.cp.elements.lang.concurrent.ThreadUtils#sleep(long, int)
   * @see java.lang.Runnable#run()
   */
  public static boolean runWithSleep(long milliseconds, Runnable runnable) {

    Assert.isTrue(milliseconds > 0, "Milliseconds [%d] must be greater than 0", milliseconds);

    runnable.run();

    return ThreadUtils.sleep(milliseconds, 0);
  }

  /**
   * Runs the given {@link Runnable} object and then causes the current, calling {@link Thread} to sleep
   * for the given number of milliseconds.
   *
   * This utility method can be used to simulate a long running, expensive operation.
   *
   * @param milliseconds a long value with the number of milliseconds for the current {@link Thread} to sleep.
   * @param runnable {@link Runnable} object to run; must not be {@literal null}.
   * @return a boolean value indicating whether the {@link Runnable} ran successfully
   * and whether the current {@link Thread} slept for the given number of milliseconds.
   * @throws IllegalArgumentException if milliseconds is less than equal to 0.
   * @throws SleepDeprivedException if the current {@link Thread} is interrupted while sleeping.
   * @see org.cp.elements.lang.concurrent.ThreadUtils#sleep(long, int)
   * @see java.lang.Runnable#run()
   */
  public static boolean runWithSleepThrowOnInterrupt(long milliseconds, Runnable runnable) {

    Assert.isTrue(milliseconds > 0, "Milliseconds [%d] must be greater than 0", milliseconds);

    runnable.run();

    if (!ThreadUtils.sleep(milliseconds, 0)) {
      throw new SleepDeprivedException(String.format("Failed to wait for [%d] millisecond(s)", milliseconds));
    }

    return true;
  }

  /**
   * Runs the given {@link Runnable} object and then causes the current, calling {@link Thread} to sleep
   * for the given number of milliseconds.
   *
   * This utility method sleeps uninterrupted, resetting the interrupt bit if the current, calling {@link Thread}
   * is interrupted during sleep.
   *
   * This utility method can be used to simulate a long running, expensive operation.
   *
   * @param milliseconds a long value with the number of milliseconds for the current {@link Thread} to sleep.
   * @param runnable {@link Runnable} object to run; must not be {@literal null}.
   * @return a boolean value indicating whether the {@link Runnable} ran successfully
   * and whether the current {@link Thread} slept for the given number of milliseconds.
   * @throws IllegalArgumentException if milliseconds is less than equal to 0.
   * @see java.lang.Runnable#run()
   * @see #safeSleep(long)
   */
  public static boolean runWithSleepUninterrupted(long milliseconds, Runnable runnable) {

    Assert.isTrue(milliseconds > 0, "Milliseconds [%d] must be greater than 0", milliseconds);

    runnable.run();

    return safeSleep(milliseconds);
  }

  /**
   * Runs the given {@link ReturningRunnable} object and then causes the current, calling {@link Thread} to sleep
   * for the given number of milliseconds before returning a value.
   *
   * This utility method can be used to simulate a long running, expensive operation.
   *
   * @param <T> {{@link Class} type of the {@link ReturningRunnable} return value.
   * @param milliseconds a long value with the number of milliseconds for the current {@link Thread} to sleep.
   * @param runnable {@link ReturningRunnable} object to run; must not be {@literal null}.
   * @return the value returned by the {@link ReturningRunnable}.
   * @throws IllegalArgumentException if milliseconds is less than equal to 0.
   * @see org.cp.elements.lang.RunnableUtils.ReturningRunnable#run()
   * @see org.cp.elements.lang.concurrent.ThreadUtils#sleep(long, int)
   */
  public static <T> T runWithSleepThenReturnValue(long milliseconds, ReturningRunnable<T> runnable) {

    Assert.isTrue(milliseconds > 0, "Milliseconds [%d] must be greater than 0", milliseconds);

    T returnValue = runnable.run();

    ThreadUtils.sleep(milliseconds, 0);

    return returnValue;
  }

  /**
   * Runs the given {@link ReturningRunnable} object and then causes the current, calling {@link Thread} to sleep
   * for the given number of milliseconds before returning a value.
   *
   * This utility method can be used to simulate a long running, expensive operation.
   *
   * @param <T> {{@link Class} type of the {@link ReturningRunnable} return value.
   * @param milliseconds a long value with the number of milliseconds for the current {@link Thread} to sleep.
   * @param runnable {@link ReturningRunnable} object to run; must not be {@literal null}.
   * @return the value returned by the {@link ReturningRunnable}.
   * @throws IllegalArgumentException if milliseconds is less than equal to 0.
   * @throws SleepDeprivedException if the current {@link Thread} is interrupted while sleeping.
   * @see org.cp.elements.lang.RunnableUtils.ReturningRunnable#run()
   * @see org.cp.elements.lang.concurrent.ThreadUtils#sleep(long, int)
   */
  public static <T> T runWithSleepThenReturnValueThrowOnInterrupt(long milliseconds, ReturningRunnable<T> runnable) {

    Assert.isTrue(milliseconds > 0, "Milliseconds [%d] must be greater than 0", milliseconds);

    T returnValue = runnable.run();

    if (!ThreadUtils.sleep(milliseconds, 0)) {
      throw new SleepDeprivedException(String.format("Failed to wait for [%d] millisecond(s)", milliseconds));
    }

    return returnValue;
  }

  /**
   * Runs the given {@link ReturningRunnable} object and then causes the current, calling {@link Thread} to sleep
   * for the given number of milliseconds before returning a value.
   *
   * This utility method sleeps uninterrupted, resetting the interrupt bit if the current, calling {@link Thread}
   * is interrupted during sleep.
   *
   * This utility method can be used to simulate a long running, expensive operation.
   *
   * @param <T> {{@link Class} type of the {@link ReturningRunnable} return value.
   * @param milliseconds a long value with the number of milliseconds for the current {@link Thread} to sleep.
   * @param runnable {@link ReturningRunnable} object to run; must not be {@literal null}.
   * @return the value returned by the {@link ReturningRunnable}.
   * @throws IllegalArgumentException if milliseconds is less than equal to 0.
   * @see org.cp.elements.lang.RunnableUtils.ReturningRunnable#run()
   * @see #safeSleep(long)
   */
  public static <T> T runWithSleepThenReturnValueUninterrupted(long milliseconds, ReturningRunnable<T> runnable) {

    Assert.isTrue(milliseconds > 0, "Milliseconds [%d] must be greater than 0", milliseconds);

    T returnValue = runnable.run();

    safeSleep(milliseconds);

    return returnValue;
  }

  /**
   * Safely sleeps for the given amount of milliseconds.
   *
   * If the {@link Thread#currentThread() current Thread} is {@link Thread#isInterrupted() interrupted} while
   * {@link Thread#sleep(long, int) sleeping}, then the {@link Thread#currentThread() current Thread} will continue
   * to {@link Thread#sleep(long, int) sleep} until the given number of milliseconds have expired and the interrupt bit
   * will be set on return.
   *
   * @param milliseconds the number of milliseconds that the {@link Thread#currentThread() current Thread} will sleep.
   * @return {@literal true} if the {@link Thread#currentThread() current Thread} was able to
   * {@link Thread#sleep(long, int)} for the given number of milliseconds uninterrupted.
   * @see java.lang.Thread#sleep(long, int)
   */
  private static boolean safeSleep(long milliseconds) {

    boolean interrupted = false;

    long timeout = (System.currentTimeMillis() + milliseconds);

    while (System.currentTimeMillis() < timeout) {
      try {
        Thread.sleep(milliseconds);
      }
      catch (InterruptedException cause) {
        interrupted = true;
      }
      finally {
        milliseconds = Math.min(timeout - System.currentTimeMillis(), 0);
      }
    }

    if (interrupted) {
      Thread.currentThread().interrupt();
    }

    return !Thread.currentThread().isInterrupted();
  }

  /**
   * Times the {@link Runnable#run()} method of the given {@link Runnable}.
   *
   * @param runnable the {@link Runnable} object to run and time.
   * @return the amount of time in milliseconds that it took to run the given {@link Runnable}.
   * @see java.lang.Runnable
   */
  public static Optional<Long> timedRun(Runnable runnable) {
    long t0 = System.currentTimeMillis();
    runnable.run();
    return Optional.of(System.currentTimeMillis() - t0);
  }

  /**
   * The {@link ReturningRunnable} interface defines a contract for implementing objects that run a computation
   * and return a value.
   *
   * @param <T> {@link Class} type of the return value.
   */
  @FunctionalInterface
  public interface ReturningRunnable<T> {

    /**
     * Computation to run.
     *
     * @return the result of the computation.
     */
    T run();
  }

  /**
   * The {@link SleepDeprivedException} is a {@link RuntimeException} that is thrown when a {@link Thread}
   * did not get all of its {@link Thread#sleep(long, int) sleep}.
   *
   * @see java.lang.RuntimeException
   */
  public static final class SleepDeprivedException extends RuntimeException {

    /**
     * Construct a new instance of the {@link SleepDeprivedException} with no {@link String message} or no known
     * {@link Throwable cause}.
     */
    public SleepDeprivedException() {
    }

    /**
     * Construct a new instance of the {@link SleepDeprivedException} initialized with given {@link String message}
     * describing the reason for this exception.
     *
     * @param message {@link String} describing the reason for this exception.
     */
    public SleepDeprivedException(String message) {
      super(message);
    }

    /**
     * Construct a new instance of the {@link SleepDeprivedException} initialized with given {@link Throwable cause}
     * of this exception.
     *
     * @param cause {@link Throwable} object as the underlying cause of this exception.
     * @see java.lang.Throwable
     */
    public SleepDeprivedException(Throwable cause) {
      super(cause);
    }

    /**
     * Construct a new instance of the {@link SleepDeprivedException} initialized with given {@link String message}
     * describing the reason for this exception along with the underlying {@link Throwable cause} of this exception.
     *
     * @param message {@link String} describing the reason for this exception.
     * @param cause {@link Throwable} object as the underlying cause of this exception.
     * @see java.lang.Throwable
     */
    public SleepDeprivedException(String message, Throwable cause) {
      super(message, cause);
    }
  }
}
