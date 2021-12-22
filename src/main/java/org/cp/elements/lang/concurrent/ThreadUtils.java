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

import java.util.Optional;
import java.util.concurrent.TimeUnit;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Condition;
import org.cp.elements.lang.FluentApiExtension;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link ThreadUtils} is an abstract utility class provides used to write concurrent programs
 * using Java {@link Thread Threads} and the {@link java.util.concurrent} API.
 *
 * @author John J. Blum
 * @see java.lang.Runnable
 * @see java.lang.Thread
 * @see java.lang.ThreadGroup
 * @see java.lang.ThreadLocal
 * @see java.util.concurrent.TimeUnit
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ThreadUtils {

  /**
   * Determines whether the specified Thread is alive.  A Thread is alive if it has been started and has not yet died.
   *
   * @param thread the Thread to evaluate.
   * @return a boolean value indicating whether the specified Thread is alive.
   * @see java.lang.Thread#isAlive()
   */
  @NullSafe
  public static boolean isAlive(@Nullable Thread thread) {
    return thread != null && thread.isAlive();
  }

  /**
   * Determines whether the specified Thread is in a blocked state.  A Thread may be currently blocked waiting on a lock
   * or performing some IO operation.
   *
   * @param thread the Thread to evaluate.
   * @return a boolean valued indicating whether he specified Thread is blocked.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#BLOCKED
   */
  @NullSafe
  public static boolean isBlocked(@Nullable Thread thread) {
    return thread != null && Thread.State.BLOCKED.equals(thread.getState());
  }

  /**
   * Determines whether the specified Thread is a daemon Thread.  A daemon Thread is a background Thread that does not
   * prevent the JVM from exiting.
   *
   * @param thread the Thread to evaluate.
   * @return a boolean value indicating whether the specified Thread is a daemon Thread.
   * @see java.lang.Thread#isDaemon()
   * @see #isNonDaemon(Thread)
   */
  @NullSafe
  public static boolean isDaemon(@Nullable Thread thread) {
    return thread != null && thread.isDaemon();
  }

  /**
   * Determines whether the specified Thread is a non-daemon Thread.  A non-daemon Thread is a background Thread
   * that prevents the JVM from exiting.
   *
   * @param thread the Thread to evaluate.
   * @return a boolean value indicating whether the specified Thread is a non-daemon Thread.
   * @see java.lang.Thread#isDaemon()
   * @see #isDaemon(Thread)
   */
  @NullSafe
  public static boolean isNonDaemon(@Nullable Thread thread) {
    return thread != null && !thread.isDaemon();
  }

  /**
   * Determines whether the specified Thread has been interrupted. The interrupted status of the Thread is unaffected
   * by this method.
   *
   * @param thread the Thread to evaluate for interruption.
   * @return a boolean value indicating whether the given Thread has been interrupted or not.
   * @see java.lang.Thread#isInterrupted()
   */
  @NullSafe
  public static boolean isInterrupted(@Nullable Thread thread) {
    return thread != null && thread.isInterrupted();
  }

  /**
   * Determines whether the specified Thread is a new Thread.  A "new" Thread is any Thread that has not been
   * started yet.
   *
   * @param thread the Thread to evaluate.
   * @return a boolean value indicating whether the Thread is new.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#NEW
   */
  @NullSafe
  public static boolean isNew(@Nullable Thread thread) {
    return thread != null && Thread.State.NEW.equals(thread.getState());
  }

  /**
   * Determines whether the specified Thread is a runnable Thread.  A "runnable" Thread is any Thread that can be
   * scheduled by the Operating System (OS) for execution.
   *
   * @param thread the Thread to evaluate.
   * @return a boolean value indicating whether the Thread is in a runnable state.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#RUNNABLE
   */
  @NullSafe
  public static boolean isRunnable(@Nullable Thread thread) {
    return thread != null && Thread.State.RUNNABLE.equals(thread.getState());
  }

  /**
   * Determines whether the specified Thread has been terminated (stopped).
   *
   * @param thread the Thread to evaluate.
   * @return a boolean value indicating whether the Thread has been terminated.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#TERMINATED
   */
  @NullSafe
  public static boolean isTerminated(@Nullable Thread thread) {
    return thread != null && Thread.State.TERMINATED.equals(thread.getState());
  }

  /**
   * Determines whether the specified Thread is currently in a timed wait.
   *
   * @param thread the Thread to evaluate.
   * @return a boolean value indicating whether the Thread is currently in a timed wait.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#TIMED_WAITING
   */
  @NullSafe
  public static boolean isTimedWaiting(@Nullable Thread thread) {
    return thread != null && Thread.State.TIMED_WAITING.equals(thread.getState());
  }

  /**
   * Determines whether the specified Thread is currently in a wait.
   *
   * @param thread the Thread to evaluate.
   * @return a boolean value indicating whether the Thread is currently in a wait.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#WAITING
   */
  @NullSafe
  public static boolean isWaiting(@Nullable Thread thread) {
    return thread != null && Thread.State.WAITING.equals(thread.getState());
  }

  /**
   * A null-safe method for getting the Thread's context ClassLoader.
   *
   * @param thread the Thread from which the context ClassLoader is returned.
   * @return the context ClassLoader of the specified Thread or the ThreadUtils class ClassLoader if the Thread is null.
   * @see java.lang.Thread#getContextClassLoader()
   * @see java.lang.ClassLoader
   */
  @NullSafe
  public static ClassLoader getContextClassLoader(@Nullable Thread thread) {

    return thread != null
      ? thread.getContextClassLoader()
      : ThreadUtils.class.getClassLoader();
  }

  /**
   * A null-safe method for getting the Thread's ID.
   *
   * @param thread the Thread from which the ID is returned.
   * @return the identifier of the specified Thread, or 0 if the Thread is null.
   * @see java.lang.Thread#getId()
   */
  @NullSafe
  public static long getId(@Nullable Thread thread) {
    return thread != null ? thread.getId() : 0L;
  }

  /**
   * A null-safe method for getting the Thread's name.
   *
   * @param thread the Thread from which the name is returned.
   * @return a String indicating the name of the specified Thread or null if the Thread is null.
   * @see java.lang.Thread#getName()
   */
  @NullSafe
  public static @Nullable String getName(@Nullable Thread thread) {
    return thread != null ? thread.getName() : null;
  }

  /**
   * A null-safe method for getting the Thread's priority.
   *
   * @param thread the Thread from which the priority is returned.
   * @return an integer value indicating the priority of the specified Thread or 0 if the Thread is null.
   * @see java.lang.Thread#getPriority()
   */
  @NullSafe
  public static int getPriority(@Nullable Thread thread) {
    return thread != null ? thread.getPriority() : Thread.NORM_PRIORITY;
  }

  /**
   * A null-safe method for getting a snapshot of the Thread's current stack trace.
   *
   * @param thread the Thread from which the stack trace is returned.
   * @return an array of StackTraceElements indicating the stack trace of the specified Thread,
   * or an empty StackTraceElement array if the Thread is null.
   * @see java.lang.Thread#getStackTrace()
   * @see java.lang.StackTraceElement
   */
  @NullSafe
  public static @NotNull StackTraceElement[] getStackTrace(@Nullable Thread thread) {
    return thread != null ? thread.getStackTrace() : new StackTraceElement[0];
  }

  /**
   * A null-safe method for getting the Thread's current state.
   *
   * @param thread the Thread from which the state is returned.
   * @return the State of the specified Thread or null if the Thread is null.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State
   */
  @NullSafe
  public static @Nullable Thread.State getState(@Nullable Thread thread) {
    return thread != null ? thread.getState() : null;
  }

  /**
   * A null-safe method for getting the Thread's ThreadGroup.
   *
   * @param thread the Thread from which the ThreadGroup is returned.
   * @return the ThreadGroup of the specified Thread or null if the Thread is null.
   * @see java.lang.Thread#getThreadGroup()
   * @see java.lang.ThreadGroup
   */
  @NullSafe
  public static @Nullable ThreadGroup getThreadGroup(@Nullable Thread thread) {
    return thread != null ? thread.getThreadGroup() : null;
  }

  /**
   * Null-safe operation to dump the (call) stack of the current Thread.
   *
   * @param tag a String labeling the stack dump of the current Thread.
   * @see java.lang.Thread#dumpStack()
   */
  @NullSafe
  public static void dumpStack(String tag) {

    Thread currentThread = Thread.currentThread();

    System.err.printf("[%1$s] %2$s Thread @ %3$d%n", String.valueOf(tag).toUpperCase(), currentThread.getName(),
      currentThread.getId());

    Thread.dumpStack();
  }

  /**
   * Null-safe operation to interrupt the specified Thread.
   *
   * @param thread the Thread to interrupt.
   * @see java.lang.Thread#interrupt()
   */
  @NullSafe
  public static void interrupt(@Nullable Thread thread) {
    if (thread != null) {
      thread.interrupt();
    }
  }

  /**
   * Causes the current Thread to join with the specified Thread.  If the current Thread is interrupted while waiting
   * for the specified Thread, then the current Threads interrupt bit will be set and this method will return false.
   * Otherwise, the current Thread will wait on the specified Thread until it dies, or until the time period has expired
   * and then the method will return true.
   *
   * @param thread the Thread that the current Thread (caller) will join.
   * @param milliseconds the number of milliseconds the current Thread will wait during the join operation.  If the
   * number of milliseconds specified is 0, then the current Thread will wait until the specified Thread dies, or the
   * current Thread is interrupted.
   * @param nanoseconds the number of nanoseconds in addition to the milliseconds the current Thread will wait during
   * the join operation.
   * @return a boolean condition indicating if the current Thread successfully joined the specified Thread without being
   * interrupted.
   * @see java.lang.Thread#join(long, int)
   * @see java.lang.Thread#interrupt()
   */
  @NullSafe
  public static boolean join(@Nullable Thread thread, long milliseconds, int nanoseconds) {

    try {
      if (thread != null) {
        thread.join(milliseconds, nanoseconds);
        return true;
      }
    }
    catch (InterruptedException ignore) {
      Thread.currentThread().interrupt();
    }

    return false;
  }

  /**
   * Causes the current Thread to sleep for the specified number of milliseconds and nanoseconds.  If the current Thread
   * is interrupted, the sleep is aborted, however, the interrupt bit is reset and this method returns false.
   *
   * @param milliseconds the number of milliseconds to cause the current Thread to sleep (sleep).  If the number
   * of milliseconds is 0, then the current Thread will sleep (sleep) until interrupted.
   * @param nanoseconds the number of nanoseconds in addition to the milliseconds to cause the current Thread to sleep
   * (sleep).
   * @return a boolean value if the sleep operation was successful without interruption.
   * @see java.lang.Thread#sleep(long)
   * @see java.lang.Thread#interrupt()
   */
  public static boolean sleep(long milliseconds, int nanoseconds) {

    try {
      Thread.sleep(milliseconds, nanoseconds);
      return true;
    }
    catch (InterruptedException ignore) {
      Thread.currentThread().interrupt();
      return false;
    }
  }

  /**
   * Waits for a specified duration on a {@link Condition} possibly checking every specified interval
   * on whether the {@link Condition} has been satisfied.
   *
   * @param duration a long value indicating the duration of time to wait for the Condition to be satisfied
   * (default time unit is MILLISECONDS).
   * @return a boolean value indicating whether the Condition has been satisfied within the given duration.
   * @see org.cp.elements.lang.concurrent.ThreadUtils.WaitTask
   * @see org.cp.elements.lang.annotation.FluentApi
   */
  @FluentApi
  public static @NotNull WaitTask waitFor(long duration) {
    return WaitTask.newWaitTask().waitFor(duration);
  }

  /**
   * Waits for a specified duration on a {@link Condition} possibly checking every specified interval
   * on whether the {@link Condition} has been satisfied.
   *
   * @param duration a long value indicating the duration of time to wait for the Condition to be satisfied.
   * @param timeUnit the TimeUnit of the duration time value.
   * @return a boolean value indicating whether the Condition has been satisfied within the given duration.
   * @see org.cp.elements.lang.concurrent.ThreadUtils.WaitTask
   * @see org.cp.elements.lang.annotation.FluentApi
   * @see java.util.concurrent.TimeUnit
   */
  @FluentApi
  public static @NotNull WaitTask waitFor(long duration, TimeUnit timeUnit) {
    return WaitTask.newWaitTask().waitFor(duration, timeUnit);
  }

  /**
   * The {@link WaitTask} class is a {@link FluentApiExtension} specifying an API for setting up a wait condition.
   *
   * @see org.cp.elements.lang.FluentApiExtension
   */
  public static class WaitTask implements FluentApiExtension {

    protected static final TimeUnit DEFAULT_TIME_UNIT = TimeUnit.MILLISECONDS;

    private long duration;
    private long interval;

    private final Object waitTaskMonitor = new Object();

    private TimeUnit durationTimeUnit;
    private TimeUnit intervalTimeUnit;

    /**
     * Factory method used to construct a new, default instance of {@link WaitTask}.
     *
     * @return a new instance of {@link WaitTask}.
     */
    public static WaitTask newWaitTask() {
      return new WaitTask();
    }

    /**
     * Returns the duration of the wait.
     *
     * @return a long value representing the length in time to wait.
     */
    public long getDuration() {
      return this.duration;
    }

    /**
     * Returns the unit of time for the wait duration.
     *
     * @return a {@link TimeUnit} indicating the unit of time used for the duration of the wait.
     * @see java.util.concurrent.TimeUnit
     */
    public TimeUnit getDurationTimeUnit() {
      return this.durationTimeUnit;
    }

    /**
     * Returns the interval of time inside the duration at which the condition is reevaluated.
     *
     * @return a long value representing the interval to reevaluate the condition of the wait.
     */
    public long getInterval() {

      long duration = getDuration();

      return this.interval > 0
        ? Math.min(this.interval, duration)
        : duration;
    }

    /**
     * Returns the unit of time for the interval.
     *
     * @return a {@link TimeUnit} indicating the unit of time used for the interval.
     * @see java.util.concurrent.TimeUnit
     */
    public TimeUnit getIntervalTimeUnit() {
      return ObjectUtils.defaultIfNull(this.intervalTimeUnit, getDurationTimeUnit());
    }

    /**
     * Causes the current {@link Thread} to wait for the specified duration, or until the condition is met.
     *
     * @param duration long value indicating the length of time to wait.
     * @return this {@link WaitTask} set to the specified duration.
     * @see #waitFor(long, TimeUnit)
     */
    public WaitTask waitFor(long duration) {
      return waitFor(duration, DEFAULT_TIME_UNIT);
    }

    /**
     * Causes the current {@link Thread} to wait for the specified duration in the given {@link TimeUnit},
     * or until the condition is met.
     *
     * @param duration long value indicating the length of time to wait.
     * @param durationTimeUnit {@link TimeUnit} used to specify the unit of time for the duration.
     * @return this {@link WaitTask} set to the specified duration in the given {@link TimeUnit}.
     * @throws IllegalArgumentException if the duration is less than equal to 0.
     * @see java.util.concurrent.TimeUnit
     */
    public WaitTask waitFor(long duration, TimeUnit durationTimeUnit) {

      Assert.argument(duration, argument -> argument > 0, "duration (%1$d) must be greater than 0", duration);

      this.duration = duration;
      this.durationTimeUnit = ObjectUtils.defaultIfNull(durationTimeUnit, DEFAULT_TIME_UNIT);

      return this;
    }

    private boolean isValidInterval(long interval, TimeUnit intervalTimeUnit) {
      return interval > 0 && intervalTimeUnit.toMillis(interval) <= getDurationTimeUnit().toMillis(getDuration());
    }

    /**
     * Causes the condition to be reevaluated every interval of time.
     *
     * @param interval long value indicating the interval of time to reevaluate the condition of the wait.
     * @return this {@link WaitTask} configured with the given interval.
     * @see #checkEvery(long, TimeUnit)
     */
    public WaitTask checkEvery(long interval) {
      return checkEvery(interval, DEFAULT_TIME_UNIT);
    }

    /**
     * Causes the condition to be reevaluated every interval of time in the given {@link TimeUnit}.
     *
     * @param interval long value indicating the interval of time to reevaluate the condition of the wait.
     * @param intervalTimeUnit {@link TimeUnit} used in the interval.
     * @return this {@link WaitTask} configured with the given interval.
     * @throws IllegalArgumentException if interval is less than equal to 0 or greater than the duration.
     * @see java.util.concurrent.TimeUnit
     * @see #isValidInterval(long, TimeUnit)
     */
    public WaitTask checkEvery(long interval, TimeUnit intervalTimeUnit) {

      TimeUnit resolvedIntervalTimeUnit = intervalTimeUnit != null
        ? intervalTimeUnit
        : DEFAULT_TIME_UNIT;

      Assert.argument(interval, argument -> isValidInterval(argument, resolvedIntervalTimeUnit), String.format(
        "Interval [%1$d %2$s] must be greater than 0 and less than equal to duration [%3$d %4$s]",
          interval, intervalTimeUnit, duration, durationTimeUnit));

      this.interval = interval;
      this.intervalTimeUnit = resolvedIntervalTimeUnit;

      return this;
    }

    /**
     * Evaluates the given {@link Condition}, waiting up to at most the specified duration.
     *
     * If the provided {@link Condition} is {@literal null}, the {@link Condition#FALSE_CONDITION} is used.
     *
     * @param condition {@link Condition} to evaluate.
     * @return a boolean value indicating whether the {@link Condition} was satisfied.
     * @see org.cp.elements.lang.Condition
     */
    public boolean on(Condition condition) {

      long timeout = System.currentTimeMillis() + getDurationTimeUnit().toMillis(getDuration());
      long interval = getIntervalTimeUnit().toMillis(getInterval());

      condition = Optional.ofNullable(condition).orElse(Condition.FALSE_CONDITION);

      try {
        while (!condition.evaluate() && System.currentTimeMillis() < timeout) {
          synchronized (waitTaskMonitor) {
            interval = Math.min(interval, (timeout - System.currentTimeMillis()));
            TimeUnit.MILLISECONDS.timedWait(waitTaskMonitor, interval);
          }
        }
      }
      catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      }

      return (Condition.FALSE_CONDITION.equals(condition) || condition.evaluate());
    }

    /**
     * Runs this {@link WaitTask} with no {@link Condition}.
     *
     * This effectively causes the current {@link Thread} to block for the full duration of the wait.
     *
     * @return a boolean value indicating whether the full wait was realized.
     * @see #on(Condition)
     */
    public boolean run() {
      return on(null);
    }
  }
}
