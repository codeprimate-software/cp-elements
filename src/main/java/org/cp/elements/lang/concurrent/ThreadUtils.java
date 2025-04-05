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

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Condition;
import org.cp.elements.lang.DslExtension;
import org.cp.elements.lang.FluentApiExtension;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract utility class used to write concurrent programs in Java with the {@link Thread}
 * and {@link java.util.concurrent} APIs.
 *
 * @author John J. Blum
 * @see java.lang.Runnable
 * @see java.lang.Thread
 * @see java.lang.ThreadGroup
 * @see java.lang.ThreadLocal
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ThreadUtils {

  /**
   * Determines whether the given {@link Thread} is {@literal alive}.
   * <p>
   * A {@link Thread} is {@literal alive} if it has been started and has not yet died.
   *
   * @param thread {@link Thread} to evaluate.
   * @return a boolean value indicating whether the given {@link Thread} is {@literal alive}.
   * @see java.lang.Thread#isAlive()
   */
  @NullSafe
  public static boolean isAlive(@Nullable Thread thread) {
    return thread != null && thread.isAlive();
  }

  /**
   * Determines whether the given {@link Thread} is in a blocked state.
   * <p>
   * A {@link Thread} may be currently blocked waiting on a lock or performing some IO operation.
   *
   * @param thread {@link Thread} to evaluate.
   * @return a boolean valued indicating whether the given {@link Thread} is {@literal blocked}.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#BLOCKED
   */
  @NullSafe
  public static boolean isBlocked(@Nullable Thread thread) {
    return thread != null && Thread.State.BLOCKED.equals(thread.getState());
  }

  /**
   * Determines whether the given {@link Thread} is a {@link Thread#isDaemon() daemon} {@link Thread}.
   * <p>
   * A {@link Thread#isDaemon() daemon} {@link Thread} is a background {@link Thread} that does not prevent
   * the JVM from exiting.
   *
   * @param thread {@link Thread} to evaluate.
   * @return a boolean value indicating whether the given {@link Thread}
   * is a {@link Thread#isDaemon() daemon} {@link Thread}.
   * @see java.lang.Thread#isDaemon()
   * @see #isNonDaemon(Thread)
   */
  @NullSafe
  public static boolean isDaemon(@Nullable Thread thread) {
    return thread != null && thread.isDaemon();
  }

  /**
   * Determines whether the given {@link Thread} is a {@link Thread#isDaemon() non-daemon},
   * {@literal user} {@link Thread}.
   * <p>
   * A {@link Thread#isDaemon() non-daemon} {@link Thread} is a background {@link Thread} that will prevent
   * the JVM from exiting.
   *
   * @param thread {@link Thread} to evaluate.
   * @return a boolean value indicating whether the given {@link Thread}
   * is a {@link Thread#isDaemon() non-daemon}, {@literal user} {@link Thread}.
   * @see java.lang.Thread#isDaemon()
   * @see #isDaemon(Thread)
   */
  @NullSafe
  public static boolean isNonDaemon(@Nullable Thread thread) {
    return thread != null && !thread.isDaemon();
  }

  /**
   * Alias for {@link #isNonDaemon(Thread)}.
   *
   * @param thread {@link Thread} to evaluate.
   * @return a boolean value indicating whether the given {@link Thread}
   * is a {@link Thread#isDaemon() non-daemon}, {@literal user} {@link Thread}.
   * @see java.lang.Thread#isDaemon()
   * @see #isNonDaemon(Thread)
   */
  public static boolean isUserThread(@Nullable Thread thread) {
    return isNonDaemon(thread);
  }

  /**
   * Determines whether the given {@link Thread} has been interrupted.
   * <p>
   * The interrupted status of the {@link Thread} is unaffected by this method.
   *
   * @param thread {@link Thread} to evaluate.
   * @return a boolean value indicating whether the given {@link Thread} was interrupted.
   * @see java.lang.Thread#isInterrupted()
   */
  @NullSafe
  public static boolean isInterrupted(@Nullable Thread thread) {
    return thread != null && thread.isInterrupted();
  }

  /**
   * Determines whether the given {@link Thread} is {@literal new}.
   * <p>
   * A {@literal new} {@link Thread} is any {@link Thread} that has not been started yet.
   *
   * @param thread {@link Thread} to evaluate.
   * @return a boolean value indicating whether the {@link Thread} is {@literal new}.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#NEW
   * @see #getState(Thread)
   */
  @NullSafe
  public static boolean isNew(@Nullable Thread thread) {
    return Thread.State.NEW.equals(getState(thread));
  }

  /**
   * Determines whether the given {@link Thread} is {@literal runnable}.
   * <p>
   * A {@literal runnable} {@link Thread} is any {@link Thread} that can be scheduled by the Operating System (OS)
   * for execution.
   *
   * @param thread {@link Thread} to evaluate.
   * @return a boolean value indicating whether the {@link Thread} is {@literal runnable}.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#RUNNABLE
   * @see #getState(Thread)
   */
  @NullSafe
  public static boolean isRunnable(@Nullable Thread thread) {
    return Thread.State.RUNNABLE.equals(getState(thread));
  }

  /**
   * Determines whether the given {@link Thread} has been {@literal terminated} (stopped).
   *
   * @param thread {@link Thread} to evaluate.
   * @return a boolean value indicating whether the {@link Thread} has been {@literal terminated}.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#TERMINATED
   * @see #getState(Thread)
   */
  @NullSafe
  public static boolean isTerminated(@Nullable Thread thread) {
    return Thread.State.TERMINATED.equals(getState(thread));
  }

  /**
   * Determines whether the given {@link Thread} is currently in a {@literal timed wait}.
   *
   * @param thread {@link Thread} to evaluate.
   * @return a boolean value indicating whether the {@link Thread} is currently in a {@literal timed wait}.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#TIMED_WAITING
   * @see #getState(Thread)
   */
  @NullSafe
  public static boolean isTimedWaiting(@Nullable Thread thread) {
    return Thread.State.TIMED_WAITING.equals(getState(thread));
  }

  /**
   * Determines whether the given {@link Thread} is currently {@literal waiting}.
   *
   * @param thread {@link Thread} to evaluate.
   * @return a boolean value indicating whether the {@link Thread} is currently {@literal waiting}.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#WAITING
   * @see #getState(Thread)
   */
  @NullSafe
  public static boolean isWaiting(@Nullable Thread thread) {
    return Thread.State.WAITING.equals(getState(thread));
  }

  /**
   * Null-safe method used to get the {@link Thread Thread's} context {@link ClassLoader}.
   *
   * @param thread {@link Thread} from which the {@literal context} {@link ClassLoader} is returned.
   * @return the {@literal context} {@link ClassLoader} from the given {@link Thread}
   * or the {@link ThreadUtils} {@link Class} {@link ClassLoader} if the given {@link Thread} is {@literal null}.
   * @see java.lang.Thread#getContextClassLoader()
   * @see java.lang.ClassLoader
   */
  @NullSafe
  public static @NotNull ClassLoader getContextClassLoader(@Nullable Thread thread) {

    return thread != null
      ? thread.getContextClassLoader()
      : ThreadUtils.class.getClassLoader();
  }

  /**
   * Null-safe method used to get the {@link Thread Thread's} {@link String name}.
   *
   * @param thread {@link Thread} from which the {@link String name} is returned.
   * @return a {@link String} containing the {@literal name} of the given {@link Thread}
   * or return {@literal null} if the given {@link Thread} is {@literal null}.
   * @see java.lang.Thread#getName()
   */
  @NullSafe
  public static @Nullable String getName(@Nullable Thread thread) {
    return thread != null ? thread.getName() : null;
  }

  /**
   * Null-safe method used to get the {@link Thread Thread's} {@link Integer priority}.
   *
   * @param thread {@link Thread} from which the {@link Integer priority} is returned.
   * @return an {@link Integer} value indicating the {@literal priority} of the given {@link Thread}
   * or {@literal 0} if the given {@link Thread} is {@literal null}.
   * @see java.lang.Thread#getPriority()
   * @see java.lang.Thread#NORM_PRIORITY
   */
  @NullSafe
  public static int getPriority(@Nullable Thread thread) {
    return thread != null ? thread.getPriority() : Thread.NORM_PRIORITY;
  }

  /**
   * Null-safe method used to get a snapshot of the given {@link Thread Thread's} current stack trace.
   *
   * @param thread {@link Thread} from which the stack trace is returned.
   * @return an array of {@link StackTraceElement StackTraceElements} indicating the current stack trace
   * of the given {@link Thread}, or an empty {@link StackTraceElement} array if the given {@link Thread}
   * is {@literal null}.
   * @see java.lang.Thread#getStackTrace()
   * @see java.lang.StackTraceElement
   */
  @NullSafe
  public static @NotNull StackTraceElement[] getStackTrace(@Nullable Thread thread) {
    return thread != null ? thread.getStackTrace() : new StackTraceElement[0];
  }

  /**
   * Null-safe method used to get the given {@link Thread Thread's} current {@link Thread.State}.
   *
   * @param thread {@link Thread} from which the {@link Thread.State} is returned.
   * @return the {@link Thread.State} from the given {@link Thread}
   * or {@literal null} if the {@link Thread} is {@literal null}.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State
   */
  @NullSafe
  public static @Nullable Thread.State getState(@Nullable Thread thread) {
    return thread != null ? thread.getState() : null;
  }

  /**
   * Null-safe method used to get the given {@link Thread Thread's} {@link ThreadGroup}.
   *
   * @param thread {@link Thread} from which the {@link ThreadGroup} is returned.
   * @return the {@link ThreadGroup} from the given {@link Thread}
   * or {@literal null} if the given {@link Thread} is {@literal null}.
   * @see java.lang.Thread#getThreadGroup()
   * @see java.lang.ThreadGroup
   */
  @NullSafe
  public static @Nullable ThreadGroup getThreadGroup(@Nullable Thread thread) {
    return thread != null ? thread.getThreadGroup() : null;
  }

  /**
   * Null-safe method used to dump the (call) stack of the current {@link Thread}.
   * <p>
   * The stack is dumped to {@link System#err standard error}.
   *
   * @param tag {@link String} label used to prefix the stack dump of the current {@link Thread}.
   * A tag (label) is useful for tracing in log files.
   * @see java.lang.Thread#dumpStack()
   */
  @NullSafe
  public static void dumpStack(@Nullable String tag) {

    Thread currentThread = Thread.currentThread();

    System.err.printf("[%1$s] %2$s Thread%n", String.valueOf(tag).toUpperCase(), currentThread.getName());

    Thread.dumpStack();
  }

  /**
   * Null-safe method used to interrupt the given {@link Thread}.
   *
   * @param thread {@link Thread} to interrupt.
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
   * Runs the given, required {@link Runnable operation} atomically if the given {@link Object lock}
   * is not {@literal null}, otherwise runs the {@link Runnable operation} normally.
   *
   * @param lock {@link Object} used as the lock.
   * @param operation {@link Runnable} containing the operation (logic) to run;
   * must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link Runnable operation} reference is {@literal null}.
   * @see java.lang.Runnable
   */
  public static void runAtomically(@Nullable Object lock, @NotNull Runnable operation) {

    Assert.notNull(operation, "Operation to run is required");

    runAtomically(lock, () -> {
      operation.run();
      return null;
    });
  }

  /**
   * Runs the given, required {@link Supplier operation} atomically if the given {@link Object lock}
   * is not {@literal null}, otherwise runs the {@link Supplier operation} normally.
   *
   * @param <T> {@link Class type} of the {@link Supplier operation's} return value.
   * @param lock {@link Object} used as the lock.
   * @param operation {@link Supplier} containing the operation (logic) to run;
   * must not be {@literal null}.
   * @return the computational result of the {@link Supplier operation}.
   * @throws IllegalArgumentException if the {@link Supplier operation} reference is {@literal null}.
   * @see java.util.function.Supplier
   */
  @SuppressWarnings("all")
  public static <T> T runAtomically(@Nullable Object lock, @NotNull Supplier<T> operation) {

    Assert.notNull(operation, "Operation to run is required");

    if (lock != null) {
      synchronized (lock) {
        return operation.get();
      }
    }

    return operation.get();
  }

  /**
   * Causes the {@link Thread#currentThread() current Thread} to sleep for the given number
   * of {@link Integer#TYPE milliseconds} and {@link Integer#TYPE nanoseconds}.
   * <p>
   * If the {@link Thread current Thread} is interrupted, the sleep is aborted, however, the interrupt bit
   * is reset and this method returns {@literal false}.
   *
   * @param milliseconds number of milliseconds that the {@link Thread#currentThread() current Thread} will sleep.
   * If the number of milliseconds is {@literal 0}, then the {@link Thread#currentThread() current Thread} will sleep
   * until interrupted.
   * @param nanoseconds number of nanoseconds in addition to the milliseconds that
   * the {@link Thread#currentThread() current Thread} will sleep.
   * @return a boolean value indicating whether the sleep operation was successful without interruption.
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
   * Waits for a given {@link Long duration} on a {@link Condition} checking every configured interval whether
   * the {@link Condition} was satisfied.
   *
   * @param duration {@link Long} value indicating the duration of time to wait for the {@link Condition}
   * to be satisfied (default {@link TimeUnit} is {@literal TimeUnit#MILLISECONDS}).
   * @return a boolean value indicating whether the {@link Condition} was satisfied
   * within the given {@link Long duration}.
   * @see org.cp.elements.lang.concurrent.ThreadUtils.WaitTask
   * @see org.cp.elements.lang.annotation.FluentApi
   * @see org.cp.elements.lang.annotation.Dsl
   */
  @Dsl
  public static @NotNull WaitTask waitFor(long duration) {
    return WaitTask.newWaitTask().waitFor(duration);
  }

  /**
   * Waits for a given {@link Long duration} in the given {@link TimeUnit} on a {@link Condition} checking every
   * configured interval whether the {@link Condition} was satisfied.
   *
   * @param duration {@link Long} value indicating the duration of time to wait for the {@link Condition}
   * to be satisfied (default {@link TimeUnit} is {@literal TimeUnit#MILLISECONDS}).
   * @param timeUnit {@link TimeUnit} for the duration of time.
   * @return a boolean value indicating whether the {@link Condition} was satisfied
   * within the given {@link Long duration}.
   * @see org.cp.elements.lang.concurrent.ThreadUtils.WaitTask
   * @see org.cp.elements.lang.annotation.FluentApi
   * @see org.cp.elements.lang.annotation.Dsl
   * @see java.util.concurrent.TimeUnit
   */
  @Dsl
  public static @NotNull WaitTask waitFor(long duration, TimeUnit timeUnit) {
    return WaitTask.newWaitTask().waitFor(duration, timeUnit);
  }

  /**
   * The {@link WaitTask} class is a {@link DslExtension} and {@link FluentApiExtension} specifying an API
   * for setting up a wait condition.
   *
   * @see org.cp.elements.lang.DslExtension
   * @see org.cp.elements.lang.FluentApiExtension
   */
  @FluentApi
  public static class WaitTask implements DslExtension, FluentApiExtension {

    protected static final TimeUnit DEFAULT_TIME_UNIT = TimeUnit.MILLISECONDS;

    private final AtomicLong duration = new AtomicLong(0L);
    private final AtomicLong interval = new AtomicLong(0L);

    private final Object waitTaskMonitor = new Object();

    private TimeUnit durationTimeUnit;
    private TimeUnit intervalTimeUnit;

    /**
     * Factory method used to construct a new, default instance of {@link WaitTask}.
     *
     * @return a new {@link WaitTask}.
     */
    public static @NotNull WaitTask newWaitTask() {
      return new WaitTask();
    }

    /**
     * Returns the duration of the wait.
     *
     * @return a long value representing the length in time to wait.
     */
    public long getDuration() {
      return this.duration.get();
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

      return this.interval.get() > 0
        ? Math.min(this.interval.get(), duration)
        : duration;
    }

    /**
     * Returns the unit of time for the interval.
     *
     * @return a {@link TimeUnit} indicating the unit of time used for the interval.
     * @see java.util.concurrent.TimeUnit
     */
    public TimeUnit getIntervalTimeUnit() {
      return ObjectUtils.returnFirstNonNullValue(this.intervalTimeUnit, getDurationTimeUnit());
    }

    /**
     * Causes the current {@link Thread} to wait for the specified duration, or until the condition is met.
     *
     * @param duration long value indicating the length of time to wait.
     * @return this {@link WaitTask} set to the specified duration.
     * @see #waitFor(long, TimeUnit)
     */
    public @NotNull WaitTask waitFor(long duration) {
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
    public @NotNull WaitTask waitFor(long duration, TimeUnit durationTimeUnit) {

      Assert.argument(duration, argument -> argument > 0, "Duration [%d] must be greater than 0", duration);

      this.duration.set(duration);
      this.durationTimeUnit = ObjectUtils.returnFirstNonNullValue(durationTimeUnit, DEFAULT_TIME_UNIT);

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
    public @NotNull WaitTask checkEvery(long interval) {
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
    public @NotNull WaitTask checkEvery(long interval, TimeUnit intervalTimeUnit) {

      TimeUnit resolvedIntervalTimeUnit = intervalTimeUnit != null
        ? intervalTimeUnit
        : DEFAULT_TIME_UNIT;

      Assert.argument(interval, argument -> isValidInterval(argument, resolvedIntervalTimeUnit),
        "Interval [%1$d %2$s] must be greater than 0 and less than equal to duration [%3$d %4$s]",
          interval, intervalTimeUnit, getDuration(), getDurationTimeUnit());

      this.interval.set(interval);
      this.intervalTimeUnit = resolvedIntervalTimeUnit;

      return this;
    }

    /**
     * Evaluates the given {@link Condition}, waiting up to at most the specified duration.
     * <p>
     * If the provided {@link Condition} is {@literal null}, the {@link Condition#FALSE_CONDITION} is used.
     *
     * @param condition {@link Condition} to evaluate.
     * @return a boolean value indicating whether the {@link Condition} was satisfied.
     * @see org.cp.elements.lang.Condition
     */
    public boolean on(@Nullable Condition condition) {

      long timeout = System.currentTimeMillis() + getDurationTimeUnit().toMillis(getDuration());
      long interval = getIntervalTimeUnit().toMillis(getInterval());

      Condition resolvedCondition = condition != null ? condition : Condition.FALSE_CONDITION;

      try {
        while (!resolvedCondition.evaluate() && System.currentTimeMillis() < timeout) {
          synchronized (this.waitTaskMonitor) {
            interval = Math.min(interval, (timeout - System.currentTimeMillis()));
            TimeUnit.MILLISECONDS.timedWait(this.waitTaskMonitor, interval);
          }
        }
      }
      catch (InterruptedException cause) {
        Thread.currentThread().interrupt();
      }

      return Condition.FALSE_CONDITION.equals(resolvedCondition) || resolvedCondition.evaluate();
    }

    /**
     * Runs this {@link WaitTask} with no {@link Condition}.
     * <p>
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
