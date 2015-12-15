/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * 
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * 
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * 
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * 
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang.concurrent;

import java.util.concurrent.TimeUnit;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Condition;
import org.cp.elements.lang.DslExtension;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.DSL;

/**
 * The ThreadUtils class provides utilities for writing concurrent programs using Java Threads
 * and the java.util.concurrent API.
 * 
 * @author John J. Blum
 * @see java.lang.Runnable
 * @see java.lang.Thread
 * @see java.lang.ThreadGroup
 * @see java.lang.ThreadLocal
 * @see java.util.concurrent
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
  public static boolean isAlive(final Thread thread) {
    return (thread != null && thread.isAlive());
  }

  /**
   * Determines whether the specified Thread is in a blocked state.  A Thread may be currently blocked waiting on a lock
   * or whlie performing some IO operation.
   *
   * @param thread the Thread who's state is evaluated.
   * @return a boolean valued indicating whether he specified Thread is blocked.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#BLOCKED
   */
  @NullSafe
  public static boolean isBlocked(final Thread thread) {
    return (thread != null && Thread.State.BLOCKED.equals(thread.getState()));
  }

  /**
   * Determines whether the specified Thread is a daemon Thread.  A daemon Thread is a background Thread that does not
   * prevent the JVM from exiting.
   *
   * @param thread the Thread to evaluate.
   * @return a boolean value indicating whether the specified Thread is a daemon.
   * @see java.lang.Thread#isDaemon()
   * @see #isNonDaemon(Thread)
   */
  @NullSafe
  public static boolean isDaemon(final Thread thread) {
    return (thread != null && thread.isDaemon());
  }

  /**
   * Determines whether the specified Thread is a non-daemon Thread.  A non-daemon Thread is a background Thread
   * that prevents the JVM from exiting.
   *
   * @param thread the Thread to evaluate.
   * @return a boolean value indicating whether the specified Thread is a non-daemon.
   * @see java.lang.Thread#isDaemon()
   * @see #isDaemon(Thread)
   */
  @NullSafe
  public static boolean isNonDaemon(final Thread thread) {
    return  (thread != null && !thread.isDaemon());
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
  public static boolean isNew(final Thread thread) {
    return (thread != null && Thread.State.NEW.equals(thread.getState()));
  }

  /**
   * Determines whether the specified Thread is a runnable Thread.  A "runnable" Thread is any Thread that can be
   * scheduled by the Operation System (OS) for execution.
   *
   * @param thread the Thread to evaluate.
   * @return a boolean value indicating whether the Thread is in a runnable state.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#RUNNABLE
   */
  @NullSafe
  public static boolean isRunnable(final Thread thread) {
    return (thread != null && Thread.State.RUNNABLE.equals(thread.getState()));
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
  public static boolean isTerminated(final Thread thread) {
    return (thread != null && Thread.State.TERMINATED.equals(thread.getState()));
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
  public static boolean isTimedWaiting(final Thread thread) {
    return (thread != null && Thread.State.TIMED_WAITING.equals(thread.getState()));
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
  public static boolean isWaiting(final Thread thread) {
    return (thread != null && Thread.State.WAITING.equals(thread.getState()));
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
  public static ClassLoader getContextClassLoader(final Thread thread) {
    return (thread != null ? thread.getContextClassLoader() : ThreadUtils.class.getClassLoader());
  }

  /**
   * A null-safe method for getting the Thread's ID.
   *
   * @param thread the Thread from which the ID is returned.
   * @return the identifier of the specified Thread, or 0 if the Thread is null.
   * @see java.lang.Thread#getId()
   */
  @NullSafe
  public static long getId(final Thread thread) {
    return (thread != null ? thread.getId() : 0l);
  }

  /**
   * A null-safe method for getting the Thread's name.
   * 
   * @param thread the Thread from which the name is returned.
   * @return a String indicating the name of the specified Thread or null if the Thread is null.
   * @see java.lang.Thread#getName()
   */
  @NullSafe
  public static String getName(final Thread thread) {
    return (thread != null ? thread.getName() : null);
  }

  /**
   * A null-safe method for getting the Thread's priority.
   *
   * @param thread the Thread from which the priority is returned.
   * @return an integer value indicating the priority of the specified Thread or 0 if the Thread is null.
   * @see java.lang.Thread#getPriority()
   */
  @NullSafe
  public static int getPriority(final Thread thread) {
    return (thread != null ? thread.getPriority() : 0);
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
  public static StackTraceElement[] getStackTrace(final Thread thread) {
    return (thread != null ? thread.getStackTrace() : new StackTraceElement[0]);
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
  public static Thread.State getState(final Thread thread) {
    return (thread != null ? thread.getState() : null);
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
  public static ThreadGroup getThreadGroup(final Thread thread) {
    return (thread != null ? thread.getThreadGroup() : null);
  }

  /**
   * Null-safe operation to dump the (call) stack of the current Thread.
   *
   * @param tag a String labeling the stack dump of the current Thread.
   * @see java.lang.Thread#dumpStack()
   */
  @NullSafe
  public static void dumpStack(final String tag) {
    Thread currentThread = Thread.currentThread();
    System.err.printf("%1$s - %2$s Thread @ %3$d%n", String.valueOf(tag).toUpperCase(), currentThread.getName(),
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
  public static void interrupt(final Thread thread) {
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
  public static boolean join(final Thread thread, final long milliseconds, final int nanoseconds) {
    try {
      if (thread != null) {
        thread.join(milliseconds, nanoseconds);
        return true;
      }
    }
    catch (InterruptedException e) {
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
  public static boolean sleep(final long milliseconds, final int nanoseconds) {
    try {
      Thread.sleep(milliseconds, nanoseconds);
      return true;
    }
    catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      return false;
    }
  }

  /**
   * Waits for a specified duration on a Condition possibly checking every specified interval on whether the Condition
   * has been satisfied.
   *
   * @param duration a long value indicating the duration of time to wait for the Condition to be satisfied
   * (default time unit is MILLISECONDS).
   * @return a boolean value indicating whether the Condition has been satisfied within the given duration.
   * @see org.cp.elements.lang.concurrent.ThreadUtils.WaitTask
   */
  @DSL
  public static WaitTask waitFor(final long duration) {
    return waitFor(duration, WaitTask.DEFAULT_TIME_UNIT);
  }

  /**
   * Waits for a specified duration on a Condition possibly checking every specified interval on whether the Condition
   * has been satisfied.
   *
   * @param duration a long value indicating the duration of time to wait for the Condition to be satisfied.
   * @param timeUnit the TimeUnit of the duration time value.
   * @return a boolean value indicating whether the Condition has been satisfied within the given duration.
   * @see org.cp.elements.lang.concurrent.ThreadUtils.WaitTask
   * @see java.util.concurrent.TimeUnit
   */
  @DSL
  public static WaitTask waitFor(final long duration, final TimeUnit timeUnit) {
    return new WaitTask().waitFor(duration, timeUnit);
  }

  /**
   * The WaitTask class is a DslExtension specifying an API for setting up a wait condition.
   *
   * @see org.cp.elements.lang.DslExtension
   */
  public static class WaitTask implements DslExtension {

    protected static final TimeUnit DEFAULT_TIME_UNIT = TimeUnit.MILLISECONDS;

    private long duration;
    private long interval;

    private final Object waitTaskMonitor = new Object();

    private TimeUnit durationTimeUnit;
    private TimeUnit intervalTimeUnit;

    /* (non-Javadoc) */
    public long getDuration() {
      return duration;
    }

    /* (non-Javadoc) */
    public TimeUnit getDurationTimeUnit() {
      return durationTimeUnit;
    }

    /* (non-Javadoc) */
    public long getInterval() {
      long duration = getDuration();
      return (interval > 0 ? Math.min(interval, duration) : duration);
    }

    /* (non-Javadoc) */
    public TimeUnit getIntervalTimeUnit() {
      return ObjectUtils.defaultIfNull(intervalTimeUnit, getDurationTimeUnit());
    }

    /* (non-Javadoc) */
    public WaitTask waitFor(final long duration) {
      return waitFor(duration, DEFAULT_TIME_UNIT);
    }

    /* (non-Javadoc) */
    public WaitTask waitFor(final long duration, final TimeUnit durationTimeUnit) {
      Assert.argument(duration > 0, String.format("duration (%1$d) must be greater than 0", duration));
      this.duration = duration;
      this.durationTimeUnit = ObjectUtils.defaultIfNull(durationTimeUnit, DEFAULT_TIME_UNIT);
      return this;
    }

    /* (non-Javadoc) */
    private boolean isValidInterval(final long interval, final TimeUnit intervalTimeUnit) {
      return (interval > 0 && intervalTimeUnit.toMillis(interval) <= durationTimeUnit.toMillis(duration));
    }

    /* (non-Javadoc) */
    public WaitTask checkEvery(final long interval) {
      return checkEvery(interval, DEFAULT_TIME_UNIT);
    }

    /* (non-Javadoc) */
    public WaitTask checkEvery(final long interval, final TimeUnit intervalTimeUnit) {
      this.intervalTimeUnit = ObjectUtils.defaultIfNull(intervalTimeUnit, DEFAULT_TIME_UNIT);

      Assert.argument(isValidInterval(interval, this.intervalTimeUnit), String.format(
        "interval (%1$d %2$s) must be greater than 0 and less than equal to duration (%3$d %4$s)",
          interval, intervalTimeUnit, duration, durationTimeUnit));

      this.interval = interval;
      this.intervalTimeUnit = intervalTimeUnit;

      return this;
    }

    /* (non-Javadoc) */
    public boolean on(Condition condition) {
      final long timeout = (System.currentTimeMillis() + getDurationTimeUnit().toMillis(getDuration()));

      long interval = getIntervalTimeUnit().toMillis(getInterval());

      condition = ObjectUtils.defaultIfNull(condition, Condition.FALSE_CONDITION);

      while (!condition.evaluate() && System.currentTimeMillis() < timeout) {
        try {
          synchronized (waitTaskMonitor) {
            TimeUnit.MILLISECONDS.timedWait(waitTaskMonitor, interval);
          }
        }
        catch (InterruptedException e) {
          Thread.currentThread().interrupt();
          break;
        }
        finally {
          interval = Math.min(interval, (timeout - System.currentTimeMillis()));
        }
      }

      return (Condition.FALSE_CONDITION.equals(condition) || condition.evaluate());
    }

    /* (non-Javadoc) */
    public boolean run() {
      return on(null);
    }
  }

}
