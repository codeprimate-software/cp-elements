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

import org.cp.elements.lang.NullSafe;

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

  @NullSafe
  public static boolean isAlive(final Thread thread) {
    return (thread != null && thread.isAlive());
  }

  @NullSafe
  public static boolean isBlocked(final Thread thread) {
    return (thread != null && Thread.State.BLOCKED.equals(thread.getState()));
  }

  @NullSafe
  public static boolean isDaemon(final Thread thread) {
    return (thread != null && thread.isDaemon());
  }

  @NullSafe
  public static boolean isNonDaemon(final Thread thread) {
    return  (thread != null && !thread.isDaemon());
  }

  @NullSafe
  public static boolean isNew(final Thread thread) {
    return (thread != null && Thread.State.NEW.equals(thread.getState()));
  }

  @NullSafe
  public static boolean isRunnable(final Thread thread) {
    return (thread != null && Thread.State.RUNNABLE.equals(thread.getState()));
  }

  @NullSafe
  public static boolean isTerminated(final Thread thread) {
    return (thread != null && Thread.State.TERMINATED.equals(thread.getState()));
  }

  @NullSafe
  public static boolean isTimedWaiting(final Thread thread) {
    return (thread != null && Thread.State.TIMED_WAITING.equals(thread.getState()));
  }

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
  public static boolean join(final Thread thread, final long milliseconds, final int nanoseconds) {
    try {
      thread.join(milliseconds, nanoseconds);
      return true;
    }
    catch (InterruptedException ignore) {
      Thread.currentThread().interrupt();
      return false;
    }
  }

  /**
   * Causes the current Thread to pause for the specified number of milliseconds and nanoseconds.  If the current Thread
   * is interrupted, the pause is aborted, however, the interrupt bit is reset and this method returns false.
   * 
   * @param milliseconds the number of milliseconds to cause the current Thread to pause (sleep).  If the number
   * of milliseconds is 0, then the current Thread will pause (sleep) until interrupted.
   * @param nanoseconds the number of nanoseconds in addition to the milliseconds to cause the current Thread to pause
   * (sleep).
   * @return a boolean value if the pause operation was successful without interruption.
   * @see java.lang.Thread#sleep(long)
   * @see java.lang.Thread#interrupt()
   */
  public static boolean pause(final long milliseconds, final int nanoseconds) {
    try {
      Thread.sleep(milliseconds, nanoseconds);
      return true;
    }
    catch (InterruptedException ignore) {
      Thread.currentThread().interrupt();
      return false;
    }
  }

}
