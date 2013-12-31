/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang.concurrent;

/**
 * The ThreadUtils class provides utilities for writing concurrent programs using Java Threads and the 
 * java.util.concurrent facilities.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Runnable
 * @see java.lang.Thread
 * @see java.lang.ThreadGroup
 * @see java.util.concurrent
 * @since 1.0.0
 */
public abstract class ThreadUtils {

  /**
   * A null-safe method of getting the Thread's name.
   * <p/>
   * @param thread the Thread from which to get the name.
   * @return a String indicating the name of the specified Thread, or null if the Thread object reference is null.
   * @see java.lang.Thread#getName()
   */
  public static String getName(final Thread thread) {
    return (thread == null ? null : thread.getName());
  }

  /**
   * Causes the current Thread to join with the specified Thread.  If the current Thread is interrupted while waiting
   * for the specified Thread, then the current Threads interrupt bit will be set and this method will return false.
   * Otherwise, the current Thread will wait on the specified Thread until it completes and the method will return
   * true.
   * <p/>
   * @param t the Thread on which the current Thread joins.
   * @param milliseconds the number of milliseconds for the current Thread to wait during the join on the specified
   * Thread. If the number of milliseconds specified is 0, then the current Thread will wait until the specified Thread
   * finishes, or the current Thread is interrupted.
   * @param nanoseconds the number of nanoseconds in addition to the milliseconds for the current Thread to wait during
   * the join on the specified Thread to finish.
   * @return a boolean condition indicating if the current Thread was successful in joining the specified Thread
   * without being interrupted.
   * @see java.lang.Thread#interrupt()
   * @see java.lang.Thread#join(long, int)
   */
  public static boolean join(final Thread t, final long milliseconds, final int nanoseconds) {
    try {
      t.join(milliseconds, nanoseconds);
      return true;
    }
    catch (InterruptedException ignore) {
      Thread.currentThread().interrupt();
      return false;
    }
  }

  /**
   * Causes the current Thread to pause for the specified number of milliseconds.  If the current Thread is interrupted,
   * the pause is aborted but the interrupt bit is set on the Thread.
   * <p/>
   * @param milliseconds the number of milliseconds to cause the current Thread to wait, or pause.  If the number
   * of milliseconds is 0, then the current Thread will pause until interrupted.
   * @param nanoseconds the number of nanoseconds in addition to the milliseconds to cause the current Thread to wait,
   * or pause.
   * @return a boolean value if the pause operation was successful.
   * @see java.lang.Thread#interrupt()
   * @see java.lang.Thread#sleep(long)
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
