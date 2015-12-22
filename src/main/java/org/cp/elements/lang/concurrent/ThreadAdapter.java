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

import static org.cp.elements.lang.LangExtensions.assertThat;

/**
 * The ThreadAdapter class is an "Adapter" (a.k.a. wrapper) around a Thread object, providing additional, convenient
 * operations on the Thread class.
 *
 * @author John J. Blum
 * @see java.lang.Thread
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ThreadAdapter {

  private final Thread delegate;

  /**
   * Constructs an instance of ThreadAdapter initialized with the current Thread as the delegate for all
   * Thread-based operations on this class.
   *
   * @see java.lang.Thread#currentThread()
   * @see #ThreadAdapter(Thread)
   */
  public ThreadAdapter() {
    this(Thread.currentThread());
  }

  /**
   * Constructs an instance of ThreadAdapter initialized with the given Thread used as the delegate for all
   * Thread-based operations on this class.
   *
   * @param delegate the Thread wrapped by this ThreadAdapter.
   * @see java.lang.Thread
   */
  public ThreadAdapter(final Thread delegate) {
    assertThat(delegate).isNotNull();
    this.delegate = delegate;
  }

  /**
   * Gets the Thread object wrapped by this ThreadAdapter.
   *
   * @return the Thread wrapped by this TheadAdapter and serving as the delegate for all Thread-based operations
   * on this class.
   * @see java.lang.Thread
   */
  protected Thread getDelegate() {
    return delegate;
  }

  /**
   * Determines whether this Thread is alive.  A Thread is alive if it has been started and has not yet died.
   *
   * @return a boolean value indicating whether this Thread is alive.
   * @see java.lang.Thread#isAlive()
   */
  public boolean isAlive() {
    return getDelegate().isAlive();
  }

  /**
   * Determines whether this Thread is in a blocked state.  A Thread may be currently blocked waiting on a lock
   * or performing some IO operation.
   *
   * @return a boolean valued indicating whether this Thread is blocked.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#BLOCKED
   */
  public boolean isBlocked() {
    return Thread.State.BLOCKED.equals(getState());
  }

  /**
   * Determines whether this Thread is a daemon Thread.  A daemon Thread is a background Thread that does not
   * prevent the JVM from exiting.
   *
   * @return a boolean value indicating whether the specified Thread is a daemon Thread.
   * @see java.lang.Thread#isDaemon()
   * @see #isNonDaemon()
   */
  public boolean isDaemon() {
    return getDelegate().isDaemon();
  }

  /**
   * Determines whether this Thread is a non-daemon Thread.  A non-daemon Thread is a background Thread
   * that prevents the JVM from exiting.
   *
   * @return a boolean value indicating whether this Thread is a non-daemon Thread.
   * @see java.lang.Thread#isDaemon()
   * @see #isDaemon()
   */
  public boolean isNonDaemon() {
    return !isDaemon();
  }

  public boolean isInterrupted() {
    return getDelegate().isInterrupted();
  }

  /**
   * Determines whether this Thread is a new Thread.  A "new" Thread is any Thread that has not been
   * started yet.
   *
   * @return a boolean value indicating whether this Thread is new.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#NEW
   */
  public boolean isNew() {
    return Thread.State.NEW.equals(getState());
  }

  public boolean isRunnable() {
    return Thread.State.RUNNABLE.equals(getState());
  }

  public boolean isTerminated() {
    return Thread.State.TERMINATED.equals(getState());
  }

  public boolean isTimedWaiting() {
    return Thread.State.TIMED_WAITING.equals(getState());
  }

  public boolean isWaiting() {
    return Thread.State.WAITING.equals(getState());
  }

  public void setContextClassLoader(final ClassLoader contextClassLoader) {
    getDelegate().setContextClassLoader(contextClassLoader);
  }

  public ClassLoader getContextClassLoader() {
    return getDelegate().getContextClassLoader();
  }

  public void setDaemon(final boolean daemon) {
    getDelegate().setDaemon(daemon);
  }

  public long getId() {
    return getDelegate().getId();
  }

  public void setName(final String name) {
    getDelegate().setName(name);
  }

  public String getName() {
    return getDelegate().getName();
  }

  public void setPriority(final int priority) {
    getDelegate().setPriority(priority);
  }

  public int getPriority() {
    return getDelegate().getPriority();
  }

  public StackTraceElement[] getStackTrace() {
    return getDelegate().getStackTrace();
  }

  public Thread.State getState() {
    return getDelegate().getState();
  }

  public ThreadGroup getThreadGroup() {
    return getDelegate().getThreadGroup();
  }

  public void setUncaughtExceptionHandler(final Thread.UncaughtExceptionHandler uncaughtExceptionHandler) {
    getDelegate().setUncaughtExceptionHandler(uncaughtExceptionHandler);
  }

  public Thread.UncaughtExceptionHandler getUncaughtExceptionHandler() {
    return getDelegate().getUncaughtExceptionHandler();
  }

  public void checkAccess() {
    getDelegate().checkAccess();
  }

  public void dumpStack() {
    Thread.dumpStack();
  }

  public void interrupt() {
    getDelegate().interrupt();
  }

  public void join() {
    try {
      getDelegate().join();
    }
    catch (InterruptedException e) {
      throw new org.cp.elements.lang.InterruptedException(e);
    }
  }

  public void join(final long milliseconds) {
    try {
      getDelegate().join(milliseconds);
    }
    catch (InterruptedException e) {
      throw new org.cp.elements.lang.InterruptedException(e);
    }
  }

  public void join(final long milliseconds, final int nanoseconds) {
    try {
      getDelegate().join(milliseconds, nanoseconds);
    }
    catch (InterruptedException e) {
      throw new org.cp.elements.lang.InterruptedException(e);
    }
  }

  public void run() {
    getDelegate().run();
  }

  public void start() {
    getDelegate().start();
  }

  /**
   * Return a String representation of this Thread.
   *
   * @return a String describing the state of this Thread.
   */
  @Override
  public String toString() {
    return String.format("{ @type = %1$s, id = %2$d, name = %3$s, daemon = %4$s, group = %5$s, priority = %6$s, state = %7$s }",
      getClass().getName(), getId(), getName(), isDaemon(), getThreadGroup(), getPriority(), getState());
  }

}
