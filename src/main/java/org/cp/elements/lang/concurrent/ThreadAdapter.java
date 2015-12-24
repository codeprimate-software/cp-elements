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
   * Constructs an instance of ThreadWrapper by creating a new Thread initialized with the given Runnable.
   *
   * @param target a Runnable object used to create and initialize a new Thread.
   * @see java.lang.Runnable
   * @see java.lang.Thread#Thread(Runnable)
   * @see #ThreadAdapter(Thread)
   */
  public ThreadAdapter(final Runnable target) {
    this(new Thread(target));
  }

  /**
   * Constructs an instance of ThreadAdapter initialized with the given Thread used as the delegate for all
   * Thread-based operations on this class.
   *
   * @param delegate the Thread wrapped by this ThreadAdapter.
   * @throws NullPointerException if the Thread delegate is null.
   * @see java.lang.Thread
   */
  public ThreadAdapter(final Thread delegate) {
    assertThat(delegate).throwing(new IllegalArgumentException("The delegate Thread must not be null")).isNotNull();
    this.delegate = delegate;
  }

  /**
   * Gets the Thread object wrapped by this ThreadAdapter.
   *
   * @return the Thread wrapped by this ThreadAdapter and serving as the delegate for all Thread-based operations
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

  /**
   * Derermine if the Thread wrapped by this ThreadAdapter has been interrupted.  The interrupted status of this Thread
   * is unaffected by this method.
   *
   * @return a boolean indicating whether the Thread wrapped by this ThreadAdapter has been interrupted.
   * @see java.lang.Thread#isInterrupted()
   */
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

  /**
   * Determines whether this Thread is runnable.  A "runnable" Thread is any Thread that can be scheduled
   * by the Operating System (OS) for execution.
   *
   * @return a boolean value indicating whether this Thread is in a runnable state.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#RUNNABLE
   */
  public boolean isRunnable() {
    return Thread.State.RUNNABLE.equals(getState());
  }

  /**
   * Determines whether the this Thread has been terminated (stopped).
   *
   * @return a boolean value indicating whether this Thread has been terminated.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#TERMINATED
   */
  public boolean isTerminated() {
    return Thread.State.TERMINATED.equals(getState());
  }

  /**
   * Determines whether this Thread is currently in a timed wait.
   *
   * @return a boolean value indicating whether this Thread is currently in a timed wait.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#TIMED_WAITING
   */
  public boolean isTimedWaiting() {
    return Thread.State.TIMED_WAITING.equals(getState());
  }

  /**
   * Determines whether this Thread is currently in a wait.
   *
   * @return a boolean value indicating whether this Thread is currently in a wait.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#WAITING
   */
  public boolean isWaiting() {
    return Thread.State.WAITING.equals(getState());
  }

  /**
   * Sets the {@link ClassLoader} used by code executing in this Thread to resolve and load class types and resources.
   *
   * @param contextClassLoader the ClassLoader used within the execution context of this Thread.
   * @return a reference to this Thread.
   * @see java.lang.ClassLoader
   * @see java.lang.Thread#setContextClassLoader(ClassLoader)
   */
  public ThreadAdapter setContextClassLoader(final ClassLoader contextClassLoader) {
    getDelegate().setContextClassLoader(contextClassLoader);
    return this;
  }

  /**
   * Gets the {@link ClassLoader} used by code executing in this Thread to resolve and load class types and resources.
   *
   * @return the {@link ClassLoader} used by code executing in this Thread to resolve and load class types.
   * @see java.lang.ClassLoader
   * @see java.lang.Thread#getContextClassLoader()
   */
  public ClassLoader getContextClassLoader() {
    return getDelegate().getContextClassLoader();
  }

  /**
   * Sets this Thread as a daemon or a user Thread.
   *
   * @param daemon a boolean indicating whether this Thread is a daemon or a user Thread.
   * @return a reference to this Thread.
   * @see java.lang.Thread#setDaemon(boolean)
   */
  public ThreadAdapter setDaemon(final boolean daemon) {
    getDelegate().setDaemon(daemon);
    return this;
  }

  /**
   * Gets the identifier identifying this Thread.
   *
   * @return a long value identifying this Thread.
   * @see java.lang.Thread#getId()
   */
  public long getId() {
    return getDelegate().getId();
  }

  /**
   * Sets the name of this Thread.
   *
   * @param name a String specifying the name of this Thread.
   * @return a reference to this Thread.
   * @see java.lang.Thread#setName(String)
   */
  public ThreadAdapter setName(final String name) {
    getDelegate().setName(name);
    return this;
  }


  /**
   * Gets the name of this Thread.
   *
   * @return the name of this Thread.
   * @see java.lang.Thread#getName()
   */
  public String getName() {
    return getDelegate().getName();
  }

  /**
   * Sets the priority of this Thread. The priority of this Thread is set to the smaller of the specified new priority
   * and the maximum permitted priority of the Thread's Thread Group.
   *
   * @param priority the priority of this Thread.
   * @return a reference to this Thread.
   * @see java.lang.Thread#setPriority(int)
   */
  public ThreadAdapter setPriority(final int priority) {
    getDelegate().setPriority(priority);
    return this;
  }

  /**
   * Get the priority of this Thread.
   *
   * @return the priority of this Thread.
   * @see java.lang.Thread#getPriority()
   */
  public int getPriority() {
    return getDelegate().getPriority();
  }

  /**
   * Gets the stack trace of this Thread contained in a array of stack trace elements representing each call
   * in the stack.
   *
   * @return an array of StackTrace elements representing the stack dump of this Thread.
   * @see java.lang.StackTraceElement
   * @see java.lang.Thread#getStackTrace()
   */
  public StackTraceElement[] getStackTrace() {
    return getDelegate().getStackTrace();
  }

  /**
   * Gets the state of this Thread.
   *
   * @return a {@link java.lang.Thread.State} enumerated value indicating the state of this Thread.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State
   */
  public Thread.State getState() {
    return getDelegate().getState();
  }

  /**
   * Gets the Thread Group to which this Thread belongs.
   *
   * @return the Thread Group to which this Thread belongs.
   * @see java.lang.Thread#getThreadGroup()
   * @see java.lang.ThreadGroup
   */
  public ThreadGroup getThreadGroup() {
    return getDelegate().getThreadGroup();
  }

  /**
   * Sets the handler used by this Thread when abruptly terminated while executing code that throws
   * an uncaught Exception.
   *
   * @param uncaughtExceptionHandler the {@link Thread.UncaughtExceptionHandler} used to handle any uncaught Exceptions
   * while executing code.
   * @return a reference to this Thread.
   * @see java.lang.Thread#setUncaughtExceptionHandler(Thread.UncaughtExceptionHandler)
   * @see java.lang.Thread.UncaughtExceptionHandler
   */
  public ThreadAdapter setUncaughtExceptionHandler(final Thread.UncaughtExceptionHandler uncaughtExceptionHandler) {
    getDelegate().setUncaughtExceptionHandler(uncaughtExceptionHandler);
    return this;
  }

  /**
   * Gets the handler used by this Thread when abruptly terminated while executing code throwing an uncaught Exception.
   *
   * @return the {@link Thread.UncaughtExceptionHandler} used to handle any uncaught Exceptions while executing code.
   * @see java.lang.Thread#getUncaughtExceptionHandler()
   * @see java.lang.Thread.UncaughtExceptionHandler
   */
  public Thread.UncaughtExceptionHandler getUncaughtExceptionHandler() {
    return getDelegate().getUncaughtExceptionHandler();
  }

  /**
   * Checks the access of the (current) calling Thread to determine whether it has permission to modify this Thread.
   *
   * @see java.lang.Thread#checkAccess()
   */
  public void checkAccess() {
    getDelegate().checkAccess();
  }

  /**
   * Causes this Thread to dump it's current call stack (a.k.a. Stack Trace) to the standard error stream.
   *
   * @see java.lang.Thread#dumpStack()
   */
  public void dumpStack() {
    Thread.dumpStack();
  }

  /**
   * Interrupts this Thread's execution.
   *
   * @see java.lang.Thread#interrupt()
   */
  public void interrupt() {
    getDelegate().interrupt();
  }

  /**
   * Causes the current executing Thread to join and wait for this Thread to terminate.
   *
   * @throws java.lang.InterruptedException if the current Thread is interrupted while waiting
   * for this Thread.
   * @see java.lang.Thread#join()
   */
  public void join() throws InterruptedException {
    getDelegate().join();
  }

  /**
   * Causes the current executing Thread to join and wait for this Thread to terminate or until the specified
   * number of milliseconds have elapsed.
   *
   * @param milliseconds the number of milliseconds to wait for this Thread to terminate.
   * @throws java.lang.InterruptedException if the current Thread is interrupted while waiting
   * for this Thread.
   * @see java.lang.Thread#join(long)
   */
  public void join(final long milliseconds) throws InterruptedException {
    getDelegate().join(milliseconds);
  }

  /**
   * Causes the current executing Thread to join and wait for this Thread to terminate or until the specified
   * number of milliseconds and nanoseconds have elapsed.
   *
   * @param milliseconds the number of milliseconds to wait for this Thread to terminate.
   * @param nanoseconds the number of nanoseconds to wait for this Thread to terminate.
   * @throws java.lang.InterruptedException if the current Thread is interrupted while waiting
   * for this Thread.
   * @see java.lang.Thread#join(long, int)
   */
  public void join(final long milliseconds, final int nanoseconds) throws InterruptedException {
    getDelegate().join(milliseconds, nanoseconds);
  }

  /**
   * If this Thread was constructed using a separate Runnable object, then the Runnable object's run method is called;
   * otherwise, this method does nothing and returns.
   *
   * @see java.lang.Thread#run()
   */
  public void run() {
    getDelegate().run();
  }

  /**
   * Causes this Thread to begin execution; the Java Virtual Machine (JVM) calls this Threads run method.
   *
   * @see java.lang.Thread#start()
   */
  public void start() {
    getDelegate().start();
  }

  /**
   * Return a String representation of this Thread.
   *
   * @return a String describing the state of this Thread.
   * @see java.lang.Thread#toString()
   */
  @Override
  public String toString() {
    return String.format("{ @type = %1$s, id = %2$d, name = %3$s, daemon = %4$s, group = %5$s, priority = %6$s, state = %7$s }",
      getClass().getName(), getId(), getName(), isDaemon(), getThreadGroup(), getPriority(), getState());
  }

}
