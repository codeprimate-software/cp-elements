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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@literal Adapter} (a.k.a. {@literal Wrapper}) around a {@link Thread} object used to provide additional information
 * on the operation of a Java {@link Thread} at runtime.
 *
 * @author John J. Blum
 * @see java.lang.Thread
 * @see <a href="https://en.wikipedia.org/wiki/Adapter_pattern">Adapter Sotware Design Pattern</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ThreadAdapter {

  /**
   * Factory method used to construct a new {@link ThreadAdapter} initialized with
   * the {@link Thread#currentThread() current Thread}.
   *
   * @return a new {@link ThreadAdapter} initialized with the {@link Thread#currentThread()}.
   * @see java.lang.Thread#currentThread()
   * @see java.lang.Thread
   * @see #from(Thread)
   */
  public static @NotNull ThreadAdapter currentThread() {
    return from(Thread.currentThread());
  }

  /**
   * Factory method used to construct a new {@link ThreadAdapter} initialized with the given, required {@link Thread}.
   *
   * @param thread {@link Thread} to adapt/wrap; must not be {@literal null}.
   * @return a new {@link ThreadAdapter} initialized with the given, required {@link Thread}.
   * @throws IllegalArgumentException if the {@link Thread} is {@literal null}.
   * @see #ThreadAdapter(Thread)
   * @see java.lang.Thread
   */
  public static @NotNull ThreadAdapter from(@NotNull Thread thread) {
    return new ThreadAdapter(thread);
  }

  protected static final String THREAD_TO_STRING =
    "{ @type = %1$s, id = %2$d, name = %3$s, daemon = %4$s, group = %5$s, priority = %6$s, state = %7$s }";

  private final Thread delegate;

  /**
   * Constructs a new instance of {@link ThreadAdapter} initialized with
   * the {@link Thread#currentThread() current Thread} as the {@literal delegate}
   * for all {@link Thread}-based operations accessible from {@literal this} adapter.
   *
   * @see java.lang.Thread#currentThread()
   * @see #ThreadAdapter(Thread)
   * @see java.lang.Thread
   */
  public ThreadAdapter() {
    this(Thread.currentThread());
  }

  /**
   * Constructs a new instance of {@link ThreadAdapter} by creating a new {@link Thread} initialized with
   * the given {@link Runnable} object.
   *
   * @param target {@link Runnable} object to execute in a new {@link Thread}.
   * @see java.lang.Thread#Thread(Runnable)
   * @see #ThreadAdapter(Thread)
   * @see java.lang.Runnable
   */
  public ThreadAdapter(@Nullable Runnable target) {
    this(new Thread(target));
  }

  /**
   * Constructs a new {@link ThreadAdapter} initialized with the given, required {@link Thread}.
   * <p>
   * The given {@link Thread} will be used as the {@literal delegate} for all {@link Thread}-based operations
   * accessible from {@literal this} adapter.
   *
   * @param delegate {@link Thread} to adapt (wrap).
   * @throws IllegalArgumentException if the {@link Thread} used as the {@literal delegate}
   * for {@literal this} {@link ThreadAdapter} is {@literal null}.
   * @see java.lang.Thread
   */
  public ThreadAdapter(@NotNull Thread delegate) {

    Assert.notNull(delegate, "Delegate Thread is required");

    this.delegate = delegate;
  }

  /**
   * Gets the configured {@literal Thread} adapted ({@literal wrapped}) by {@literal this} {@link ThreadAdapter}.
   * <p>
   * All operations accessible from {@literal this} Adapter are delegated to the underlying, configured {@link Thread}.
   *
   * @return the {@literal Thread} adapted ({@literal wrapped}) by {@literal this} {@link ThreadAdapter};
   * never {@literal null}.
   * @see java.lang.Thread
   */
  protected @NotNull Thread getDelegate() {
    return this.delegate;
  }

  /**
   * Determines whether {@literal this} {@link Thread} is alive.
   * <p>
   * A {@link Thread} is {@literal alive} if it has been started and has not yet died.
   *
   * @return a boolean value indicating whether {@literal this} {@link Thread} is alive.
   * @see java.lang.Thread#isAlive()
   */
  public boolean isAlive() {
    return getDelegate().isAlive();
  }

  /**
   * Determines whether {@literal this} {@link Thread} is currently in a {@literal blocked}
   * {@link Thread#getState() state}.
   * <p>
   * A {@link Thread} may currently be {@literal blocked} waiting on a lock or be performing some IO operation.
   *
   * @return a boolean valued indicating whether {@literal this} {@link Thread} is blocked.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#BLOCKED
   */
  public boolean isBlocked() {
    return Thread.State.BLOCKED.equals(getState());
  }

  /**
   * Determines whether {@literal this} {@link Thread} is a {@link Thread#isDaemon() daemon} {@link Thread}.
   * <p>
   * A {@link Thread#isDaemon() daemon} {@link Thread} is a background {@link Thread} that does not prevent
   * the JVM from exiting.
   *
   * @return a boolean value indicating whether {@literal this} {@link Thread}
   * is a {@link Thread#isDaemon() daemon} {@link Thread}.
   * @see java.lang.Thread#isDaemon()
   * @see #isNonDaemon()
   */
  public boolean isDaemon() {
    return getDelegate().isDaemon();
  }

  /**
   * Determines whether {@literal this} {@link Thread} is a {@link Thread#isDaemon() non-daemon} {@link Thread}.
   * <p>
   * A {@link Thread#isDaemon() non-daemon} {@link Thread} is a background {@link Thread} that prevents
   * the JVM from exiting. A {@link Thread#isDaemon() daemon} {@link Thread} is also known as
   * a {@literal user} {@link Thread}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Thread}
   * is a {@link Thread#isDaemon() non-daemon}, {@literal user} {@link Thread}.
   * @see java.lang.Thread#isDaemon()
   * @see #isDaemon()
   */
  public boolean isNonDaemon() {
    return !isDaemon();
  }

  /**
   * Alias method for {@link #isNonDaemon()}.
   * <p>
   * A {@link Thread#isDaemon() non-daemon} {@link Thread} is also known as a {@literal user} {@link Thread}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Thread} is a {@literal user} {@link Thread}.
   * @see java.lang.Thread#isDaemon()
   * @see #isNonDaemon()
   */
  public boolean isUser() {
    return isNonDaemon();
  }

  /**
   * Determines whether {@literal this} {@link Thread} has been {@link Thread#isInterrupted() interrupted}.
   * <p>
   * The {@link Thread#isInterrupted() interrupted status} of {@literal this} {@link Thread} is unaffected
   * by this method. This {@link Thread} can only be interrupted by another {@link Thread} when {@literal this}
   * {@link Thread} is blocked waiting on a lock or performing some blocking IO operation.
   *
   * @return a boolean indicating whether {@literal this} {@link Thread}
   * has been {@link Thread#isInterrupted() interrupted}.
   * @see java.lang.Thread#isInterrupted()
   */
  public boolean isInterrupted() {
    return getDelegate().isInterrupted();
  }

  /**
   * Determines whether {@literal this} {@link Thread} is {@literal new}.
   * <p>
   * A {@literal new} {@link Thread} is any {@link Thread} that has not been started yet.
   *
   * @return a boolean value indicating whether {@literal this} {@link Thread} is {@literal new}.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#NEW
   */
  public boolean isNew() {
    return Thread.State.NEW.equals(getState());
  }

  /**
   * Determines whether {@literal this} {@link Thread} is {@literal runnable}.
   * <p>
   * A {@literal runnable} {@link Thread} is any {@link Thread} that can be scheduled by the Operating System (OS)
   * for execution.
   *
   * @return a boolean value indicating whether {@literal this} {@link Thread} is in a {@literal runnable} state.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#RUNNABLE
   */
  public boolean isRunnable() {
    return Thread.State.RUNNABLE.equals(getState());
  }

  /**
   * Determines whether {@literal this} {@link Thread} has been {@literal terminated} (stopped).
   *
   * @return a boolean value indicating whether {@literal this} {@link Thread} has been {@literal terminated}.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#TERMINATED
   */
  public boolean isTerminated() {
    return Thread.State.TERMINATED.equals(getState());
  }

  /**
   * Determines whether {@literal this} {@link Thread} is currently in a {@literal timed wait}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Thread} is currently in a {@literal timed wait}.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#TIMED_WAITING
   */
  public boolean isTimedWaiting() {
    return Thread.State.TIMED_WAITING.equals(getState());
  }

  /**
   * Determines whether {@literal this} {@link Thread} is currently {@literal waiting}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Thread} is currently {@literal waiting}.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State#WAITING
   */
  public boolean isWaiting() {
    return Thread.State.WAITING.equals(getState());
  }

  /**
   * Sets the {@literal context} {@link ClassLoader} used by code executing in {@literal this} {@link Thread}
   * to find and load (resolve) {@link Class} types and resources.
   *
   * @param contextClassLoader {@link ClassLoader} used within the execution context of {@literal this} {@link Thread}
   * to resolve {@link Class} types and resources.
   * @return {@literal this} {@link ThreadAdapter}.
   * @see java.lang.Thread#setContextClassLoader(ClassLoader)
   * @see java.lang.ClassLoader
   */
  public @NotNull ThreadAdapter setContextClassLoader(@Nullable ClassLoader contextClassLoader) {
    getDelegate().setContextClassLoader(contextClassLoader);
    return this;
  }

  /**
   * Gets the {@literal context} {@link ClassLoader} used by code executing in {@literal this} {@link Thread}
   * to find and load (resolve) {@link Class} types and resources.
   *
   * @return the {@link ClassLoader} used by code executing in {@literal this} {@link Thread}
   * to resolve {@link Class} types and resources. May be {@literal null}.
   * @see java.lang.Thread#getContextClassLoader()
   * @see java.lang.ClassLoader
   */
  public @Nullable ClassLoader getContextClassLoader() {
    return getDelegate().getContextClassLoader();
  }

  /**
   * Sets {@literal this} {@link Thread} as a {@link Thread#isDaemon() daemon} or as a {@literal user} {@link Thread}.
   *
   * @param daemon a boolean value indicating whether {@literal this} {@link Thread}
   * is a {@link Thread#isDaemon() daemon} {@link Thread} or a {@literal user} {@link Thread}.
   * @return {@literal this} {@link ThreadAdapter}.
   * @see java.lang.Thread#setDaemon(boolean)
   */
  public @NotNull ThreadAdapter setDaemon(boolean daemon) {
    getDelegate().setDaemon(daemon);
    return this;
  }

  /**
   * Gets the identifier uniquely identifying {@literal this} {@link Thread}.
   *
   * @return a {@link Long} value identifying {@literal this} {@link Thread}.
   * @see java.lang.Thread#getId()
   */
  public long getId() {
    return getDelegate().getId();
  }

  /**
   * Sets the {@link String name} of {@literal this} {@link Thread}.
   *
   * @param name {@link String} containing the {@literal name} for {@literal this} {@link Thread}.
   * @return {@literal this} {@link ThreadAdapter}.
   * @throws IllegalArgumentException if the {@link String name} is {@literal null} or {@literal empty}.
   * @see java.lang.Thread#setName(String)
   */
  public @NotNull ThreadAdapter setName(@NotNull String name) {
    Assert.hasText(name, "Name [%s] is required", name);
    getDelegate().setName(name);
    return this;
  }

  /**
   * Gets the {@link String name} of {@literal this} {@link Thread}.
   *
   * @return the {@link String name} of {@literal this} {@link Thread}.
   * @see java.lang.Thread#getName()
   */
  public @NotNull String getName() {
    return getDelegate().getName();
  }

  /**
   * Sets the priority of {@literal this} {@link Thread}.
   * <p>
   * The priority of {@literal this} {@link Thread} is set to the smallest of the specified new priority
   * and the maximum permitted priority of the {@link Thread Thread's} {@link ThreadGroup}.
   *
   * @param priority priority for {@literal this} {@link Thread}.
   * @return {@literal this} {@link ThreadAdapter}.
   * @see java.lang.Thread#setPriority(int)
   */
  public @NotNull ThreadAdapter setPriority(int priority) {
    getDelegate().setPriority(priority);
    return this;
  }

  /**
   * Gets the priority of {@literal this} {@link Thread}.
   *
   * @return the priority of {@literal this} {@link Thread}.
   * @see java.lang.Thread#getPriority()
   */
  public int getPriority() {
    return getDelegate().getPriority();
  }

  /**
   * Gets the {@literal stack trace} of {@literal this} {@link Thread} contained in
   * an array of {@link StackTraceElement StackTraceElements} representing each call in the stack.
   *
   * @return array of {@link StackTraceElement StackTraceElements} representing a stack dump
   * of {@literal this} {@link Thread}.
   * @see java.lang.Thread#getStackTrace()
   * @see java.lang.StackTraceElement
   */
  public @NotNull StackTraceElement[] getStackTrace() {
    return getDelegate().getStackTrace();
  }

  /**
   * Gets the {@link Thread.State} of {@literal this} {@link Thread}.
   *
   * @return a {@link Thread.State} enumerated value indicating the {@literal state} of {@literal this} {@link Thread}.
   * @see java.lang.Thread#getState()
   * @see java.lang.Thread.State
   */
  public @NotNull Thread.State getState() {
    return getDelegate().getState();
  }

  /**
   * Gets the {@link ThreadGroup} to which {@literal this} {@link Thread} belongs.
   *
   * @return the {@link ThreadGroup} to which {@literal this} {@link Thread} belongs.
   * @see java.lang.Thread#getThreadGroup()
   * @see java.lang.ThreadGroup
   */
  public @NotNull ThreadGroup getThreadGroup() {
    return getDelegate().getThreadGroup();
  }

  /**
   * Sets the handler used by {@literal this} {@link Thread} when {@literal this} {@link Thread} is abruptly terminated
   * while executing code that throws an uncaught {@link Exception} or {@link Error} ({@link Throwable}).
   *
   * @param uncaughtExceptionHandler {@link Thread.UncaughtExceptionHandler} used by {@literal this} {@link Thread}
   * to handle any uncaught {@link Exception Exceptions} or {@link Error Errors} while executing code.
   * @return {@literal this} {@link ThreadAdapter}.
   * @see java.lang.Thread#setUncaughtExceptionHandler(Thread.UncaughtExceptionHandler)
   * @see java.lang.Thread.UncaughtExceptionHandler
   */
  public @NotNull ThreadAdapter setUncaughtExceptionHandler(
      @Nullable Thread.UncaughtExceptionHandler uncaughtExceptionHandler) {

    getDelegate().setUncaughtExceptionHandler(uncaughtExceptionHandler);

    return this;
  }

  /**
   * Gets the handler used by {@literal this} {@link Thread} when {@literal this} {@link Thread} is abruptly terminated
   * while executing code throwing an uncaught {@link Exception} or {@link Error} ({@link Throwable}).
   *
   * @return the {@link Thread.UncaughtExceptionHandler} used to handle any uncaught {@link Exception Exceptions}
   * or {@link Error Errors} while executing code in {@literal this} {@link Thread}.
   * @see java.lang.Thread#getUncaughtExceptionHandler()
   * @see java.lang.Thread.UncaughtExceptionHandler
   */
  public @Nullable Thread.UncaughtExceptionHandler getUncaughtExceptionHandler() {
    return getDelegate().getUncaughtExceptionHandler();
  }

  /**
   * Checks the access of the current calling {@link Thread} to determine whether it has permission
   * to modify {@literal this} {@link Thread}.
   *
   * @see java.lang.Thread#checkAccess()
   */
  @SuppressWarnings("deprecated")
  public void checkAccess() {
    getDelegate().checkAccess();
  }

  /**
   * Causes {@literal this} {@link Thread} to dump its current call stack (a.k.a. {@literal Stack Trace}) to
   * the {@link System#err standard error stream}.
   *
   * @see java.lang.Thread#dumpStack()
   */
  public void dumpStack() {
    Thread.dumpStack();
  }

  /**
   * Interrupts {@literal this} {@link Thread Thread's} execution.
   *
   * @see java.lang.Thread#interrupt()
   */
  public void interrupt() {
    getDelegate().interrupt();
  }

  /**
   * Causes the currently executing {@link Thread} to join and wait for {@literal this} {@link Thread} to terminate.
   * <p>
   * The currently executing {@link Thread} waits indefinitely for {@literal this} {@link Thread} to terminate,
   * or until the currently executing {@link Thread} is interrupted.
   *
   * @throws java.lang.InterruptedException if the {@link Thread#currentThread() current Thread} is interrupted
   * while waiting for {@literal this} {@link Thread}.
   * @see java.lang.Thread#join()
   */
  public void join() throws InterruptedException {
    getDelegate().join();
  }

  /**
   * Causes the currently executing {@link Thread} to join and wait for {@literal this} {@link Thread} to terminate,
   * or until the specified number of milliseconds have elapsed.
   * <p>
   * The currently executing {@link Thread} will wait until the timeout or the currently executing {@link Thread}
   * is interrupted.
   *
   * @param milliseconds number of milliseconds to wait for {@literal this} {@link Thread} to terminate.
   * @throws java.lang.InterruptedException if the {@link Thread#currentThread() current Thread} is interrupted
   * while waiting for {@literal this} {@link Thread}.
   * @see java.lang.Thread#join(long)
   */
  public void join(long milliseconds) throws InterruptedException {
    getDelegate().join(milliseconds);
  }

  /**
   * Causes the currently executing {@link Thread} to join and wait for {@literal this} {@link Thread} to terminate,
   * or until the specified number of milliseconds and nanoseconds have elapsed.
   * <p>
   * The currently executing {@link Thread} will wait until the timeout or the currently executing {@link Thread}
   * is interrupted.
   *
   * @param milliseconds number of milliseconds to wait for {@literal this} {@link Thread} to terminate.
   * @param nanoseconds number of nanoseconds to wait for {@literal this} {@link Thread} to terminate.
   * @throws java.lang.InterruptedException if the {@link Thread#currentThread() current Thread} is interrupted
   * while waiting for {@literal this} {@link Thread}.
   * @see java.lang.Thread#join(long, int)
   */
  public void join(long milliseconds, int nanoseconds) throws InterruptedException {
    getDelegate().join(milliseconds, nanoseconds);
  }

  /**
   * If {@literal this} {@link Thread} was constructed using a separate {@link Runnable} object,
   * then the {@link Runnable} object's {@link Runnable#run()} method is called; otherwise, {@literal this} method
   * does nothing and returns.
   *
   * @see java.lang.Thread#run()
   */
  @SuppressWarnings("all")
  public void run() {
    getDelegate().run();
  }

  /**
   * Causes {@literal this} {@link Thread} to begin execution.
   * <p>
   * The Java Virtual Machine (JVM) will call {@literal this} {@link Thread Thread's} {@link Thread#run()} method.
   *
   * @see java.lang.Thread#start()
   */
  public void start() {
    getDelegate().start();
  }

  /**
   * Return a {@link String} representation of {@literal this} {@link Thread}.
   *
   * @return a {@link String} describing {@literal this} {@link Thread}.
   * @see java.lang.Thread#toString()
   */
  @Override
  public @NotNull String toString() {

    return String.format(THREAD_TO_STRING, getClass().getName(), getId(), getName(), isDaemon(), getThreadGroup(),
      getPriority(), getState());
  }
}
