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

import static java.lang.Thread.UncaughtExceptionHandler;

import java.util.UUID;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.IdentifierSequence;
import org.cp.elements.lang.ThrowableUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.support.UUIDIdentifierSequence;

/**
 * Element's {@link ThreadFactory} implementation used to construct and initialize a new {@link Thread}.
 *
 * @author John Blum
 * @see java.lang.Thread
 * @see java.util.concurrent.ThreadFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SimpleThreadFactory implements ThreadFactory {

  private static final Logger logger = Logger.getLogger(SimpleThreadFactory.class.getName());

  protected static final boolean DEFAULT_DAEMON = true;

  protected static final int DEFAULT_PRIORITY = Thread.NORM_PRIORITY;

  protected static final String THREAD_NAME_FORMAT = "%1$s.THREAD-%2$s";

  protected static final ThreadGroup DEFAULT_THREAD_GROUP =
    new ThreadGroup(String.format("%s.THREAD-GROUP", SimpleThreadFactory.class.getName()));

  /**
   * Factory method used to construct a new {@link SimpleThreadFactory} that then can be used to construct and start
   * a new {@link Thread}.
   *
   * @return a new instance of {@link SimpleThreadFactory}.
   * @see org.cp.elements.lang.concurrent.SimpleThreadFactory
   */
  public static @NotNull SimpleThreadFactory newThreadFactory() {
    return new SimpleThreadFactory();
  }

  private Boolean daemon = DEFAULT_DAEMON;

  private ClassLoader contextClassLoader;

  private final IdentifierSequence<UUID> threadIdGenerator = new UUIDIdentifierSequence();

  private Integer priority;

  private ThreadGroup threadGroup;

  private Thread.UncaughtExceptionHandler uncaughtExceptionHandler;

  /**
   * Generates a unique {@link String identifier} ({@literal ID}) for the new {@link Thread}.
   *
   * @return a {@link String} containing a {@literal unique identifier} (ID) for the new {@link Thread}.
   * @see org.cp.elements.lang.IdentifierSequence
   * @see #generateThreadName()
   */
  protected @NotNull String generateThreadId() {
    return this.threadIdGenerator.nextId().toString();
  }

  /**
   * Generates a unique {@link String name} for the new {@link Thread}.
   *
   * @return a {@link String} containing a {@literal unique name} for the new {@link Thread}.
   * @see java.lang.Thread#getName()
   * @see #generateThreadId()
   */
  protected @NotNull String generateThreadName() {
    return THREAD_NAME_FORMAT.formatted(SimpleThreadFactory.class.getName(), generateThreadId());
  }

  /**
   * Constructs a new {@link Thread} initialized with the given, required {@link Runnable} task.
   *
   * @param task {@link Runnable} task to run in a new {@link Thread};  must not be {@literal null}.
   * @return a new {@link Thread} initialized with the given {@link Runnable} task
   * that will be run by the {@link Thread} when started.
   * @see #newThread(String, Runnable)
   * @see #generateThreadName()
   * @see java.lang.Runnable
   * @see java.lang.Thread
   */
  @Override
  @SuppressWarnings("all")
  public @NotNull Thread newThread(@NotNull Runnable task) {
    return newThread(generateThreadName(), task);
  }

  /**
   * Constructs a new {@link Thread} initialized with the given, required {@link String name} and {@link Runnable} task
   * that will be run by the {@link Thread} when started.
   *
   * @param name {@link String} containing the {@literal name} given to the new {@link Thread};
   * must not be {@literal null}.
   * @param task {@link Runnable} task for the {@link Thread} to execute;
   * must not be {@literal null}.
   * @return a new {@link Thread} created and initialized by {@literal this} {@link ThreadFactory} with the given,
   * required {@link String name} and {@link Runnable} task to execute.
   * @throws IllegalArgumentException if the {@link String name} or the {@link Runnable} task are {@literal null}.
   * @see #getContextClassLoader()
   * @see #isDaemon()
   * @see #getPriority()
   * @see #getThreadGroup()
   * @see #getUncaughtExceptionHandler()
   * @see java.lang.Runnable
   * @see java.lang.Thread
   */
  public @NotNull Thread newThread(@NotNull String name, @NotNull Runnable task) {

    Assert.hasText(name, "Name [%s] is required", name);
    Assert.notNull(task, "Runnable task is required");

    Thread thread = new Thread(getThreadGroup(), task, name);

    thread.setContextClassLoader(getContextClassLoader());
    thread.setDaemon(isDaemon());
    thread.setPriority(getPriority());
    thread.setUncaughtExceptionHandler(getUncaughtExceptionHandler());

    return thread;
  }

  /**
   * Returns the configured {@literal context} {@link ClassLoader} used by new {@link Thread}
   * to resolve {@link Class} types.
   * <p>
   * Returns the calling {@link Thread Thread's} {@link Thread#getContextClassLoader() Thread Context ClassLoader}
   * if not set.
   *
   * @return a reference to the configured context {@link ClassLoader}.
   * @see java.lang.Thread#getContextClassLoader()
   * @see java.lang.ClassLoader
   */
  public @NotNull ClassLoader getContextClassLoader() {

    ClassLoader contextClassLoader = this.contextClassLoader;

    return contextClassLoader != null
      ? contextClassLoader
      : Thread.currentThread().getContextClassLoader();
  }

  /**
   * Determine whether the new {@link Thread} will execute as a {@link Thread#isDaemon() daemon} {@link Thread}.
   * <p>
   * A {@link Thread#isDaemon() daemon} {@link Thread} is a {@link Thread} that will not prevent the JVM
   * from shutting down.
   *
   * @return a boolean value indicating whether the new {@link Thread} will execute as
   * a {@link Thread#isDaemon() daemon} {@link Thread}.
   * @see java.lang.Thread#isDaemon()
   */
  public boolean isDaemon() {
    return Boolean.TRUE.equals(this.daemon);
  }

  /**
   * Returns the configured {@link Thread#getPriority() priority} used by {@literal this} {@link ThreadFactory}
   * when initializing the new {@link Thread}.
   *
   * @return an {@link Integer} value specifying the new {@link Thread Thread's} {@link Thread#getPriority() priority}.
   * @see java.lang.Thread#getPriority()
   */
  public int getPriority() {

    Integer priority = this.priority;

    return priority != null ? priority : DEFAULT_PRIORITY;
  }

  /**
   * Returns the {@link ThreadGroup} to which the new {@link Thread} will be assigned.
   *
   * @return the {@link ThreadGroup} to which the new {@link Thread} will be assigned.
   * @see java.lang.Thread#getThreadGroup()
   * @see java.lang.ThreadGroup
   */
  public @NotNull ThreadGroup getThreadGroup() {

    ThreadGroup threadGroup = this.threadGroup;

    return threadGroup != null ? threadGroup : DEFAULT_THREAD_GROUP;
  }

  /**
   * Returns the {@link Thread.UncaughtExceptionHandler} used for handling any uncaught {@link Exception Exceptions}
   * thrown by the {@link Runnable} tasks during normal execution of the new {@link Thread}.
   *
   * @return a {@link Thread.UncaughtExceptionHandler} used to handle any uncaught {@link Exception Exceptions}
   * thrown by the {@link Runnable} tasks during normal execution of the new {@link Thread}.
   * @see java.lang.Thread.UncaughtExceptionHandler
   */
  public @NotNull UncaughtExceptionHandler getUncaughtExceptionHandler() {

    UncaughtExceptionHandler uncaughtExceptionHandler = this.uncaughtExceptionHandler;

    return uncaughtExceptionHandler != null
      ? uncaughtExceptionHandler
      : SimpleUncaughtExceptionHandler.INSTANCE;
  }

  /**
   * Builder method used to set the new {@link Thread} to execute as a {@link Thread#isDaemon() daemon} {@link Thread}.
   *
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread#setDaemon(boolean)
   * @see #asUserThread()
   * @see #isDaemon()
   */
  public @NotNull SimpleThreadFactory asDaemonThread() {
    this.daemon = true;
    return this;
  }

  /**
   * Builder method used to set the new {@link Thread} to execute as a {@link Thread#isDaemon() non-daemon},
   * {@literal user} {@link Thread}.
   *
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread#setDaemon(boolean)
   * @see #asDaemonThread()
   * @see #isDaemon()
   */
  public @NotNull SimpleThreadFactory asUserThread() {
    this.daemon = false;
    return this;
  }

  /**
   * Builder method used to set the {@link Thread.UncaughtExceptionHandler} used by the new {@link Thread}
   * to handle any uncaught {@link Exception Exceptions} throws by the {@link Thread Thread's} {@link Runnable} task.
   *
   * @param uncaughtExceptionHandler {@link Thread.UncaughtExceptionHandler} used by the new {@link Thread}
   * to handle any uncaught {@link Exception Exceptions} thrown by the {@link Thread Thread's} {@link Runnable} task.
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread#setUncaughtExceptionHandler(UncaughtExceptionHandler)
   * @see java.lang.Thread.UncaughtExceptionHandler
   * @see #getUncaughtExceptionHandler()
   */
  public @NotNull SimpleThreadFactory handleUncaughtExceptionsWith(
      @Nullable UncaughtExceptionHandler uncaughtExceptionHandler) {

    this.uncaughtExceptionHandler = uncaughtExceptionHandler;

    return this;
  }

  /**
   * Builder method used to set the {@link ThreadGroup} in which the new {@link Thread} will be assigned.
   *
   * @param threadGroup {@link ThreadGroup} in which new {@link Thread} will belong.
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread#Thread(ThreadGroup, Runnable, String)
   * @see java.lang.ThreadGroup
   * @see #getThreadGroup()
   */
  public @NotNull SimpleThreadFactory in(@Nullable ThreadGroup threadGroup) {
    this.threadGroup = threadGroup;
    return this;
  }

  /**
   * Builder method used to set the {@literal context} {@link ClassLoader} used by the new {@link Thread}
   * to resolve {@link Class} types.
   *
   * @param contextClassLoader {@literal context} {@link ClassLoader} used by the new {@link Thread}
   * to resolve {@link Class} types.
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread#setContextClassLoader(ClassLoader)
   * @see #getContextClassLoader()
   * @see java.lang.ClassLoader
   */
  public @NotNull SimpleThreadFactory resolveTypesWith(@Nullable ClassLoader contextClassLoader) {
    this.contextClassLoader = contextClassLoader;
    return this;
  }

  /**
   * Builder method used to set the {@link Thread#getPriority() priority} of the new {{@link Thread}
   * to {@link Thread#MAX_PRIORITY maximum priority}.
   *
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread#setPriority(int)
   * @see #withPriority(int)
   * @see #getPriority()
   */
  public @NotNull SimpleThreadFactory withMaxPriority() {
    return withPriority(Thread.MAX_PRIORITY);
  }

  /**
   * Builder method used to set the {@link Thread#getPriority() priority} of the new {{@link Thread}
   * to {@link Thread#MIN_PRIORITY minimum priority}.
   *
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread#setPriority(int)
   * @see #withPriority(int)
   * @see #getPriority()
   */
  public @NotNull SimpleThreadFactory withMinPriority() {
    return withPriority(Thread.MIN_PRIORITY);
  }

  /**
   * Builder method used to set the {@link Thread#getPriority() priority} of the new {{@link Thread}
   * to {@link Thread#NORM_PRIORITY normal priority}.
   *
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread#setPriority(int)
   * @see #withPriority(int)
   * @see #getPriority()
   */
  public @NotNull SimpleThreadFactory withNormalPriority() {
    return withPriority(Thread.NORM_PRIORITY);
  }

  /**
   * Builder method used to set the {@link Thread#getPriority() priority} of the new {@link Thread}.
   *
   * @param priority {@link Integer} value specifying the {@link Thread#getPriority() Thread Priority}.
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread#setPriority(int)
   * @see #getPriority()
   */
  public @NotNull SimpleThreadFactory withPriority(int priority) {
    this.priority = priority;
    return this;
  }

  /**
   * The {@link SimpleUncaughtExceptionHandler} class implements {@link Thread.UncaughtExceptionHandler} by simply
   * {@literal logging} the unhandled error.
   * <p>
   * Use and configure the {@link SimpleThreadFactory} {@link Logger} to adjust logging detail.
   *
   * @see java.lang.Thread.UncaughtExceptionHandler
   */
  protected static class SimpleUncaughtExceptionHandler implements UncaughtExceptionHandler {

    protected static final SimpleUncaughtExceptionHandler INSTANCE = new SimpleUncaughtExceptionHandler();

    /**
     * Returns the {@link Logger} used to log details of the uncaught, unhandled {@link Throwable Exceptions and Errors}
     * thrown by {@link Thread Threads} during execution of the program.
     *
     * @return the {@link Logger} used to log uncaught, unhandled {@link Throwable Exceptions and Errors}.
     * @see java.util.logging.Logger
     */
    protected Logger getLogger() {
      return logger;
    }

    /**
     * Handles any uncaught and unhandled {@link Throwable Exceptions and Errors} thrown from any {@link Thread}
     * of execution.
     *
     * @param thread {@link Thread} from which the uncaught/unhandled {@link Throwable Exception or Error} was thrown.
     * @param cause the uncaught, unhandled {@link Throwable Exception or Error} thrown by the given {@link Thread}.
     * @see java.lang.Thread
     * @see java.lang.Throwable
     * @see #getLogger()
     */
    @Override
    public void uncaughtException(@NotNull Thread thread, @NotNull Throwable cause) {

      Logger logger = getLogger();

      if (logger.isLoggable(Level.WARNING)) {
        String message = "An unhandled error [%1$s] was thrown by Thread [%2$s]"
          .formatted(cause.getClass().getName(), thread.getName());
        logger.warning(message);
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.fine(ThrowableUtils.getStackTrace(cause));
      }
    }
  }
}
