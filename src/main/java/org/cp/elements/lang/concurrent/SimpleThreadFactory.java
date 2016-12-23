/*
 * Copyright 2016 Author or Authors.
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
import static org.cp.elements.lang.ObjectUtils.defaultIfNull;

import java.util.concurrent.ThreadFactory;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.IdentifierSequence;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.ThrowableUtils;
import org.cp.elements.lang.support.UUIDIdentifierSequence;

/**
 * The {@link SimpleThreadFactory} class is a {@link ThreadFactory} implementation that constructs
 * and initializes a new instance of {@link Thread}.
 *
 * @author John Blum
 * @see java.lang.Thread
 * @see java.util.concurrent.ThreadFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SimpleThreadFactory implements ThreadFactory {

  protected static final boolean DEFAULT_DAEMON = true;

  protected static final Logger logger = Logger.getLogger(SimpleThreadFactory.class.getName());

  protected static final ThreadGroup DEFAULT_THREAD_GROUP = new ThreadGroup(
    String.format("%s.THREAD-GROUP", SimpleThreadFactory.class.getName()));

  /**
   * Factory method to construct a new instance of the {@link SimpleThreadFactory}
   * in order to construct a {@link Thread}.
   *
   * @return a new instance of the {@link SimpleThreadFactory}.
   * @see org.cp.elements.lang.concurrent.SimpleThreadFactory
   */
  public static SimpleThreadFactory newThreadFactory() {
    return new SimpleThreadFactory();
  }

  private Boolean daemon = DEFAULT_DAEMON;

  private ClassLoader contextClassLoader;

  private final IdentifierSequence<String> threadIdGenerator = new UUIDIdentifierSequence();

  private Integer priority;

  private ThreadGroup threadGroup;

  private Thread.UncaughtExceptionHandler uncaughtExceptionHandler;

  /**
   * Generates a unique thread identifier (ID) for the new {@link Thread}.
   *
   * @return a {@link String} value containing a unique thread identifier (ID).
   * @see org.cp.elements.lang.IdentifierSequence
   * @see #generateThreadName()
   */
  protected String generatedThreadId() {
    return this.threadIdGenerator.nextId();
  }

  /**
   * Generates a unique thread name for the new {@link Thread}.
   *
   * @return a {@link String} containing a unique thread name.
   * @see #generatedThreadId()
   */
  protected String generateThreadName() {
    return String.format("%1$s.THREAD-%2$s", SimpleThreadFactory.class.getName(), generatedThreadId());
  }

  /**
   * @inheritDoc
   */
  @Override
  public Thread newThread(Runnable task) {
    return newThread(generateThreadName(), task);
  }

  /**
   * Constructs and initializes a new {@link Thread}.
   *
   * @param name name given to the new {@link Thread}.
   * @param task {@link Runnable} task for the {@link Thread} to execute.
   * @return a new {@link Thread} initialized by this factory.
   * @throws IllegalArgumentException if {@code name} is unspecified or {@link Runnable} task is {@literal null}.
   * @see java.lang.Thread
   */
  public Thread newThread(String name, Runnable task) {

    Assert.hasText(name, "Thread name must be specified");
    Assert.notNull(task, "Thread task must not be null");

    Thread thread = new Thread(getThreadGroup(), task, name);

    thread.setContextClassLoader(getContextClassLoader());
    thread.setDaemon(isDaemon());
    thread.setPriority(getPriority());
    thread.setUncaughtExceptionHandler(getUncaughtExceptionHandler());

    return thread;
  }

  /**
   * Returns the {@link Thread} context {@link ClassLoader} used to resolve {@link Class} types.
   * Returns the calling {@link Thread Thread's} context {@link ClassLoader} if not set.
   *
   * @return a reference to the {@link Thread} context {@link ClassLoader}.
   * @see java.lang.Thread#getContextClassLoader()
   * @see java.lang.ClassLoader
   */
  @NullSafe
  public ClassLoader getContextClassLoader() {
    return defaultIfNull(this.contextClassLoader, Thread.currentThread().getContextClassLoader());
  }

  /**
   * Determine whether the constructed factory {@link Thread} will execute as a daemon thread.
   *
   * @return a boolean value indicating whether the constructed factory {@link Thread}
   * will execute as a daemon thread.
   * @see java.lang.Thread#isDaemon()
   */
  public boolean isDaemon() {
    return Boolean.TRUE.equals(this.daemon);
  }

  /**
   * Returns the priority used by this factory when initializing the new {@link Thread}.
   *
   * @return an integer value specifying the new {@link Thread Thread's} priority.
   * @see java.lang.Thread#getPriority()
   */
  public int getPriority() {
    return (this.priority != null ? this.priority : Thread.NORM_PRIORITY);
  }

  /**
   * Returns the {@link ThreadGroup} in which the new {@link Thread} will be assigned.
   *
   * @return the {@link ThreadGroup} to which the new {@link Thread} will belong.
   * @see java.lang.Thread#getThreadGroup()
   * @see java.lang.ThreadGroup
   */
  @NullSafe
  public ThreadGroup getThreadGroup() {
    return defaultIfNull(this.threadGroup, DEFAULT_THREAD_GROUP);
  }

  /**
   * Returns the {@link Thread.UncaughtExceptionHandler} for handling any unhandled {@link Exception Exceptions}
   * thrown by the {@link Runnable} tasks during normal execution of the factory {@link Thread}.
   *
   * @return a {@link Thread.UncaughtExceptionHandler} to handle any unhandled {@link Exception Exceptions}
   * thrown by the {@link Runnable} tasks during normal execution of the factory {@link Thread}.
   * @see java.lang.Thread.UncaughtExceptionHandler
   */
  @NullSafe
  public UncaughtExceptionHandler getUncaughtExceptionHandler() {
    return defaultIfNull(this.uncaughtExceptionHandler, SimpleUncaughtExceptionHandler.INSTANCE);
  }

  /**
   * Sets whether the new factory {@link Thread} will execute as a daemon thread.
   *
   * @param daemon boolean value indicating whether the {@link Thread} will execute as a daemon thread.
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread#setDaemon(boolean)
   * @see #isDaemon()
   */
  public SimpleThreadFactory as(boolean daemon) {
    this.daemon = daemon;
    return this;
  }

  /**
   * Sets the {@link ThreadGroup} in which the new factory {@link Thread} will belong.
   *
   * @param threadGroup {@link ThreadGroup} assigned to the new factory {@link Thread}.
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.ThreadGroup
   * @see #getThreadGroup()
   */
  public SimpleThreadFactory in(ThreadGroup threadGroup) {
    this.threadGroup = threadGroup;
    return this;
  }

  /**
   * Sets the context {@link ClassLoader} used by the new factory {@link Thread} to resolve {@link Class} types.
   *
   * @param contextClassLoader context {@link ClassLoader} used by the new factory {@link Thread}
   * to resolve {@link Class} types.
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread#setContextClassLoader(ClassLoader)
   * @see #getContextClassLoader()
   */
  public SimpleThreadFactory using(ClassLoader contextClassLoader) {
    this.contextClassLoader = contextClassLoader;
    return this;
  }

  /**
   * Sets the {@link Thread.UncaughtExceptionHandler} used by the new factory {@link Thread} to handle any unhandled
   * {@link Exception Exceptions} throws by the thread's {@link Runnable} task.
   *
   * @param uncaughtExceptionHandler {@link Thread.UncaughtExceptionHandler} used by the new factory {@link Thread}
   * to handle any unhandled {@link Exception Exceptions} throws by the thread's {@link Runnable} task.
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread.UncaughtExceptionHandler
   * @see #getUncaughtExceptionHandler()
   */
  public SimpleThreadFactory using(UncaughtExceptionHandler uncaughtExceptionHandler) {
    this.uncaughtExceptionHandler = uncaughtExceptionHandler;
    return this;
  }

  /**
   * Sets the priority of the new factory {@link Thread}.
   *
   * @param priority integer value specifying the thread priority.
   * @return this {@link SimpleThreadFactory}.
   * @see java.lang.Thread#setPriority(int)
   * @see #getPriority()
   */
  public SimpleThreadFactory with(int priority) {
    this.priority = priority;
    return this;
  }

  /**
   * The {@link SimpleUncaughtExceptionHandler} class implements {@link Thread.UncaughtExceptionHandler} by simply
   * logging the unhandled error.  Use and configure the {@link SimpleThreadFactory} logger to adjust logging detail.
   *
   * @see java.lang.Thread.UncaughtExceptionHandler
   */
  protected static final class SimpleUncaughtExceptionHandler implements UncaughtExceptionHandler {

    protected static final SimpleUncaughtExceptionHandler INSTANCE = new SimpleUncaughtExceptionHandler();

    /**
     * @inheritDoc
     */
    @Override
    public void uncaughtException(Thread thread, Throwable throwable) {
      logger.warning(String.format("An unhandled error [%1$s] was thrown by Thread [%2$s] having ID [%3$d]",
        throwable.getClass().getName(), thread.getName(), thread.getId()));

      if (logger.isLoggable(Level.FINE)) {
        logger.fine(ThrowableUtils.getStackTrace(throwable));
      }
    }
  }
}
