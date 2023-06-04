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
package org.cp.elements.process;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;
import static org.cp.elements.lang.concurrent.SimpleThreadFactory.newThreadFactory;
import static org.cp.elements.process.ProcessContext.newProcessContext;
import static org.cp.elements.process.support.RuntimeProcessExecutor.newRuntimeProcessExecutor;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.cp.elements.context.env.Environment;
import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Initable;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.lang.ThrowableUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.process.event.ProcessStreamListener;
import org.cp.elements.process.util.ProcessUtils;
import org.cp.elements.util.CollectionUtils;

/**
 * The {@link ProcessAdapter} class is an Adapter (wrapper) for a Java {@link Process} object.
 *
 * This class provides additional, convenient operations on an instance of {@link Process} that
 * are not available in the Java {@link Process} API.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.InputStream
 * @see java.io.OutputStream
 * @see java.lang.Process
 * @see java.util.UUID
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.Initable
 * @see org.cp.elements.lang.Nameable
 * @see org.cp.elements.process.ProcessContext
 * @see org.cp.elements.process.ProcessExecutor
 * @see org.cp.elements.process.event.ProcessStreamListener
 * @see Environment
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ProcessAdapter implements Identifiable<Integer>, Initable, Nameable<String> {

  protected static final long DEFAULT_TIMEOUT_MILLISECONDS = TimeUnit.SECONDS.toMillis(30);

  /**
   * Factory method used to construct a new instance of {@link ProcessAdapter} initialized with
   * the given, required {@link Process}.
   *
   * @param process {@link Process} object to adapt as an instance of {@link ProcessAdapter};
   * must not be {@literal null}.
   * @return a new {@link ProcessAdapter} adapting the given {@link Process}.
   * @throws IllegalArgumentException if {@link Process} is {@literal null}.
   * @see #newProcessAdapter(Process, ProcessContext)
   * @see java.lang.Process
   */
  public static @NotNull ProcessAdapter newProcessAdapter(@NotNull Process process) {

    ProcessContext processContext = newProcessContext(process)
      .ranBy(SystemUtils.USERNAME)
      .ranIn(FileSystemUtils.WORKING_DIRECTORY)
      .usingEnvironmentVariables();

    return newProcessAdapter(process, processContext);
  }

  /**
   * Factory method used to construct a new instance of {@link ProcessAdapter} initialized with
   * the given, required {@link Process} and corresponding {@link ProcessContext}.
   *
   * @param process {@link Process} object to adapt as an instance of {@link ProcessAdapter};
   * must not be {@literal null}.
   * @param processContext {@link ProcessContext} object containing contextual information about the environment
   * in which the {@link Process} is running; must not be {@literal null}.
   * @return a new {@link ProcessAdapter} adapting the given {@link Process}.
   * @throws IllegalArgumentException if {@link Process} or {@link ProcessContext} is {@literal null}.
   * @see #ProcessAdapter(Process, ProcessContext)
   * @see org.cp.elements.process.ProcessContext
   * @see java.lang.Process
   */
  public static @NotNull ProcessAdapter newProcessAdapter(@NotNull Process process,
      @NotNull ProcessContext processContext) {

    return new ProcessAdapter(process, processContext);
  }

  private final AtomicBoolean initialized = new AtomicBoolean(false);

  private final CopyOnWriteArraySet<ProcessStreamListener> listeners = new CopyOnWriteArraySet<>();

  private final Logger logger = Logger.getLogger(getClass().getName());

  private final Process process;

  private final ProcessContext processContext;

  private final ProcessStreamListener compositeProcessStreamListener =
    line -> this.listeners.forEach(listener -> listener.onInput(line));

  private final ThreadGroup threadGroup;

  /**
   * Constructs a new {@link ProcessAdapter} initialized with the given, required {@link Process}
   * and {@link ProcessContext}.
   *
   * @param process {@link Process} object adapted (wrapped) by this {@link ProcessAdapter};
   * must not be {@literal null}.
   * @param processContext {@link ProcessContext} object containing contextual information about the environment
   * in which the {@link Process} is running; must not be {@literal null}.
   * @throws IllegalArgumentException if {@link Process} or {@link ProcessContext} is {@literal null}.
   * @see org.cp.elements.process.ProcessContext
   * @see java.lang.Process
   */
  public ProcessAdapter(@NotNull Process process, @NotNull ProcessContext processContext) {

    this.process = ObjectUtils.requireObject(process, "Process is required");
    this.processContext = ObjectUtils.requireObject(processContext, "ProcessContext is required");
    this.threadGroup = new ThreadGroup(String.format("Process [%s] Thread Group", UUID.randomUUID()));
  }

  /**
   * Initializes the {@link ProcessAdapter} by starting {@link Thread Threads} to process the {@link Process Process's}
   * input and error IO streams.
   *
   * @see org.cp.elements.lang.Initable#init()
   */
  @Override
  public void init() {

    if (!getProcessContext().inheritsIO()) {

      newThread(String.format("Process [%d] Standard Out Reader", safeGetId()),
        newProcessStreamReader(getProcess().getInputStream())).start();

      if (!getProcessContext().isRedirectingErrorStream()) {

        newThread(String.format("Process [%d] Standard Error Reader", safeGetId()),
          newProcessStreamReader(getProcess().getErrorStream())).start();
      }
    }

    this.initialized.set(true);
  }

  protected Runnable newProcessStreamReader(@NotNull InputStream in) {

    return () -> {

      if (isRunning()) {
        try (BufferedReader reader = newReader(in)) {
          for (String input = reader.readLine(); input != null; input = reader.readLine()) {
            this.compositeProcessStreamListener.onInput(input);
          }
        }
        catch (IOException ignore) {
          // Ignore IO error and just stop reading from the process input stream
          // The IO error occurred most likely because the process was terminated
        }
      }
    };
  }

  protected BufferedReader newReader(@NotNull InputStream in) {
    return new BufferedReader(new InputStreamReader(in, Charset.defaultCharset()));
  }

  protected Thread newThread(String name, Runnable task) {

    return newThreadFactory()
      .asDaemonThread()
      .in(resolveThreadGroup())
      .withNormalPriority()
      .newThread(name, task);
  }

  protected ThreadGroup resolveThreadGroup() {
    return this.threadGroup;
  }

  /**
   * Returns a reference to the {@link Process} object adapted/wrapped by this {@link ProcessAdapter}.
   *
   * @return a reference to the {@link Process} object adapted/wrapped by this {@link ProcessAdapter}.
   * @see java.lang.Process
   */
  public @NotNull Process getProcess() {
    return this.process;
  }

  /**
   * Returns the {@link ProcessContext} containing contextual information about the environment and system
   * in which the {@link Process} is running.
   *
   * @return the {@link ProcessContext} containing contextual information about the environment and system
   * in which the {@link Process} is running.
   * @see org.cp.elements.process.ProcessContext
   */
  public @NotNull ProcessContext getProcessContext() {
    return this.processContext;
  }

  /**
   * Determines whether this {@link Process} is still alive.
   *
   * @return a boolean value indicating whether this {@link Process} is still alive.
   * @see org.cp.elements.process.util.ProcessUtils#isAlive(Process)
   * @see #getProcess()
   */
  public boolean isAlive() {
    return ProcessUtils.isAlive(getProcess());
  }

  /**
   * Determines whether this {@link Process} is still alive.
   *
   * @return a boolean value indicating whether this {@link Process} is still alive.
   * @see #isAlive()
   */
  public boolean isNotAlive() {
    return !isAlive();
  }

  /**
   * Determines whether this {@link ProcessAdapter} has been initialized.
   *
   * @return a boolean value indicating whether this {@link ProcessAdapter} has been initialized yet.
   * @see org.cp.elements.lang.Initable#isInitialized()
   */
  @Override
  public boolean isInitialized() {
    return this.initialized.get();
  }

  /**
   * Determines whether this {@link Process} is still running.
   *
   * @return a boolean value indicating whether this {@link Process} is still running.
   * @see org.cp.elements.process.util.ProcessUtils#isRunning(Process)
   * @see #isNotRunning()
   * @see #getProcess()
   */
  public boolean isRunning() {
    return ProcessUtils.isRunning(getProcess());
  }

  /**
   * Determines whether this {@link Process} is still running.
   *
   * @return a boolean value indicating whether this {@link Process} is still running.
   * @see #isRunning()
   */
  public boolean isNotRunning() {
    return !isRunning();
  }

  /**
   * Returns the command-line used to execute and run this {@link Process}.
   *
   * @return a {@link List} of {@link String Strings} containing the command-line elements used to execute
   * and run this {@link Process}.
   * @see org.cp.elements.process.ProcessContext#getCommandLine()
   * @see #getProcessContext()
   */
  public List<String> getCommandLine() {
    return getProcessContext().getCommandLine();
  }

  /**
   * Returns the file system {@link File directory} in which this {@link Process} is running.
   *
   * @return a {@link File} reference to the file system directory in which this {@link Process} is running.
   * @see org.cp.elements.process.ProcessContext#getDirectory()
   * @see #getProcessContext()
   */
  public File getDirectory() {
    return getProcessContext().getDirectory();
  }

  /**
   * Returns a reference to the {@link Environment} configuration used to execute the {@link Process}.
   *
   * @return a reference to the {@link Environment} configuration used to execute the {@link Process}.
   * @see org.cp.elements.process.ProcessContext#getEnvironment()
   * @see #getProcessContext()
   */
  public Environment getEnvironment() {
    return getProcessContext().getEnvironment();
  }

  /**
   * Returns the identifier of the {@link Process} (PID).
   *
   * @return an integer value containing the {@link Process} ID (PID).
   * @throws PidUnknownException if the {@link Process} ID (PID) cannot be determined.
   * @see org.cp.elements.lang.Identifiable#getId()
   * @see org.cp.elements.process.util.ProcessUtils#findPidFile(File)
   * @see org.cp.elements.process.util.ProcessUtils#readPid(File)
   * @see #getDirectory()
   */
  @Override
  public Integer getId() {

    try {
      return ProcessUtils.readPid(ProcessUtils.findPidFile(getDirectory()));
    }
    catch (Throwable cause) {

      if (cause instanceof PidUnknownException) {
        throw (PidUnknownException) cause;
      }

      throw new PidUnknownException("Process ID (PID) cannot be determined", cause);
    }
  }

  /**
   * Returns the process identifier (PID) of this running {@link Process}.
   *
   * This operation is safe from the unhandled {@link PidUnknownException}, which will be thrown
   * if the {@link Process} has terminated and the PID file was deleted.
   *
   * @return an integer value specifying the ID of the running {@link Process} or {@literal -1}
   * if the process ID cannot be determined.
   * @see org.cp.elements.process.PidUnknownException
   * @see #getId()
   */
  public Integer safeGetId() {

    try {
      return getId();
    }
    catch (PidUnknownException ignore) {
      return -1;
    }
  }

  /**
   * Throws an {@link UnsupportedOperationException} since the identifier of a {@link Process} cannot be set.
   * It is only ever assigned by the host operating system (OS).
   *
   * @param id identifier.
   * @throws UnsupportedOperationException since the ID of a {@link Process} cannot be set.
   * @see org.cp.elements.lang.Identifiable#setId(Comparable)
   */
  @Override
  public final void setId(Integer id) {
    throw newUnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  /**
   * Returns the configured {@link Logger} to log runtime details.
   *
   * @return the {@link Logger}.
   * @see java.util.logging.Logger
   */
  protected Logger getLogger() {
    return this.logger;
  }

  /**
   * Gets the Operating System (OS) {@link String name} of this {@link Process}.
   *
   * @return the {@link String name} of this {@link Process}.
   */
  @Override
  public String getName() {

    List<String> commandLine = getProcessContext().getCommandLine();

    return CollectionUtils.isNotEmpty(commandLine)
      ? CollectionUtils.getLastElement(commandLine)
      : String.valueOf(getId());
  }

  /**
   * Returns the standard error stream (stderr) of this process.
   *
   * @return an {@link InputStream} connected to the standard error stream (stderr) of this process.
   * @see java.lang.Process#getErrorStream()
   * @see #getProcess()
   */
  public InputStream getStandardErrorStream() {
    return getProcess().getErrorStream();
  }

  /**
   * Returns the standard input stream (stdin) of this process.
   *
   * @return an {@link OutputStream} connecting to the standard input stream (stdin) of this process.
   * @see java.lang.Process#getOutputStream()
   * @see #getProcess()
   */
  public OutputStream getStandardInStream() {
    return getProcess().getOutputStream();
  }

  /**
   * Returns the standard output stream (stdout) of this process.
   *
   * @return an {@link InputStream} connecting to the standard output stream (stdout) of this process.
   * @see java.lang.Process#getInputStream()
   * @see #getProcess()
   */
  public InputStream getStandardOutStream() {
    return getProcess().getInputStream();
  }

  /**
   * Returns the name of the user used to run this {@link Process}.
   *
   * @return a {@link String} containing the name of the user used to run this {@link Process}.
   * @see org.cp.elements.process.ProcessContext#getUsername()
   * @see #getProcessContext()
   */
  public String getUsername() {
    return getProcessContext().getUsername();
  }

  /**
   * Returns the exit value of this terminated {@link Process}.
   *
   * @return the exit value of this terminated {@link Process}.
   * @throws IllegalThreadStateException if the {@link Process} has not yet stopped.
   * @see java.lang.Process#exitValue()
   * @see #getProcess()
   */
  public int exitValue() {
    return getProcess().exitValue();
  }

  /**
   * Returns the exit value of this terminated {@link Process}.  Handles the {@link IllegalThreadStateException}
   * if this {@link Process} has not yet stopped by returning a {@literal -1}.
   *
   * @return the exit value of this terminated {@link Process} or {@literal -1}
   * if this {@link Process} has not yet stopped.
   * @see java.lang.IllegalThreadStateException
   * @see #exitValue()
   */
  public int safeExitValue() {

    try {
      return exitValue();
    }
    catch (IllegalThreadStateException ignore) {
      return -1;
    }
  }

  /**
   * Forcibly terminates this {@link Process} if still running.  Returns the exit value even if this {@link Process}
   * was previously terminated.
   *
   * @return an integer value indicating the exit value of this [forcibly] terminated {@link Process}.
   * @see java.lang.Process#destroyForcibly()
   * @see #safeExitValue()
   * @see #isRunning()
   */
  public synchronized int kill() {

    return isRunning()
      ? newProcessAdapter(getProcess().destroyForcibly(), getProcessContext()).waitFor()
      : safeExitValue();
  }

  /**
   * Restarts this {@link Process} by first attempting to stop this {@link Process} if running
   * and then running this {@link Process} by executing this {@link Process Process's}
   * {@link #getCommandLine() command-line} in the given {@link #getDirectory() directory}.
   *
   * @return a reference to this newly started and running {@link ProcessAdapter}.
   * @throws IllegalStateException if this {@link Process} cannot be stopped and restarted.
   * @see #execute(ProcessAdapter, ProcessContext)
   * @see #isNotRunning()
   * @see #isRunning()
   * @see #stopAndWait()
   */
  public synchronized ProcessAdapter restart() {

    if (isRunning()) {
      stopAndWait();
    }

    Assert.state(isNotRunning(), "Process [%d] failed to stop", safeGetId());

    return execute(this, getProcessContext());
  }

  protected ProcessAdapter execute(ProcessAdapter processAdapter, ProcessContext processContext) {
    return newProcessExecutor().execute(processAdapter.getDirectory(), processAdapter.getCommandLine());
  }

  protected ProcessExecutor<ProcessAdapter> newProcessExecutor() {
    return newRuntimeProcessExecutor();
  }

  /**
   * Terminates this {@link Process} if still running.  {@code #stop()} has no effect if this {@link Process}
   * was previously terminated and will just return the exit value.
   *
   * @return an integer value indicating the exit value of this {@link Process} after it has stopped.
   * @see #stop(long, TimeUnit)
   */
  public synchronized int stop() {
    return stop(DEFAULT_TIMEOUT_MILLISECONDS, TimeUnit.MILLISECONDS);
  }

  /**
   * Attempts to terminate this {@link Process} within the given timeout if still running.
   * {@code #stop(long, TimeUnit)} has no effect if this {@link Process} was previously terminated
   * and will just return the exit value.
   *
   * @param timeout duration to wait for this {@link Process} to stop.
   * @param unit {@link TimeUnit} used in the duration to wait for this {@link Process} to stop.
   * @return an integer value indicating the exit value of this {@link Process} after it has stopped.
   * @see java.util.concurrent.TimeUnit
   * @see java.lang.Process#destroy()
   * @see java.lang.Process#waitFor()
   * @see #isRunning()
   * @see #exitValue()
   * @see #safeExitValue()
   */
  public synchronized int stop(long timeout, TimeUnit unit) {

    if (isRunning()) {

      ExecutorService executorService = Executors
        .newSingleThreadExecutor(newThreadFactory().asDaemonThread().in(resolveThreadGroup()).withNormalPriority());

      try {

        Future<Integer> futureExitValue = executorService.submit(() -> {
          getProcess().destroy();
          return getProcess().waitFor();
        });

        try {
          int exitValue = futureExitValue.get(timeout, unit);
          getLogger().info(String.format("Process [%d] has been stopped", safeGetId()));
          return exitValue;
        }
        catch (ExecutionException cause) {
          if (getLogger().isLoggable(Level.FINE)) {
            getLogger().fine(ThrowableUtils.getStackTrace(cause));
          }
        }
        catch (InterruptedException cause) {
          Thread.currentThread().interrupt();
        }
        catch (TimeoutException cause) {
          getLogger().warning(String.format("Process [%1$d] could not be stopped within the given timeout [%2$d ms]",
            safeGetId(), unit.toMillis(timeout)));
        }
      }
      finally {
        executorService.shutdownNow();
      }

      return safeExitValue();
    }
    else {
      return exitValue();
    }
  }

  /**
   * Terminates this {@link Process} and wait indefinitely for this {@link Process} to stop.
   *
   * @return the exit value for this {@link Process} when it stops.
   * @see #stop()
   * @see #waitFor()
   */
  public int stopAndWait() {

    stop();

    return waitFor();
  }

  /**
   * Terminates this {@link Process} and wait until the given timeout for this {@link Process} to stop.
   *
   * @param timeout long value to indicate the number of units in the timeout.
   * @param unit {@link TimeUnit} of the timeout (e.g. seconds).
   * @return the exit value for this {@link Process} when if it stops, or {@literal -1} if this {@link Process}
   * does not stop within the given timeout.
   * @see #stop(long, TimeUnit)
   * @see #waitFor(long, TimeUnit)
   * @see #safeExitValue()
   */
  public int stopAndWait(long timeout, TimeUnit unit) {

    stop(timeout, unit);

    return (waitFor(timeout, unit) ? exitValue() : safeExitValue());
  }

  /**
   * Registers the provided {@link ProcessStreamListener} to listen for the {@link Process Process's}
   * standard out and error stream events.
   *
   * @param listener {@link ProcessStreamListener} to unregister.
   * @return this {@link ProcessAdapter}.
   * @see org.cp.elements.process.event.ProcessStreamListener
   */
  public ProcessAdapter register(ProcessStreamListener listener) {

    this.listeners.add(listener);

    return this;
  }

  /**
   * Registers a {@link Runtime} shutdown hook to stop this {@link Process} when the JVM exits.
   *
   * @return this {@link ProcessAdapter}.
   * @see java.lang.Runtime#addShutdownHook(Thread)
   * @see #newThread(String, Runnable)
   * @see #stop()
   */
  public ProcessAdapter registerShutdownHook() {

    Runtime.getRuntime()
      .addShutdownHook(newThread(String.format("Process [%d] Runtime Shutdown Hook", safeGetId()), this::stop));

    return this;
  }

  /**
   * Unregisters the given {@link ProcessStreamListener} from listening for the {@link Process Process's}
   * standard output and error stream events.
   *
   * @param listener {@link ProcessStreamListener} to unregister.
   * @return this {@link ProcessAdapter}.
   * @see org.cp.elements.process.event.ProcessStreamListener
   */
  public ProcessAdapter unregister(ProcessStreamListener listener) {

    this.listeners.remove(listener);

    return this;
  }

  /**
   * Waits indefinitely until this {@link Process} stops.
   *
   * This method handles the {@link InterruptedException} thrown by {@link Process#waitFor()}
   * by resetting the interrupt bit on the current (calling) {@link Thread}.
   *
   * @return an integer value specifying the exit value of the stopped {@link Process}.
   * @see java.lang.Process#waitFor()
   * @see #safeExitValue()
   */
  public int waitFor() {

    try {
      return getProcess().waitFor();
    }
    catch (InterruptedException ignore) {

      Thread.currentThread().interrupt();

      return safeExitValue();
    }
  }

  /**
   * Waits until the specified timeout for this {@link Process} to stop.
   *
   * This method handles the {@link InterruptedException} thrown by {@link Process#waitFor(long, TimeUnit)}
   * by resetting the interrupt bit on the current (calling) {@link Thread}.
   *
   * @param timeout long value to indicate the number of units in the timeout.
   * @param unit {@link TimeUnit} of the timeout (e.g. seconds).
   * @return a boolean value indicating whether the {@link Process} stopped within the given timeout.
   * @see java.lang.Process#waitFor(long, TimeUnit)
   * @see #isNotRunning()
   */
  public boolean waitFor(long timeout, TimeUnit unit) {

    try {
      return getProcess().waitFor(timeout, unit);
    }
    catch (InterruptedException ignore) {

      Thread.currentThread().interrupt();

      return isNotRunning();
    }
  }
}
