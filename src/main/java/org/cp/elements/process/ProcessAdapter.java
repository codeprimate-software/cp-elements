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

package org.cp.elements.process;

import static org.cp.elements.io.IOUtils.close;
import static org.cp.elements.lang.concurrent.SimpleThreadFactory.newThreadFactory;
import static org.cp.elements.process.ProcessContext.newProcessContext;
import static org.cp.elements.process.support.RuntimeProcessExecutor.newRuntimeProcessExecutor;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Initable;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.lang.ThrowableUtils;
import org.cp.elements.lang.concurrent.ThreadUtils;
import org.cp.elements.process.event.ProcessInputStreamListener;
import org.cp.elements.process.util.ProcessUtils;
import org.cp.elements.util.Environment;

/**
 * The {@link ProcessAdapter} class is an Adapter (wrapper) around a Java {@link Process} object.
 *
 * @author John J. Blum
 * @see java.lang.Process
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ProcessAdapter implements Identifiable<Integer>, Initable {

  protected static final boolean DAEMON_THREAD = true;

  protected static final int THREAD_PRIORITY = Thread.NORM_PRIORITY;

  protected static final long DEFAULT_TIMEOUT_MILLISECONDS = TimeUnit.SECONDS.toMillis(30);

  private boolean buildCompositeProcessInputStreamListener = true;

  private final AtomicBoolean initialized = new AtomicBoolean(false);

  private final CopyOnWriteArraySet<ProcessInputStreamListener> listeners = new CopyOnWriteArraySet<>();

  protected final Logger logger = Logger.getLogger(getClass().getName());

  private final Process process;

  private final ProcessContext processContext;

  private final ProcessInputStreamListener compositeProcessInputStreamListener = (line) -> {
    for (ProcessInputStreamListener listener : this.listeners) {
      listener.onInput(line);
    }
  };

  /**
   * Factory method to construct an instance of {@link ProcessAdapter} initialized with the given {@link Process}.
   *
   * @param process {@link Process} object to adapt/wrap with an instance of the {@link ProcessAdapter} class.
   * @return a newly constructed {@link ProcessAdapter} adapting/wrapping the given {@link Process}.
   * @throws IllegalArgumentException if {@link Process} is {@literal null}.
   * @see #newProcessAdapter(Process, ProcessContext)
   * @see java.lang.Process
   */
  public static ProcessAdapter newProcessAdapter(Process process) {
    return newProcessAdapter(process, newProcessContext(process).ranBy(SystemUtils.USERNAME)
      .ranIn(FileSystemUtils.WORKING_DIRECTORY).usingEnvironmentVariables());
  }

  /**
   * Factory method to construct an instance of {@link ProcessAdapter} initialized with
   * the given {@link Process} and corresponding {@link ProcessContext}.
   *
   * @param process {@link Process} object to adapt/wrap with an instance of the {@link ProcessAdapter} class.
   * @param processContext {@link ProcessContext} used to capture the context in which the {@link Process} is running.
   * @return a newly constructed {@link ProcessAdapter} adapting/wrapping the given {@link Process}.
   * @throws IllegalArgumentException if {@link Process} or {@link ProcessContext} is {@literal null}.
   * @see #ProcessAdapter(Process, ProcessContext)
   * @see org.cp.elements.process.ProcessContext
   * @see java.lang.Process
   */
  public static ProcessAdapter newProcessAdapter(Process process, ProcessContext processContext) {
    return new ProcessAdapter(process, processContext);
  }

  /**
   * Constructs an instance of {@link ProcessAdapter} initialized with the given {@link Process}
   * and {@link ProcessContext}.
   *
   * @param process {@link Process} object adapted/wrapped by this {@link ProcessAdapter}.
   * @param processContext {@link ProcessContext} object containing contextual information about the environment
   * in which the {@link Process} is running.
   * @throws IllegalArgumentException if {@link Process} or {@link ProcessContext} is {@literal null}.
   * @see org.cp.elements.process.ProcessContext
   * @see java.lang.Process
   */
  public ProcessAdapter(Process process, ProcessContext processContext) {
    Assert.notNull(process, "Process cannot be null");
    Assert.notNull(processContext, "ProcessContext cannot be null");

    this.process = process;
    this.processContext = processContext;
  }

  /**
   * @inheritDoc
   */
  @Override
  public void init() {
    if (!getProcessContext().inheritsIO()) {
      newThread("Process Standard Output Stream Reader",
        newProcessInputStreamReader(getProcess().getInputStream())).start();

      if (!getProcessContext().isRedirectingErrorStream()) {
        newThread("Process Standard Error Stream Reader",
          newProcessInputStreamReader(getProcess().getErrorStream())).start();
      }
    }

    initialized.set(true);
  }

  /* (non-Javadoc) */
  protected Runnable newProcessInputStreamReader(InputStream in) {
    return () -> {
      if (isRunning()) {
        BufferedReader reader = newReader(in);

        try {
          for (String input = reader.readLine(); input != null; input = reader.readLine()) {
            this.compositeProcessInputStreamListener.onInput(input);
          }
        }
        catch (IOException ignore) {
          // Ignore IO error and just stop reading from the process input stream
          // IO error occurred most likely because the process was terminated
        }
        finally {
          close(reader);
        }
      }
    };
  }

  /* (non-Javadoc) */
  protected BufferedReader newReader(InputStream in) {
    return new BufferedReader(new InputStreamReader(in));
  }

  /* (non-Javadoc) */
  protected Thread newThread(String name, Runnable task) {
    return newThreadFactory().as(DAEMON_THREAD).with(THREAD_PRIORITY).newThread(name, task);
  }

  /**
   * Returns a reference to the {@link Process} object adapted/wrapped by this {@link ProcessAdapter}.
   *
   * @return a reference to the {@link Process} object adapted/wrapped by this {@link ProcessAdapter}.
   * @see java.lang.Process
   */
  public Process getProcess() {
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
  public ProcessContext getProcessContext() {
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
   * @inheritDoc
   */
  @Override
  public boolean isInitialized() {
    return initialized.get();
  }

  /**
   * Determines whether this {@link Process} is still running.
   *
   * @return a boolean value indicating whether this {@link Process} is still running.
   * @see org.cp.elements.process.util.ProcessUtils#isRunning(int)
   * @see #getProcess()
   */
  public boolean isRunning() {
    return ProcessUtils.isRunning(getProcess());
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
   * @inheritDoc
   */
  @Override
  public Integer getId() {
    try {
      return ProcessUtils.readPid(ProcessUtils.findPidFile(getDirectory()));
    }
    catch (Throwable t) {
      if (t instanceof PidUnknownException) {
        throw (PidUnknownException) t;
      }

      throw new PidUnknownException("Process ID (PID) cannot be determined", t);
    }
  }

  /**
   * Returns the process identifier (PID) of this running {@link Process}.
   *
   * This operation is safe from the unhandled {@link PidUnknownException PidUnknownException},
   * which can be thrown if the {@link Process} has stopped and the PID file was deleted.
   *
   * @return an integer specifying the ID of the running {@link Process} or {@literal -1}
   * if the process ID cannot be determined.
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
   * @inheritDoc
   */
  @Override
  public final void setId(Integer id) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
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
   * Returns the exit value of this stopped {@link Process}.
   *
   * @return the exit value of this stopped {@link Process}.
   * @throws IllegalThreadStateException if the {@link Process} has not yet stopped.
   * @see java.lang.Process#exitValue()
   * @see #getProcess()
   */
  public int exitValue() {
    return getProcess().exitValue();
  }

  /**
   * Returns the exit value of this stopped {@link Process}.
   *
   * @return the exit value of this stopped {@link Process} or {@literal -1}
   * if the {@link Process} has not yet stopped.
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

  /* (non-Javadoc) */
  public synchronized int kill() {
    return (isRunning() ? newProcessAdapter(getProcess().destroyForcibly(), getProcessContext()).waitFor()
      : safeExitValue());
  }

  /* (non-Javadoc) */
  public synchronized ProcessAdapter restart() {
    if (isRunning()) {
      stop();
      waitFor();
    }

    return newProcessAdapter(newRuntimeProcessExecutor().execute(getDirectory(), getCommandLine()),
      getProcessContext());
  }

  /* (non-Javadoc) */
  public synchronized int stop() {
    return stop(DEFAULT_TIMEOUT_MILLISECONDS, TimeUnit.MILLISECONDS);
  }

  /* (non-Javadoc) */
  public synchronized int stop(long timeout, TimeUnit unit) {
    if (isRunning()) {
      AtomicBoolean exited = new AtomicBoolean(false);
      AtomicInteger exitValue = new AtomicInteger(-1);

      final int pid = safeGetId();

      ExecutorService executorService = Executors.newSingleThreadExecutor();

      try {
        Future<Integer> futureExitValue = executorService.submit(() -> {
          getProcess().destroy();
          int localExitValue = getProcess().waitFor();
          exited.set(true);
          return localExitValue;
        });

        ThreadUtils.waitFor(timeout, unit).on(() -> {
          try {
            exitValue.set(futureExitValue.get(timeout, unit));
            logger.info(String.format("Process [%d] has been stopped", pid));
          }
          catch (ExecutionException | InterruptedException ignore) {
            if (logger.isLoggable(Level.FINE)) {
              logger.fine(ThrowableUtils.getStackTrace(ignore));
            }
          }
          catch (TimeoutException e) {
            logger.warning(String.format("Process [%1$d] could not be stopped within the given timeout [%2$d ms]",
              pid, unit.toMillis(timeout)));
            exitValue.set(safeExitValue());
          }

          return exited.get();
        });
      }
      finally {
        executorService.shutdownNow();
      }

      return exitValue.get();
    }
    else {
      return exitValue();
    }
  }

  /**
   * Registers the provided {@link ProcessInputStreamListener} to listen for the {@link Process Process's}
   * standard output and error stream events.
   *
   * @param listener {@link ProcessInputStreamListener} to unregister.
   * @return this {@link ProcessAdapter}.
   * @see org.cp.elements.process.event.ProcessInputStreamListener
   */
  public ProcessAdapter register(ProcessInputStreamListener listener) {
    this.listeners.add(listener);
    return this;
  }

  /**
   * Registers a {@link Runtime} shutdown hood to stop this {@link Process} when the JVM exits.
   *
   * @return this {@link ProcessAdapter}.
   * @see java.lang.Runtime#addShutdownHook(Thread)
   * @see #newThread(String, Runnable)
   */
  public ProcessAdapter registerShutdownHook() {
    Runtime.getRuntime().addShutdownHook(
      newThread(String.format("Process [%d] Runtime Shutdown Hook", safeGetId()), this::stop));

    return this;
  }

  /**
   * Unregisters the given {@link ProcessInputStreamListener} from listening for the {@link Process Process's}
   * standard output and error stream events.
   *
   * @param listener {@link ProcessInputStreamListener} to unregister.
   * @return this {@link ProcessAdapter}.
   * @see org.cp.elements.process.event.ProcessInputStreamListener
   */
  public ProcessAdapter unregister(ProcessInputStreamListener listener) {
    this.listeners.remove(listener);
    return this;
  }

  /**
   * Waits indefinitely until the {@link Process} stops.
   *
   * This method handles the {@link InterruptedException} thrown by {@link Process#waitFor()}
   * by resetting the interrupt bit on the current (calling) {@link Thread}.
   *
   * @return an integer value specifying the exit value of the stopped {@link Process}.
   * @see java.lang.Process#waitFor()
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
   * Waits until the specified timeout for the {@link Process} to stop.
   *
   * This method handles the {@link InterruptedException} thrown by {@link Process#waitFor(long, TimeUnit)}
   * by resetting the interrupt bit on the current (calling) {@link Thread}.
   *
   * @param timeout long value to indicate the number of units in the timeout.
   * @param unit {@link TimeUnit} of the timeout (e.g. seconds).
   * @return a boolean value indicating whether the {@link Process} stopped within the given timeout.
   * @see java.lang.Process#waitFor(long, TimeUnit)
   */
  public boolean waitFor(long timeout, TimeUnit unit) {
    try {
      return getProcess().waitFor(timeout, unit);
    }
    catch (InterruptedException ignore) {
      Thread.currentThread().interrupt();
      return !isRunning();
    }
  }
}
