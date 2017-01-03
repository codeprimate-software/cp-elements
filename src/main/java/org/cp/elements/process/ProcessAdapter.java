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

import static org.cp.elements.lang.concurrent.SimpleThreadFactory.newThreadFactory;
import static org.cp.elements.process.ProcessContext.newProcessContext;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.atomic.AtomicBoolean;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Initable;
import org.cp.elements.lang.SystemUtils;
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

  private boolean buildCompositeProcessInputStreamListener = true;

  private final AtomicBoolean initialized = new AtomicBoolean(false);

  private final CopyOnWriteArraySet<ProcessInputStreamListener> listeners = new CopyOnWriteArraySet<>();

  private final Process process;

  private final ProcessContext processContext;

  private final ProcessInputStreamListener compositeProcessInputStreamListener = (line) -> {
    for (ProcessInputStreamListener listener : this.listeners) {
      listener.onInput(line);
    }
  };

  /**
   * Factory method to construct an instance of {@link ProcessAdapter} initialized with
   * the given {@link Process} object.
   *
   * @param process {@link Process} object to adapt/wrap with an instance of the {@link ProcessAdapter} class.
   * @return a newly constructed {@link ProcessAdapter} adapting/wrapping the given {@link Process} object.
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
   * the given {@link Process} object and corresponding {@link ProcessContext}.
   *
   * @param process {@link Process} object to adapt/wrap with an instance of the {@link ProcessAdapter} class.
   * @param processContext {@link ProcessContext} used to capture the context in which the {@link Process} is running.
   * @return a newly constructed {@link ProcessAdapter} adapting/wrapping the given {@link Process} object.
   * @throws IllegalArgumentException if {@link Process} is {@literal null}.
   * @see #ProcessAdapter(Process, ProcessContext)
   * @see org.cp.elements.process.ProcessContext
   * @see java.lang.Process
   */
  public static ProcessAdapter newProcessAdapter(Process process, ProcessContext processContext) {
    return new ProcessAdapter(process, processContext);
  }

  /**
   * Constructs an instance of {@link ProcessAdapter} initialized with the given {@link Process} object
   * and {@link ProcessContext}.
   *
   * @param process {@link Process} object adapted/wrapped by this {@link ProcessAdapter}.
   * @param processContext {@link ProcessContext} object containing contextual information in which
   * the {@link Process} is operating.
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
      newThread("Process OUT Stream Reader", newProcessInputStreamReader(getProcess().getInputStream())).start();

      if (!getProcessContext().isRedirectingErrorStream()) {
        newThread("Process ERR Stream Reader", newProcessInputStreamReader(getProcess().getErrorStream())).start();
      }
    }

    initialized.set(true);
  }

  protected Runnable newProcessInputStreamReader(InputStream in) {
    return () -> {
      if (isRunning()) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(in));

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
          FileSystemUtils.close(reader);
        }
      }
    };
  }

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
   * Returns the {@link ProcessContext} containing contextual information in which the {@link Process} is operating.
   *
   * @return the {@link ProcessContext} containing contextual information in which the {@link Process} is executing.
   * @see org.cp.elements.process.ProcessContext
   */
  protected ProcessContext getProcessContext() {
    return this.processContext;
  }

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

  public boolean isRunning() {
    return ProcessUtils.isRunning(getProcess());
  }

  public List<String> getCommandLine() {
    return getProcessContext().getCommandLine();
  }

  public File getDirectory() {
    return getProcessContext().getDirectory();
  }

  public Environment getEnvironment() {
    return getProcessContext().getEnvironment();
  }

  /**
   * @inheritDoc
   */
  @Override
  public Integer getId() {
    return ProcessUtils.readPid(ProcessUtils.findPidFile(getDirectory()));
  }

  /**
   * @inheritDoc
   */
  @Override
  public void setId(Integer id) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  public String getUsername() {
    return getProcessContext().getUsername();
  }

  public ProcessAdapter register(ProcessInputStreamListener listener) {
    this.listeners.add(listener);
    return this;
  }

  public ProcessAdapter unregister(ProcessInputStreamListener listener) {
    this.listeners.remove(listener);
    return this;
  }
}
