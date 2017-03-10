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

package org.cp.elements.process.util;

import static org.cp.elements.lang.StringUtils.hasText;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.util.concurrent.TimeUnit;

import com.sun.tools.attach.VirtualMachine;
import com.sun.tools.attach.VirtualMachineDescriptor;

import org.cp.elements.io.ComposableFileFilter;
import org.cp.elements.io.DirectoriesOnlyFilter;
import org.cp.elements.io.FileExtensionFilter;
import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.io.FilesOnlyFilter;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.process.PidUnknownException;
import org.cp.elements.process.ProcessAdapter;

/**
 * The {@link ProcessUtils} class is an abstract utility class for working with Java {@link Process} objects.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.lang.Process
 * @see org.cp.elements.process.ProcessAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ProcessUtils {

  protected static final long KILL_WAIT_TIMEOUT = 5;

  protected static final String PROCESS_ID_FILENAME = ".pid";
  protected static final String WINDOWS_KILL_COMMAND = "taskkill /F /PID";
  protected static final String UNIX_KILL_COMMAND = "kill -KILL";
  protected static final String VIRTUAL_MACHINE_CLASS_NAME = "com.sun.tools.attach.VirtualMachine";

  protected static final FileFilter PID_FILE_FILTER = ComposableFileFilter.and(
    FilesOnlyFilter.INSTANCE, new FileExtensionFilter(PROCESS_ID_FILENAME));

  protected static final FileFilter DIRECTORY_PID_FILE_FILTER = ComposableFileFilter.or(
    DirectoriesOnlyFilter.INSTANCE, PID_FILE_FILTER);

  protected static final TimeUnit KILL_WAIT_TIME_UNIT = TimeUnit.SECONDS;

  /**
   * Determines the Process ID (PID) of this Java {@link Process}.
   *
   * @return an {@link Integer} value indicating the Process ID (PID) of this Java {@link Process}.
   * @throws PidUnknownException if the Process ID (PID) of this Java {@link Process} cannot be determined.
   */
  public static int getProcessId() {
    RuntimeMXBean runtimeMxBean = ManagementFactory.getRuntimeMXBean();
    String runtimeMxBeanName = runtimeMxBean.getName();

    Throwable cause = null;

    if (hasText(runtimeMxBeanName)) {
      int atSignIndex = runtimeMxBeanName.indexOf('@');

      if (atSignIndex > 0) {
        try {
          return Integer.parseInt(runtimeMxBeanName.substring(0, atSignIndex));
        }
        catch (NumberFormatException e) {
          cause = e;
        }
      }
    }

    throw new PidUnknownException(String.format("Process ID (PID) unknown [%s]", runtimeMxBeanName), cause);
  }

  /**
   * Null-safe method to determine whether the given {@link Process} is still alive, or that the {@link Process}
   * has not yet terminated yet.
   *
   * @param process {@link Process} to evaluate if still alive.
   * @return a boolean value indicating whether the given {@link Process} is still alive.
   * @see java.lang.Process#isAlive()
   * @see #isRunning(Process)
   */
  @NullSafe
  public static boolean isAlive(Process process) {
    return (process != null && process.isAlive());
  }

  /**
   * Determines whether the Java Process identified by the given Process ID (PID) is running.
   *
   * @param processId Operating System (OS) Process ID (PID) of the Java Process.
   * @return a boolean value indicating whether the Java Process identified by the given Process ID (PID) is running.
   * @see org.cp.elements.process.util.ProcessUtils.VirtualMachineAccessor#isRunning(int)
   */
  public static boolean isRunning(int processId) {
    return VirtualMachineAccessor.INSTANCE.isRunning(processId);
  }

  /**
   * Null-safe method to determine whether the given {@link Process} is still running.
   *
   * @param process {@link Process} object to evaluate if running.
   * @return a boolean value indicating whether the given {@link Process} is still running.
   * @see java.lang.Process
   */
  @NullSafe
  public static boolean isRunning(Process process) {
    try {
      return (process != null && process.exitValue() == Double.NaN);
    }
    catch (IllegalThreadStateException ignore) {
      return true;
    }
  }

  /**
   * Null-safe method to determine if the {@link Process} represented by the given {@link ProcessAdapter} is running.
   *
   * @param processAdapter {@link ProcessAdapter} wraping the {@link Process} to evaluate if running.
   * @return a boolean value indicating whether the {@link Process} object adapted/wrapped by
   * the given {@link ProcessAdapter} is running.
   * @see org.cp.elements.process.ProcessAdapter
   */
  @NullSafe
  public static boolean isRunning(ProcessAdapter processAdapter) {
    return (processAdapter != null && isRunning(processAdapter.getProcess()));
  }

  /**
   * Kills the Operating System (OS) process identified by the process ID (PID).
   *
   * @param processId ID of the process to kill.
   * @return a boolean value indicating whether the process identified by the process ID (ID)
   * was successfully terminated.
   * @see Runtime#exec(String)
   */
  public static boolean kill(int processId) {
    String killCommand = String.format("%s %d",
      (SystemUtils.isWindows() ? WINDOWS_KILL_COMMAND : UNIX_KILL_COMMAND), processId);

    try {
      Process killProcess = Runtime.getRuntime().exec(killCommand);
      return killProcess.waitFor(KILL_WAIT_TIMEOUT, KILL_WAIT_TIME_UNIT);
    }
    catch (Throwable ignore) {
      return false;
    }
  }

  /**
   * Kills the given {@link Process}.
   *
   * @param process {@link Process} to kill.
   * @return a boolean value indicating whether the the given {@link Process} was successfully terminated.
   * @see java.lang.Process
   * @see java.lang.Process#destroy()
   * @see java.lang.Process#destroyForcibly()
   * @see #isAlive(Process)
   */
  @NullSafe
  public static boolean kill(Process process) {
    boolean alive = isAlive(process);

    if (alive) {
      process.destroy();

      try {
        alive = !process.waitFor(KILL_WAIT_TIMEOUT, KILL_WAIT_TIME_UNIT);
      }
      catch (InterruptedException ignore) {
        Thread.currentThread().interrupt();
      }
      finally {
        if (alive) {
          process.destroyForcibly();

          try {
            alive = !process.waitFor(KILL_WAIT_TIMEOUT, KILL_WAIT_TIME_UNIT);
          }
          catch (InterruptedException ignore) {
            Thread.currentThread().interrupt();
          }
        }
      }
    }

    return !(alive && isAlive(process));
  }

  /**
   * Kills the process represented by the given {@link ProcessAdapter}.
   *
   * @param processAdapter {@link ProcessAdapter} representing the process to kill.
   * @return a boolean value indicating whether the the given process represented by the {@link ProcessAdapter}
   * was successfully terminated.
   * @see org.cp.elements.process.ProcessAdapter
   * @see #kill(Process)
   */
  @NullSafe
  public static boolean kill(ProcessAdapter processAdapter) {
    return (processAdapter != null && kill(processAdapter.getProcess()));
  }

  /**
   * Constructs a new instance of {@link Thread} initialized with the given {@link Runnable}.
   *
   * @param runnable {@link Runnable} object to execute in a {@link Thread}.
   * @return a new {@link Thread} initialized with the given {@link Runnable}.
   * @see java.lang.Runnable
   * @see java.lang.Thread
   */
  protected static Thread newThread(Runnable runnable) {
    return new Thread(runnable, "Process Shutdown Hook Thread");
  }

  /**
   * Registers a {@link Runtime} shutdown hook to kill the Operating System (OS) process
   * identified by the process ID (PID).
   *
   * @param processId ID of the process to terminate on shutdown.
   * @see java.lang.Runtime#addShutdownHook(Thread)
   * @see #newThread(Runnable)
   * @see #kill(int)
   */
  public static void registerShutdownHook(int processId) {
    Runtime.getRuntime().addShutdownHook(newThread(() -> kill(processId)));
  }

  /**
   * Registers a {@link Runtime} shutdown hook to kill the given {@link Process}.
   *
   * @param process {@link Process} to terminate on shutdown.
   * @see java.lang.Runtime#addShutdownHook(Thread)
   * @see #newThread(Runnable)
   * @see #kill(Process)
   */
  public static void registerShutdownHook(Process process) {
    Runtime.getRuntime().addShutdownHook(newThread(() -> kill(process)));
  }

  /**
   * Registers a {@link Runtime} shutdown hook to kill the process represented by the given {@link ProcessAdapter}.
   *
   * @param processAdapter {@link ProcessAdapter} representing the process to terminate on shutdown.
   * @see org.cp.elements.process.ProcessAdapter#getProcess()
   * @see #registerShutdownHook(Process)
   */
  public static void registerShutdownHook(ProcessAdapter processAdapter) {
    registerShutdownHook(processAdapter.getProcess());
  }

  /**
   * Searches the given {@link File} path for a {@literal .pid} {@link File}.
   *
   * If the given {@link File} is a directory then the directory is recursively searched (depth-first) until the first
   * {@literal .pid} {@link File} is found.  If the algorithm exhausts its search and no {@literal .pid} {@link File}
   * can be found, then {@literal null} is returned.
   *
   * If the given {@link File} is a file then the file's containing directory is used as the base directory
   * for the search and the algorithm continues as described above.
   *
   * @param path base {@link File} path to begin a search for a {@literal .pid} {@link File}.
   * @return the first {@literal .pid} {@link File} found or {@literal null} if no {@literal .pid} {@link File}
   * could be found starting from the given {@link File path} until the search is exhausted.
   * @throws IllegalArgumentException if the given {@link File} path is {@literal null} or does not exist.
   * @see java.io.File
   */
  @SuppressWarnings("all")
  public static File findPidFile(File path) {
    Assert.isTrue(FileSystemUtils.isExisting(path),
      "The path [%s] to search for the .pid file must not be null and must actually exist", path);

    File searchDirectory = (path.isDirectory() ? path : path.getParentFile());

    for (File file : searchDirectory.listFiles(DIRECTORY_PID_FILE_FILTER)) {
      if (file.isDirectory()) {
        file = findPidFile(file);
      }

      if (PID_FILE_FILTER.accept(file)) {
        return file;
      }
    }

    return null;
  }

  /**
   * Reads the process ID (pid) from the given {@link File}.
   *
   * @param pid {@link File} containing the process ID (pid) to read.
   * @return the process ID (pid) stored in the given {@link File}.
   * @throws PidUnknownException if the process ID (pid) could not be read from the given {@link File}.
   * @throws NumberFormatException if the contents of the given {@link File} is not a valid process ID (pid).
   * @see java.io.File
   * @see #writePid(int)
   */
  public static int readPid(File pid) {
    try {
      return Integer.parseInt(FileSystemUtils.read(pid));
    }
    catch (IOException | IllegalArgumentException | IllegalStateException e) {
      throw new PidUnknownException(String.format("Failed to read Process ID (PID) from file [%s]", pid), e);
    }
  }

  /**
   * Writes the given process ID ({@code pid}) to a {@literal .pid} {@link File}.
   *
   * @param pid process ID to store in the {@literal .pid} {@link File}.
   * @return a {@link File} containing the given process ID ({@code pid}).
   * @throws IOException if the process ID (pid) could not be stored in the {@link File}.
   * @see java.io.File
   * @see #readPid(File)
   */
  public static File writePid(int pid) throws IOException {
    File pidFile = FileSystemUtils.newFile(PROCESS_ID_FILENAME);

    pidFile.deleteOnExit();

    PrintWriter writer = newPrintWriter(pidFile);

    try {
      writer.print(pid);
      writer.flush();

      return pidFile;
    }
    finally {
      FileSystemUtils.close(writer);
    }
  }

  /* (non-Javadoc) */
  private static PrintWriter newPrintWriter(File file) throws IOException {
    return new PrintWriter(new BufferedWriter(new FileWriter(file, false), 32), true);
  }

  /* (non-Javadoc) */
  enum VirtualMachineAccessor {
    INSTANCE;

    public boolean isRunning(int processId) {
      return (ObjectUtils.isPresent(VIRTUAL_MACHINE_CLASS_NAME) && doIsRunning(processId));
    }

    protected boolean doIsRunning(int processId) {
      for (VirtualMachineDescriptor vmDescriptor : VirtualMachine.list()) {
        if (String.valueOf(processId).equals(vmDescriptor.id())) {
          return true;
        }
      }

      return false;
    }
  }
}
