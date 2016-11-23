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
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;

import com.sun.tools.attach.VirtualMachine;
import com.sun.tools.attach.VirtualMachineDescriptor;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.process.PidUnknownException;
import org.cp.elements.process.ProcessAdapter;

/**
 * The {@link ProcessUtils} class is an abstract utility class for working with Java {@link Process} objects.
 *
 * @author John J. Blum
 * @see java.lang.Process
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ProcessUtils {

  protected static final String PROCESS_ID_FILENAME = ".pid";

  protected static final String VIRTUAL_MACHINE_CLASS_NAME = "com.sun.tools.attach.VirtualMachine";

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
   * Null-safe method to determine whether the given {@link Process} is still alive, or that the {@lnk Process}
   * has not yet terminated yet.
   *
   * @param process {@link Process} to evaluate if still alive.
   * @return a boolean value indicating whether the given {@link Process} is still alive.
   * @see java.lang.Process#isAlive()
   * @see #isRunning(Process)
   */
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
  @NullSafe
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
      if (process != null) {
        process.exitValue();
      }

      return false;
    }
    catch (IllegalMonitorStateException ignore) {
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

  public static int readPid(File pid) {
    try {
      return Integer.parseInt(FileSystemUtils.read(pid));
    }
    catch (IOException e) {
      throw new PidUnknownException(String.format("Failed to read Process ID (PID) from file [%s]", pid), e);
    }
  }

  public static File writePid(int pid) throws IOException {
    File pidFile = FileSystemUtils.newFile(PROCESS_ID_FILENAME);

    pidFile.deleteOnExit();

    PrintWriter writer = new PrintWriter(new BufferedWriter(new FileWriter(pidFile, false), 32), true);

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
  enum VirtualMachineAccessor {
    INSTANCE;

    public boolean isRunning(int processId) {
      if (ObjectUtils.isPresent("com.sun.tools.attach.VirtualMachineDescriptor")) {
        for (VirtualMachineDescriptor vmDescriptor : VirtualMachine.list()) {
          if (String.valueOf(processId).equals(vmDescriptor.id())) {
            return true;
          }
        }
      }

      return false;
    }
  }
}
