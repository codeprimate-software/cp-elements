/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.cp.elements.process;

import java.io.File;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.util.ArrayUtils;

/**
 * The ProcessExecutor interface defines a contract for executing operating system programs.
 *
 * @author John Blum
 * @see java.lang.Process
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ProcessExecutor {

  /**
   * Executes the program defined by the given command-line in the given {@link File directory}.
   *
   * @param directory {@link File directory} in which the program will run.
   * @param commandLine array of {@link String} values constituting the program and it's runtime arguments.
   * @return a {@link Process} object representing the running program.
   * @see java.io.File
   * @see java.lang.Process
   */
  Process execute(File directory, String... commandLine);

  /**
   * Executes the program defined by the given command-line in the current working {@link File directory}.
   *
   * @param commandLine array of {@link String} values constituting the program and it's runtime arguments.
   * @return a {@link Process} object representing the running program.
   * @see #execute(File, String...)
   * @see java.lang.Process
   */
  default Process execute(String... commandLine) {
    return execute(FileSystemUtils.WORKING_DIRECTORY, commandLine);
  }

  /**
   * Executes the program defined by the given command-line in the current working {@link File directory}.
   *
   * @param commandLine {@link Iterable} of {@link String} values constituting the program and it's runtime arguments.
   * @return a {@link Process} object representing the running program.
   * @see #execute(File, String...)
   * @see java.lang.Process
   */
  default Process execute(Iterable<String> commandLine) {
    return execute(FileSystemUtils.WORKING_DIRECTORY, ArrayUtils.asArray(commandLine, String.class));
  }

  /**
   * Executes the program defined by the given command-line in the given {@link File directory}.
   *
   * @param directory {@link File directory} in which the program will run.
   * @param commandLine {@link Iterable} of {@link String} values constituting the program and it's runtime arguments.
   * @return a {@link Process} object representing the running program.
   * @see #execute(File, String...)
   * @see java.io.File
   * @see java.lang.Process
   */
  default Process execute(File directory, Iterable<String> commandLine) {
    return execute(directory, ArrayUtils.asArray(commandLine, String.class));
  }
}
