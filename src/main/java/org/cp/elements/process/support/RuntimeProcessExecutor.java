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

package org.cp.elements.process.support;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.process.ProcessExecutionException;
import org.cp.elements.process.ProcessExecutor;

/**
 * The {@link RuntimeProcessExecutor} class is a {@link ProcessExecutor} using the {@link Runtime} API
 * to configure and execute a running Operating System (OS) programs, returning a reference
 * to the running OS {@link Process}.
 *
 * @author John Blum
 * @see java.lang.Process
 * @see java.lang.Runtime
 * @see org.cp.elements.process.ProcessExecutor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class RuntimeProcessExecutor implements ProcessExecutor {

  /**
   * Uses the Java {@link Runtime} to execute the program defined by the given {@code commandLine}
   * in the given {@link File directory}.
   *
   * @param directory {@link File directory} in which the program will run.
   * @param commandLine array of {@link String} values constituting the program and it's runtime arguments.
   * @return a {@link Process} object representing the running program.
   * @throws IllegalArgumentException if the {@link File} reference is not a valid, existing directory
   * or the command-line is unspecified.
   * @see java.lang.Process
   * @see java.io.File
   */
  @Override
  public Process execute(File directory, String... commandLine) {

    Assert.isTrue(FileSystemUtils.isDirectory(directory), "[%s] is not a valid directory", directory);

    Assert.notEmpty(commandLine, "The command-line [%s] must contain at least 1 command",
      Arrays.toString(commandLine));

    try {
      return Runtime.getRuntime().exec(commandLine, StringUtils.EMPTY_STRING_ARRAY, directory);
    }
    catch (IOException e) {
      throw new ProcessExecutionException(String.format("Failed to execute program [%1$s] in directory [%2$s]",
        Arrays.toString(commandLine), directory), e);
    }
  }
}
