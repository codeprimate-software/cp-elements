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

package org.cp.elements.process.support;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.process.ProcessExecutionException;
import org.cp.elements.process.ProcessExecutor;
import org.cp.elements.util.ArrayUtils;

/**
 * The ProcessBuilderProcessExecutor class is a {@link ProcessExecutor} that uses the {@link ProcessBuilder} API
 * to configure and execute (start) a running operating system {@link Process}.
 *
 * @author John Blum
 * @see java.lang.ProcessBuilder
 * @see java.lang.Process
 * @see org.cp.elements.process.ProcessExecutor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ProcessBuilderProcessExecutor implements ProcessExecutor {

  @Override
  public Process execute(File directory, String... commandLine) {
    Assert.isTrue(FileSystemUtils.isDirectory(directory), "File [%s] is not valid directory", directory);
    Assert.isTrue(ArrayUtils.isNotEmpty(commandLine), "The command-line [%s] must contain at least 1 command",
      Arrays.toString(commandLine));

    try {
      ProcessBuilder builder = new ProcessBuilder(commandLine);

      builder.directory(directory);

      return builder.start();
    }
    catch (IOException e) {
      throw new ProcessExecutionException(String.format("Failed to execute program [%1$s] in directory [%2$s]",
        Arrays.toString(commandLine), directory), e);
    }
  }
}
