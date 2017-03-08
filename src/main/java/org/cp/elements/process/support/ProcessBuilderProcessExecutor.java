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

import static org.cp.elements.lang.ObjectUtils.defaultIfNull;
import static org.cp.elements.process.ProcessAdapter.newProcessAdapter;
import static org.cp.elements.process.ProcessContext.newProcessContext;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.process.ProcessAdapter;
import org.cp.elements.process.ProcessContext;
import org.cp.elements.process.ProcessExecutionException;
import org.cp.elements.process.ProcessExecutor;
import org.cp.elements.util.Environment;

/**
 * The {@link ProcessBuilderProcessExecutor} class is a {@link ProcessExecutor} using the {@link ProcessBuilder} API
 * to configure and execute (start) a running Operating System (OS) program, returning a reference
 * to the running OS {@link Process}.
 *
 * @author John Blum
 * @see java.lang.Process
 * @see java.lang.ProcessBuilder
 * @see org.cp.elements.process.ProcessExecutor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ProcessBuilderProcessExecutor implements ProcessExecutor<ProcessAdapter> {

  /**
   * Factory method to construct a new instance of {@link ProcessBuilderProcessExecutor} used to execute and run
   * a {@link Process} using Java's {@link ProcessBuilder} API.
   *
   * @return a new instance of {@link ProcessBuilderProcessExecutor} to execute and run {@link Process Processes}.
   * @see org.cp.elements.process.support.ProcessBuilderProcessExecutor
   */
  public static ProcessBuilderProcessExecutor newProcessBuilderProcessExecutor() {
    return new ProcessBuilderProcessExecutor();
  }

  private Environment environment;

  /**
   * Uses the Java {@link ProcessBuilder} to execute the program defined by the given {@code commandLine}
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
  public ProcessAdapter execute(File directory, String... commandLine) {

    Assert.isTrue(FileSystemUtils.isDirectory(directory), "[%s] is not a valid directory", directory);

    Assert.notEmpty(commandLine, "The command-line [%s] must contain at least 1 command",
      Arrays.toString(commandLine));

    try {
      Environment environment = getEnvironment();

      ProcessBuilder processBuilder = newProcessBuilder(commandLine, directory, environment);

      Process process = doExecute(processBuilder);

      ProcessContext processContext = newProcessContext(process)
        .ranBy(SystemUtils.USERNAME)
        .ranIn(processBuilder.directory())
        .ranWith(processBuilder.command())
        .using(Environment.from(processBuilder.environment()));

      return newProcessAdapter(process, processContext);
    }
    catch (IOException e) {
      throw new ProcessExecutionException(String.format("Failed to execute program [%1$s] in directory [%2$s]",
        Arrays.toString(commandLine), directory), e);
    }
  }

  protected Process doExecute(ProcessBuilder processBuilder) throws IOException {
    return processBuilder.start();
  }

  protected ProcessBuilder newProcessBuilder(String[] commandLine, File directory, Environment environment) {
    ProcessBuilder processBuilder = new ProcessBuilder(commandLine).directory(directory);

    processBuilder.environment().clear();
    processBuilder.environment().putAll(environment.toMap());

    return processBuilder;
  }

  protected Environment getEnvironment() {
    return defaultIfNull(this.environment, Environment.fromEnvironmentVariables());
  }

  public ProcessBuilderProcessExecutor using(Environment environment) {
    this.environment = environment;
    return this;
  }
}
