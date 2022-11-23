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
package org.cp.elements.process.support;

import static org.cp.elements.io.FileUtils.isDirectory;
import static org.cp.elements.process.ProcessAdapter.newProcessAdapter;
import static org.cp.elements.process.ProcessContext.newProcessContext;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Optional;

import org.cp.elements.context.env.Environment;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.process.ProcessAdapter;
import org.cp.elements.process.ProcessContext;
import org.cp.elements.process.ProcessExecutionException;
import org.cp.elements.process.ProcessExecutor;

/**
 * The {@link RuntimeProcessExecutor} class is a {@link ProcessExecutor} using the {@link Runtime} API
 * to configure and execute a running Operating System (OS) programs, returning a reference
 * to the running OS {@link Process}.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.lang.Process
 * @see java.lang.Runtime
 * @see org.cp.elements.process.ProcessAdapter
 * @see org.cp.elements.process.ProcessContext
 * @see org.cp.elements.process.ProcessExecutor
 * @see Environment
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class RuntimeProcessExecutor implements ProcessExecutor<ProcessAdapter> {

  /**
   * Factory method to construct a new instance of {@link RuntimeProcessExecutor} used to execute and run
   * a {@link Process} with {@link Runtime#exec(String[], String[], File)}.
   *
   * @return a new instance of {@link RuntimeProcessExecutor}.
   * @see org.cp.elements.process.support.RuntimeProcessExecutor
   */
  public static RuntimeProcessExecutor newRuntimeProcessExecutor() {
    return new RuntimeProcessExecutor();
  }

  private Environment environment;

  /**
   * Uses the Java {@link Runtime} to execute the program defined by the given {@code commandLine}
   * in the given {@link File directory}.
   *
   * @param directory {@link File directory} in which the program will run.
   * @param commandLine array of {@link String} values constituting the program and its runtime arguments.
   * @return a {@link Process} object representing the running program.
   * @throws IllegalArgumentException if the {@link File} reference is not a valid, existing directory
   * or the command-line is unspecified.
   * @throws ProcessExecutionException if the {@link Process} fails to start.
   * @see #doExecute(String[], File, Environment)
   * @see org.cp.elements.process.ProcessAdapter
   * @see org.cp.elements.process.ProcessContext
   * @see java.lang.Process
   * @see java.io.File
   */
  @Override
  public ProcessAdapter execute(File directory, String... commandLine) {

    Assert.isTrue(isDirectory(directory), "[%s] is not a valid directory", directory);

    Assert.notEmpty(commandLine, "The command-line %s must contain at least 1 command",
      Arrays.toString(commandLine));

    try {
      Environment environment = getEnvironment();

      Process process = doExecute(commandLine, directory, environment);

      ProcessContext processContext = newProcessContext(process)
        .ranBy(SystemUtils.USERNAME)
        .ranIn(directory)
        .ranWith(commandLine)
        .using(environment);

      return newProcessAdapter(process, processContext);
    }
    catch (IOException e) {
      throw new ProcessExecutionException(String.format("Failed to execute program %1$s in directory [%2$s]",
        Arrays.toString(commandLine), directory), e);
    }
  }

  /**
   * Executes the {@code commandLine} with the Java {@link Runtime} class in the given {@link File directory}
   * with the provided {@link Environment} configuration.
   *
   * @param commandLine array of {@link String} values constituting the program and its runtime arguments.
   * @param directory {@link File directory} in which the program will run.
   * @param environment {@link Environment} configuration to set when the program runs.
   * @return a {@link Process} object representing the running program.
   * @throws IOException if the {@link Process} cannot be started.
   * @see java.lang.Runtime#exec(String, String[], File)
   * @see java.lang.Process
   */
  protected Process doExecute(String[] commandLine, File directory, Environment environment) throws IOException {
    return Runtime.getRuntime().exec(commandLine, environment.toAssociativeArray(), directory);
  }

  /**
   * Null-safe operation to get a reference to the configured {@link Environment} used when running the program.
   *
   * @return a reference to the {@link Environment} configuration used when running the program.
   * If the configured {@link Environment} is {@literal null}, then this method
   * returns {@link Environment#fromEnvironmentVariables()}.
   * @see Environment
   */
  @NullSafe
  protected Environment getEnvironment() {
    return Optional.ofNullable(this.environment).orElseGet(Environment::fromEnvironmentVariables);
  }

  /**
   * Sets the {@link Environment} configuration used when running the program.
   *
   * @param environment {@link Environment} configuration used when running the program.
   * @return this {@link RuntimeProcessExecutor}.
   * @see Environment
   */
  public RuntimeProcessExecutor using(Environment environment) {
    this.environment = environment;
    return this;
  }
}
