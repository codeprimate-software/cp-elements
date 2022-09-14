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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.process.ProcessAdapter;
import org.cp.elements.process.ProcessContext;
import org.cp.elements.process.ProcessExecutionException;
import org.cp.elements.process.ProcessExecutor;
import org.cp.elements.context.env.Environment;

/**
 * The {@link ProcessBuilderProcessExecutor} class is a {@link ProcessExecutor} using the {@link ProcessBuilder} API
 * to configure and execute (start) a running Operating System (OS) program, returning a reference
 * to the running OS {@link Process}.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.lang.Process
 * @see java.lang.ProcessBuilder
 * @see org.cp.elements.process.ProcessAdapter
 * @see org.cp.elements.process.ProcessContext
 * @see org.cp.elements.process.ProcessExecutor
 * @see Environment
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ProcessBuilderProcessExecutor implements ProcessExecutor<ProcessAdapter> {

  /**
   * Factory method to construct a new instance of {@link ProcessBuilderProcessExecutor} used to execute and run
   * a {@link Process} with Java's {@link ProcessBuilder} API.
   *
   * @return a new instance of {@link ProcessBuilderProcessExecutor} to execute and run {@link Process processes}.
   * @see org.cp.elements.process.support.ProcessBuilderProcessExecutor
   */
  public static ProcessBuilderProcessExecutor newProcessBuilderProcessExecutor() {
    return new ProcessBuilderProcessExecutor();
  }

  private boolean redirectErrorStream;

  private Environment environment;

  private ProcessBuilder.Redirect error;
  private ProcessBuilder.Redirect input;
  private ProcessBuilder.Redirect output;

  /**
   * Uses the Java {@link ProcessBuilder} to execute the program defined by the given {@code commandLine}
   * in the given {@link File directory}.
   *
   * @param directory {@link File directory} in which the program will run.
   * @param commandLine array of {@link String} values constituting the program and its runtime arguments.
   * @return a {@link Process} object representing the running program.
   * @throws IllegalArgumentException if the {@link File} reference is not a valid, existing directory
   * or the command-line is unspecified.
   * @throws ProcessExecutionException if the {@link Process} failed to start.
   * @see #newProcessBuilder(String[], File, Environment)
   * @see #doExecute(ProcessBuilder)
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

      ProcessBuilder processBuilder = newProcessBuilder(commandLine, directory, environment);

      Process process = doExecute(processBuilder);

      ProcessContext processContext = newProcessContext(process).from(processBuilder);

      return newProcessAdapter(process, processContext);
    }
    catch (IOException e) {
      throw new ProcessExecutionException(String.format("Failed to execute program %1$s in directory [%2$s]",
        Arrays.toString(commandLine), directory), e);
    }
  }

  /**
   * Executes the program using the given {@link ProcessBuilder}.
   *
   * @param processBuilder {@link ProcessBuilder} used to execute and run the program.
   * @return a reference to the {@link Process} object representing the running program.
   * @throws IOException if the {@link Process} fails to start.
   * @see java.lang.ProcessBuilder#start()
   * @see java.lang.Process
   */
  protected Process doExecute(ProcessBuilder processBuilder) throws IOException {
    return processBuilder.start();
  }

  /**
   * Constructs, configures and initializes a new instance of {@link ProcessBuilder} used to execute and run programs.
   *
   * The configuration of the {@link ProcessBuilder} is based on this {@link ProcessBuilderProcessExecutor}.
   *
   * @param commandLine array of {@link String} values constituting the program and its runtime arguments.
   * @param directory {@link File directory} in which the program will run.
   * @param environment {@link Environment} configuration used to run the program.
   * @return a new instance of {@link ProcessBuilder} configured and initialized with this
   * {@link ProcessBuilderProcessExecutor} configuration.
   * @see Environment
   * @see java.lang.ProcessBuilder
   * @see java.io.File
   */
  protected ProcessBuilder newProcessBuilder(String[] commandLine, File directory, Environment environment) {
    ProcessBuilder processBuilder = new ProcessBuilder(commandLine);

    processBuilder.directory(directory);
    processBuilder.environment().clear();
    processBuilder.environment().putAll(environment.toMap());
    processBuilder.redirectErrorStream(isRedirectingErrorStream());

    getError().ifPresent(processBuilder::redirectError);
    getIn().ifPresent(processBuilder::redirectInput);
    getOut().ifPresent(processBuilder::redirectOutput);

    return processBuilder;
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
   * Returns the {@link Process Process's} standard error stream {@link ProcessBuilder.Redirect} configuration.
   *
   * @return a {@link ProcessBuilder.Redirect} specifying the destination of the {@link Process} error stream.
   * @see java.lang.Process#getErrorStream()
   * @see java.lang.ProcessBuilder.Redirect
   * @see java.util.Optional
   */
  @NullSafe
  protected Optional<ProcessBuilder.Redirect> getError() {
    return Optional.ofNullable(this.error);
  }

  /**
   * Returns the {@link Process Process's} standard input stream {@link ProcessBuilder.Redirect} configuration.
   *
   * @return a {@link ProcessBuilder.Redirect} specifying the destination of the {@link Process} input stream.
   * @see java.lang.Process#getInputStream()
   * @see java.lang.ProcessBuilder.Redirect
   * @see java.util.Optional
   */
  @NullSafe
  protected Optional<ProcessBuilder.Redirect> getIn() {
    return Optional.ofNullable(this.input);
  }

  /**
   * Returns the {@link Process Process's} standard output stream {@link ProcessBuilder.Redirect} configuration.
   *
   * @return a {@link ProcessBuilder.Redirect} specifying the destination of the {@link Process} output stream.
   * @see java.lang.Process#getOutputStream()
   * @see java.lang.ProcessBuilder.Redirect
   * @see java.util.Optional
   */
  @NullSafe
  protected Optional<ProcessBuilder.Redirect> getOut() {
    return Optional.ofNullable(this.output);
  }

  /**
   * Determines whether the {@link Process Process's} standard error stream is being redirected and merged with
   * the {@link Process Process's} standard output stream.
   *
   * @return a boolean value indicating whether the {@link Process Process's} standard error stream
   * and standard input stream are being merged.
   */
  protected boolean isRedirectingErrorStream() {
    return this.redirectErrorStream;
  }

  /**
   * Configures the destination for a {@link Process Process's} standard error stream.
   *
   * @param error {@link ProcessBuilder.Redirect} indicating the destination for the {@link Process Process's}
   * standard error stream.
   * @return this {@link ProcessBuilderProcessExecutor}.
   * @see java.lang.ProcessBuilder#redirectError(ProcessBuilder.Redirect)
   * @see java.lang.ProcessBuilder.Redirect
   */
  public ProcessBuilderProcessExecutor redirectError(ProcessBuilder.Redirect error) {
    this.error = error;
    return this;
  }

  /**
   * Configures the {@link Process Process's} standard error stream to be redirected and merged with
   * the {@link Process Process's} standard output stream.
   *
   * @return this {@link ProcessBuilderProcessExecutor}.
   * @see java.lang.ProcessBuilder#redirectErrorStream(boolean)
   */
  public ProcessBuilderProcessExecutor redirectErrorStream() {
    this.redirectErrorStream = true;
    return this;
  }

  /**
   * Configures the destination for a {@link Process Process's} standard input  stream.
   *
   * @param in  {@link ProcessBuilder.Redirect} indicating the destination for the {@link Process Process's}
   * standard input stream.
   * @return this {@link ProcessBuilderProcessExecutor}.
   * @see java.lang.ProcessBuilder#redirectInput(ProcessBuilder.Redirect)
   * @see java.lang.ProcessBuilder.Redirect
   */
  public ProcessBuilderProcessExecutor redirectIn(ProcessBuilder.Redirect in) {
    this.input = in;
    return this;
  }

  /**
   * Configures the destination for a {@link Process Process's} standard output stream.
   *
   * @param out {@link ProcessBuilder.Redirect} indicating the destination for the {@link Process Process's}
   * standard output stream.
   * @return this {@link ProcessBuilderProcessExecutor}.
   * @see java.lang.ProcessBuilder#redirectOutput(ProcessBuilder.Redirect)
   * @see java.lang.ProcessBuilder.Redirect
   */
  public ProcessBuilderProcessExecutor redirectOut(ProcessBuilder.Redirect out) {
    this.output = out;
    return this;
  }

  /**
   * Sets the {@link Environment} configuration used when running the program.
   *
   * @param environment {@link Environment} configuration used when running the program.
   * @return this {@link ProcessBuilderProcessExecutor}.
   * @see Environment
   */
  public ProcessBuilderProcessExecutor using(Environment environment) {
    this.environment = environment;
    return this;
  }
}
