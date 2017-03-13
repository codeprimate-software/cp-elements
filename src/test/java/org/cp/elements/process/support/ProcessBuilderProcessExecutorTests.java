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

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.process.support.ProcessBuilderProcessExecutor.newProcessBuilderProcessExecutor;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.process.ProcessAdapter;
import org.cp.elements.process.ProcessExecutionException;
import org.cp.elements.util.Environment;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * Unit tests for {@link ProcessBuilderProcessExecutor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.cp.elements.process.support.ProcessBuilderProcessExecutor
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ProcessBuilderProcessExecutorTests {

  @Mock
  private Process mockProcess;

  @Test
  public void newProcessBuilderProcessExecutorIsNotNull() {
    ProcessBuilderProcessExecutor processExecutor = newProcessBuilderProcessExecutor();

    assertThat(processExecutor).isNotNull();
  }

  @Test
  public void executeIsSuccessful() {
    ProcessBuilderProcessExecutor processExecutor = new ProcessBuilderProcessExecutor() {
      @Override
      protected Process doExecute(ProcessBuilder processBuilder) throws IOException {
        return ProcessBuilderProcessExecutorTests.this.mockProcess;
      }
    };

    String[] expectedCommandLine = { "java", "-server", "-ea", "-classpath",
      "/class/path/to/application.jar", "test.Application" };

    ProcessAdapter process = processExecutor.execute(FileSystemUtils.USER_HOME_DIRECTORY, expectedCommandLine);

    assertThat(process).isNotNull();
    assertThat(process.getProcess()).isSameAs(this.mockProcess);
    assertThat(process.getProcessContext()).isNotNull();
    assertThat(process.getProcessContext().getCommandLine()).isEqualTo(Arrays.asList(expectedCommandLine));
    assertThat(process.getProcessContext().getDirectory()).isEqualTo(FileSystemUtils.USER_HOME_DIRECTORY);
    assertThat(process.getProcessContext().getEnvironment()).isEqualTo(Environment.fromEnvironmentVariables());
    assertThat(process.getProcessContext().getError()).isEqualTo(ProcessBuilder.Redirect.PIPE);
    assertThat(process.getProcessContext().getInput()).isEqualTo(ProcessBuilder.Redirect.PIPE);
    assertThat(process.getProcessContext().getOutput()).isEqualTo(ProcessBuilder.Redirect.PIPE);
    assertThat(process.getProcessContext().getUsername()).isEqualTo(SystemUtils.USERNAME);
    assertThat(process.getProcessContext().inheritsIO()).isFalse();
    assertThat(process.getProcessContext().isRedirectingErrorStream()).isFalse();
  }

  @Test(expected = IllegalArgumentException.class)
  public void executeWithInvalidDirectoryThrowsIllegalArgumentException() {
    try {
      newProcessBuilderProcessExecutor().execute(new File("/path/to/non/existing/directory"),
        "java", "-server", "-ea", "-classpath", "/class/path/to/application.jar", "test.Application");
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("[/path/to/non/existing/directory] is not a valid directory");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void executeWithNoCommandThrowsIllegalArgumentException() {
    try {
      newProcessBuilderProcessExecutor().execute(FileSystemUtils.WORKING_DIRECTORY);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("The command-line [] must contain at least 1 command");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ProcessExecutionException.class)
  public void executeFailedProcessHandlesIOException() {
    ProcessBuilderProcessExecutor processExecutor = new ProcessBuilderProcessExecutor() {
      @Override
      protected Process doExecute(ProcessBuilder processBuilder) throws IOException {
        throw new IOException("test");
      }
    };

    String[] expectedCommandLine = { "java", "-server", "-ea", "-classpath",
      "/class/path/to/application.jar", "test.Application" };

    try {
      processExecutor.execute(FileSystemUtils.WORKING_DIRECTORY, expectedCommandLine);
    }
    catch (ProcessExecutionException expected) {
      assertThat(expected).hasMessage("Failed to execute program %1$s in directory [%2$s]",
        Arrays.toString(expectedCommandLine), FileSystemUtils.WORKING_DIRECTORY);
      assertThat(expected).hasCauseInstanceOf(IOException.class);
      assertThat(expected.getCause()).hasMessage("test");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void newProcessBuilderIsInitializedCorrectly() {
    String[] expectedCommandLine = { "java", "-server", "-ea", "-classpath",
      "/class/path/to/application.jar", "test.Application" };

    Environment expectedEnvironment = Environment.from(Collections.singletonMap("testKey", "testValue"));

    ProcessBuilder processBuilder = newProcessBuilderProcessExecutor()
      .newProcessBuilder(expectedCommandLine, FileSystemUtils.USER_HOME_DIRECTORY, expectedEnvironment);

    assertThat(processBuilder).isNotNull();
    assertThat(processBuilder.command()).isEqualTo(Arrays.asList(expectedCommandLine));
    assertThat(processBuilder.directory()).isEqualTo(FileSystemUtils.USER_HOME_DIRECTORY);
    assertThat(processBuilder.environment()).isEqualTo(expectedEnvironment.toMap());
  }

  @Test
  public void usingEnvironmentVariables() {
    assertThat(newProcessBuilderProcessExecutor().getEnvironment()).isEqualTo(Environment.fromEnvironmentVariables());
  }

  @Test
  public void usingProvidedEnvironment() {
    Environment expectedEnvironment = Environment.from(Collections.singletonMap("testKey", "testValue"));
    ProcessBuilderProcessExecutor processExecutor = newProcessBuilderProcessExecutor();

    assertThat(processExecutor).isNotNull();
    assertThat(processExecutor.using(expectedEnvironment)).isSameAs(processExecutor);
    assertThat(processExecutor.getEnvironment()).isEqualTo(expectedEnvironment);
  }
}
