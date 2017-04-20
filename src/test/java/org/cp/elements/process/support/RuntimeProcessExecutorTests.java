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
import static org.cp.elements.process.support.RuntimeProcessExecutor.newRuntimeProcessExecutor;

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
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link RuntimeProcessExecutor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.process.support.RuntimeProcessExecutor
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class RuntimeProcessExecutorTests {

  @Mock
  private Process mockProcess;

  @Test
  public void newRuntimeProcessExecutorIsNotNull() {
    RuntimeProcessExecutor processExecutor = newRuntimeProcessExecutor();

    assertThat(processExecutor).isNotNull();
  }

  @Test
  public void executeIsSuccessful() {
    RuntimeProcessExecutor processExecutor = new RuntimeProcessExecutor() {
      @Override
      protected Process doExecute(String[] commandLine, File directory, Environment environment) throws IOException {
        return RuntimeProcessExecutorTests.this.mockProcess;
      }
    };

    String[] expectedCommandLine = { "java", "-server", "-ea", "-classpath",
      "/class/path/to/application.jar", "example.Application" };

    ProcessAdapter process = processExecutor.execute(FileSystemUtils.USER_HOME_DIRECTORY, expectedCommandLine);

    assertThat(process).isNotNull();
    assertThat(process.getProcess()).isSameAs(this.mockProcess);
    assertThat(process.getProcessContext()).isNotNull();
    assertThat(process.getProcessContext().getCommandLine()).isEqualTo(Arrays.asList(expectedCommandLine));
    assertThat(process.getProcessContext().getDirectory()).isEqualTo(FileSystemUtils.USER_HOME_DIRECTORY);
    assertThat(process.getProcessContext().getEnvironment()).isEqualTo(Environment.fromEnvironmentVariables());
    assertThat(process.getProcessContext().getUsername()).isEqualTo(SystemUtils.USERNAME);
  }

  @Test(expected = IllegalArgumentException.class)
  public void executeWithInvalidDirectoryThrowsIllegalArgumentException() {
    try {
      newRuntimeProcessExecutor().execute(new File("/path/to/non/existing/diretory"),
        "java", "-server", "-ea", "-classpath", "/class/path/to/application.jar", "example.Application");
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("[/path/to/non/existing/diretory] is not a valid directory");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void executeWithNoCommandThrowsIllegalArgumentException() {
    try {
      newRuntimeProcessExecutor().execute(FileSystemUtils.WORKING_DIRECTORY);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("The command-line [] must contain at least 1 command");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ProcessExecutionException.class)
  public void executeFailedProcessHandlesIOException() {
    RuntimeProcessExecutor processExecutor = new RuntimeProcessExecutor() {
      @Override
      protected Process doExecute(String[] commandLine, File directory, Environment environment) throws IOException {
        throw new IOException("test");
      }
    };

    String[] expectedCommandLine = { "java", "-server", "-ea", "-classpath",
      "/class/path/to/application.jar", "example.Application" };

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
  public void usingEnvironmentVariables() {
    assertThat(newRuntimeProcessExecutor().getEnvironment()).isEqualTo(Environment.fromEnvironmentVariables());
  }

  @Test
  public void usingProvidedEnvironment() {
    Environment expectedEnvironment = Environment.from(Collections.singletonMap("testKey", "testValue"));
    RuntimeProcessExecutor processExecutor = newRuntimeProcessExecutor();

    assertThat(processExecutor.using(expectedEnvironment)).isSameAs(processExecutor);
    assertThat(processExecutor.getEnvironment()).isEqualTo(expectedEnvironment);
  }
}
