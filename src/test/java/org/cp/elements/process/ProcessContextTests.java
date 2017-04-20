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

package org.cp.elements.process;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.io.FileUtils.newFile;
import static org.cp.elements.process.ProcessContext.newProcessContext;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.util.Environment;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link ProcessContext}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ProcessContextTests extends AbstractBaseTestSuite {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Mock
  private File mockFile;

  @Mock
  private Process mockProcess;

  @Test
  public void newProcessContextWithNonNullProcessIsSuccessful() {
    ProcessContext processContext = newProcessContext(this.mockProcess);

    assertThat(processContext).isNotNull();
    assertThat(processContext.getProcess()).isSameAs(this.mockProcess);
    assertThat(processContext.getCommandLine()).isEmpty();
    assertThat(processContext.getDirectory()).isNull();
    assertThat(processContext.getEnvironment()).isNull();
    assertThat(processContext.getError()).isNull();
    assertThat(processContext.getInput()).isNull();
    assertThat(processContext.getOutput()).isNull();
    assertThat(processContext.getUsername()).isNull();
    assertThat(processContext.inheritsIO()).isFalse();
    assertThat(processContext.isRedirectingErrorStream()).isFalse();
  }

  @Test
  public void newProcessContextWithNullProcessThrowsIllegalArgumentException() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Process cannot be null");

    newProcessContext(null);
  }

  @Test
  public void constructFullyInitializedProcessContext() {
    ProcessContext processContext = new ProcessContext(this.mockProcess)
      .inheritIO(true)
      .ranBy("mockUser")
      .ranIn(FileSystemUtils.WORKING_DIRECTORY)
      .ranWith("java", "-server", "example.Application")
      .redirectError(ProcessBuilder.Redirect.to(this.mockFile))
      .redirectErrorStream(true)
      .redirectInput(ProcessBuilder.Redirect.from(this.mockFile))
      .redirectOutput(ProcessBuilder.Redirect.appendTo(this.mockFile))
      .usingEnvironmentVariables();

    assertThat(processContext).isNotNull();
    assertThat(processContext.getProcess()).isSameAs(this.mockProcess);
    assertThat(processContext.getCommandLine()).isEqualTo(Arrays.asList("java", "-server", "example.Application"));
    assertThat(processContext.getDirectory()).isEqualTo(FileSystemUtils.WORKING_DIRECTORY);
    assertThat(processContext.getEnvironment()).isInstanceOf(Environment.class);
    assertThat(processContext.getError().type()).isEqualTo(ProcessBuilder.Redirect.Type.WRITE);
    assertThat(processContext.getInput().type()).isEqualTo(ProcessBuilder.Redirect.Type.READ);
    assertThat(processContext.getOutput().type()).isEqualTo(ProcessBuilder.Redirect.Type.APPEND);
    assertThat(processContext.getUsername()).isEqualTo("mockUser");
    assertThat(processContext.inheritsIO()).isTrue();
    assertThat(processContext.isRedirectingErrorStream()).isTrue();
  }

  @Test
  public void fromProcessBuilderIsSuccessful() {
    ProcessBuilder processBuilder = new ProcessBuilder("java", "-server", "-ea", "-classpath",
      "/class/path/to/application.jar", "example.Application");

    processBuilder.directory(FileSystemUtils.USER_HOME_DIRECTORY);
    processBuilder.environment().clear();
    processBuilder.environment().put("testKey", "testValue");
    processBuilder.inheritIO();
    processBuilder.redirectError(this.mockFile);
    processBuilder.redirectErrorStream(true);
    processBuilder.redirectInput(this.mockFile);
    processBuilder.redirectOutput(this.mockFile);

    ProcessContext processContext = newProcessContext(this.mockProcess);

    assertThat(processContext).isNotNull();
    assertThat(processContext.from(processBuilder)).isSameAs(processContext);
    assertThat(processContext.getCommandLine()).isEqualTo(Arrays.asList("java", "-server", "-ea", "-classpath",
      "/class/path/to/application.jar", "example.Application"));
    assertThat(processContext.getDirectory()).isEqualTo(FileSystemUtils.USER_HOME_DIRECTORY);
    assertThat(processContext.getEnvironment()).isEqualTo(Environment.from(
      Collections.singletonMap("testKey", "testValue")));
    assertThat(processContext.getError()).isEqualTo(ProcessBuilder.Redirect.to(this.mockFile));
    assertThat(processContext.getInput()).isEqualTo(ProcessBuilder.Redirect.from(this.mockFile));
    assertThat(processContext.getOutput()).isEqualTo(ProcessBuilder.Redirect.to(this.mockFile));
    assertThat(processContext.getProcess()).isSameAs(this.mockProcess);
    assertThat(processContext.getUsername()).isSameAs(SystemUtils.USERNAME);
    assertThat(processContext.inheritsIO()).isFalse();
    assertThat(processContext.isRedirectingErrorStream()).isTrue();
  }

  @Test
  public void toProcessBuilderIsSuccessful() {
    ProcessBuilder processBuilder = new ProcessBuilder();

    ProcessContext processContext = newProcessContext(this.mockProcess)
      .ranBy(SystemUtils.USERNAME)
      .ranIn(FileSystemUtils.USER_HOME_DIRECTORY)
      .ranWith("java", "-server", "-ea", "-classpath", "/class/path/to/application.jar", "example.Application")
      .redirectError(ProcessBuilder.Redirect.PIPE)
      .redirectErrorStream(true)
      .redirectInput(ProcessBuilder.Redirect.INHERIT)
      .redirectOutput(ProcessBuilder.Redirect.PIPE)
      .usingEnvironmentVariables();

    assertThat(processContext).isNotNull();
    assertThat(processContext.to(processBuilder)).isSameAs(processContext);
    assertThat(processBuilder.command()).isEqualTo(processContext.getCommandLine());
    assertThat(processBuilder.directory()).isEqualTo(processContext.getDirectory());
    assertThat(processBuilder.environment()).isEqualTo(processContext.getEnvironment().toMap());
    assertThat(processBuilder.redirectErrorStream()).isTrue();
    assertThat(processBuilder.redirectError()).isEqualTo(processContext.getError());
    assertThat(processBuilder.redirectInput()).isEqualTo(processContext.getInput());
    assertThat(processBuilder.redirectOutput()).isEqualTo(processContext.getOutput());
  }

  @Test
  public void setInDirectoryToDirectory() {
    assertThat(newProcessContext(this.mockProcess).ranIn(FileSystemUtils.USER_HOME_DIRECTORY).getDirectory())
      .isEqualTo(FileSystemUtils.USER_HOME_DIRECTORY);
  }

  @Test
  public void setInDirectoryToFile() {
    File processContextJava = getLocation(ProcessContext.class);

    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(String.format("[%s] must be a valid directory", processContextJava.getAbsolutePath()));

    newProcessContext(this.mockProcess).ranIn(processContextJava);
  }

  @Test
  public void setInDirectoryToNonExistingDirectory() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[/absolute/path/to/non/existing/directory] must be a valid directory");

    newProcessContext(this.mockProcess).ranIn(newFile("/absolute/path/to/non/existing/directory"));
  }

  @Test
  public void setInDirectoryToNullDirectory() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[null] must be a valid directory");

    newProcessContext(this.mockProcess).ranIn(null);
  }
}
