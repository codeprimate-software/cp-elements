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

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.util.Environment;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * Unit tests for {@link ProcessContext}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
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
    ProcessContext processContext = newProcessContext(mockProcess);

    assertThat(processContext).isNotNull();
    assertThat(processContext.getProcess()).isSameAs(mockProcess);
    assertThat(processContext.getCommandLine()).isEmpty();
    assertThat(processContext.getDirectory()).isNull();
    assertThat(processContext.getEnvironment()).isNull();
    assertThat(processContext.getError()).isNull();
    assertThat(processContext.getInput()).isNull();
    assertThat(processContext.getOutput()).isNull();
    assertThat(processContext.isRedirectingErrorStream()).isFalse();
    assertThat(processContext.getUsername()).isNull();
    assertThat(processContext.inheritsIO()).isFalse();
  }

  @Test
  public void newProcessContextWithNullProcessThrowsIllegalArgumentException() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Process cannot be null");

    newProcessContext(null);
  }

  @Test
  public void fullyInitializedProcessContext() {
    ProcessContext processContext = newProcessContext(mockProcess)
      .inheritIO(true)
      .ranBy("mockUser")
      .ranIn(FileSystemUtils.WORKING_DIRECTORY)
      .ranWith("java", "-server", "example.Application")
      .redirectError(ProcessBuilder.Redirect.to(mockFile))
      .redirectErrorStream(true)
      .redirectInput(ProcessBuilder.Redirect.from(mockFile))
      .redirectOutput(ProcessBuilder.Redirect.appendTo(mockFile))
      .usingEnvironmentVariables();

    assertThat(processContext).isNotNull();
    assertThat(processContext.getProcess()).isSameAs(mockProcess);
    assertThat(processContext.getCommandLine()).isEqualTo(Arrays.asList("java", "-server", "example.Application"));
    assertThat(processContext.getDirectory()).isEqualTo(FileSystemUtils.WORKING_DIRECTORY);
    assertThat(processContext.getEnvironment()).isInstanceOf(Environment.class);
    assertThat(processContext.getError().type()).isEqualTo(ProcessBuilder.Redirect.Type.WRITE);
    assertThat(processContext.getInput().type()).isEqualTo(ProcessBuilder.Redirect.Type.READ);
    assertThat(processContext.getOutput().type()).isEqualTo(ProcessBuilder.Redirect.Type.APPEND);
    assertThat(processContext.isRedirectingErrorStream()).isTrue();
    assertThat(processContext.getUsername()).isEqualTo("mockUser");
    assertThat(processContext.inheritsIO()).isTrue();
  }

  @Test
  public void setInDirectoryToFile() {
    File processContextJava = getLocation(ProcessContext.class);

    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(String.format("[%s] must be a valid directory", processContextJava.getAbsolutePath()));

    newProcessContext(mockProcess).ranIn(processContextJava);
  }

  @Test
  public void setInDirectoryToNonExistingDirectory() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[/absolute/path/to/non/existing/directory] must be a valid directory");

    newProcessContext(mockProcess).ranIn(newFile("/absolute/path/to/non/existing/directory"));
  }

  @Test
  public void setInDirectoryToNullDirectory() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("[null] must be a valid directory");

    newProcessContext(mockProcess).ranIn(null);
  }
}
