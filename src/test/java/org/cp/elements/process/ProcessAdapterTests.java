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

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.process.ProcessAdapter.newProcessAdapter;
import static org.cp.elements.process.ProcessContext.newProcessContext;
import static org.cp.elements.util.ArrayUtils.asArray;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collections;

import com.sun.tools.doclets.internal.toolkit.util.DocFinder;

import org.cp.elements.io.FileExtensionFilter;
import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.util.Environment;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * Unit tests for {@link ProcessAdapter}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.process.ProcessAdapter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ProcessAdapterTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Mock
  private Process mockProcess;

  private ProcessContext processContext;

  @AfterClass
  public static void tearDown() {
    FileSystemUtils.deleteRecursive(FileSystemUtils.WORKING_DIRECTORY, new FileExtensionFilter(".pid"));
  }

  @Before
  public void setup() {
    processContext = newProcessContext(mockProcess);
  }

  @Test
  public void newProcessAdapterWithProcess() {
    ProcessAdapter processAdapter = newProcessAdapter(mockProcess);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(mockProcess);

    ProcessContext processContext = processAdapter.getProcessContext();

    assertThat(processContext).isInstanceOf(ProcessContext.class);
    assertThat(processContext).isNotSameAs(this.processContext);
    assertThat(processContext.getCommandLine()).isEmpty();
    assertThat(processContext.getDirectory()).isEqualTo(FileSystemUtils.WORKING_DIRECTORY);
    assertThat(processContext.getEnvironment()).isEqualTo(Environment.fromEnvironmentVariables());
    assertThat(processContext.getProcess()).isSameAs(mockProcess);
    assertThat(processContext.getUsername()).isEqualTo(SystemUtils.USERNAME);
    assertThat(processContext.isRedirectingErrorStream()).isFalse();
  }

  @Test
  public void newProcessAdapterWithProcessAndProcessContext() {
    ProcessAdapter processAdapter = newProcessAdapter(mockProcess, processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(mockProcess);
    assertThat(processAdapter.getProcessContext()).isSameAs(processContext);
  }

  @Test
  public void newProcessAdapterWithNullProcess() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Process cannot be null");

    newProcessAdapter(null, processContext);
  }

  @Test
  public void newProcessAdapterWithNullProcessContext() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("ProcessContext cannot be null");

    newProcessAdapter(mockProcess, null);
  }

  @Test
  public void isAliveForRunningProcessIsTrue() {
    when(mockProcess.isAlive()).thenReturn(true);

    assertThat(newProcessAdapter(mockProcess).isAlive()).isTrue();

    verify(mockProcess, times(1)).isAlive();
  }

  @Test
  public void isAliveForTerminatedProcessIsFalse() {
    when(mockProcess.isAlive()).thenReturn(true);

    assertThat(newProcessAdapter(mockProcess).isAlive()).isTrue();

    verify(mockProcess, times(1)).isAlive();
  }

  @Test
  public void isInitializedBeforeInitIsFalse() {
    assertThat(newProcessAdapter(mockProcess).isInitialized()).isFalse();
  }

  @Test
  public void isInitializedAfterInitIsTrue() {
    processContext.inheritIO(true);

    ProcessAdapter processAdapter = newProcessAdapter(mockProcess, processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.isInitialized()).isFalse();
    assertThat(processContext.inheritsIO()).isTrue();

    processAdapter.init();

    assertThat(processAdapter.isInitialized()).isTrue();
  }

  @Test
  public void isRunningForRunningProcessIsTrue() {
    when(mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running"));

    assertThat(newProcessAdapter(mockProcess).isRunning()).isTrue();

    verify(mockProcess, times(1)).exitValue();
  }

  @Test
  public void isRunningForStoppedProcessIsFalse() {
    when(mockProcess.exitValue()).thenReturn(0);

    assertThat(newProcessAdapter(mockProcess).isRunning()).isFalse();

    verify(mockProcess, times(1)).exitValue();
  }

  @Test
  public void getCommandLineReturnsProcessCommand() {
    String[] commandLine = asArray("java", "-server", "-ea", "--classpath", "/path/to/application.jar", "example.Application");

    processContext.ranWith(commandLine);

    assertThat(newProcessAdapter(mockProcess, processContext).getCommandLine()).isEqualTo(asList(commandLine));
  }

  @Test
  public void getDirectoryReturnsProcessWorkingDirectory() {
    processContext.ranIn(FileSystemUtils.USER_HOME_DIRECTORY);

    assertThat(newProcessAdapter(mockProcess, processContext).getDirectory())
      .isEqualTo(FileSystemUtils.USER_HOME_DIRECTORY);
  }

  @Test
  public void getEnvironmentReturnsProcessEnvironment() {
    Environment environment = Environment.from(Collections.singletonMap("testVariable", "testValue"));

    processContext.using(environment);

    assertThat(newProcessAdapter(mockProcess, processContext).getEnvironment()).isEqualTo(environment);
  }

  @Test
  public void getIdReturnProcessId() throws IOException {
    File testPid = File.createTempFile("test", ".pid", FileSystemUtils.WORKING_DIRECTORY);

    testPid.deleteOnExit();
    FileSystemUtils.write(new ByteArrayInputStream("112358".getBytes()), testPid);
    processContext.ranIn(testPid.getParentFile());

    ProcessAdapter processAdapter = newProcessAdapter(mockProcess, processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getDirectory()).isEqualTo(testPid.getParentFile());
    assertThat(processAdapter.getId()).isEqualTo(112358);
    assertThat(processAdapter.safeGetId()).isEqualTo(112358);
  }

  @Test
  public void safeGetIdForNonExistingPidFileHandlesPidUnknownExceptionAndReturnsMinusOne() {
    processContext.ranIn(FileSystemUtils.WORKING_DIRECTORY);

    assertThat(newProcessAdapter(mockProcess, processContext).safeGetId()).isEqualTo(-1);
  }

  @Test
  public void getIdThrowsPidUnknownException() {
    exception.expect(PidUnknownException.class);
    exception.expectCause(is(notNullValue(Throwable.class)));
    exception.expectMessage("Failed to read Process ID (PID) from file [null]");

    newProcessAdapter(mockProcess).getId();
  }

  @Test
  public void setIdThrowsUnsupportedSupportedException() {
    exception.expect(UnsupportedOperationException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(Constants.OPERATION_NOT_SUPPORTED);

    newProcessAdapter(mockProcess).setId(123);
  }

  @Test
  public void getStandardErrorStream() {
    InputStream mockErrorStream = mock(InputStream.class, "Standard Error Stream");

    when(mockProcess.getErrorStream()).thenReturn(mockErrorStream);

    assertThat(newProcessAdapter(mockProcess).getStandardErrorStream()).isSameAs(mockErrorStream);

    verify(mockProcess, times(1)).getErrorStream();
    verify(mockProcess, never()).getInputStream();
    verify(mockProcess, never()).getOutputStream();
  }

  @Test
  public void getStandardInStream() {
    OutputStream mockOutputStream = mock(OutputStream.class, "Standard In Stream");

    when(mockProcess.getOutputStream()).thenReturn(mockOutputStream);

    assertThat(newProcessAdapter(mockProcess).getStandardInStream()).isSameAs(mockOutputStream);

    verify(mockProcess, times(1)).getOutputStream();
    verify(mockProcess, never()).getErrorStream();
    verify(mockProcess, never()).getInputStream();
  }

  @Test
  public void getStandardOutStream() {
    InputStream mockInputStream = mock(InputStream.class, "Standard Out Stream");

    when(mockProcess.getInputStream()).thenReturn(mockInputStream);

    assertThat(newProcessAdapter(mockProcess).getStandardOutStream()).isSameAs(mockInputStream);

    verify(mockProcess, times(1)).getInputStream();
    verify(mockProcess, never()).getErrorStream();
    verify(mockProcess, never()).getOutputStream();
  }

  @Test
  public void getUsernameReturnsNameOfUserUsedToRunProcess() {
    processContext.ranBy("jblum");

    assertThat(newProcessAdapter(mockProcess, processContext).getUsername()).isEqualTo("jblum");
  }
}
