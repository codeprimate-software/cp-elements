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
package org.cp.elements.process;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.cp.elements.lang.CheckedExceptionsFactory.newIOException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newRuntimeException;
import static org.cp.elements.process.ProcessAdapter.newProcessAdapter;
import static org.cp.elements.process.ProcessContext.newProcessContext;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import org.junit.Before;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.context.env.Environment;
import org.cp.elements.io.FileExtensionFilter;
import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.io.FileUtils;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.process.event.ProcessStreamListener;
import org.cp.elements.process.support.RuntimeProcessExecutor;
import org.cp.elements.util.ArrayUtils;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit Tests for {@link ProcessAdapter}.
 *
 * @author John Blum
 * @see java.lang.Process
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @see org.cp.elements.process.ProcessAdapter
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public class ProcessAdapterTests {

  private static final FileFilter PID_FILE_EXTENSION_FILTER = new FileExtensionFilter(".pid");

  @Mock
  private Process mockProcess;

  private ProcessContext processContext;

  @BeforeAll
  public static void setupBeforeTests() {
    FileSystemUtils.deleteRecursive(FileSystemUtils.TEMPORARY_DIRECTORY, PID_FILE_EXTENSION_FILTER);
  }

  @AfterAll
  @SuppressWarnings("all")
  public static void tearDownAfterTests() {

    FileSystemUtils.deleteRecursive(FileSystemUtils.WORKING_DIRECTORY, PID_FILE_EXTENSION_FILTER);

    Thread.interrupted();
  }

  @BeforeEach
  public void setup() {

    this.processContext = newProcessContext(this.mockProcess)
      .ranBy(SystemUtils.USERNAME)
      .ranIn(FileSystemUtils.WORKING_DIRECTORY);

    this.processContext = spy(this.processContext);
  }

  @Test
  public void newProcessAdapterWithProcess() {

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);

    ProcessContext processContext = processAdapter.getProcessContext();

    assertThat(processContext).isNotNull();
    assertThat(processContext).isNotSameAs(this.processContext);
    assertThat(processContext.getCommandLine()).isEmpty();
    assertThat(processContext.getDirectory()).isEqualTo(FileSystemUtils.WORKING_DIRECTORY);
    assertThat(processContext.getEnvironment()).isEqualTo(Environment.fromEnvironmentVariables());
    assertThat(processContext.getProcess()).isSameAs(this.mockProcess);
    assertThat(processContext.getUsername()).isEqualTo(SystemUtils.USERNAME);
    assertThat(processContext.isRedirectingErrorStream()).isFalse();
    assertThat(processContext.inheritsIO()).isFalse();
    assertThat(processContext.getError()).isNull();
    assertThat(processContext.getInput()).isNull();
    assertThat(processContext.getOutput()).isNull();
  }

  @Test
  public void newProcessAdapterWithProcessAndProcessContext() {

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess, this.processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);
    assertThat(processAdapter.getProcessContext()).isSameAs(this.processContext);
  }

  @Test
  public void newProcessAdapterWithNullProcess() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newProcessAdapter(null, this.processContext))
      .withMessage("Process is required")
      .withNoCause();
  }

  @Test
  public void newProcessAdapterWithNullProcessContext() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newProcessAdapter(this.mockProcess, null))
      .withMessage("ProcessContext is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("all")
  public void constructProcessAdapter() {

    ProcessAdapter processAdapter = new ProcessAdapter(this.mockProcess, this.processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);
    assertThat(processAdapter.getProcessContext()).isSameAs(this.processContext);
    assertThat(processAdapter.isInitialized()).isFalse();

    ThreadGroup resolvedThreadGroup = processAdapter.resolveThreadGroup();

    assertThat(resolvedThreadGroup).isNotNull();
    assertThat(resolvedThreadGroup.getName()).matches(Pattern.compile("Process \\[.*\\] Thread Group"));
  }

  @Test
  public void initStartsBothProcessStandardInAndStandardErrorStreamThreads() {

    InputStream mockStandardOut = mock(InputStream.class, "Process Standard Out");
    InputStream mockStandardError = mock(InputStream.class, "Process Standard Error");

    Runnable mockRunnableOne = mock(Runnable.class, "Process Standard Out Reader Runnable");
    Runnable mockRunnableTwo = mock(Runnable.class, "Process Standard Error Reader Runnable");

    Thread mockThreadOne = mock(Thread.class, "Process Standard Out Reader Thread");
    Thread mockThreadTwo = mock(Thread.class, "Process Standard Error Reader Thread");

    when(this.mockProcess.getInputStream()).thenReturn(mockStandardOut);
    when(this.mockProcess.getErrorStream()).thenReturn(mockStandardError);

    this.processContext.inheritIO(false).redirectErrorStream(false);

    ProcessAdapter processAdapter = spy(newProcessAdapter(this.mockProcess, this.processContext));

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);
    assertThat(processAdapter.getProcessContext()).isSameAs(this.processContext);
    assertThat(processAdapter.isInitialized()).isFalse();

    doReturn(123).when(processAdapter).safeGetId();
    doReturn(mockRunnableOne).when(processAdapter).newProcessStreamReader(eq(mockStandardOut));
    doReturn(mockRunnableTwo).when(processAdapter).newProcessStreamReader(eq(mockStandardError));
    doReturn(mockThreadOne).when(processAdapter)
      .newThread(eq("Process [123] Standard Out Reader"), eq(mockRunnableOne));
    doReturn(mockThreadTwo).when(processAdapter)
      .newThread(eq("Process [123] Standard Error Reader"), eq(mockRunnableTwo));

    processAdapter.init();

    assertThat(processAdapter.isInitialized()).isTrue();

    verify(processAdapter, times(2)).safeGetId();
    verify(processAdapter, times(1)).newProcessStreamReader(eq(mockStandardOut));
    verify(processAdapter, times(1)).newProcessStreamReader(eq(mockStandardError));
    verify(processAdapter, times(1)).newThread(
      eq("Process [123] Standard Out Reader"), eq(mockRunnableOne));
    verify(processAdapter, times(1)).newThread(
      eq("Process [123] Standard Error Reader"), eq(mockRunnableTwo));
    verify(mockThreadOne, times(1)).start();
    verify(mockThreadTwo, times(1)).start();
  }

  @Test
  public void initStartsOnlyProcessStandardInStreamThread() {

    InputStream mockStandardOut = mock(InputStream.class, "Process Standard Out");

    Runnable mockRunnable = mock(Runnable.class, "Process Standard Out Reader Runnable");

    Thread mockThread = mock(Thread.class, "Process Standard Out Reader Thread");

    when(this.mockProcess.getInputStream()).thenReturn(mockStandardOut);

    this.processContext.inheritIO(false).redirectErrorStream(true);

    ProcessAdapter processAdapter = spy(newProcessAdapter(this.mockProcess, this.processContext));

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);
    assertThat(processAdapter.getProcessContext()).isSameAs(this.processContext);
    assertThat(processAdapter.isInitialized()).isFalse();

    doReturn(123).when(processAdapter).safeGetId();
    doReturn(mockRunnable).when(processAdapter).newProcessStreamReader(eq(mockStandardOut));
    doReturn(mockThread).when(processAdapter)
      .newThread(eq("Process [123] Standard Out Reader"), eq(mockRunnable));

    processAdapter.init();

    assertThat(processAdapter.isInitialized()).isTrue();

    verify(processAdapter, times(1)).safeGetId();
    verify(processAdapter, times(1)).newProcessStreamReader(eq(mockStandardOut));
    verify(processAdapter, times(1)).newThread(
      eq("Process [123] Standard Out Reader"), eq(mockRunnable));
    verify(processAdapter, never()).newThread(eq("Process [123] Standard Error Reader"), any(Runnable.class));
    verify(mockThread, times(1)).start();
  }

  @Test
  public void initDoesNotStartAnyProcessStreamThreads() {

    this.processContext.inheritIO(true).redirectErrorStream(false);

    ProcessAdapter processAdapter = spy(newProcessAdapter(this.mockProcess, this.processContext));

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);
    assertThat(processAdapter.getProcessContext()).isSameAs(this.processContext);
    assertThat(processAdapter.isInitialized()).isFalse();

    processAdapter.init();

    assertThat(processAdapter.isInitialized()).isTrue();

    verify(processAdapter, never()).safeGetId();
    verify(processAdapter, never()).newProcessStreamReader(any(InputStream.class));
    verify(processAdapter, never()).newThread(anyString(), any(Runnable.class));
  }

  @Test
  public void newProcessStreamListenerIsInitializedCorrectly() {

    when(this.mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running"));

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);

    StringBuilder bufferOne = new StringBuilder();
    StringBuilder bufferTwo = new StringBuilder();

    assertThat(processAdapter.register(bufferOne::append)).isSameAs(processAdapter);
    assertThat(processAdapter.register(bufferTwo::append)).isSameAs(processAdapter);

    ByteArrayInputStream in = new ByteArrayInputStream("This is the end of the line!\n".getBytes());

    Runnable processStreamReaderRunnable = processAdapter.newProcessStreamReader(in);

    assertThat(processStreamReaderRunnable).isNotNull();

    processStreamReaderRunnable.run();

    assertThat(bufferOne.toString()).isEqualTo("This is the end of the line!");
    assertThat(bufferTwo.toString()).isEqualTo("This is the end of the line!");

    verify(this.mockProcess, times(1)).exitValue();
  }

  @Test
  public void newProcessStreamListenerRunnableIsNotRunWhenProcessIsNotRunning() {

    when(this.mockProcess.exitValue()).thenReturn(0);

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);

    StringBuilder buffer = new StringBuilder();

    assertThat(processAdapter.register(buffer::append)).isSameAs(processAdapter);
    assertThat(buffer.toString()).isEmpty();

    ByteArrayInputStream in =
      new ByteArrayInputStream("Do I deserve to be and if so, if so, who answers, who answers?!".getBytes());

    Runnable processStreamReaderRunnable = processAdapter.newProcessStreamReader(in);

    assertThat(processStreamReaderRunnable).isNotNull();

    processStreamReaderRunnable.run();

    assertThat(buffer.toString()).isEmpty();

    verify(this.mockProcess, times(1)).exitValue();
  }

  @Test
  public void newProcessStreamListenerHandlesIOException() throws IOException {

    BufferedReader mockReader = mock(BufferedReader.class);

    InputStream mockInputStream = mock(InputStream.class);

    ProcessAdapter processAdapter = spy(newProcessAdapter(this.mockProcess));

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);

    when(this.mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running"));
    doReturn(mockReader).when(processAdapter).newReader(eq(mockInputStream));
    when(mockReader.readLine()).thenThrow(newIOException("test"));

    Runnable runnable = processAdapter.newProcessStreamReader(mockInputStream);

    assertThat(runnable).isNotNull();

    runnable.run();

    verify(this.mockProcess, times(1)).exitValue();
    verify(processAdapter, times(1)).newReader(eq(mockInputStream));
    verify(mockReader, times(1)).readLine();
    verify(mockReader, times(1)).close();
  }

  @Test
  public void newReaderIsInitializedCorrectly() {

    InputStream mockInputStream = mock(InputStream.class);

    Reader reader = newProcessAdapter(this.mockProcess).newReader(mockInputStream);

    assertThat(reader).isInstanceOf(BufferedReader.class);

    verifyNoInteractions(mockInputStream);
  }

  @Test
  public void newThreadIsInitializedCorrectly() {

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess);

    Thread testThread = processAdapter.newThread("TestThread", () -> {});

    assertThat(testThread).isNotNull();
    assertThat(testThread.isAlive()).isFalse();
    assertThat(testThread.isDaemon()).isTrue();
    assertThat(testThread.isInterrupted()).isFalse();
    assertThat(testThread.getName()).isEqualTo("TestThread");
    assertThat(testThread.getPriority()).isEqualTo(Thread.NORM_PRIORITY);
    assertThat(testThread.getState()).isEqualTo(Thread.State.NEW);
    assertThat(testThread.getThreadGroup()).isEqualTo(processAdapter.resolveThreadGroup());
  }

  @Test
  public void isAliveForRunningProcessIsTrue() {

    when(this.mockProcess.isAlive()).thenReturn(true);

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess, this.processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);
    assertThat(processAdapter.isAlive()).isTrue();
    assertThat(processAdapter.isNotAlive()).isFalse();

    verify(this.mockProcess, times(2)).isAlive();
  }

  @Test
  public void isAliveForTerminatedProcessIsFalse() {

    when(this.mockProcess.isAlive()).thenReturn(false);

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess, this.processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);
    assertThat(processAdapter.isAlive()).isFalse();
    assertThat(processAdapter.isNotAlive()).isTrue();

    verify(this.mockProcess, times(2)).isAlive();
  }

  @Test
  public void isInitializedBeforeInitIsFalse() {
    assertThat(newProcessAdapter(this.mockProcess, this.processContext).isInitialized()).isFalse();
  }

  @Test
  public void isInitializedAfterInitIsTrue() {

    this.processContext.inheritIO(true);

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess, this.processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.isInitialized()).isFalse();
    assertThat(this.processContext.inheritsIO()).isTrue();

    processAdapter.init();

    assertThat(processAdapter.isInitialized()).isTrue();
  }

  @Test
  public void isRunningForRunningProcessIsTrue() {

    when(this.mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running"));

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess, this.processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);
    assertThat(processAdapter.isRunning()).isTrue();
    assertThat(processAdapter.isNotRunning()).isFalse();

    verify(this.mockProcess, times(2)).exitValue();
  }

  @Test
  public void isRunningForTerminatedProcessIsFalse() {

    when(this.mockProcess.exitValue()).thenReturn(0);

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess, this.processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);
    assertThat(processAdapter.isRunning()).isFalse();
    assertThat(processAdapter.isNotRunning()).isTrue();

    verify(this.mockProcess, times(2)).exitValue();
  }

  @Test
  public void getCommandLineReturnsProcessCommand() {

    String[] commandLine = ArrayUtils.asArray("java", "-server", "-ea", "--classpath",
      "/path/to/application.jar", "example.Application");

    this.processContext.ranWith(commandLine);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).getCommandLine())
      .isEqualTo(Arrays.asList(commandLine));
  }

  @Test
  public void getDirectoryReturnsUserHomeDirectoryAsProcessWorkingDirectory() {

    this.processContext.ranIn(FileSystemUtils.USER_HOME_DIRECTORY);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).getDirectory())
      .isEqualTo(FileSystemUtils.USER_HOME_DIRECTORY);
  }

  @Test
  public void getEnvironmentReturnsProcessEnvironment() {

    Environment environment = Environment.from(Collections.singletonMap("testVariable", "testValue"));

    this.processContext.using(environment);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).getEnvironment()).isEqualTo(environment);
  }

  @Test
  public void getIdReturnsProcessId() throws IOException {

    File testPid = File.createTempFile("test", ".pid", FileSystemUtils.WORKING_DIRECTORY);

    testPid.deleteOnExit();
    FileSystemUtils.write(new ByteArrayInputStream("112358".getBytes()), testPid);
    this.processContext.ranIn(testPid.getParentFile());

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess, this.processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getDirectory()).isEqualTo(testPid.getParentFile());
    assertThat(processAdapter.getId()).isEqualTo(112358);
    assertThat(processAdapter.safeGetId()).isEqualTo(112358);
  }

  @Test
  @SuppressWarnings("all")
  public void getIdThrowsPidUnkownExceptionForNonExistingDirectory() {

    ThrowableAssertions.assertThatThrowableOfType(PidUnknownException.class)
      .isThrownBy(args -> {

        doReturn(FileUtils.newFile("/absolute/path/to/non/existing/directory"))
          .when(this.processContext).getDirectory();

        return newProcessAdapter(this.mockProcess, this.processContext).getId();
      })
      .havingMessage("Process ID (PID) cannot be determined")
      .causedBy(IllegalArgumentException.class)
      .havingMessage("The path [/absolute/path/to/non/existing/directory] used to search for a .pid file must exist")
      .withNoCause();
  }

  @Test
  public void getIdThrowsPidUnknownExceptionForNonExistingPidFile() {

    ThrowableAssertions.assertThatThrowableOfType(PidUnknownException.class)
      .isThrownBy(args -> {
        this.processContext.ranIn(FileSystemUtils.TEMPORARY_DIRECTORY);
        return newProcessAdapter(this.mockProcess, this.processContext).getId();
      })
      .havingMessage("Failed to read Process ID (PID) from file [null]")
      .causedBy(IllegalArgumentException.class)
      .havingMessage("[null] must be a file")
      .withNoCause();
  }

  @Test
  public void safeGetIdForNonExistingPidFileHandlesPidUnknownExceptionAndReturnsMinusOne() {

    this.processContext.ranIn(FileSystemUtils.TEMPORARY_DIRECTORY);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).safeGetId()).isEqualTo(-1);
  }

  @Test
  public void setIdThrowsUnsupportedOperationException() {

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess, this.processContext);

    assertThat(processAdapter).isNotNull();

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> {
        this.processContext.ranIn(FileSystemUtils.TEMPORARY_DIRECTORY);
        processAdapter.setId(123);
      })
      .withMessage(Constants.OPERATION_NOT_SUPPORTED)
      .withNoCause();

    assertThat(processAdapter.safeGetId()).isEqualTo(-1);
  }

  @Test
  public void getNameReturnsProcessId() {

    ProcessAdapter processAdapter = spy(newProcessAdapter(this.mockProcess, this.processContext));

    doReturn(101).when(processAdapter).getId();

    assertThat(processAdapter.getId()).isEqualTo(101);
    assertThat(processAdapter.getCommandLine()).isEmpty();
    assertThat(processAdapter.getName()).isEqualTo("101");
  }

  @Test
  public void getNameReturnsProcessName() {

    ProcessAdapter processAdapter = spy(newProcessAdapter(this.mockProcess, this.processContext));

    doReturn(100).when(processAdapter).getId();

    this.processContext.ranWith("path", "to", "java.exe");

    assertThat(processAdapter.getId()).isEqualTo(100);
    assertThat(processAdapter.getCommandLine()).containsExactly("path", "to", "java.exe");
    assertThat(processAdapter.getName()).isEqualTo("java.exe");
  }

  @Test
  public void getStandardErrorStream() {

    InputStream mockErrorStream = mock(InputStream.class, "Standard Error Stream");

    when(this.mockProcess.getErrorStream()).thenReturn(mockErrorStream);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).getStandardErrorStream())
      .isSameAs(mockErrorStream);

    verify(this.mockProcess, times(1)).getErrorStream();
    verify(this.mockProcess, never()).getInputStream();
    verify(this.mockProcess, never()).getOutputStream();
  }

  @Test
  public void getStandardInStream() {

    OutputStream mockOutputStream = mock(OutputStream.class, "Standard In Stream");

    when(this.mockProcess.getOutputStream()).thenReturn(mockOutputStream);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).getStandardInStream())
      .isSameAs(mockOutputStream);

    verify(this.mockProcess, never()).getErrorStream();
    verify(this.mockProcess, never()).getInputStream();
    verify(this.mockProcess, times(1)).getOutputStream();
  }

  @Test
  public void getStandardOutStream() {

    InputStream mockInputStream = mock(InputStream.class, "Standard Out Stream");

    when(this.mockProcess.getInputStream()).thenReturn(mockInputStream);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).getStandardOutStream())
      .isSameAs(mockInputStream);

    verify(this.mockProcess, never()).getErrorStream();
    verify(this.mockProcess, times(1)).getInputStream();
    verify(this.mockProcess, never()).getOutputStream();
  }

  @Test
  public void getUsernameReturnsNameOfUserUsedToRunProcess() {

    this.processContext.ranBy("jblum");

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).getUsername()).isEqualTo("jblum");
  }

  @Test
  public void exitValueFoRunningProcessThrowsIllegalThreadStateException() {

    doThrow(new IllegalThreadStateException("running")).when(this.mockProcess).exitValue();

    assertThatExceptionOfType(IllegalThreadStateException.class)
      .isThrownBy(() -> newProcessAdapter(this.mockProcess, this.processContext).exitValue())
      .withMessage("running")
      .withNoCause();

    verify(this.mockProcess, times(1)).exitValue();
  }

  @Test
  public void exitValueForTerminatedProcessReturnsExitValue() {

    when(this.mockProcess.exitValue()).thenReturn(1);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).exitValue()).isEqualTo(1);

    verify(this.mockProcess, times(1)).exitValue();
  }

  @Test
  public void safeExitValueForRunningProcessReturnsMinusOne() {

    when(this.mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running"));

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).safeExitValue()).isEqualTo(-1);

    verify(this.mockProcess, times(1)).exitValue();
  }

  @Test
  public void safeExitValueForTerminatedProcessReturnsExitValue() {

    when(this.mockProcess.exitValue()).thenReturn(1);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).safeExitValue()).isEqualTo(1);

    verify(this.mockProcess, times(1)).exitValue();
  }

  @Test
  public void killRunningProcessIsSuccessful() throws InterruptedException {

    when(this.mockProcess.destroyForcibly()).thenReturn(this.mockProcess);
    when(this.mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running"));
    when(this.mockProcess.waitFor()).thenReturn(1);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).kill()).isEqualTo(1);

    verify(this.mockProcess, times(1)).destroyForcibly();
    verify(this.mockProcess, times(1)).exitValue();
    verify(this.mockProcess, times(1)).waitFor();
  }

  @Test
  public void killTerminatedProcessIsSuccessful() throws InterruptedException {

    when(this.mockProcess.exitValue()).thenReturn(0);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).kill()).isEqualTo(0);

    verify(this.mockProcess, times(2)).exitValue();
    verify(this.mockProcess, never()).destroyForcibly();
    verify(this.mockProcess, never()).waitFor();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void restartRunningProcessIsSuccessful() throws InterruptedException {

    ProcessExecutor<ProcessAdapter> mockProcessExecutor = mock(ProcessExecutor.class);

    Process mockRestartedProcess = mock(Process.class);

    List<String> expectedCommandLine = Arrays.asList("java", "-server", "-ea", "-classpath",
      "/class/path/to/application.jar", "mock.Application");

    when(mockProcessExecutor.execute(eq(FileSystemUtils.USER_HOME_DIRECTORY), eq(expectedCommandLine)))
      .thenReturn(newProcessAdapter(mockRestartedProcess, this.processContext));

    doNothing().when(this.mockProcess).destroy();
    when(this.mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running")).thenReturn(0);
    when(this.mockProcess.waitFor()).thenReturn(0);

    this.processContext.ranIn(FileSystemUtils.USER_HOME_DIRECTORY).ranWith(expectedCommandLine);

    ProcessAdapter processAdapter = spy(new ProcessAdapter(this.mockProcess, this.processContext) {

      @Override
      @SuppressWarnings({ "rawtypes", "unchecked" })
      protected ProcessExecutor newProcessExecutor() {
        return mockProcessExecutor;
      }

      @Override
      public Integer safeGetId() {
        return 123;
      }

      @Override
      public synchronized int stop(long timeout, TimeUnit unit) {
        getProcess().destroy();
        return 0;
      }
    });

    ProcessAdapter restartedProcessAdapter = processAdapter.restart();

    assertThat(restartedProcessAdapter).isNotNull();
    assertThat(restartedProcessAdapter.getProcess()).isSameAs(mockRestartedProcess);
    assertThat(restartedProcessAdapter.getProcessContext()).isSameAs(this.processContext);

    verify(this.mockProcess, times(2)).exitValue();
    verify(this.mockProcess, times(1)).destroy();
    verify(this.mockProcess, times(1)).waitFor();
    verify(processAdapter, times(1))
      .stop(eq(ProcessAdapter.DEFAULT_TIMEOUT_MILLISECONDS), eq(TimeUnit.MILLISECONDS));
    verify(mockProcessExecutor, times(1))
      .execute(eq(FileSystemUtils.USER_HOME_DIRECTORY), eq(expectedCommandLine));
    verifyNoInteractions(mockRestartedProcess);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void restartTerminatedProcessIsSuccessful() throws InterruptedException {

    ProcessExecutor<ProcessAdapter> mockProcessExecutor = mock(ProcessExecutor.class);

    Process mockRestartedProcess = mock(Process.class);

    List<String> expectedCommandLine = Arrays.asList("java", "-server", "-ea", "-classpath",
      "/class/path/to/application.jar", "mock.Application");

    when(mockProcessExecutor.execute(eq(FileSystemUtils.USER_HOME_DIRECTORY), eq(expectedCommandLine)))
      .thenReturn(newProcessAdapter(mockRestartedProcess, this.processContext));

    this.processContext.ranIn(FileSystemUtils.USER_HOME_DIRECTORY).ranWith(expectedCommandLine);

    when(this.mockProcess.exitValue()).thenReturn(0);

    ProcessAdapter processAdapter = spy(new ProcessAdapter(this.mockProcess, this.processContext) {

      @Override
      @SuppressWarnings({ "rawtypes", "unchecked" })
      protected ProcessExecutor newProcessExecutor() {
        return mockProcessExecutor;
      }

      @Override
      public Integer safeGetId() {
        return 123;
      }

      @Override
      public synchronized int stop(long timeout, TimeUnit unit) {
        getProcess().destroy();
        return 0;
      }
    });

    ProcessAdapter restartedProcessAdapter = processAdapter.restart();

    assertThat(restartedProcessAdapter).isNotNull();
    assertThat(restartedProcessAdapter.getProcess()).isSameAs(mockRestartedProcess);
    assertThat(restartedProcessAdapter.getProcessContext()).isSameAs(this.processContext);

    verify(this.mockProcess, times(2)).exitValue();
    verify(this.mockProcess, never()).destroy();
    verify(this.mockProcess, never()).waitFor();
    verify(processAdapter, never()).stop(eq(ProcessAdapter.DEFAULT_TIMEOUT_MILLISECONDS), eq(TimeUnit.MILLISECONDS));
    verify(mockProcessExecutor, times(1)).execute(
      eq(FileSystemUtils.USER_HOME_DIRECTORY), eq(expectedCommandLine));
    verifyNoInteractions(mockRestartedProcess);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void restartUnstoppableProcessThrowsIllegalStateException() throws InterruptedException {

    ProcessExecutor mockProcessExecutor = mock(ProcessExecutor.class);

    doNothing().when(this.mockProcess).destroy();

    when(this.mockProcess.exitValue())
      .thenThrow(new IllegalThreadStateException("running"))
      .thenThrow(new IllegalThreadStateException("still running"));

    ProcessAdapter processAdapter = spy(new ProcessAdapter(this.mockProcess, this.processContext) {

      @Override
      protected ProcessExecutor newProcessExecutor() {
        return mockProcessExecutor;
      }

      @Override
      public Integer safeGetId() {
        return 123;
      }

      @Override
      public synchronized int stop(long timeout, TimeUnit unit) {
        getProcess().destroy();
        return 0;
      }
    });

    assertThatIllegalStateException()
      .isThrownBy(processAdapter::restart)
      .withMessage("Process [123] failed to stop")
      .withNoCause();

    verify(this.mockProcess, times(2)).exitValue();
    verify(this.mockProcess, times(1)).destroy();
    verify(this.mockProcess, times(1)).waitFor();
    verify(processAdapter, times(1))
      .stop(eq(ProcessAdapter.DEFAULT_TIMEOUT_MILLISECONDS), eq(TimeUnit.MILLISECONDS));
    verify(mockProcessExecutor, never()).execute(any(File.class), any(String[].class));
  }

  @Test
  public void newProcessExecutorReturnsRuntimeProcessExecutor() {
    assertThat(newProcessAdapter(this.mockProcess, this.processContext).newProcessExecutor())
      .isInstanceOf(RuntimeProcessExecutor.class);
  }

  @Test
  @SuppressWarnings("all")
  public void stopHandlesProcessDestroyRuntimeException() throws InterruptedException {

    Logger mockLogger = mock(Logger.class);

    doThrow(newRuntimeException("test")).when(this.mockProcess).destroy();
    when(this.mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running")).thenReturn(1);
    when(mockLogger.isLoggable(any(Level.class))).thenReturn(true);

    ProcessAdapter processAdapter = spy(newProcessAdapter(this.mockProcess));

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);

    doReturn(mockLogger).when(processAdapter).getLogger();

    assertThat(processAdapter.stop()).isEqualTo(1);

    verify(this.mockProcess, times(1)).destroy();
    verify(this.mockProcess, times(2)).exitValue();
    verify(this.mockProcess, never()).waitFor();
    verify(mockLogger, times(1)).isLoggable(eq(Level.FINE));
    verify(mockLogger, times(1)).fine(contains(RuntimeException.class.getName() + ": test"));
  }

  @Test
  @SuppressWarnings("all")
  public void stopHandlesProcessWaitForTimeoutException() throws InterruptedException {

    Logger mockLogger = mock(Logger.class);

    when(this.mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running"));

    doAnswer(invocationOnMock -> {
      Thread.sleep(250);
      return -1;
    }).when(this.mockProcess).waitFor();

    ProcessAdapter processAdapter = spy(newProcessAdapter(this.mockProcess));

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);

    doReturn(123).when(processAdapter).safeGetId();
    doReturn(mockLogger).when(processAdapter).getLogger();

    assertThat(processAdapter.stop(100L, TimeUnit.MILLISECONDS)).isEqualTo(-1);

    verify(this.mockProcess, times(1)).destroy();
    verify(this.mockProcess, times(1)).waitFor();
    verify(this.mockProcess, times(2)).exitValue();
    verify(mockLogger, times(1))
      .warning(eq("Process [123] could not be stopped within the given timeout [100 ms]"));
  }

  @Test
  public void stopIsInterruptedWhileWaitingIsSuccessful() throws Throwable {
    TestFramework.runOnce(new StopInterruptedTestCase());
  }

  @Test
  public void stopNonRunningProcessIsSuccessful() {

    when(this.mockProcess.exitValue()).thenReturn(1);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).stop()).isEqualTo(1);

    verify(this.mockProcess, times(2)).exitValue();
  }

  @Test
  public void stopRunningProcessIsSuccessful() throws InterruptedException {

    doNothing().when(this.mockProcess).destroy();
    when(this.mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running"));
    when(this.mockProcess.waitFor()).thenReturn(0);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).stop()).isEqualTo(0);

    verify(this.mockProcess, times(1)).destroy();
    verify(this.mockProcess, times(1)).exitValue();
    verify(this.mockProcess, times(1)).waitFor();
  }

  @Test
  public void stopAndWaitCallsProcessAdapterStopAndWait() throws InterruptedException {

    doNothing().when(this.mockProcess).destroy();
    when(this.mockProcess.waitFor()).thenReturn(0);

    ProcessAdapter processAdapter = spy(new ProcessAdapter(this.mockProcess, this.processContext) {

      @Override
      public synchronized int stop() {
        getProcess().destroy();
        return 1;
      }

      @Override
      public int waitFor() {
        try {
          return getProcess().waitFor();
        }
        catch (InterruptedException ignore) {
          return -1;
        }
      }
    });

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);
    assertThat(processAdapter.getProcessContext()).isSameAs(this.processContext);
    assertThat(processAdapter.stopAndWait()).isEqualTo(0);

    verify(this.mockProcess, times(1)).destroy();
    verify(this.mockProcess, times(1)).waitFor();
    verifyNoMoreInteractions(this.mockProcess);
    verify(processAdapter, times(1)).stop();
    verify(processAdapter, times(1)).waitFor();
  }

  @Test
  public void stopAndWaitWithTimeoutCallsProcessAdapterStopAndWaitWithTimeout() throws InterruptedException {

    doNothing().when(this.mockProcess).destroy();
    when(this.mockProcess.exitValue()).thenReturn(1);
    when(this.mockProcess.waitFor(anyLong(), any(TimeUnit.class))).thenReturn(true);

    ProcessAdapter processAdapter = spy(new ProcessAdapter(this.mockProcess, this.processContext) {

      @Override
      public synchronized int stop(long timeout, TimeUnit unit) {
        getProcess().destroy();
        return 0;
      }

      @Override
      public boolean waitFor(long timeout, TimeUnit unit) {
        try {
          return getProcess().waitFor(timeout, unit);
        }
        catch (InterruptedException ignore) {
          return false;
        }
      }
    });

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);
    assertThat(processAdapter.getProcessContext()).isSameAs(this.processContext);
    assertThat(processAdapter.stopAndWait(15L, TimeUnit.SECONDS)).isEqualTo(1);

    verify(this.mockProcess, times(1)).destroy();
    verify(this.mockProcess, times(1)).exitValue();
    verify(this.mockProcess, times(1)).waitFor(eq(15L), eq(TimeUnit.SECONDS));
    verifyNoMoreInteractions(this.mockProcess);
    verify(processAdapter, times(1)).stop(eq(15L), eq(TimeUnit.SECONDS));
    verify(processAdapter, times(1)).waitFor(eq(15L), eq(TimeUnit.SECONDS));
  }

  // test registerShutdownHook()

  @Test
  public void registerAndUnregisterProcessStreamListenerIsCorrect() {

    ProcessStreamListener mockProcessStreamListenerOne = mock(ProcessStreamListener.class,
      "MockProcessStreamListenerOne");

    ProcessStreamListener mockProcessStreamListenerTwo = mock(ProcessStreamListener.class,
      "MockProcessStreamListenerTwo");

    when(this.mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running"));

    ProcessAdapter processAdapter = newProcessAdapter(this.mockProcess, this.processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(this.mockProcess);
    assertThat(processAdapter.getProcessContext()).isSameAs(this.processContext);
    assertThat(processAdapter.register(mockProcessStreamListenerOne)).isSameAs(processAdapter);
    assertThat(processAdapter.register(mockProcessStreamListenerTwo)).isSameAs(processAdapter);

    ByteArrayInputStream in = new ByteArrayInputStream("Line one.\n".getBytes());

    Runnable runnable = processAdapter.newProcessStreamReader(in);

    assertThat(runnable).isNotNull();

    runnable.run();

    assertThat(processAdapter.unregister(mockProcessStreamListenerTwo)).isSameAs(processAdapter);

    in = new ByteArrayInputStream("Line two.\n".getBytes());
    runnable = processAdapter.newProcessStreamReader(in);

    assertThat(runnable).isNotNull();

    runnable.run();

    verify(mockProcessStreamListenerOne, times(1)).onInput(eq("Line one."));
    verify(mockProcessStreamListenerOne, times(1)).onInput(eq("Line two."));
    verify(mockProcessStreamListenerTwo, times(1)).onInput(eq("Line one."));
    verify(mockProcessStreamListenerTwo, never()).onInput(eq("Line two."));
  }

  @Test
  public void waitForCallsProcessWaitFor() throws InterruptedException {

    when(this.mockProcess.waitFor()).thenReturn(1);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).waitFor()).isEqualTo(1);

    verify(this.mockProcess, times(1)).waitFor();
  }

  @Test
  public void waitForCallsProcessWaitForAndIsInterrupted() throws Throwable {
    TestFramework.runOnce(new WaitForInterruptedTestCase());
  }

  @Test
  public void waitForWithTimeoutCallsProcessWaitForWithTimeout() throws InterruptedException {

    when(this.mockProcess.waitFor(anyLong(), any(TimeUnit.class))).thenReturn(true);

    assertThat(newProcessAdapter(this.mockProcess, this.processContext).waitFor(30, TimeUnit.SECONDS)).isTrue();

    verify(this.mockProcess, times(1)).waitFor(eq(30L), eq(TimeUnit.SECONDS));
  }

  @Test
  public void waitForWithTimeoutCallsProcessWaitForWithTimeoutAndIsInterrupted() throws Throwable {
    TestFramework.runOnce(new WaitForWithTimeoutInterruptedTestCase());
  }

  @SuppressWarnings("unused")
  protected abstract class AbstractWaitInterruptingTestCase extends MultithreadedTestCase {

    final Object mutex = new Object();

    private Thread waitingThread;

    public void thread1() {
      waitingThread = Thread.currentThread();
      waitingThread.setName("Waiting Thread");

      performWait(newProcessAdapter(mockProcess));

      assertThat(waitingThread.isInterrupted()).isTrue();
    }

    protected abstract void performWait(ProcessAdapter processAdapter);

    public void thread2() {
      Thread.currentThread().setName("Interrupting Thread");

      waitForTick(1);

      assertThat(waitingThread).isNotNull();

      waitingThread.interrupt();
    }
  }

  @SuppressWarnings("unused")
  protected class StopInterruptedTestCase extends AbstractWaitInterruptingTestCase {

    @Override
    public void initialize() {
      super.initialize();

      try {
        doNothing().when(mockProcess).destroy();

        when(mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running"));

        when(mockProcess.waitFor()).thenAnswer(invocationOnMock -> {
          synchronized (mutex) {
            mutex.wait();
          }

          return 1;
        });
      }
      catch (InterruptedException ignore) {
      }
    }

    @Override
    public void thread1() {
      super.thread1();
    }

    @Override
    protected void performWait(ProcessAdapter processAdapter) {
      assertThat(processAdapter.stop(30, TimeUnit.SECONDS)).isEqualTo(-1);
    }

    @Override
    public void thread2() {
      super.thread2();
    }

    @Override
    public void finish() {
      super.finish();

      try {
        verify(mockProcess, times(1)).destroy();
        verify(mockProcess, times(2)).exitValue();
        verify(mockProcess, times(1)).waitFor();
        verify(mockProcess, never()).waitFor(anyLong(), any(TimeUnit.class));
      }
      catch (InterruptedException ignore) {
      }
    }
  }

  @SuppressWarnings("unused")
  protected class WaitForInterruptedTestCase extends AbstractWaitInterruptingTestCase {

    @Override
    public void initialize() {
      super.initialize();

      try {
        when(mockProcess.exitValue()).thenReturn(1);

        when(mockProcess.waitFor()).thenAnswer(invocationOnMock -> {
          synchronized (mutex) {
            mutex.wait();
          }

          return 1;
        });
      }
      catch (InterruptedException ignore) {
      }
    }

    @Override
    public void thread1() {
      super.thread1();
    }

    @Override
    protected void performWait(ProcessAdapter processAdapter) {
      assertThat(processAdapter.waitFor()).isEqualTo(1);
    }

    @Override
    public void thread2() {
      super.thread2();
    }

    @Override
    public void finish() {
      super.finish();

      try {
        verify(mockProcess, times(1)).exitValue();
        verify(mockProcess, times(1)).waitFor();
        verify(mockProcess, never()).waitFor(anyLong(), any(TimeUnit.class));
      }
      catch (InterruptedException ignore) {
      }
    }
  }

  @SuppressWarnings("unused")
  protected class WaitForWithTimeoutInterruptedTestCase extends AbstractWaitInterruptingTestCase {

    @Override
    public void initialize() {
      super.initialize();

      try {
        when(mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("running"));

        when(mockProcess.waitFor(anyLong(), any(TimeUnit.class))).thenAnswer(invocationOnMock -> {
          assertThat(invocationOnMock.<Long>getArgument(0)).isEqualTo(15L);
          assertThat(invocationOnMock.<TimeUnit>getArgument(1)).isEqualTo(TimeUnit.SECONDS);

          synchronized (mutex) {
            mutex.wait();
          }

          return false;
        });
      }
      catch (InterruptedException ignore) {
      }
    }

    @Override
    public void thread1() {
      super.thread1();
    }

    @Override
    protected void performWait(ProcessAdapter processAdapter) {
      assertThat(processAdapter.waitFor(15L, TimeUnit.SECONDS)).isFalse();
    }

    @Override
    public void thread2() {
      super.thread2();
    }

    @Override
    public void finish() {
      super.finish();

      try {
        verify(mockProcess, times(1)).exitValue();
        verify(mockProcess, times(1)).waitFor(eq(15L), eq(TimeUnit.SECONDS));
        verify(mockProcess, never()).waitFor();
      }
      catch (InterruptedException ignore) {
      }
    }
  }
}
