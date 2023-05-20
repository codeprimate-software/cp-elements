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
package org.cp.elements.process.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.process.ProcessAdapter.newProcessAdapter;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

import org.junit.AfterClass;
import org.junit.jupiter.api.Test;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.process.PidUnknownException;
import org.cp.elements.process.ProcessAdapter;
import org.cp.elements.test.annotation.IntegrationTest;

/**
 * Unit Tests for {@link ProcessUtils}.
 *
 * @author John J. Blum
 * @see java.lang.Process
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.process.util.ProcessUtils
 * @see org.cp.elements.test.TestUtils
 * @see org.cp.elements.test.annotation.IntegrationTest
 * @since 1.0.0
 */
public class ProcessUtilsTests {

  @AfterClass
  @SuppressWarnings("all")
  public static void tearDown() {
    Thread.interrupted();
  }

  @Test
  public void getProcessIdIsSuccessful() {
    assertThat(ProcessUtils.getProcessId()).isGreaterThan(0);
  }

  @Test
  public void isAliveWithNullProcess() {
    assertThat(ProcessUtils.isAlive(null)).isFalse();
  }

  @Test
  public void isAliveWithRunningProcess() {

    Process mockProcess = mock(Process.class);

    when(mockProcess.isAlive()).thenReturn(true);

    assertThat(ProcessUtils.isAlive(mockProcess)).isTrue();

    verify(mockProcess, times(1)).isAlive();
    verifyNoMoreInteractions(mockProcess);
  }

  @Test
  public void isAliveWithTerminatedProcess() {

    Process mockProcess = mock(Process.class);

    when(mockProcess.isAlive()).thenReturn(false);

    assertThat(ProcessUtils.isAlive(mockProcess)).isFalse();

    verify(mockProcess, times(1)).isAlive();
    verifyNoMoreInteractions(mockProcess);
  }

  @Test
  public void isRunningWithNonExistingProcessId() {
    assertThat(ProcessUtils.isRunning(-12345)).isFalse();
  }

  @Test
  public void isRunningWithRunningProcessId() {
    assertThat(ProcessUtils.isRunning(ProcessUtils.getProcessId())).isTrue();
  }

  @Test
  public void isRunningWithNullProcess() {
    assertThat(ProcessUtils.isRunning((Process) null)).isFalse();
  }

  @Test
  public void isRunningWithRunningProcess() {

    Process mockProcess = mock(Process.class);

    when(mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("Process is running"));

    assertThat(ProcessUtils.isRunning(mockProcess)).isTrue();

    verify(mockProcess, times(1)).exitValue();
    verifyNoMoreInteractions(mockProcess);
  }

  @Test
  public void isRunningWithTerminatedProcess() {

    Process mockProcess = mock(Process.class);

    when(mockProcess.exitValue()).thenReturn(0);

    assertThat(ProcessUtils.isRunning(mockProcess)).isFalse();

    verify(mockProcess, times(1)).exitValue();
    verifyNoMoreInteractions(mockProcess);
  }

  @Test
  public void isRunningWithNullProcessAdapter() {
    assertThat(ProcessUtils.isRunning((ProcessAdapter) null)).isFalse();
  }

  @Test
  public void isRunningWithRunningProcessAdapter() {

    Process mockProcess = mock(Process.class);

    when(mockProcess.exitValue()).thenThrow(new IllegalThreadStateException("Process is running"));

    assertThat(ProcessUtils.isRunning(newProcessAdapter(mockProcess))).isTrue();

    verify(mockProcess, times(1)).exitValue();
    verifyNoMoreInteractions(mockProcess);
  }

  @Test
  public void isRunningWithTerminatedProcessAdapter() {

    Process mockProcess = mock(Process.class);

    when(mockProcess.exitValue()).thenReturn(-1);

    assertThat(ProcessUtils.isRunning(newProcessAdapter(mockProcess))).isFalse();

    verify(mockProcess, times(1)).exitValue();
    verifyNoMoreInteractions(mockProcess);
  }

  @Test
  public void killTerminatedProcessIsSuccessful() throws Exception {

    Process mockProcess = mock(Process.class);

    when(mockProcess.isAlive()).thenReturn(false);

    assertThat(ProcessUtils.kill(mockProcess)).isTrue();

    verify(mockProcess, times(1)).isAlive();
    verify(mockProcess, never()).destroy();
    verify(mockProcess, never()).destroyForcibly();
    verify(mockProcess, never()).waitFor(anyLong(), any(TimeUnit.class));
  }

  @Test
  public void killRunningProcessIsSuccessful() throws Exception {

    Process mockProcess = mock(Process.class);

    when(mockProcess.isAlive()).thenReturn(true);
    when(mockProcess.waitFor(anyLong(), any(TimeUnit.class))).thenReturn(true);

    assertThat(ProcessUtils.kill(mockProcess)).isTrue();

    verify(mockProcess, times(1)).isAlive();
    verify(mockProcess, times(1)).destroy();
    verify(mockProcess, never()).destroyForcibly();
    verify(mockProcess, times(1)).waitFor(eq(ProcessUtils.KILL_WAIT_TIMEOUT), eq(ProcessUtils.KILL_WAIT_TIME_UNIT));
  }

  @Test
  public void killRunningProcessForciblyIsSuccessful() throws Exception {

    Process mockProcess = mock(Process.class);

    when(mockProcess.isAlive()).thenReturn(true);
    when(mockProcess.waitFor(anyLong(), any(TimeUnit.class))).thenReturn(false).thenReturn(true);

    assertThat(ProcessUtils.kill(mockProcess)).isTrue();

    verify(mockProcess, times(1)).isAlive();
    verify(mockProcess, times(1)).destroy();
    verify(mockProcess, times(1)).destroyForcibly();
    verify(mockProcess, times(2)).waitFor(eq(ProcessUtils.KILL_WAIT_TIMEOUT), eq(ProcessUtils.KILL_WAIT_TIME_UNIT));
  }

  @Test
  public void killRunningProcessIsUnsuccessful() throws Exception {

    Process mockProcess = mock(Process.class);

    when(mockProcess.isAlive()).thenReturn(true).thenReturn(true);
    when(mockProcess.waitFor(anyLong(), any(TimeUnit.class))).thenReturn(false).thenReturn(false);

    assertThat(ProcessUtils.kill(mockProcess)).isFalse();

    verify(mockProcess, times(2)).isAlive();
    verify(mockProcess, times(1)).destroy();
    verify(mockProcess, times(1)).destroyForcibly();
    verify(mockProcess, times(2)).waitFor(
      eq(ProcessUtils.KILL_WAIT_TIMEOUT), eq(ProcessUtils.KILL_WAIT_TIME_UNIT));
  }

  @Test
  public void killNullProcessIsSuccessful() {
    assertThat(ProcessUtils.kill((Process) null)).isTrue();
  }

  @Test
  public void killRunningProcessIsInterruptedWhileWaitingIsSuccessful() throws Exception {

    Process mockProcess = mock(Process.class);

    when(mockProcess.isAlive()).thenReturn(true);
    when(mockProcess.waitFor(anyLong(), any(TimeUnit.class))).thenThrow(new InterruptedException("test interrupt"))
      .thenReturn(true);

    assertThat(ProcessUtils.kill(mockProcess)).isTrue();

    verify(mockProcess, times(1)).isAlive();
    verify(mockProcess, times(1)).destroy();
    verify(mockProcess, times(1)).destroyForcibly();
    verify(mockProcess, times(2)).waitFor(
      eq(ProcessUtils.KILL_WAIT_TIMEOUT), eq(ProcessUtils.KILL_WAIT_TIME_UNIT));
  }

  @Test
  public void killRunningProcessIsInterruptedTwiceWhileWaitingIsUnsuccessful() throws Exception {

    Process mockProcess = mock(Process.class);

    when(mockProcess.isAlive()).thenReturn(true).thenReturn(true);
    when(mockProcess.waitFor(anyLong(), any(TimeUnit.class))).thenThrow(new InterruptedException("test interrupt"))
      .thenThrow(new InterruptedException("test interrupt again"));

    assertThat(ProcessUtils.kill(mockProcess)).isFalse();

    verify(mockProcess, times(2)).isAlive();
    verify(mockProcess, times(1)).destroy();
    verify(mockProcess, times(1)).destroyForcibly();
    verify(mockProcess, times(2)).waitFor(
      eq(ProcessUtils.KILL_WAIT_TIMEOUT), eq(ProcessUtils.KILL_WAIT_TIME_UNIT));
  }

  @Test
  public void killRunningProcessAdapterIsSuccessful() throws Exception {

    Process mockProcess = mock(Process.class);

    when(mockProcess.isAlive()).thenReturn(true);
    when(mockProcess.waitFor(anyLong(), any(TimeUnit.class))).thenReturn(true);

    assertThat(ProcessUtils.kill(newProcessAdapter(mockProcess))).isTrue();

    verify(mockProcess, times(1)).isAlive();
    verify(mockProcess, times(1)).destroy();
    verify(mockProcess, never()).destroyForcibly();
    verify(mockProcess, times(1)).waitFor(eq(ProcessUtils.KILL_WAIT_TIMEOUT), eq(ProcessUtils.KILL_WAIT_TIME_UNIT));
  }

  @Test
  @IntegrationTest
  public void writeAndReadPidIsSuccessful() throws IOException {

    File pidFile = null;

    try {
      int expectedPid = ProcessUtils.getProcessId();

      assertThat(expectedPid).isGreaterThan(0);

      pidFile = ProcessUtils.writePid(expectedPid);

      assertThat(pidFile).isNotNull();
      assertThat(pidFile.isFile()).isTrue();

      int actualPid = ProcessUtils.readPid(pidFile);

      assertThat(actualPid).isEqualTo(expectedPid);
    }
    finally {
      FileSystemUtils.delete(pidFile);
    }
  }

  @Test
  @IntegrationTest
  public void readPidFromEmptyFileThrowsPidUnknownExceptionCausedByNumberFormatException() throws IOException {

    File tmpPid = File.createTempFile("tempFile", ".pid");

    assertThat(tmpPid).isNotNull();
    assertThat(tmpPid.isFile()).isTrue();

    tmpPid.deleteOnExit();

    ThrowableAssertions.assertThatThrowableOfType(PidUnknownException.class)
      .isThrownBy(args -> ProcessUtils.readPid(tmpPid))
      .havingMessage("Failed to read Process ID (PID) from file [%s]",
        tmpPid.getAbsolutePath())
      .causedBy(NumberFormatException.class)
      .withNoCause();
  }

  @Test
  @SuppressWarnings("all")
  public void readPidFromNonExistingFileThrowsPidUnknownExceptionCausedByIllegalArgumentException() {

    File mockFile = mock(File.class);

    when(mockFile.isFile()).thenReturn(false);
    when(mockFile.toString()).thenReturn("mockFile.pid");

    ThrowableAssertions.assertThatThrowableOfType(PidUnknownException.class)
      .isThrownBy(args -> ProcessUtils.readPid(mockFile))
      .havingMessage("Failed to read Process ID (PID) from file [mockFile.pid]")
      .causedBy(IllegalArgumentException.class)
      .withNoCause();

    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).canRead();
  }

  @Test
  @SuppressWarnings("all")
  public void readPidFromUnreadableFileThrowsPidUnknownExceptionCausedByIllegalStateException() {

    File mockFile = mock(File.class);

    when(mockFile.isFile()).thenReturn(true);
    when(mockFile.canRead()).thenReturn(false);
    when(mockFile.toString()).thenReturn("mockFile.pid");

    ThrowableAssertions.assertThatThrowableOfType(PidUnknownException.class)
      .isThrownBy(args -> ProcessUtils.readPid(mockFile))
      .havingMessage("Failed to read Process ID (PID) from file [mockFile.pid]")
      .causedBy(IllegalStateException.class)
      .withNoCause();

    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).canRead();
  }

  @Test
  public void findPidFileInCurrentWorkingDirectoryReturnsNull() {
    assertThat(ProcessUtils.findPidFile(FileSystemUtils.WORKING_DIRECTORY)).isNull();
  }

  @Test
  public void findPidFileWithNonExistingPathThrowsIllegalArgumentException() {

    File nonExistingFile = new File("/absolute/path/to/non/existing/file");

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ProcessUtils.findPidFile(nonExistingFile))
      .withMessage("The path [/absolute/path/to/non/existing/file] used to search for a .pid file must exist")
      .withNoCause();
  }

  @Test
  public void findPidFileWithNullThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ProcessUtils.findPidFile(null))
      .withMessage("The path [null] used to search for a .pid file must exist")
      .withNoCause();
  }
}
