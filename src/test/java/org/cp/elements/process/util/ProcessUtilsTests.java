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

package org.cp.elements.process.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.process.ProcessAdapter.newProcessAdapter;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.cp.elements.process.ProcessAdapter;
import org.junit.Test;

/**
 * Unit tests for {@link ProcessUtils}.
 *
 * @author John J. Blum
 * @see java.lang.Process
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.process.util.ProcessUtils
 * @since 1.0.0
 */
public class ProcessUtilsTests {

  @Test
  public void getProcessIdIsSuccessful() {
    assertThat(ProcessUtils.getProcessId()).isGreaterThan(0);
  }

  @Test
  public void isAliveWithNull() {
    assertThat(ProcessUtils.isAlive(null)).isFalse();
  }

  @Test
  public void isAliveWithRunningProcess() {
    Process mockProcess = mock(Process.class);

    when(mockProcess.isAlive()).thenReturn(true);

    assertThat(ProcessUtils.isAlive(mockProcess)).isTrue();

    verify(mockProcess, times(1)).isAlive();
  }

  @Test
  public void isAliveWithTerminatedProcess() {
    Process mockProcess = mock(Process.class);

    when(mockProcess.isAlive()).thenReturn(false);

    assertThat(ProcessUtils.isAlive(mockProcess)).isFalse();

    verify(mockProcess, times(1)).isAlive();
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

    when(mockProcess.exitValue()).thenThrow(new IllegalMonitorStateException("Process is running"));

    assertThat(ProcessUtils.isRunning(mockProcess)).isTrue();

    verify(mockProcess, times(1)).exitValue();
  }

  @Test
  public void isRunningWithTerminatedProcess() {
    Process mockProcess = mock(Process.class);

    when(mockProcess.exitValue()).thenReturn(0);

    assertThat(ProcessUtils.isRunning(mockProcess)).isFalse();

    verify(mockProcess, times(1)).exitValue();
  }

  @Test
  public void isRunningWithNullProcessAdapter() {
    assertThat(ProcessUtils.isRunning((ProcessAdapter) null)).isFalse();
  }

  @Test
  public void isRunningWithRunningProcessAdapter() {
    Process mockProcess = mock(Process.class);

    when(mockProcess.exitValue()).thenThrow(new IllegalMonitorStateException("Process is running"));

    assertThat(ProcessUtils.isRunning(newProcessAdapter(mockProcess))).isTrue();

    verify(mockProcess, times(1)).exitValue();
  }

  @Test
  public void isRunningWithTerminatedProcessAdapter() {
    Process mockProcess = mock(Process.class);

    when(mockProcess.exitValue()).thenReturn(-1);

    assertThat(ProcessUtils.isRunning(newProcessAdapter(mockProcess))).isFalse();

    verify(mockProcess, times(1)).exitValue();
  }
}
