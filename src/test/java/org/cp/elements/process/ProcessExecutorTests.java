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
import static org.cp.elements.util.ArrayUtils.asIterable;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;

import org.cp.elements.io.FileSystemUtils;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link ProcessExecutor}.
 *
 * @author John J. Blum
 * @see java.lang.Process
 * @see org.junit.jupiter.api.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.process.ProcessExecutor
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ProcessExecutorTests {

  @Mock
  private File mockDirectory;

  @Mock
  private Process mockProcess;

  @Mock
  private TestProcessExecutor processExecutor;

  @Test
  public void executeWithVarargs() {

    when(processExecutor.execute(ArgumentMatchers.<String[]>any())).thenCallRealMethod();
    when(processExecutor.execute(any(File.class), ArgumentMatchers.<String[]>any())).thenReturn(mockProcess);

    assertThat(processExecutor.execute("java", "example.App", "arg")).isEqualTo(mockProcess);

    verify(processExecutor, times(1)).execute(eq(FileSystemUtils.WORKING_DIRECTORY),
      eq("java"), eq("example.App"), eq("arg"));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void executeWithIterable() {

    when(processExecutor.execute(any(Iterable.class))).thenCallRealMethod();
    when(processExecutor.execute(any(File.class), ArgumentMatchers.<String[]>any())).thenReturn(mockProcess);

    assertThat(processExecutor.execute(asIterable("java", "example.App", "arg"))).isEqualTo(mockProcess);

    verify(processExecutor, times(1)).execute(eq(FileSystemUtils.WORKING_DIRECTORY),
      eq("java"), eq("example.App"), eq("arg"));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void executeWithDirectoryAndIterable() {

    when(processExecutor.execute(any(File.class), any(Iterable.class))).thenCallRealMethod();
    when(processExecutor.execute(any(File.class), ArgumentMatchers.<String[]>any())).thenReturn(mockProcess);

    assertThat(processExecutor.execute(mockDirectory, asIterable("java", "example.App", "arg"))).isEqualTo(mockProcess);

    verify(processExecutor, times(1)).execute(eq(mockDirectory),
      eq("java"), eq("example.App"), eq("arg"));
  }

  abstract class TestProcessExecutor implements ProcessExecutor { }

}
