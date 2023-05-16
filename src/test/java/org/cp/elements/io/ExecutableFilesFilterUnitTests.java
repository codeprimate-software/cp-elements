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
package org.cp.elements.io;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;

import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;

import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link ExecutableFilesFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.io.ExecutableFilesFilter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ExecutableFilesFilterUnitTests {

  @Mock
  private File mockFile;

  @Test
  public void getInstanceAcceptsExecutableFiles() {
    assertThat(ExecutableFilesFilter.getInstance(true)).isSameAs(ExecutableFilesFilter.EXECUTABLE_FILES);
  }

  @Test
  public void getInstanceRejectsExecutableFiles() {
    assertThat(ExecutableFilesFilter.getInstance(false)).isSameAs(ExecutableFilesFilter.NON_EXECUTABLE_FILES);
  }

  @Test
  public void executableFilesFilterAcceptsExecutableFile() {

    doReturn(true).when(this.mockFile).canExecute();

    assertThat(ExecutableFilesFilter.EXECUTABLE_FILES.accept(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).canExecute();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void executableFilesFilterRejectsNonExecutableFile() {

    doReturn(false).when(this.mockFile).canExecute();

    assertThat(ExecutableFilesFilter.EXECUTABLE_FILES.accept(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).canExecute();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void executableFilesFilterIsNullSafeRejectsNull() {
    assertThat(ExecutableFilesFilter.EXECUTABLE_FILES.accept(null)).isFalse();
  }

  @Test
  public void nonExecutableFilesFilterAcceptsNonExecutableFile() {

    doReturn(false).when(this.mockFile).canExecute();

    assertThat(ExecutableFilesFilter.NON_EXECUTABLE_FILES.accept(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).canExecute();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void nonExecutableFilesFilterRejectsExecutableFile() {

    doReturn(true).when(this.mockFile).canExecute();

    assertThat(ExecutableFilesFilter.NON_EXECUTABLE_FILES.accept(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).canExecute();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void nonExecutableFilesFilterIsNullSafeRejectsNull() {
    assertThat(ExecutableFilesFilter.NON_EXECUTABLE_FILES.accept(null)).isFalse();
  }
}
