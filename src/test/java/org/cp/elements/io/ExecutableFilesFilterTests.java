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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * The ExecutableFilesFilterTests class is a test suite of test cases testing the contract and functionality
 * of the {@link ExecutableFilesFilter} class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.io.ExecutableFilesFilter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ExecutableFilesFilterTests {

  @Mock
  private File mockFile;

  @Test
  @SuppressWarnings("all")
  public void executableFileFilterAcceptsExecutableFile() {
    when(mockFile.canExecute()).thenReturn(true);
    assertTrue(ExecutableFilesFilter.EXECUTABLE_FILES.accept(mockFile));
    verify(mockFile, times(1)).canExecute();
  }

  @Test
  @SuppressWarnings("all")
  public void executableFileFilterRejectsNonExecutableFile() {
    when(mockFile.canExecute()).thenReturn(false);
    assertFalse(ExecutableFilesFilter.EXECUTABLE_FILES.accept(mockFile));
    verify(mockFile, times(1)).canExecute();
  }

  @Test
  public void executableFileFilterRejectsNull() {
    assertFalse(ExecutableFilesFilter.EXECUTABLE_FILES.accept(null));
  }

  @Test
  @SuppressWarnings("all")
  public void nonExecutableFileFilterAcceptsNonExecutableFile() {
    when(mockFile.canExecute()).thenReturn(false);
    assertTrue(ExecutableFilesFilter.NON_EXECUTABLE_FILES.accept(mockFile));
    verify(mockFile, times(1)).canExecute();
  }

  @Test
  @SuppressWarnings("all")
  public void nonExecutableFileFilterRejectsExecutableFile() {
    when(mockFile.canExecute()).thenReturn(true);
    assertFalse(ExecutableFilesFilter.NON_EXECUTABLE_FILES.accept(mockFile));
    verify(mockFile, times(1)).canExecute();
  }

  @Test
  public void nonExecutableFileFilterRejectsNull() {
    assertFalse(ExecutableFilesFilter.NON_EXECUTABLE_FILES.accept(null));
  }
}
