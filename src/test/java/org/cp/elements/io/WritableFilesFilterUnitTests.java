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
 * Unit Tests for {@link WritableFilesFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.io.WritableFilesFilter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class WritableFilesFilterUnitTests {

  @Mock
  private File mockFile;

  @Test
  @SuppressWarnings("all")
  public void writableFilesFilterAcceptsWritableFile() {

    doReturn(true).when(this.mockFile).canWrite();

    assertThat(WritableFilesFilter.WRITABLE_FILES.accept(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).canWrite();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void writableFilesFilterRejectsNonWritableFile() {

    doReturn(false).when(this.mockFile).canWrite();

    assertThat(WritableFilesFilter.WRITABLE_FILES.accept(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).canWrite();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void writableFilesFilterRejectsNullFileIsNullSafe() {
    assertThat(WritableFilesFilter.WRITABLE_FILES.accept(null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void nonWritableFilesFilterAcceptsNonWritableFile() {

    doReturn(false).when(this.mockFile).canWrite();

    assertThat(WritableFilesFilter.NON_WRITABLE_FILES.accept(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).canWrite();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void nonWritableFilesFilterRejectsWritableFile() {

    doReturn(true).when(this.mockFile).canWrite();

    assertThat(WritableFilesFilter.NON_WRITABLE_FILES.accept(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).canWrite();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void nonWritableFilesFilterRejectsNullIsNullSafe() {
    assertThat(WritableFilesFilter.NON_WRITABLE_FILES.accept(null)).isFalse();
  }
}
