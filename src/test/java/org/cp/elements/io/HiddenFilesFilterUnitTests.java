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

import org.junit.Test;
import org.junit.runner.RunWith;

import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link HiddenFilesFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.io.HiddenFilesFilter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class HiddenFilesFilterUnitTests {

  @Mock
  private File mockFile;

  @Test
  @SuppressWarnings("all")
  public void hiddenFilesFilterAcceptsHiddenFile() {

    doReturn(true).when(this.mockFile).isHidden();

    assertThat(HiddenFilesFilter.HIDDEN_FILES.accept(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isHidden();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void hiddenFilesFilterRejectsNonHiddenFile() {

    doReturn(false).when(this.mockFile).isHidden();

    assertThat(HiddenFilesFilter.HIDDEN_FILES.accept(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).isHidden();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void hiddenFilesFilterRejectsNullIsNullSafe() {
    assertThat(HiddenFilesFilter.HIDDEN_FILES.accept(null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void nonHiddenFilesFilterAcceptsNonHiddenFile() {

    doReturn(false).when(this.mockFile).isHidden();

    assertThat(HiddenFilesFilter.NON_HIDDEN_FILES.accept(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isHidden();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void nonHiddenFilesFilterRejectsHiddenFile() {

    doReturn(true).when(this.mockFile).isHidden();

    assertThat(HiddenFilesFilter.NON_HIDDEN_FILES.accept(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).isHidden();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void nonHiddenFilesFilterRejectsNullIsNullSafe() {
    assertThat(HiddenFilesFilter.NON_HIDDEN_FILES.accept(null)).isFalse();
  }
}
