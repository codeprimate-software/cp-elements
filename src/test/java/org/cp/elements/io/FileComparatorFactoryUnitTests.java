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
import static org.cp.elements.lang.CheckedExceptionsFactory.newIOException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;
import java.io.IOException;
import java.util.Comparator;

import org.junit.After;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.mockito.Mock;
import org.mockito.Mock.Strictness;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link FileComparatorFactory}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.util.Comparator
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.FileComparatorFactory
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
@SuppressWarnings("all")
public class FileComparatorFactoryUnitTests {

  @Mock(strictness = Strictness.LENIENT)
  private File mockFileOne;

  @Mock(strictness = Strictness.LENIENT)
  private File mockFileTwo;

  @BeforeEach
  public void setupMockFiles() throws IOException {

    doThrow(newIOException("TEST")).when(this.mockFileOne).getCanonicalPath();
    doThrow(newIOException("TEST")).when(this.mockFileTwo).getCanonicalPath();
  }

  @After
  public void verifyMockFiles() {
    verifyNoMoreInteractions(this.mockFileOne, this.mockFileTwo);
  }

  @Test
  public void fileExtensionComparator() {

    doReturn("relative/path/to/source/file.groovy").when(this.mockFileOne).getName();
    doReturn("/absolute/path/to/source/file.java").when(this.mockFileTwo).getName();

    Comparator<File> fileExtensionComparator = FileComparatorFactory.fileExtensionComparator();

    assertThat(fileExtensionComparator).isNotNull();
    assertThat(fileExtensionComparator.compare(this.mockFileOne, this.mockFileOne)).isZero();
    assertThat(fileExtensionComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(fileExtensionComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);
    assertThat(fileExtensionComparator.compare(this.mockFileTwo, this.mockFileTwo)).isZero();

    verify(this.mockFileOne, times(4)).getName();
    verify(this.mockFileTwo, times(4)).getName();
  }

  @Test
  public void fileExtensionComparatorComparesFileWithoutExtensionToFileWithExtensionSuccessfully() {

    doReturn("/path/to/file").when(this.mockFileOne).getName();
    doReturn("/path/to/file.ext").when(this.mockFileTwo).getName();

    Comparator<File> fileExtensionComparator = FileComparatorFactory.fileExtensionComparator();

    assertThat(fileExtensionComparator).isNotNull();
    assertThat(fileExtensionComparator.compare(this.mockFileOne, this.mockFileOne)).isZero();
    assertThat(fileExtensionComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(fileExtensionComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);
    assertThat(fileExtensionComparator.compare(this.mockFileTwo, this.mockFileTwo)).isZero();

    verify(this.mockFileOne, times(4)).getName();
    verify(this.mockFileTwo, times(4)).getName();
  }

  @Test
  public void fileLastModifiedComparator() {

    doReturn(1L).when(this.mockFileOne).lastModified();
    doReturn(2L).when(this.mockFileTwo).lastModified();

    Comparator<File> fileLastModifiedComparator = FileComparatorFactory.fileLastModifiedComparator();

    assertThat(fileLastModifiedComparator).isNotNull();
    assertThat(fileLastModifiedComparator.compare(this.mockFileOne, this.mockFileOne)).isZero();
    assertThat(fileLastModifiedComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(fileLastModifiedComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);
    assertThat(fileLastModifiedComparator.compare(this.mockFileTwo, this.mockFileTwo)).isZero();

    verify(this.mockFileOne, times(4)).lastModified();
    verify(this.mockFileTwo, times(4)).lastModified();
  }

  @Test
  public void fileNameComparator() {

    doReturn("relative/path/to/source/fileOne.java").when(this.mockFileOne).getName();
    doReturn("/absolute/path/to/source/fileTwo.java").when(this.mockFileTwo).getName();

    Comparator<File> fileNameComparator = FileComparatorFactory.fileNameComparator();

    assertThat(fileNameComparator).isNotNull();
    assertThat(fileNameComparator.compare(this.mockFileOne, this.mockFileOne)).isZero();
    assertThat(fileNameComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(fileNameComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);
    assertThat(fileNameComparator.compare(this.mockFileTwo, this.mockFileTwo)).isZero();

    verify(this.mockFileOne, times(4)).getName();
    verify(this.mockFileTwo, times(4)).getName();
  }

  @Test
  public void fileNameComparatorComparesFileWithoutNameToFileWithNameSuccessfully() {

    doReturn("/path/to/.ext").when(this.mockFileOne).getName();
    doReturn("/path/to/file.ext").when(this.mockFileTwo).getName();

    Comparator<File> fileNameComparator = FileComparatorFactory.fileNameComparator();

    assertThat(fileNameComparator).isNotNull();
    assertThat(fileNameComparator.compare(this.mockFileOne, this.mockFileOne)).isZero();
    assertThat(fileNameComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(fileNameComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);
    assertThat(fileNameComparator.compare(this.mockFileTwo, this.mockFileTwo)).isZero();

    verify(this.mockFileOne, times(4)).getName();
    verify(this.mockFileTwo, times(4)).getName();
  }

  @Test
  public void filePathComparator() throws IOException {

    doReturn("/absolute/path/to/file.ext").when(this.mockFileOne).getAbsolutePath();
    doReturn("relative/path/to/file.ext").when(this.mockFileTwo).getAbsolutePath();

    Comparator<File> filePathComparator = FileComparatorFactory.filePathComparator();

    assertThat(filePathComparator).isNotNull();
    assertThat(filePathComparator.compare(this.mockFileOne, this.mockFileOne)).isZero();
    assertThat(filePathComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(filePathComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);
    assertThat(filePathComparator.compare(this.mockFileTwo, this.mockFileTwo)).isZero();

    verify(this.mockFileOne, times(4)).getAbsolutePath();
    verify(this.mockFileOne, times(4)).getCanonicalPath();
    verify(this.mockFileTwo, times(4)).getAbsolutePath();
    verify(this.mockFileTwo, times(4)).getCanonicalPath();
  }

  @Test
  // NOTE: sorts directories before files appropriately
  public void filePathComparatorComparesFileWithoutPathToFileWithPathSuccessfully() throws IOException {

    doReturn("file.ext").when(this.mockFileOne).getAbsolutePath();
    doReturn("/a/path/to/file.ext").when(this.mockFileTwo).getAbsolutePath();

    Comparator<File> filePathComparator = FileComparatorFactory.filePathComparator();

    assertThat(filePathComparator).isNotNull();
    assertThat(filePathComparator.compare(this.mockFileOne, this.mockFileOne)).isZero();
    assertThat(filePathComparator.compare(this.mockFileOne, this.mockFileTwo)).isGreaterThan(0);
    assertThat(filePathComparator.compare(this.mockFileTwo, this.mockFileOne)).isLessThan(0);
    assertThat(filePathComparator.compare(this.mockFileTwo, this.mockFileTwo)).isZero();

    verify(this.mockFileOne, times(4)).getAbsolutePath();
    verify(this.mockFileOne, times(4)).getCanonicalPath();
    verify(this.mockFileTwo, times(4)).getAbsolutePath();
    verify(this.mockFileTwo, times(4)).getCanonicalPath();
  }

  @Test
  public void fileSizeComparator() {

    doReturn(1024L).when(this.mockFileOne).length();
    doReturn(1024000L).when(this.mockFileTwo).length();

    Comparator<File> fileSizeComparator = FileComparatorFactory.fileSizeComparator();

    assertThat(fileSizeComparator).isNotNull();
    assertThat(fileSizeComparator.compare(this.mockFileOne, this.mockFileOne)).isZero();
    assertThat(fileSizeComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(fileSizeComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);
    assertThat(fileSizeComparator.compare(this.mockFileTwo, this.mockFileTwo)).isZero();

    verify(this.mockFileOne, times(4)).length();
    verify(this.mockFileTwo, times(4)).length();
  }
}
