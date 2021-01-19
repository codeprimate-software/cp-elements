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
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.Comparator;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link FileComparatorFactory}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.util.Comparator
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.io.FileComparatorFactory
 * @since 1.0.0
 */
@SuppressWarnings("all")
@RunWith(MockitoJUnitRunner.class)
public class FileComparatorFactoryTests {

  @Mock
  private File mockFileOne;

  @Mock
  private File mockFileTwo;

  @Test
  public void fileExtensionComparator() {

    when(this.mockFileOne.getName()).thenReturn("/path/to/source/file.groovy");
    when(this.mockFileTwo.getName()).thenReturn("/path/to/source/file.java");

    Comparator<File> fileExtensionComparator = FileComparatorFactory.fileExtensionComparator();

    assertThat(fileExtensionComparator.compare(this.mockFileOne, this.mockFileOne)).isEqualTo(0);
    assertThat(fileExtensionComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(fileExtensionComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);
    assertThat(fileExtensionComparator.compare(this.mockFileTwo, this.mockFileTwo)).isEqualTo(0);

    verify(this.mockFileOne, times(4)).getName();
    verify(this.mockFileTwo, times(4)).getName();
  }

  @Test
  public void fileExtensionComparatorComparesFileWithoutExtensionToFileWithExtensionSuccessfully() {

    when(this.mockFileOne.getName()).thenReturn("/path/to/file");
    when(this.mockFileTwo.getName()).thenReturn("/path/to/file.ext");

    Comparator<File> fileExtensionComparator = FileComparatorFactory.fileExtensionComparator();

    assertThat(fileExtensionComparator.compare(this.mockFileOne, this.mockFileOne)).isEqualTo(0);
    assertThat(fileExtensionComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(fileExtensionComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);

    verify(this.mockFileOne, times(4)).getName();
    verify(this.mockFileTwo, times(2)).getName();
  }

  @Test
  public void fileLastModifiedComparator() {

    when(this.mockFileOne.lastModified()).thenReturn(1l);
    when(this.mockFileTwo.lastModified()).thenReturn(2l);

    Comparator<File> fileLastModifiedComparator = FileComparatorFactory.fileLastModifiedComparator();

    assertThat(fileLastModifiedComparator.compare(this.mockFileOne, this.mockFileOne)).isEqualTo(0);
    assertThat(fileLastModifiedComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(fileLastModifiedComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);
    assertThat(fileLastModifiedComparator.compare(this.mockFileTwo, this.mockFileTwo)).isEqualTo(0);

    verify(this.mockFileOne, times(4)).lastModified();
    verify(this.mockFileTwo, times(4)).lastModified();
  }

  @Test
  public void fileNameComparator() {

    when(this.mockFileOne.getName()).thenReturn("/path/to/source/fileOne.java");
    when(this.mockFileTwo.getName()).thenReturn("/path/to/source/fileTwo.java");

    Comparator<File> fileNameComparator = FileComparatorFactory.fileNameComparator();

    assertThat(fileNameComparator.compare(this.mockFileOne, this.mockFileOne)).isEqualTo(0);
    assertThat(fileNameComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(fileNameComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);
    assertThat(fileNameComparator.compare(this.mockFileTwo, this.mockFileTwo)).isEqualTo(0);

    verify(this.mockFileOne, times(4)).getName();
    verify(this.mockFileTwo, times(4)).getName();
  }

  @Test
  public void fileNameComparatorComparesFileWithoutNameToFileWithNameSuccessfully() {

    when(this.mockFileOne.getName()).thenReturn("/path/to/.ext");
    when(this.mockFileTwo.getName()).thenReturn("/path/to/file.ext");

    Comparator<File> fileNameComparator = FileComparatorFactory.fileNameComparator();

    assertThat(fileNameComparator.compare(this.mockFileOne, this.mockFileOne)).isEqualTo(0);
    assertThat(fileNameComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(fileNameComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);

    verify(this.mockFileOne, times(4)).getName();
    verify(this.mockFileTwo, times(2)).getName();
  }

  @Test
  public void filePathComparator() {

    when(this.mockFileOne.getAbsolutePath()).thenReturn("/absolute/path/to/file.ext");
    when(this.mockFileTwo.getAbsolutePath()).thenReturn("/relative/path/to/file.ext");

    Comparator<File> filePathComparator = FileComparatorFactory.filePathComparator();

    assertThat(filePathComparator.compare(this.mockFileOne, this.mockFileOne)).isEqualTo(0);
    assertThat(filePathComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(filePathComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);
    assertThat(filePathComparator.compare(this.mockFileTwo, this.mockFileTwo)).isEqualTo(0);

    verify(this.mockFileOne, times(4)).getAbsolutePath();
    verify(this.mockFileTwo, times(4)).getAbsolutePath();
  }

  @Test
  // NOTE: sorts directories before files appropriately
  public void filePathComparatorComparesFileWithoutPathToFileWithPathSuccessfully() {

    when(this.mockFileOne.getAbsolutePath()).thenReturn("file.ext");
    when(this.mockFileTwo.getAbsolutePath()).thenReturn("/a/path/to/file.ext");

    Comparator<File> filePathComparator = FileComparatorFactory.filePathComparator();

    assertThat(filePathComparator.compare(this.mockFileOne, this.mockFileOne)).isEqualTo(0);
    assertThat(filePathComparator.compare(this.mockFileOne, this.mockFileTwo)).isGreaterThan(0);
    assertThat(filePathComparator.compare(this.mockFileTwo, this.mockFileOne)).isLessThan(0);

    verify(this.mockFileOne, times(4)).getAbsolutePath();
    verify(this.mockFileTwo, times(2)).getAbsolutePath();
  }

  @Test
  public void fileSizeComparator() {

    when(this.mockFileOne.length()).thenReturn(1024l);
    when(this.mockFileTwo.length()).thenReturn(1024000l);

    Comparator<File> fileSizeComparator = FileComparatorFactory.fileSizeComparator();

    assertThat(fileSizeComparator.compare(this.mockFileOne, this.mockFileOne)).isEqualTo(0);
    assertThat(fileSizeComparator.compare(this.mockFileOne, this.mockFileTwo)).isLessThan(0);
    assertThat(fileSizeComparator.compare(this.mockFileTwo, this.mockFileOne)).isGreaterThan(0);
    assertThat(fileSizeComparator.compare(this.mockFileTwo, this.mockFileTwo)).isEqualTo(0);

    verify(this.mockFileOne, times(4)).length();
    verify(this.mockFileTwo, times(4)).length();
  }
}
