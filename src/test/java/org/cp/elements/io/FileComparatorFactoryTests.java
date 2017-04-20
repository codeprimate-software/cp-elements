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

package org.cp.elements.io;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.Assert.assertThat;
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
 * The FileComparatorFactoryTests class is a test suite of test cases testing the contract and functionality
 * of the {@link FileComparatorFactory} class.
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
    when(mockFileOne.getName()).thenReturn("/path/to/source/file.groovy");
    when(mockFileTwo.getName()).thenReturn("/path/to/source/file.java");

    Comparator<File> fileExtensionComparator = FileComparatorFactory.fileExtensionComparator();

    assertThat(fileExtensionComparator.compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(fileExtensionComparator.compare(mockFileOne, mockFileTwo), is(lessThan(0)));
    assertThat(fileExtensionComparator.compare(mockFileTwo, mockFileOne), is(greaterThan(0)));
    assertThat(fileExtensionComparator.compare(mockFileTwo, mockFileTwo), is(equalTo(0)));

    verify(mockFileOne, times(4)).getName();
    verify(mockFileTwo, times(4)).getName();
  }

  @Test
  public void fileExtensionComparatorComparesFileWithoutExtensionToFileWithExtensionSuccessfully() {
    when(mockFileOne.getName()).thenReturn("/path/to/file");
    when(mockFileTwo.getName()).thenReturn("/path/to/file.ext");

    Comparator<File> fileExtensionComparator = FileComparatorFactory.fileExtensionComparator();

    assertThat(fileExtensionComparator.compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(fileExtensionComparator.compare(mockFileOne, mockFileTwo), is(lessThan(0)));
    assertThat(fileExtensionComparator.compare(mockFileTwo, mockFileOne), is(greaterThan(0)));

    verify(mockFileOne, times(4)).getName();
    verify(mockFileTwo, times(2)).getName();
  }

  @Test
  public void fileLastModifiedComparator() {
    when(mockFileOne.lastModified()).thenReturn(1l);
    when(mockFileTwo.lastModified()).thenReturn(2l);

    Comparator<File> fileLastModifiedComparator = FileComparatorFactory.fileLastModifiedComparator();

    assertThat(fileLastModifiedComparator.compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(fileLastModifiedComparator.compare(mockFileOne, mockFileTwo), is(lessThan(0)));
    assertThat(fileLastModifiedComparator.compare(mockFileTwo, mockFileOne), is(greaterThan(0)));
    assertThat(fileLastModifiedComparator.compare(mockFileTwo, mockFileTwo), is(equalTo(0)));

    verify(mockFileOne, times(4)).lastModified();
    verify(mockFileTwo, times(4)).lastModified();
  }

  @Test
  public void fileNameComparator() {
    when(mockFileOne.getName()).thenReturn("/path/to/source/fileOne.java");
    when(mockFileTwo.getName()).thenReturn("/path/to/source/fileTwo.java");

    Comparator<File> fileNameComparator = FileComparatorFactory.fileNameComparator();

    assertThat(fileNameComparator.compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(fileNameComparator.compare(mockFileOne, mockFileTwo), is(lessThan(0)));
    assertThat(fileNameComparator.compare(mockFileTwo, mockFileOne), is(greaterThan(0)));
    assertThat(fileNameComparator.compare(mockFileTwo, mockFileTwo), is(equalTo(0)));

    verify(mockFileOne, times(4)).getName();
    verify(mockFileTwo, times(4)).getName();
  }

  @Test
  public void fileNameComparatorComparesFileWithoutNameToFileWithNameSuccessfully() {
    when(mockFileOne.getName()).thenReturn("/path/to/.ext");
    when(mockFileTwo.getName()).thenReturn("/path/to/file.ext");

    Comparator<File> fileNameComparator = FileComparatorFactory.fileNameComparator();

    assertThat(fileNameComparator.compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(fileNameComparator.compare(mockFileOne, mockFileTwo), is(lessThan(0)));
    assertThat(fileNameComparator.compare(mockFileTwo, mockFileOne), is(greaterThan(0)));

    verify(mockFileOne, times(4)).getName();
    verify(mockFileTwo, times(2)).getName();
  }

  @Test
  public void filePathComparator() {
    when(mockFileOne.getAbsolutePath()).thenReturn("/absolute/path/to/file.ext");
    when(mockFileTwo.getAbsolutePath()).thenReturn("/relative/path/to/file.ext");

    Comparator<File> filePathComparator = FileComparatorFactory.filePathComparator();

    assertThat(filePathComparator.compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(filePathComparator.compare(mockFileOne, mockFileTwo), is(lessThan(0)));
    assertThat(filePathComparator.compare(mockFileTwo, mockFileOne), is(greaterThan(0)));
    assertThat(filePathComparator.compare(mockFileTwo, mockFileTwo), is(equalTo(0)));

    verify(mockFileOne, times(4)).getAbsolutePath();
    verify(mockFileTwo, times(4)).getAbsolutePath();
  }

  @Test
  // NOTE: sorts directories before files appropriately
  public void filePathComparatorComparesFileWithoutPathToFileWithPathSuccessfully() {
    when(mockFileOne.getAbsolutePath()).thenReturn("file.ext");
    when(mockFileTwo.getAbsolutePath()).thenReturn("/a/path/to/file.ext");

    Comparator<File> filePathComparator = FileComparatorFactory.filePathComparator();

    assertThat(filePathComparator.compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(filePathComparator.compare(mockFileOne, mockFileTwo), is(greaterThan(0)));
    assertThat(filePathComparator.compare(mockFileTwo, mockFileOne), is(lessThan(0)));

    verify(mockFileOne, times(4)).getAbsolutePath();
    verify(mockFileTwo, times(2)).getAbsolutePath();
  }

  @Test
  public void fileSizeComparator() {
    when(mockFileOne.length()).thenReturn(1024l);
    when(mockFileTwo.length()).thenReturn(1024000l);

    Comparator<File> fileSizeComparator = FileComparatorFactory.fileSizeComparator();

    assertThat(fileSizeComparator.compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(fileSizeComparator.compare(mockFileOne, mockFileTwo), is(lessThan(0)));
    assertThat(fileSizeComparator.compare(mockFileTwo, mockFileOne), is(greaterThan(0)));
    assertThat(fileSizeComparator.compare(mockFileTwo, mockFileTwo), is(equalTo(0)));

    verify(mockFileOne, times(4)).length();
    verify(mockFileTwo, times(4)).length();
  }
}
