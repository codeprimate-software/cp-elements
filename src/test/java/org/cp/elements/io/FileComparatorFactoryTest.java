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

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * The FileComparatorFactoryTest class is a test suite of test cases testing the contract and functionality
 * of the FileComparatorFactory class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.util.Comparator
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.cp.elements.io.FileComparatorFactory
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
@SuppressWarnings("all")
@RunWith(MockitoJUnitRunner.class)
public class FileComparatorFactoryTest {

  @Mock
  private File mockFileOne;

  @Mock
  private File mockFileTwo;

  @Test
  public void fileExtensionComparator() {
    when(mockFileOne.getName()).thenReturn("/path/to/source/file.groovy");
    when(mockFileTwo.getName()).thenReturn("/path/to/source/file.java");

    assertThat(FileComparatorFactory.fileExtensionComparator().compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(FileComparatorFactory.fileExtensionComparator().compare(mockFileOne, mockFileTwo), is(lessThan(0)));
    assertThat(FileComparatorFactory.fileExtensionComparator().compare(mockFileTwo, mockFileOne), is(greaterThan(0)));
    assertThat(FileComparatorFactory.fileExtensionComparator().compare(mockFileTwo, mockFileTwo), is(equalTo(0)));

    verify(mockFileOne, times(4)).getName();
    verify(mockFileTwo, times(4)).getName();
  }

  @Test
  public void fileLastModifiedComparator() {
    when(mockFileOne.lastModified()).thenReturn(1l);
    when(mockFileTwo.lastModified()).thenReturn(2l);

    assertThat(FileComparatorFactory.fileLastModifiedComparator().compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(FileComparatorFactory.fileLastModifiedComparator().compare(mockFileOne, mockFileTwo), is(lessThan(0)));
    assertThat(FileComparatorFactory.fileLastModifiedComparator().compare(mockFileTwo, mockFileOne), is(greaterThan(0)));
    assertThat(FileComparatorFactory.fileLastModifiedComparator().compare(mockFileTwo, mockFileTwo), is(equalTo(0)));

    verify(mockFileOne, times(4)).lastModified();
    verify(mockFileTwo, times(4)).lastModified();
  }

  @Test
  public void fileNameComparator() {
    when(mockFileOne.getName()).thenReturn("/path/to/source/fileOne.java");
    when(mockFileTwo.getName()).thenReturn("/path/to/source/fileTwo.java");

    assertThat(FileComparatorFactory.fileNameComparator().compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(FileComparatorFactory.fileNameComparator().compare(mockFileOne, mockFileTwo), is(lessThan(0)));
    assertThat(FileComparatorFactory.fileNameComparator().compare(mockFileTwo, mockFileOne), is(greaterThan(0)));
    assertThat(FileComparatorFactory.fileNameComparator().compare(mockFileTwo, mockFileTwo), is(equalTo(0)));

    verify(mockFileOne, times(4)).getName();
    verify(mockFileTwo, times(4)).getName();
  }

  @Test
  public void filePathComparator() {
    when(mockFileOne.getAbsolutePath()).thenReturn("/absolute/path/to/file.ext");
    when(mockFileTwo.getAbsolutePath()).thenReturn("/relative/path/to/file.ext");

    assertThat(FileComparatorFactory.filePathComparator().compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(FileComparatorFactory.filePathComparator().compare(mockFileOne, mockFileTwo), is(lessThan(0)));
    assertThat(FileComparatorFactory.filePathComparator().compare(mockFileTwo, mockFileOne), is(greaterThan(0)));
    assertThat(FileComparatorFactory.filePathComparator().compare(mockFileTwo, mockFileTwo), is(equalTo(0)));

    verify(mockFileOne, times(4)).getAbsolutePath();
    verify(mockFileTwo, times(4)).getAbsolutePath();
  }

  @Test
  public void fileSizeComparator() {
    when(mockFileOne.length()).thenReturn(1024l);
    when(mockFileTwo.length()).thenReturn(1024000l);

    assertThat(FileComparatorFactory.fileSizeComparator().compare(mockFileOne, mockFileOne), is(equalTo(0)));
    assertThat(FileComparatorFactory.fileSizeComparator().compare(mockFileOne, mockFileTwo), is(lessThan(0)));
    assertThat(FileComparatorFactory.fileSizeComparator().compare(mockFileTwo, mockFileOne), is(greaterThan(0)));
    assertThat(FileComparatorFactory.fileSizeComparator().compare(mockFileTwo, mockFileTwo), is(equalTo(0)));

    verify(mockFileOne, times(4)).length();
    verify(mockFileTwo, times(4)).length();
  }

}
