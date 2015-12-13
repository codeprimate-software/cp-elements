/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.io;

import static org.cp.elements.test.TestUtils.assertNegative;
import static org.cp.elements.test.TestUtils.assertPositive;
import static org.cp.elements.test.TestUtils.assertZero;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;

import org.junit.Test;

/**
 * The FileComparatorFactoryTest class is a test suite of test cases testing the contract and functionality
 * of the FileComparatorFactory class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.util.Comparator
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.FileComparatorFactory
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class FileComparatorFactoryTest {

  @Test
  public void testFileExtensionComparator() {
    File mockFileOne = mock(File.class, "mockFileOne");
    File mockFileTwo = mock(File.class, "mockFileTwo");

    when(mockFileOne.getName()).thenReturn("/path/to/fileOne.groovy");
    when(mockFileTwo.getName()).thenReturn("/path/to/fileTwo.java");

    assertZero(FileComparatorFactory.fileExtensionComparator().compare(mockFileOne, mockFileOne));
    assertNegative(FileComparatorFactory.fileExtensionComparator().compare(mockFileOne, mockFileTwo));
    assertPositive(FileComparatorFactory.fileExtensionComparator().compare(mockFileTwo, mockFileOne));
    assertZero(FileComparatorFactory.fileExtensionComparator().compare(mockFileTwo, mockFileTwo));

    verify(mockFileOne, times(4)).getName();
    verify(mockFileTwo, times(4)).getName();
  }

  @Test
  public void testFileLastModifiedComparator() {
    File mockFileOne = mock(File.class, "mockFileOne");
    File mockFileTwo = mock(File.class, "mockFileTwo");

    when(mockFileOne.lastModified()).thenReturn(1l);
    when(mockFileTwo.lastModified()).thenReturn(2l);

    assertZero(FileComparatorFactory.fileLastModifiedComparator().compare(mockFileOne, mockFileOne));
    assertNegative(FileComparatorFactory.fileLastModifiedComparator().compare(mockFileOne, mockFileTwo));
    assertPositive(FileComparatorFactory.fileLastModifiedComparator().compare(mockFileTwo, mockFileOne));
    assertZero(FileComparatorFactory.fileLastModifiedComparator().compare(mockFileTwo, mockFileTwo));

    verify(mockFileOne, times(4)).lastModified();
    verify(mockFileTwo, times(4)).lastModified();
  }

  @Test
  public void testFileNameComparator() {
    File mockFileOne = mock(File.class, "mockFileOne");
    File mockFileTwo = mock(File.class, "mockFileTwo");

    when(mockFileOne.getName()).thenReturn("/path/to/fileOne.ext");
    when(mockFileTwo.getName()).thenReturn("/path/to/fileTwo.ext");

    assertZero(FileComparatorFactory.fileNameComparator().compare(mockFileOne, mockFileOne));
    assertNegative(FileComparatorFactory.fileNameComparator().compare(mockFileOne, mockFileTwo));
    assertPositive(FileComparatorFactory.fileNameComparator().compare(mockFileTwo, mockFileOne));
    assertZero(FileComparatorFactory.fileNameComparator().compare(mockFileTwo, mockFileTwo));

    verify(mockFileOne, times(4)).getName();
    verify(mockFileTwo, times(4)).getName();
  }

  @Test
  public void testFilePathComparator() {
    File mockFileOne = mock(File.class, "mockFileOne");
    File mockFileTwo = mock(File.class, "mockFileTwo");

    when(mockFileOne.getAbsolutePath()).thenReturn("/a/path/to/fileOne.ext");
    when(mockFileTwo.getAbsolutePath()).thenReturn("/different/path/to/fileTwo.ext");

    assertZero(FileComparatorFactory.filePathComparator().compare(mockFileOne, mockFileOne));
    assertNegative(FileComparatorFactory.filePathComparator().compare(mockFileOne, mockFileTwo));
    assertPositive(FileComparatorFactory.filePathComparator().compare(mockFileTwo, mockFileOne));
    assertZero(FileComparatorFactory.filePathComparator().compare(mockFileTwo, mockFileTwo));

    verify(mockFileOne, times(4)).getAbsolutePath();
    verify(mockFileTwo, times(4)).getAbsolutePath();
  }

  @Test
  public void testFileSizeComparator() {
    File mockFileOne = mock(File.class, "mockFileOne");
    File mockFileTwo = mock(File.class, "mockFileTwo");

    when(mockFileOne.length()).thenReturn(1024l);
    when(mockFileTwo.length()).thenReturn(1024000l);

    assertZero(FileComparatorFactory.fileSizeComparator().compare(mockFileOne, mockFileOne));
    assertNegative(FileComparatorFactory.fileSizeComparator().compare(mockFileOne, mockFileTwo));
    assertPositive(FileComparatorFactory.fileSizeComparator().compare(mockFileTwo, mockFileOne));
    assertZero(FileComparatorFactory.fileSizeComparator().compare(mockFileTwo, mockFileTwo));

    verify(mockFileOne, times(4)).length();
    verify(mockFileTwo, times(4)).length();
  }

}
