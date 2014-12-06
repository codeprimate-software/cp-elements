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

import static org.cp.elements.test.TestUtils.*;

import java.io.File;

import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The FileComparatorFactoryTest class is a test suite of test cases testing the contract and functionality
 * of the FileComparatorFactory class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.jmock.Mockery
 * @see org.junit.Test
 * @see org.cp.elements.io.FileComparatorFactory
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
public class FileComparatorFactoryTest extends AbstractMockingTestSuite {

  @Test
  public void testFileExtensionComparator() {
    final File mockFileOne = mock(File.class, "testFileExtensionComparator.mockFileOne");
    final File mockFileTwo = mock(File.class, "testFileExtensionComparator.mockFileTwo");

    mockContext.checking(new Expectations() {{
      allowing(mockFileOne).getName();
      will(returnValue("/path/to/fileOne.groovy"));
      allowing(mockFileTwo).getName();
      will(returnValue("/path/to/fileTwo.java"));
    }});

    assertZero(FileComparatorFactory.fileExtensionComparator().compare(mockFileOne, mockFileOne));
    assertNegative(FileComparatorFactory.fileExtensionComparator().compare(mockFileOne, mockFileTwo));
    assertPositive(FileComparatorFactory.fileExtensionComparator().compare(mockFileTwo, mockFileOne));
    assertZero(FileComparatorFactory.fileExtensionComparator().compare(mockFileTwo, mockFileTwo));
  }

  @Test
  public void testFileLastModifiedComparator() {
    final File mockFileOne = mock(File.class, "testFileLastModifiedComparator.mockFileOne");
    final File mockFileTwo = mock(File.class, "testFileLastModifiedComparator.mockFileTwo");

    mockContext.checking(new Expectations() {{
      allowing(mockFileOne).lastModified();
      will(returnValue(System.currentTimeMillis()));
      allowing(mockFileTwo).lastModified();
      will(returnValue(System.currentTimeMillis() + 100000));
    }});

    assertZero(FileComparatorFactory.fileLastModifiedComparator().compare(mockFileOne, mockFileOne));
    assertNegative(FileComparatorFactory.fileLastModifiedComparator().compare(mockFileOne, mockFileTwo));
    assertPositive(FileComparatorFactory.fileLastModifiedComparator().compare(mockFileTwo, mockFileOne));
    assertZero(FileComparatorFactory.fileLastModifiedComparator().compare(mockFileTwo, mockFileTwo));
  }

  @Test
  public void testFileNameComparator() {
    final File mockFileOne = mock(File.class, "testFileNameComparator.mockFileOne");
    final File mockFileTwo = mock(File.class, "testFileNameComparator.mockFileTwo");

    mockContext.checking(new Expectations() {{
      allowing(mockFileOne).getName();
      will(returnValue("/path/to/fileOne.ext"));
      allowing(mockFileTwo).getName();
      will(returnValue("/path/to/fileTwo.ext"));
    }});

    assertZero(FileComparatorFactory.fileNameComparator().compare(mockFileOne, mockFileOne));
    assertNegative(FileComparatorFactory.fileNameComparator().compare(mockFileOne, mockFileTwo));
    assertPositive(FileComparatorFactory.fileNameComparator().compare(mockFileTwo, mockFileOne));
    assertZero(FileComparatorFactory.fileNameComparator().compare(mockFileTwo, mockFileTwo));
  }

  @Test
  public void testFilePathComparator() {
    final File mockFileOne = mock(File.class, "testFilePathComparator.mockFileOne");
    final File mockFileTwo = mock(File.class, "testFilePathComparator.mockFileTwo");

    mockContext.checking(new Expectations() {{
      allowing(mockFileOne).getAbsolutePath();
      will(returnValue("/a/path/to/fileOne.ext"));
      allowing(mockFileTwo).getAbsolutePath();
      will(returnValue("/different/path/to/fileTwo.ext"));
    }});

    assertZero(FileComparatorFactory.filePathComparator().compare(mockFileOne, mockFileOne));
    assertNegative(FileComparatorFactory.filePathComparator().compare(mockFileOne, mockFileTwo));
    assertPositive(FileComparatorFactory.filePathComparator().compare(mockFileTwo, mockFileOne));
    assertZero(FileComparatorFactory.filePathComparator().compare(mockFileTwo, mockFileTwo));
  }

  @Test
  public void testFileSizeComparator() {
    final File mockFileOne = mock(File.class, "testFileSizeComparator.mockFileOne");
    final File mockFileTwo = mock(File.class, "testFileSizeComparator.mockFileTwo");

    mockContext.checking(new Expectations() {{
      allowing(mockFileOne).length();
      will(returnValue(1024l));
      allowing(mockFileTwo).length();
      will(returnValue(1024000l));
    }});

    assertZero(FileComparatorFactory.fileSizeComparator().compare(mockFileOne, mockFileOne));
    assertNegative(FileComparatorFactory.fileSizeComparator().compare(mockFileOne, mockFileTwo));
    assertPositive(FileComparatorFactory.fileSizeComparator().compare(mockFileTwo, mockFileOne));
    assertZero(FileComparatorFactory.fileSizeComparator().compare(mockFileTwo, mockFileTwo));
  }

}
