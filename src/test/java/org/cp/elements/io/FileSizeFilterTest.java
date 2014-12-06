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

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileFilter;

import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The FileSizeFilterTest class is a test suite of test cases testing the contract and functionality
 * of the FileSizeFilter class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.jmock.Mockery
 * @see org.junit.Test
 * @see org.cp.elements.io.FileSizeFilter
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @since 1.0.0
 */
public class FileSizeFilterTest extends AbstractMockingTestSuite {

  @Test
  public void testAcceptBetween() {
    FileSizeFilter fileFilter = FileSizeFilter.between(2048l, 4096l);

    final File fileBetween = mock(File.class, "testAcceptBetween.fileBetween");
    final File fileBigger = mock(File.class, "testAcceptBetween.fileBigger");
    final File fileOnMin = mock(File.class, "testAcceptBetween.fileOnMin");
    final File fileOnMax = mock(File.class, "testAcceptBetween.fileOnMax");
    final File fileSmaller = mock(File.class, "testAcceptBetween.fileSmaller");

    checking(new Expectations() {{
      allowing(fileBetween).length();
      will(returnValue(3072l));
      allowing(fileBigger).length();
      will(returnValue(8192l));
      allowing(fileOnMin).length();
      will(returnValue(2048l));
      allowing(fileOnMax).length();
      will(returnValue(4096l));
      allowing(fileSmaller).length();
      will(returnValue(1024l));
    }});

    assertFalse(fileFilter.accept(fileSmaller));
    assertTrue(fileFilter.accept(fileOnMin));
    assertTrue(fileFilter.accept(fileBetween));
    assertTrue(fileFilter.accept(fileOnMax));
    assertFalse(fileFilter.accept(fileBigger));
  }

  @Test
  public void testAcceptOutside() {
    FileFilter fileFilter = new InverseFileFilter(FileSizeFilter.between(2048l, 4096l));

    final File fileBetween = mock(File.class, "testAcceptOutside.fileBetween");
    final File fileBigger = mock(File.class, "testAcceptOutside.fileBigger");
    final File fileOnMin = mock(File.class, "testAcceptOutside.fileOnMin");
    final File fileOnMax = mock(File.class, "testAcceptOutside.fileOnMax");
    final File fileSmaller = mock(File.class, "testAcceptOutside.fileSmaller");

    checking(new Expectations() {{
      allowing(fileBetween).length();
      will(returnValue(3072l));
      allowing(fileBigger).length();
      will(returnValue(8192l));
      allowing(fileOnMin).length();
      will(returnValue(2048l));
      allowing(fileOnMax).length();
      will(returnValue(4096l));
      allowing(fileSmaller).length();
      will(returnValue(1024l));
    }});

    assertTrue(fileFilter.accept(fileSmaller));
    assertFalse(fileFilter.accept(fileOnMin));
    assertFalse(fileFilter.accept(fileBetween));
    assertFalse(fileFilter.accept(fileOnMax));
    assertTrue(fileFilter.accept(fileBigger));
  }

  @Test
  public void testAcceptEqualTo() {
    FileSizeFilter fileFilter = FileSizeFilter.equalTo(4096l);

    final File fileBigger = mock(File.class, "testAcceptEqualTo.fileBigger");
    final File fileOn = mock(File.class, "testAcceptEqualTo.fileOn");
    final File fileSmaller = mock(File.class, "testAcceptEqualTo.fileSmaller");

    checking(new Expectations() {{
      allowing(fileBigger).length();
      will(returnValue(8192l));
      allowing(fileOn).length();
      will(returnValue(4096l));
      allowing(fileSmaller).length();
      will(returnValue(2048l));
    }});

    assertFalse(fileFilter.accept(fileSmaller));
    assertTrue(fileFilter.accept(fileOn));
    assertFalse(fileFilter.accept(fileBigger));
  }

  @Test
  public void testAcceptNotEqualTo() {
    FileFilter fileFilter = new InverseFileFilter(FileSizeFilter.equalTo(4096l));

    final File fileBigger = mock(File.class, "testAcceptNotEqualTo.fileBigger");
    final File fileOn = mock(File.class, "testAcceptNotEqualTo.fileOn");
    final File fileSmaller = mock(File.class, "testAcceptNotEqualTo.fileSmaller");

    checking(new Expectations() {{
      allowing(fileBigger).length();
      will(returnValue(8192l));
      allowing(fileOn).length();
      will(returnValue(4096l));
      allowing(fileSmaller).length();
      will(returnValue(2048l));
    }});

    assertTrue(fileFilter.accept(fileSmaller));
    assertFalse(fileFilter.accept(fileOn));
    assertTrue(fileFilter.accept(fileBigger));
  }

  @Test
  public void testAcceptGreaterThan() {
    FileSizeFilter fileFilter = FileSizeFilter.greaterThan(4096l);

    final File fileBigger = mock(File.class, "testAcceptGreaterThan.fileBigger");
    final File fileOn = mock(File.class, "testAcceptGreaterThan.fileOn");
    final File fileSmaller = mock(File.class, "testAcceptGreaterThan.fileSmaller");

    checking(new Expectations() {{
      allowing(fileBigger).length();
      will(returnValue(8192l));
      allowing(fileOn).length();
      will(returnValue(4096l));
      allowing(fileSmaller).length();
      will(returnValue(2048l));
    }});

    assertFalse(fileFilter.accept(fileSmaller));
    assertFalse(fileFilter.accept(fileOn));
    assertTrue(fileFilter.accept(fileBigger));
  }

  @Test
  public void testAcceptGreaterThanEqualTo() {
    FileFilter fileFilter = ComposableFileFilter.or(FileSizeFilter.greaterThan(4096l), FileSizeFilter.equalTo(4096l));

    final File fileBigger = mock(File.class, "testAcceptGreaterThanEqualTo.fileBigger");
    final File fileOn = mock(File.class, "testAcceptGreaterThanEqualTo.fileOn");
    final File fileSmaller = mock(File.class, "testAcceptGreaterThanEqualTo.fileSmaller");

    checking(new Expectations() {{
      allowing(fileBigger).length();
      will(returnValue(8192l));
      allowing(fileOn).length();
      will(returnValue(4096l));
      allowing(fileSmaller).length();
      will(returnValue(2048l));
    }});

    assertFalse(fileFilter.accept(fileSmaller));
    assertTrue(fileFilter.accept(fileOn));
    assertTrue(fileFilter.accept(fileBigger));
  }

  @Test
  public void testAcceptLessThan() {
    FileSizeFilter fileFilter = FileSizeFilter.lessThan(4096l);

    final File fileBigger = mock(File.class, "testAcceptLessThan.fileBigger");
    final File fileOn = mock(File.class, "testAcceptLessThan.fileOn");
    final File fileSmaller = mock(File.class, "testAcceptLessThan.fileSmaller");

    checking(new Expectations() {{
      allowing(fileBigger).length();
      will(returnValue(8192l));
      allowing(fileOn).length();
      will(returnValue(4096l));
      allowing(fileSmaller).length();
      will(returnValue(2048l));
    }});

    assertTrue(fileFilter.accept(fileSmaller));
    assertFalse(fileFilter.accept(fileOn));
    assertFalse(fileFilter.accept(fileBigger));
  }

  @Test
  public void testAcceptLessThanEqualTo() {
    FileFilter fileFilter = ComposableFileFilter.or(FileSizeFilter.lessThan(4096l), FileSizeFilter.equalTo(4096l));

    final File fileBigger = mock(File.class, "testAcceptLessThanEqualTo.fileBigger");
    final File fileOn = mock(File.class, "testAcceptLessThanEqualTo.fileOn");
    final File fileSmaller = mock(File.class, "testAcceptLessThanEqualTo.fileSmaller");

    checking(new Expectations() {{
      allowing(fileBigger).length();
      will(returnValue(8192l));
      allowing(fileOn).length();
      will(returnValue(4096l));
      allowing(fileSmaller).length();
      will(returnValue(2048l));
    }});

    assertTrue(fileFilter.accept(fileSmaller));
    assertTrue(fileFilter.accept(fileOn));
    assertFalse(fileFilter.accept(fileBigger));
  }

}
