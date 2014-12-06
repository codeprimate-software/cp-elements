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
import java.util.Calendar;

import org.cp.elements.lang.RelationalOperator;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.cp.elements.test.TestUtils;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The FileLastModifiedFilterTest class is a test suite of test cases testing the contract and functionality
 * of the FileLastModifiedFilter class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.jmock.Mockery
 * @see org.junit.Test
 * @see org.cp.elements.io.FileLastModifiedFilter
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
public class FileLastModifiedFilterTest extends AbstractMockingTestSuite {

  @Test
  public void testAcceptAfter() {
    FileLastModifiedFilter fileFilter = FileLastModifiedFilter.after(TestUtils.createCalendar(
      2014, Calendar.DECEMBER, 6));

    final File fileAfter = mock(File.class, "testAcceptAfter.fileAfter");
    final File fileBefore = mock(File.class, "testAcceptAfter.fileBefore");
    final File fileOn = mock(File.class, "testAcceptAfter.fileOn");

    checking(new Expectations() {{
      allowing(fileAfter).lastModified();
      will(returnValue(TestUtils.createCalendar(2024, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileBefore).lastModified();
      will(returnValue(TestUtils.createCalendar(2004, Calendar.DECEMBER, 31).getTimeInMillis()));
      allowing(fileOn).lastModified();
      will(returnValue(TestUtils.createCalendar(2014, Calendar.DECEMBER, 6).getTimeInMillis()));
    }});

    assertFalse(fileFilter.accept(fileBefore));
    assertFalse(fileFilter.accept(fileOn));
    assertTrue(fileFilter.accept(fileAfter));
  }

  @Test
  public void testAcceptBefore() {
    FileLastModifiedFilter fileFilter = FileLastModifiedFilter.before(TestUtils.createCalendar(
      2014, Calendar.DECEMBER, 6));

    final File fileAfter = mock(File.class, "testAcceptBefore.fileAfter");
    final File fileBefore = mock(File.class, "testAcceptBefore.fileBefore");
    final File fileOn = mock(File.class, "testAcceptBefore.fileOn");

    checking(new Expectations() {{
      allowing(fileAfter).lastModified();
      will(returnValue(TestUtils.createCalendar(2024, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileBefore).lastModified();
      will(returnValue(TestUtils.createCalendar(2004, Calendar.DECEMBER, 31).getTimeInMillis()));
      allowing(fileOn).lastModified();
      will(returnValue(TestUtils.createCalendar(2014, Calendar.DECEMBER, 6).getTimeInMillis()));
    }});

    assertTrue(fileFilter.accept(fileBefore));
    assertFalse(fileFilter.accept(fileOn));
    assertFalse(fileFilter.accept(fileAfter));
  }

  @Test
  public void testAcceptDuring() {
    FileLastModifiedFilter fileFilter = FileLastModifiedFilter.during(TestUtils.createCalendar(
      2014, Calendar.DECEMBER, 6), TestUtils.createCalendar(2015, Calendar.DECEMBER, 5));

    final File fileAfter = mock(File.class, "testAcceptDuring.fileAfter");
    final File fileBefore = mock(File.class, "testAcceptDuring.fileBefore");
    final File fileDuring = mock(File.class, "testAcceptDuring.fileDuring");
    final File fileOnStart = mock(File.class, "testAcceptDuring.fileOnStart");
    final File fileOnEnd = mock(File.class, "testAcceptDuring.fileOnEnd");

    checking(new Expectations() {{
      allowing(fileAfter).lastModified();
      will(returnValue(TestUtils.createCalendar(2024, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileBefore).lastModified();
      will(returnValue(TestUtils.createCalendar(2004, Calendar.DECEMBER, 31).getTimeInMillis()));
      allowing(fileDuring).lastModified();
      will(returnValue(TestUtils.createCalendar(2015, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileOnStart).lastModified();
      will(returnValue(TestUtils.createCalendar(2014, Calendar.DECEMBER, 6).getTimeInMillis()));
      allowing(fileOnEnd).lastModified();
      will(returnValue(TestUtils.createCalendar(2015, Calendar.DECEMBER, 5).getTimeInMillis()));
    }});

    assertFalse(fileFilter.accept(fileBefore));
    assertTrue(fileFilter.accept(fileDuring));
    assertTrue(fileFilter.accept(fileOnStart));
    assertTrue(fileFilter.accept(fileOnEnd));
    assertFalse(fileFilter.accept(fileAfter));
  }

  @Test
  public void testAcceptOn() {
    FileLastModifiedFilter fileFilter = FileLastModifiedFilter.on(TestUtils.createCalendar(
      2014, Calendar.DECEMBER, 6));

    final File fileAfter = mock(File.class, "testAcceptOn.fileAfter");
    final File fileBefore = mock(File.class, "testAcceptOn.fileBefore");
    final File fileOn = mock(File.class, "testAcceptOn.fileOn");

    checking(new Expectations() {{
      allowing(fileAfter).lastModified();
      will(returnValue(TestUtils.createCalendar(2024, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileBefore).lastModified();
      will(returnValue(TestUtils.createCalendar(2004, Calendar.DECEMBER, 31).getTimeInMillis()));
      allowing(fileOn).lastModified();
      will(returnValue(TestUtils.createCalendar(2014, Calendar.DECEMBER, 6).getTimeInMillis()));
    }});

    assertFalse(fileFilter.accept(fileBefore));
    assertTrue(fileFilter.accept(fileOn));
    assertFalse(fileFilter.accept(fileAfter));
  }

  @Test
  public void testAcceptOnOrAfter() {
    final long lastModified  = TestUtils.createCalendar(2014, Calendar.DECEMBER, 6).getTimeInMillis();

    FileFilter fileFilter = ComposableFileFilter.or(FileLastModifiedFilter.on(lastModified),
      FileLastModifiedFilter.after(lastModified));

    final File fileAfter = mock(File.class, "testAcceptOnOrAfter.fileAfter");
    final File fileBefore = mock(File.class, "testAcceptOnOrAfter.fileBefore");
    final File fileOn = mock(File.class, "testAcceptOnOrAfter.fileOn");

    checking(new Expectations() {{
      allowing(fileAfter).lastModified();
      will(returnValue(TestUtils.createCalendar(2024, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileBefore).lastModified();
      will(returnValue(TestUtils.createCalendar(2004, Calendar.DECEMBER, 31).getTimeInMillis()));
      allowing(fileOn).lastModified();
      will(returnValue(TestUtils.createCalendar(2014, Calendar.DECEMBER, 6).getTimeInMillis()));
    }});

    assertFalse(fileFilter.accept(fileBefore));
    assertTrue(fileFilter.accept(fileOn));
    assertTrue(fileFilter.accept(fileAfter));
  }

  @Test
  public void testAcceptOnOrBefore() {
    final long lastModified  = TestUtils.createCalendar(2014, Calendar.DECEMBER, 6).getTimeInMillis();

    FileFilter fileFilter = ComposableFileFilter.or(FileLastModifiedFilter.on(lastModified),
      FileLastModifiedFilter.before(lastModified));

    final File fileAfter = mock(File.class, "testAcceptOnOrBefore.fileAfter");
    final File fileBefore = mock(File.class, "testAcceptOnOrBefore.fileBefore");
    final File fileOn = mock(File.class, "testAcceptOnOrBefore.fileOn");

    checking(new Expectations() {{
      allowing(fileAfter).lastModified();
      will(returnValue(TestUtils.createCalendar(2024, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileBefore).lastModified();
      will(returnValue(TestUtils.createCalendar(2004, Calendar.DECEMBER, 31).getTimeInMillis()));
      allowing(fileOn).lastModified();
      will(returnValue(TestUtils.createCalendar(2014, Calendar.DECEMBER, 6).getTimeInMillis()));
    }});

    assertTrue(fileFilter.accept(fileBefore));
    assertTrue(fileFilter.accept(fileOn));
    assertFalse(fileFilter.accept(fileAfter));
  }

  @Test
  public void testAcceptAfterAndBefore() {
    final long afterLastModified  = TestUtils.createCalendar(2014, Calendar.DECEMBER, 6).getTimeInMillis();
    final long beforeLastModified  = TestUtils.createCalendar(2015, Calendar.DECEMBER, 5).getTimeInMillis();

    FileFilter fileFilter = ComposableFileFilter.and(FileLastModifiedFilter.after(afterLastModified),
      FileLastModifiedFilter.before(beforeLastModified));

    final File fileAfter = mock(File.class, "testAcceptAfterAndBefore.fileAfter");
    final File fileBefore = mock(File.class, "testAcceptAfterAndBefore.fileBefore");
    final File fileDuring = mock(File.class, "testAcceptAfterAndBefore.fileDuring");
    final File fileOnAfter = mock(File.class, "testAcceptAfterAndBefore.fileOnAfter");
    final File fileOnBefore = mock(File.class, "testAcceptAfterAndBefore.fileOnBefore");

    checking(new Expectations() {{
      allowing(fileAfter).lastModified();
      will(returnValue(TestUtils.createCalendar(2024, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileBefore).lastModified();
      will(returnValue(TestUtils.createCalendar(2004, Calendar.DECEMBER, 31).getTimeInMillis()));
      allowing(fileDuring).lastModified();
      will(returnValue(TestUtils.createCalendar(2015, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileOnAfter).lastModified();
      will(returnValue(afterLastModified));
      allowing(fileOnBefore).lastModified();
      will(returnValue(beforeLastModified));
    }});


    assertFalse(fileFilter.accept(fileBefore));
    assertFalse(fileFilter.accept(fileOnBefore));
    assertTrue(fileFilter.accept(fileDuring));
    assertFalse(fileFilter.accept(fileOnAfter));
    assertFalse(fileFilter.accept(fileAfter));
  }

  @Test
  public void testAcceptBeforeOrAfter() {
    final long beforeLastModified  = TestUtils.createCalendar(2014, Calendar.DECEMBER, 6).getTimeInMillis();
    final long afterLastModified  = TestUtils.createCalendar(2015, Calendar.DECEMBER, 5).getTimeInMillis();

    FileFilter fileFilter = ComposableFileFilter.or(FileLastModifiedFilter.before(beforeLastModified),
      FileLastModifiedFilter.after(afterLastModified));

    final File fileAfter = mock(File.class, "testAcceptAfterAndBefore.fileAfter");
    final File fileBefore = mock(File.class, "testAcceptAfterAndBefore.fileBefore");
    final File fileDuring = mock(File.class, "testAcceptAfterAndBefore.fileDuring");
    final File fileOnAfter = mock(File.class, "testAcceptAfterAndBefore.fileOnAfter");
    final File fileOnBefore = mock(File.class, "testAcceptAfterAndBefore.fileOnBefore");

    checking(new Expectations() {{
      allowing(fileAfter).lastModified();
      will(returnValue(TestUtils.createCalendar(2024, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileBefore).lastModified();
      will(returnValue(TestUtils.createCalendar(2004, Calendar.DECEMBER, 31).getTimeInMillis()));
      allowing(fileDuring).lastModified();
      will(returnValue(TestUtils.createCalendar(2015, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileOnAfter).lastModified();
      will(returnValue(afterLastModified));
      allowing(fileOnBefore).lastModified();
      will(returnValue(beforeLastModified));
    }});

    assertTrue(fileFilter.accept(fileBefore));
    assertFalse(fileFilter.accept(fileOnBefore));
    assertFalse(fileFilter.accept(fileDuring));
    assertFalse(fileFilter.accept(fileOnAfter));
    assertTrue(fileFilter.accept(fileAfter));
  }

  @Test
  public void testCreate() {
    final long beforeOrOnLastModified  = TestUtils.createCalendar(2014, Calendar.DECEMBER, 6).getTimeInMillis();
    final long afterOrOnLastModified  = TestUtils.createCalendar(2015, Calendar.DECEMBER, 5).getTimeInMillis();

    FileLastModifiedFilter fileFilter = FileLastModifiedFilter.create(
      RelationalOperator.lessThanEqualToOrGreaterThanEqualTo(beforeOrOnLastModified, afterOrOnLastModified));

    final File fileAfter = mock(File.class, "testAcceptAfterAndBefore.fileAfter");
    final File fileBefore = mock(File.class, "testAcceptAfterAndBefore.fileBefore");
    final File fileDuring = mock(File.class, "testAcceptAfterAndBefore.fileDuring");
    final File fileOnAfter = mock(File.class, "testAcceptAfterAndBefore.fileOnAfter");
    final File fileOnBefore = mock(File.class, "testAcceptAfterAndBefore.fileOnBefore");

    checking(new Expectations() {{
      allowing(fileAfter).lastModified();
      will(returnValue(TestUtils.createCalendar(2024, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileBefore).lastModified();
      will(returnValue(TestUtils.createCalendar(2004, Calendar.DECEMBER, 31).getTimeInMillis()));
      allowing(fileDuring).lastModified();
      will(returnValue(TestUtils.createCalendar(2015, Calendar.APRIL, 1).getTimeInMillis()));
      allowing(fileOnAfter).lastModified();
      will(returnValue(afterOrOnLastModified));
      allowing(fileOnBefore).lastModified();
      will(returnValue(beforeOrOnLastModified));
    }});

    assertTrue(fileFilter.accept(fileBefore));
    assertTrue(fileFilter.accept(fileOnBefore));
    assertFalse(fileFilter.accept(fileDuring));
    assertTrue(fileFilter.accept(fileOnAfter));
    assertTrue(fileFilter.accept(fileAfter));
  }

}
