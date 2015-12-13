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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileFilter;
import java.util.Calendar;

import org.cp.elements.lang.RelationalOperator;
import org.cp.elements.test.TestUtils;
import org.junit.Test;

/**
 * The FileLastModifiedFilterTest class is a test suite of test cases testing the contract and functionality
 * of the FileLastModifiedFilter class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.FileLastModifiedFilter
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class FileLastModifiedFilterTest {

  protected long toMilliseconds(final int year, final int month, final int day) {
    return TestUtils.createCalendar(year, month, day).getTimeInMillis();
  }

  @Test
  public void testAcceptAfter() {
    FileLastModifiedFilter fileFilter = FileLastModifiedFilter.after(TestUtils.createCalendar(
      2014, Calendar.DECEMBER, 6));

    File fileAfter = mock(File.class, "fileAfter");
    File fileBefore = mock(File.class, "fileBefore");
    File fileOn = mock(File.class, "fileOn");

    when(fileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(fileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(fileOn.lastModified()).thenReturn(toMilliseconds(2014, Calendar.DECEMBER, 6));

    assertFalse(fileFilter.accept(fileBefore));
    assertFalse(fileFilter.accept(fileOn));
    assertTrue(fileFilter.accept(fileAfter));

    verify(fileAfter, times(1)).lastModified();
    verify(fileBefore, times(1)).lastModified();
    verify(fileOn, times(1)).lastModified();
  }

  @Test
  public void testAcceptBefore() {
    FileLastModifiedFilter fileFilter = FileLastModifiedFilter.before(TestUtils.createCalendar(
      2014, Calendar.DECEMBER, 6));

    File fileAfter = mock(File.class, "fileAfter");
    File fileBefore = mock(File.class, "fileBefore");
    File fileOn = mock(File.class, "fileOn");

    when(fileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(fileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(fileOn.lastModified()).thenReturn(toMilliseconds(2014, Calendar.DECEMBER, 6));

    assertTrue(fileFilter.accept(fileBefore));
    assertFalse(fileFilter.accept(fileOn));
    assertFalse(fileFilter.accept(fileAfter));

    verify(fileAfter, times(1)).lastModified();
    verify(fileBefore, times(1)).lastModified();
    verify(fileOn, times(1)).lastModified();
  }

  @Test
  public void testAcceptDuring() {
    FileLastModifiedFilter fileFilter = FileLastModifiedFilter.during(TestUtils.createCalendar(
      2014, Calendar.DECEMBER, 6), TestUtils.createCalendar(2015, Calendar.DECEMBER, 5));

    File fileAfter = mock(File.class, "fileAfter");
    File fileBefore = mock(File.class, "fileBefore");
    File fileDuring = mock(File.class, "fileDuring");
    File fileOnStart = mock(File.class, "fileOnStart");
    File fileOnEnd = mock(File.class, "fileOnEnd");

    when(fileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(fileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(fileDuring.lastModified()).thenReturn(toMilliseconds(2015, Calendar.APRIL, 1));
    when(fileOnStart.lastModified()).thenReturn(toMilliseconds(2014, Calendar.DECEMBER, 6));
    when(fileOnEnd.lastModified()).thenReturn(toMilliseconds(2015, Calendar.DECEMBER, 5));

    assertFalse(fileFilter.accept(fileBefore));
    assertTrue(fileFilter.accept(fileOnStart));
    assertTrue(fileFilter.accept(fileDuring));
    assertTrue(fileFilter.accept(fileOnEnd));
    assertFalse(fileFilter.accept(fileAfter));

    verify(fileAfter, times(1)).lastModified();
    verify(fileBefore, times(1)).lastModified();
    verify(fileDuring, times(1)).lastModified();
    verify(fileOnStart, times(1)).lastModified();
    verify(fileOnEnd, times(1)).lastModified();
  }

  @Test
  public void testAcceptOn() {
    FileLastModifiedFilter fileFilter = FileLastModifiedFilter.on(TestUtils.createCalendar(
      2014, Calendar.DECEMBER, 6));

    File fileAfter = mock(File.class, "fileAfter");
    File fileBefore = mock(File.class, "fileBefore");
    File fileOn = mock(File.class, "fileOn");

    when(fileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(fileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(fileOn.lastModified()).thenReturn(toMilliseconds(2014, Calendar.DECEMBER, 6));

    assertFalse(fileFilter.accept(fileBefore));
    assertTrue(fileFilter.accept(fileOn));
    assertFalse(fileFilter.accept(fileAfter));

    verify(fileAfter, times(1)).lastModified();
    verify(fileBefore, times(1)).lastModified();
    verify(fileOn, times(1)).lastModified();
  }

  @Test
  public void testAcceptOnOrAfter() {
    long lastModified  = toMilliseconds(2014, Calendar.DECEMBER, 6);

    File fileAfter = mock(File.class, "fileAfter");
    File fileBefore = mock(File.class, "fileBefore");
    File fileOn = mock(File.class, "fileOn");

    FileFilter fileFilter = ComposableFileFilter.or(FileLastModifiedFilter.on(lastModified),
      FileLastModifiedFilter.after(lastModified));

    when(fileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(fileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(fileOn.lastModified()).thenReturn(toMilliseconds(2014, Calendar.DECEMBER, 6));

    assertFalse(fileFilter.accept(fileBefore));
    assertTrue(fileFilter.accept(fileOn));
    assertTrue(fileFilter.accept(fileAfter));

    verify(fileAfter, times(2)).lastModified();
    verify(fileBefore, times(2)).lastModified();
    verify(fileOn, times(2)).lastModified();
  }

  @Test
  public void testAcceptOnOrBefore() {
    long lastModified  = toMilliseconds(2014, Calendar.DECEMBER, 6);

    File fileAfter = mock(File.class, "fileAfter");
    File fileBefore = mock(File.class, "fileBefore");
    File fileOn = mock(File.class, "fileOn");

    FileFilter fileFilter = ComposableFileFilter.or(FileLastModifiedFilter.on(lastModified),
      FileLastModifiedFilter.before(lastModified));

    when(fileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(fileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(fileOn.lastModified()).thenReturn(toMilliseconds(2014, Calendar.DECEMBER, 6));

    assertTrue(fileFilter.accept(fileBefore));
    assertTrue(fileFilter.accept(fileOn));
    assertFalse(fileFilter.accept(fileAfter));

    verify(fileAfter, times(2)).lastModified();
    verify(fileBefore, times(2)).lastModified();
    verify(fileOn, times(2)).lastModified();
  }

  @Test
  public void testAcceptAfterAndBefore() {
    long afterLastModified  = toMilliseconds(2014, Calendar.DECEMBER, 6);
    long beforeLastModified  = toMilliseconds(2015, Calendar.DECEMBER, 5);

    File fileAfter = mock(File.class, "fileAfter");
    File fileBefore = mock(File.class, "fileBefore");
    File fileDuring = mock(File.class, "fileDuring");
    File fileOnAfter = mock(File.class, "fileOnAfter");
    File fileOnBefore = mock(File.class, "fileOnEnd");

    FileFilter fileFilter = ComposableFileFilter.and(FileLastModifiedFilter.after(afterLastModified),
      FileLastModifiedFilter.before(beforeLastModified));

    when(fileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(fileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(fileDuring.lastModified()).thenReturn(toMilliseconds(2015, Calendar.APRIL, 1));
    when(fileOnAfter.lastModified()).thenReturn(afterLastModified);
    when(fileOnBefore.lastModified()).thenReturn(beforeLastModified);

    assertFalse(fileFilter.accept(fileBefore));
    assertFalse(fileFilter.accept(fileOnBefore));
    assertTrue(fileFilter.accept(fileDuring));
    assertFalse(fileFilter.accept(fileOnAfter));
    assertFalse(fileFilter.accept(fileAfter));

    verify(fileAfter, times(2)).lastModified();
    verify(fileBefore, times(2)).lastModified();
    verify(fileDuring, times(2)).lastModified();
    verify(fileOnAfter, times(2)).lastModified();
    verify(fileOnBefore, times(2)).lastModified();
  }

  @Test
  public void testAcceptBeforeOrAfter() {
    long beforeLastModified  = toMilliseconds(2014, Calendar.DECEMBER, 6);
    long afterLastModified  = toMilliseconds(2015, Calendar.DECEMBER, 5);

    File fileAfter = mock(File.class, "fileAfter");
    File fileBefore = mock(File.class, "fileBefore");
    File fileDuring = mock(File.class, "fileDuring");
    File fileOnAfter = mock(File.class, "fileOnAfter");
    File fileOnBefore = mock(File.class, "fileOnEnd");

    FileFilter fileFilter = ComposableFileFilter.or(FileLastModifiedFilter.before(beforeLastModified),
      FileLastModifiedFilter.after(afterLastModified));

    when(fileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(fileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(fileDuring.lastModified()).thenReturn(toMilliseconds(2015, Calendar.APRIL, 1));
    when(fileOnAfter.lastModified()).thenReturn(afterLastModified);
    when(fileOnBefore.lastModified()).thenReturn(beforeLastModified);

    assertTrue(fileFilter.accept(fileBefore));
    assertFalse(fileFilter.accept(fileOnBefore));
    assertFalse(fileFilter.accept(fileDuring));
    assertFalse(fileFilter.accept(fileOnAfter));
    assertTrue(fileFilter.accept(fileAfter));

    verify(fileAfter, times(2)).lastModified();
    verify(fileBefore, times(2)).lastModified();
    verify(fileDuring, times(2)).lastModified();
    verify(fileOnAfter, times(2)).lastModified();
    verify(fileOnBefore, times(2)).lastModified();
  }

  @Test
  public void testCreate() {
    long beforeOrOnLastModified  = toMilliseconds(2014, Calendar.DECEMBER, 6);
    long afterOrOnLastModified  = toMilliseconds(2015, Calendar.DECEMBER, 5);

    File fileAfter = mock(File.class, "fileAfter");
    File fileBefore = mock(File.class, "fileBefore");
    File fileDuring = mock(File.class, "fileDuring");
    File fileOnAfter = mock(File.class, "fileOnAfter");
    File fileOnBefore = mock(File.class, "fileOnEnd");

    FileLastModifiedFilter fileFilter = FileLastModifiedFilter.create(
      RelationalOperator.lessThanEqualToOrGreaterThanEqualTo(beforeOrOnLastModified, afterOrOnLastModified));

    when(fileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(fileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(fileDuring.lastModified()).thenReturn(toMilliseconds(2015, Calendar.APRIL, 1));
    when(fileOnAfter.lastModified()).thenReturn(afterOrOnLastModified);
    when(fileOnBefore.lastModified()).thenReturn(beforeOrOnLastModified);

    assertTrue(fileFilter.accept(fileBefore));
    assertTrue(fileFilter.accept(fileOnBefore));
    assertFalse(fileFilter.accept(fileDuring));
    assertTrue(fileFilter.accept(fileOnAfter));
    assertTrue(fileFilter.accept(fileAfter));

    verify(fileAfter, times(1)).lastModified();
    verify(fileBefore, times(1)).lastModified();
    verify(fileDuring, times(1)).lastModified();
    verify(fileOnAfter, times(1)).lastModified();
    verify(fileOnBefore, times(1)).lastModified();
  }

}
