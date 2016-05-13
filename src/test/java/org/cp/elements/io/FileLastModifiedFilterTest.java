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

  protected File mockFile(String name) {
    return mock(File.class, name);
  }

  protected long toMilliseconds(int year, int month, int day) {
    return TestUtils.createCalendar(year, month, day).getTimeInMillis();
  }

  @Test
  public void testAcceptAfter() {
    FileLastModifiedFilter fileFilter = FileLastModifiedFilter.after(TestUtils.createCalendar(
      2014, Calendar.DECEMBER, 6));

    File mockFileAfter = mockFile("fileAfter");
    File mockFileBefore = mockFile("fileBefore");
    File mockFileOn = mockFile("fileOn");

    when(mockFileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(mockFileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(mockFileOn.lastModified()).thenReturn(toMilliseconds(2014, Calendar.DECEMBER, 6));

    assertFalse(fileFilter.accept(mockFileBefore));
    assertFalse(fileFilter.accept(mockFileOn));
    assertTrue(fileFilter.accept(mockFileAfter));

    verify(mockFileAfter, times(1)).lastModified();
    verify(mockFileBefore, times(1)).lastModified();
    verify(mockFileOn, times(1)).lastModified();
  }

  @Test
  public void testAcceptBefore() {
    FileLastModifiedFilter fileFilter = FileLastModifiedFilter.before(TestUtils.createCalendar(
      2014, Calendar.DECEMBER, 6));

    File fileAfter = mockFile("fileAfter");
    File fileBefore = mockFile("fileBefore");
    File fileOn = mockFile("fileOn");

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

    File fileAfter = mockFile("fileAfter");
    File fileBefore = mockFile("fileBefore");
    File fileDuring = mockFile("fileDuring");
    File fileOnStart = mockFile("fileOnStart");
    File fileOnEnd = mockFile("fileOnEnd");

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

    File fileAfter = mockFile("fileAfter");
    File fileBefore = mockFile("fileBefore");
    File fileOn = mockFile("fileOn");

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

    File mockFileAfter = mockFile("MockFileAfter");
    File mockFileBefore =  mockFile("MockFileBefore");
    File mockFileOn = mockFile("MockFileOn");

    FileFilter fileFilter = ComposableFileFilter.or(FileLastModifiedFilter.on(lastModified),
      FileLastModifiedFilter.after(lastModified));

    when(mockFileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(mockFileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(mockFileOn.lastModified()).thenReturn(toMilliseconds(2014, Calendar.DECEMBER, 6));

    assertFalse(fileFilter.accept(mockFileBefore));
    assertTrue(fileFilter.accept(mockFileOn));
    assertTrue(fileFilter.accept(mockFileAfter));

    verify(mockFileAfter, times(2)).lastModified();
    verify(mockFileBefore, times(2)).lastModified();
    verify(mockFileOn, times(1)).lastModified();
  }

  @Test
  public void testAcceptOnOrBefore() {
    long lastModified  = toMilliseconds(2014, Calendar.DECEMBER, 6);

    File mockFileAfter = mockFile("MockFileAfter");
    File mockFileBefore = mockFile("MockFileBefore");
    File mockFileOn = mockFile("MockFileOn");

    FileFilter fileFilter = ComposableFileFilter.or(FileLastModifiedFilter.on(lastModified),
      FileLastModifiedFilter.before(lastModified));

    when(mockFileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(mockFileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(mockFileOn.lastModified()).thenReturn(toMilliseconds(2014, Calendar.DECEMBER, 6));

    assertFalse(fileFilter.accept(mockFileAfter));
    assertTrue(fileFilter.accept(mockFileBefore));
    assertTrue(fileFilter.accept(mockFileOn));

    verify(mockFileAfter, times(2)).lastModified();
    verify(mockFileBefore, times(2)).lastModified();
    verify(mockFileOn, times(1)).lastModified();
  }

  @Test
  public void testAcceptAfterAndBefore() {
    long afterLastModified  = toMilliseconds(2014, Calendar.DECEMBER, 6);
    long beforeLastModified  = toMilliseconds(2015, Calendar.DECEMBER, 5);

    File mockFileAfter = mockFile("MockFileAfter");
    File mockFileBefore = mockFile("MockFileBefore");
    File mockFileDuring = mockFile("MockFileDuring");
    File mockFileOnAfter = mockFile("MockFileOnAfter");
    File mockFileOnBefore = mockFile("MockFileOnBefore");

    FileFilter fileFilter = ComposableFileFilter.and(FileLastModifiedFilter.after(afterLastModified),
      FileLastModifiedFilter.before(beforeLastModified));

    when(mockFileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(mockFileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(mockFileDuring.lastModified()).thenReturn(toMilliseconds(2015, Calendar.APRIL, 1));
    when(mockFileOnAfter.lastModified()).thenReturn(afterLastModified);
    when(mockFileOnBefore.lastModified()).thenReturn(beforeLastModified);

    assertFalse(fileFilter.accept(mockFileBefore));
    assertFalse(fileFilter.accept(mockFileOnBefore));
    assertTrue(fileFilter.accept(mockFileDuring));
    assertFalse(fileFilter.accept(mockFileOnAfter));
    assertFalse(fileFilter.accept(mockFileAfter));

    verify(mockFileAfter, times(2)).lastModified();
    verify(mockFileBefore, times(1)).lastModified();
    verify(mockFileDuring, times(2)).lastModified();
    verify(mockFileOnAfter, times(1)).lastModified();
    verify(mockFileOnBefore, times(2)).lastModified();
  }

  @Test
  public void testAcceptBeforeOrAfter() {
    long beforeLastModified  = toMilliseconds(2014, Calendar.DECEMBER, 6);
    long afterLastModified  = toMilliseconds(2015, Calendar.DECEMBER, 5);

    File mockFileAfter = mockFile("MockFileAfter");
    File mockFileBefore = mockFile("MockFileBefore");
    File mockFileDuring = mockFile("MockFileDuring");
    File mockFileOnAfter = mockFile("MockFileOnAfter");
    File mockFileOnBefore = mockFile("MockFileOnBefore");

    FileFilter fileFilter = ComposableFileFilter.or(FileLastModifiedFilter.before(beforeLastModified),
      FileLastModifiedFilter.after(afterLastModified));

    when(mockFileAfter.lastModified()).thenReturn(toMilliseconds(2024, Calendar.APRIL, 1));
    when(mockFileBefore.lastModified()).thenReturn(toMilliseconds(2004, Calendar.DECEMBER, 31));
    when(mockFileDuring.lastModified()).thenReturn(toMilliseconds(2015, Calendar.APRIL, 1));
    when(mockFileOnAfter.lastModified()).thenReturn(afterLastModified);
    when(mockFileOnBefore.lastModified()).thenReturn(beforeLastModified);

    assertTrue(fileFilter.accept(mockFileBefore));
    assertFalse(fileFilter.accept(mockFileOnBefore));
    assertFalse(fileFilter.accept(mockFileDuring));
    assertFalse(fileFilter.accept(mockFileOnAfter));
    assertTrue(fileFilter.accept(mockFileAfter));

    verify(mockFileAfter, times(2)).lastModified();
    verify(mockFileBefore, times(1)).lastModified();
    verify(mockFileDuring, times(2)).lastModified();
    verify(mockFileOnAfter, times(2)).lastModified();
    verify(mockFileOnBefore, times(2)).lastModified();
  }

  @Test
  public void testCreate() {
    long beforeOrOnLastModified  = toMilliseconds(2014, Calendar.DECEMBER, 6);
    long afterOrOnLastModified  = toMilliseconds(2015, Calendar.DECEMBER, 5);

    File fileAfter = mockFile("fileAfter");
    File fileBefore = mockFile("fileBefore");
    File fileDuring = mockFile("fileDuring");
    File fileOnAfter = mockFile("fileOnAfter");
    File fileOnBefore = mockFile("fileOnEnd");

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
