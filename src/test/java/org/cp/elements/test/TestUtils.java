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

package org.cp.elements.test;

import java.util.Calendar;
import java.util.List;

import org.junit.Assert;

@SuppressWarnings("unused")
public class TestUtils {

  public static <T> void assertEquals(final T[] expected, final T[] actual) {
    Assert.assertEquals(expected.length, actual.length);

    for (int index = 0; index < expected.length; index++) {
      Assert.assertEquals(expected[index], actual[index]);
    }
  }

  public static void assertEquals(final List<?> expected, final List<?> actual) {
    Assert.assertEquals(expected.size(), actual.size());

    for (int index = 0, size = expected.size(); index < size; index++) {
      Assert.assertEquals(expected.get(index), actual.get(index));
    }
  }

  public static void assertEqualDate(final Calendar expectedDate, final Calendar actualDate) {
    Assert.assertEquals(expectedDate.get(Calendar.YEAR), actualDate.get(Calendar.YEAR));
    Assert.assertEquals(expectedDate.get(Calendar.MONTH), actualDate.get(Calendar.MONTH));
    Assert.assertEquals(expectedDate.get(Calendar.DAY_OF_MONTH), actualDate.get(Calendar.DAY_OF_MONTH));
  }

  public static void assertEqualDateTime(final Calendar expectedDateTime, final Calendar actualDateTime) {
    assertEqualDate(expectedDateTime, actualDateTime);
    Assert.assertEquals(expectedDateTime.get(Calendar.HOUR_OF_DAY), actualDateTime.get(Calendar.HOUR_OF_DAY));
    Assert.assertEquals(expectedDateTime.get(Calendar.MINUTE), actualDateTime.get(Calendar.MINUTE));
    Assert.assertEquals(expectedDateTime.get(Calendar.SECOND), actualDateTime.get(Calendar.SECOND));
    Assert.assertEquals(expectedDateTime.get(Calendar.MILLISECOND), actualDateTime.get(Calendar.MILLISECOND));
  }

  public static void assertNegative(final int value) {
    Assert.assertTrue(value < 0);
  }

  public static void assertPositive(final int value) {
    Assert.assertTrue(value > 0);
  }

  public static void assertZero(final int value) {
    Assert.assertTrue(value == 0);
  }

  public static Calendar createCalendar(final int year, final int month, final int day) {
    final Calendar dateTime = Calendar.getInstance();
    dateTime.clear();
    dateTime.set(year, month, day);
    return dateTime;
  }

  public static Calendar createCalendar(final int year, final int month, final int day, final int hour, final int minute, final int second) {
    final Calendar dateTime = createCalendar(year, month, day);
    dateTime.set(Calendar.HOUR_OF_DAY, hour);
    dateTime.set(Calendar.MINUTE, minute);
    dateTime.set(Calendar.SECOND, second);
    return dateTime;
  }

  public static String toString(final Object... array) {
    if (array != null) {
      final StringBuilder buffer = new StringBuilder("[");

      for (final Object element : array) {
        buffer.append(buffer.length() > 1 ? ", " : "");
        buffer.append(element);
      }

      return buffer.append("]").toString();
    }

    return null;
  }

}
