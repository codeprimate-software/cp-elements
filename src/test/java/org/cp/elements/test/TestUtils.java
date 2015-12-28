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

package org.cp.elements.test;

import java.util.Calendar;
import java.util.List;

import org.junit.Assert;

@SuppressWarnings("unused")
public abstract class TestUtils {

  public static <T> void assertEquals(final T[] expected, final T[] actual) {
    Assert.assertEquals(expected.getClass(), actual.getClass());
    Assert.assertEquals(expected.getClass().getComponentType(), actual.getClass().getComponentType());
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

  public static void assertNullEquals(final Object expectedValue, final Object actualValue) {
    if (expectedValue == null) {
      Assert.assertNull("expected 'actual' value to be null", actualValue);
    }
    else {
      Assert.assertEquals(expectedValue, actualValue);
    }
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
    Calendar dateTime = Calendar.getInstance();
    dateTime.clear();
    dateTime.set(year, month, day);
    return dateTime;
  }

  public static Calendar createCalendar(final int year,
                                        final int month,
                                        final int day,
                                        final int hour,
                                        final int minute,
                                        final int second)
  {
    Calendar dateTime = createCalendar(year, month, day);
    dateTime.set(Calendar.HOUR_OF_DAY, hour);
    dateTime.set(Calendar.MINUTE, minute);
    dateTime.set(Calendar.SECOND, second);
    return dateTime;
  }

  public static String toString(final Object... array) {
    StringBuilder buffer = new StringBuilder("[");

    if (array != null) {
      for (Object element : array) {
        buffer.append(buffer.length() > 1 ? ", " : "");
        buffer.append(element);
      }
    }

    return buffer.append("]").toString();
  }

}
