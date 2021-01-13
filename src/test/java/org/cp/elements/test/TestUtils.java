/*
 * Copyright 2011-Present Author or Authors.
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

import org.junit.Assert;

@SuppressWarnings("unused")
public abstract class TestUtils {

  public static <T> void assertEquals(T[] expected, T[] actual) {
    Assert.assertEquals(expected.getClass(), actual.getClass());
    Assert.assertEquals(expected.getClass().getComponentType(), actual.getClass().getComponentType());
    Assert.assertEquals(expected.length, actual.length);

    for (int index = 0; index < expected.length; index++) {
      Assert.assertEquals(expected[index], actual[index]);
    }
  }

  public static Calendar createCalendar(int year, int month, int day) {
    Calendar dateTime = Calendar.getInstance();
    dateTime.clear();
    dateTime.set(year, month, day);
    return dateTime;
  }

  public static Calendar createCalendar(int year, int month, int day, int hour, int minute, int second) {
    Calendar dateTime = createCalendar(year, month, day);
    dateTime.set(Calendar.HOUR_OF_DAY, hour);
    dateTime.set(Calendar.MINUTE, minute);
    dateTime.set(Calendar.SECOND, second);
    dateTime.set(Calendar.MILLISECOND, 0);
    return dateTime;
  }

  public static long timeIt(Runnable runnable) {
    long t0 = System.currentTimeMillis();
    runnable.run();
    return (System.currentTimeMillis() - t0);
  }
}
