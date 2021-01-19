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

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Calendar;
import java.util.function.Supplier;

import org.cp.elements.lang.annotation.NotNull;
import org.junit.Assert;

@SuppressWarnings("unused")
public abstract class TestUtils {

  public static <T> void assertEquals(@NotNull T[] expected, @NotNull T[] actual) {

    Assert.assertEquals(expected.getClass(), actual.getClass());
    Assert.assertEquals(expected.getClass().getComponentType(), actual.getClass().getComponentType());
    Assert.assertEquals(expected.length, actual.length);

    for (int index = 0; index < expected.length; index++) {
      Assert.assertEquals(expected[index], actual[index]);
    }
  }

  public static @NotNull Calendar createCalendar(int year, int month, int day) {

    Calendar dateTime = Calendar.getInstance();

    dateTime.clear();
    dateTime.set(year, month, day);

    return dateTime;
  }

  public static @NotNull Calendar createCalendar(int year, int month, int day, int hour, int minute, int second) {

    Calendar dateTime = createCalendar(year, month, day);

    dateTime.set(Calendar.HOUR_OF_DAY, hour);
    dateTime.set(Calendar.MINUTE, minute);
    dateTime.set(Calendar.SECOND, second);
    dateTime.set(Calendar.MILLISECOND, 0);

    return dateTime;
  }

  public static void doIllegalArgumentExceptionThrowingOperation(@NotNull Runnable operation,
      @NotNull Supplier<String> message) {

    doIllegalArgumentExceptionThrowingOperation(operation, message, () -> { });
  }

  public static void doIllegalArgumentExceptionThrowingOperation(@NotNull Runnable operation,
      @NotNull Supplier<String> message, @NotNull Runnable verifications) {

    try {
      operation.run();
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage(message.get());
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifications.run();
    }
  }

  public static void doIllegalStateExceptionThrowingOperation(@NotNull Runnable operation,
      @NotNull Supplier<String> message) {

    doIllegalStateExceptionThrowingOperation(operation, message, () -> { });
  }

  public static void doIllegalStateExceptionThrowingOperation(@NotNull Runnable operation,
      @NotNull Supplier<String> message, @NotNull Runnable verifications) {

    try {
      operation.run();
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage(message.get());
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifications.run();
    }
  }

  public static long timeIt(@NotNull Runnable runnable) {

    long t0 = System.currentTimeMillis();

    runnable.run();

    return (System.currentTimeMillis() - t0);
  }
}
