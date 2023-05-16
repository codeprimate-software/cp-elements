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
package org.cp.elements.time;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.DayOfWeek;
import java.util.Arrays;
import java.util.Calendar;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.StringUtils;
import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Weekday}.
 *
 * @author John J. Blum
 * @see java.time.DayOfWeek
 * @see java.util.Calendar
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.time.Weekday
 * @since 1.0.0
 */
public class WeekdayUnitTests {

  private static final int[] CALENDAR_DAYS = {
    Calendar.SUNDAY,
    Calendar.MONDAY,
    Calendar.TUESDAY,
    Calendar.WEDNESDAY,
    Calendar.THURSDAY,
    Calendar.FRIDAY,
    Calendar.SATURDAY
  };

  private static final DayOfWeek[] DAYS_OF_WEEK = {
    DayOfWeek.SUNDAY,
    DayOfWeek.MONDAY,
    DayOfWeek.TUESDAY,
    DayOfWeek.WEDNESDAY,
    DayOfWeek.THURSDAY,
    DayOfWeek.FRIDAY,
    DayOfWeek.SATURDAY,
  };

  @Test
  public void valueOfReturnsWeekday() {

    AtomicInteger position = new AtomicInteger(0);

    Arrays.stream(Weekday.values()).forEach(weekday -> {

      String name = StringUtils.capitalize(weekday.name().toLowerCase());

      assertThat(Weekday.valueOf(weekday.name())).isEqualTo(weekday);
      assertThat(weekday.getAbbreviation()).isEqualTo(name.substring(0, 3));
      assertThat(weekday.getName()).isEqualTo(name);
      assertThat(weekday.getCalendarDay()).isEqualTo(CALENDAR_DAYS[position.get()]);
      assertThat(weekday.getDayOfWeek()).isEqualTo(DAYS_OF_WEEK[position.get()]);
      assertThat(weekday.getPosition()).isEqualTo(position.incrementAndGet());
    });
  }

  @Test
  public void valueOfAbbreviationIsCorrect() {

    Arrays.stream(Weekday.values()).forEach(weekday ->
      assertThat(Weekday.valueOfAbbreviation(weekday.getAbbreviation())).isEqualTo(weekday));
  }

  @Test
  public void valueOfAbbreviationIsLenient() {

    assertThat(Weekday.valueOfAbbreviation("fri")).isEqualTo(Weekday.FRIDAY);
    assertThat(Weekday.valueOfAbbreviation("sAT")).isEqualTo(Weekday.SATURDAY);
    assertThat(Weekday.valueOfAbbreviation("SUN")).isEqualTo(Weekday.SUNDAY);
  }

  @Test
  public void valueOfAbbreviationUsingNameReturnsNull() {

    assertThat(Weekday.valueOfAbbreviation("Sunday")).isNull();
    assertThat(Weekday.valueOfAbbreviation("Moonday")).isNull();
  }

  @Test
  public void valueOfInvalidAbbreviationReturnsNull() {

    assertThat(Weekday.valueOfAbbreviation("")).isNull();
    assertThat(Weekday.valueOfAbbreviation("  ")).isNull();
    assertThat(Weekday.valueOfAbbreviation("May")).isNull();
    assertThat(Weekday.valueOfAbbreviation("2014")).isNull();
  }

  @Test
  public void valueOfNullAbbreviationIsNullSafeReturnsNull() {
    assertThat(Weekday.valueOfAbbreviation(null)).isNull();
  }

  @Test
  public void valueOfCalendarDayIsCorrect() {

    int index = 0;

    for (Weekday weekday : Weekday.values()) {
      assertThat(Weekday.valueOfCalendarDay(CALENDAR_DAYS[index++])).isEqualTo(weekday);
    }

    assertThat(index).isEqualTo(7);
  }

  @Test
  public void valueOfInvalidCalendarDayReturnsNull() {

    assertThat(Weekday.valueOfCalendarDay(-1)).isNull();
    assertThat(Weekday.valueOfCalendarDay(0)).isNull();
    assertThat(Weekday.valueOfCalendarDay(8)).isNull();
  }

  @Test
  public void valueOfDayOfWeekIsCorrect() {

    int index = 0;

    for (Weekday weekday : Weekday.values()) {
      assertThat(Weekday.valueOfDayOfWeek(DAYS_OF_WEEK[index++])).isEqualTo(weekday);
    }

    assertThat(index).isEqualTo(7);
  }

  @Test
  public void valueOfNullDayOfWeekReturnsNull() {
    assertThat(Weekday.valueOfDayOfWeek(null)).isNull();
  }

  @Test
  public void valueOfNameIsCorrect() {

    Arrays.stream(Weekday.values()).forEach(weekday -> {
      assertThat(Weekday.valueOfName(weekday.getName())).isEqualTo(weekday);
      assertThat(Weekday.valueOfName(weekday.name())).isEqualTo(weekday);
    });
  }

  @Test
  public void valueOfNameIsLenient() {

    assertThat(Weekday.valueOfName("friday")).isEqualTo(Weekday.FRIDAY);
    assertThat(Weekday.valueOfName("sATurDaY")).isEqualTo(Weekday.SATURDAY);
    assertThat(Weekday.valueOfName("SUNDAY")).isEqualTo(Weekday.SUNDAY);
  }

  @Test
  public void valueOfNameUsingAbbreviationReturnsNull() {

    assertThat(Weekday.valueOfName("Mon")).isNull();
    assertThat(Weekday.valueOfName("Tues")).isNull();
    assertThat(Weekday.valueOfName("Thurs")).isNull();
  }

  @Test
  public void valueOfInvalidNamesReturnsNull() {

    assertThat(Weekday.valueOfName("")).isNull();
    assertThat(Weekday.valueOfName("  ")).isNull();
    assertThat(Weekday.valueOfName("nil")).isNull();
    assertThat(Weekday.valueOfName("null")).isNull();
    assertThat(Weekday.valueOfName("October")).isNull();
    assertThat(Weekday.valueOfName("2014")).isNull();
  }

  @Test
  public void valueOfNullNameIsNullSafeReturnsNull() {
    assertThat(Weekday.valueOfName(null)).isNull();
  }

  @Test
  public void valueOfPositionIsCorrect() {

    AtomicInteger position = new AtomicInteger(0);

    Arrays.stream(Weekday.values()).forEach(weekday ->  {
      assertThat(weekday.getPosition()).isEqualTo(position.incrementAndGet());
    });

    assertThat(position.get()).isEqualTo(7);
  }

  @Test
  public void valueOfInvalidPositionReturnsNull() {

    assertThat(Weekday.valueOfPosition(-1)).isNull();
    assertThat(Weekday.valueOfPosition(0)).isNull();
    assertThat(Weekday.valueOfPosition(8)).isNull();
  }

  @Test
  public void getTomorrowIsCorrect() {

    assertThat(Weekday.SUNDAY.getTomorrow()).isEqualTo(Weekday.MONDAY);
    assertThat(Weekday.MONDAY.getTomorrow()).isEqualTo(Weekday.TUESDAY);
    assertThat(Weekday.TUESDAY.getTomorrow()).isEqualTo(Weekday.WEDNESDAY);
    assertThat(Weekday.WEDNESDAY.getTomorrow()).isEqualTo(Weekday.THURSDAY);
    assertThat(Weekday.THURSDAY.getTomorrow()).isEqualTo(Weekday.FRIDAY);
    assertThat(Weekday.FRIDAY.getTomorrow()).isEqualTo(Weekday.SATURDAY);
    assertThat(Weekday.SATURDAY.getTomorrow()).isEqualTo(Weekday.SUNDAY);
  }

  @Test
  public void getYesterdayIsCorrect() {

    assertThat(Weekday.SUNDAY.getYesterday()).isEqualTo(Weekday.SATURDAY);
    assertThat(Weekday.MONDAY.getYesterday()).isEqualTo(Weekday.SUNDAY);
    assertThat(Weekday.TUESDAY.getYesterday()).isEqualTo(Weekday.MONDAY);
    assertThat(Weekday.WEDNESDAY.getYesterday()).isEqualTo(Weekday.TUESDAY);
    assertThat(Weekday.THURSDAY.getYesterday()).isEqualTo(Weekday.WEDNESDAY);
    assertThat(Weekday.FRIDAY.getYesterday()).isEqualTo(Weekday.THURSDAY);
    assertThat(Weekday.SATURDAY.getYesterday()).isEqualTo(Weekday.FRIDAY);
  }

  private void isDayReturnsFalse(Function<Weekday, Boolean> weekdayFunction) {

    for (Weekday weekday : Weekday.values()) {
      if (FunctionUtils.toPredicate(weekdayFunction).negate().test(weekday)) {
        assertThat(weekdayFunction.apply(weekday)).isFalse();
      }
    }
  }

  @Test
  public void isSundayReturnsTrue() {
    assertThat(Weekday.SUNDAY.isSunday()).isTrue();
  }

  @Test
  public void isSundayReturnsFalse() {
    isDayReturnsFalse(Weekday::isSunday);
  }

  @Test
  public void isMondayReturnsTrue() {
    assertThat(Weekday.MONDAY.isMonday()).isTrue();
  }

  @Test
  public void isMondayReturnsFalse() {
    isDayReturnsFalse(Weekday::isMonday);
  }

  @Test
  public void isTuesdayReturnsTrue() {
    assertThat(Weekday.TUESDAY.isTuesday()).isTrue();
  }

  @Test
  public void isTuesdayReturnsFalse() {
    isDayReturnsFalse(Weekday::isTuesday);  }

  @Test
  public void isWednesdayReturnsTrue() {
    assertThat(Weekday.WEDNESDAY.isWednesday()).isTrue();
  }

  @Test
  public void isWednesdayReturnsFalse() {
    isDayReturnsFalse(Weekday::isWednesday);
  }

  @Test
  public void isThursdayReturnsTrue() {
    assertThat(Weekday.THURSDAY.isThursday()).isTrue();
  }

  @Test
  public void isThursdayReturnsFalse() {
    isDayReturnsFalse(Weekday::isThursday);
  }

  @Test
  public void isFridayReturnsTrue() {
    assertThat(Weekday.FRIDAY.isFriday()).isTrue();
  }

  @Test
  public void isFridayReturnsFalse() {
    isDayReturnsFalse(Weekday::isFriday);
  }

  @Test
  public void isSaturdayReturnsTrue() {
    assertThat(Weekday.SATURDAY.isSaturday()).isTrue();
  }

  @Test
  public void isSaturdayReturnsFalse() {
    isDayReturnsFalse(Weekday::isSaturday);
  }

  @Test
  public void isWeekdayReturnsTrue() {

    assertThat(Weekday.MONDAY.isWeekday()).isTrue();
    assertThat(Weekday.TUESDAY.isWeekday()).isTrue();
    assertThat(Weekday.WEDNESDAY.isWeekday()).isTrue();
    assertThat(Weekday.THURSDAY.isWeekday()).isTrue();
    assertThat(Weekday.FRIDAY.isWeekday()).isTrue();
  }

  @Test
  public void isWeekdayReturnsFalse() {

    assertThat(Weekday.SUNDAY.isWeekday()).isFalse();
    assertThat(Weekday.SATURDAY.isWeekday()).isFalse();
  }

  @Test
  public void isWeekendReturnsTrue() {

    assertThat(Weekday.SUNDAY.isWeekend()).isTrue();
    assertThat(Weekday.SATURDAY.isWeekend()).isTrue();
  }

  @Test
  public void isWeekendReturnsFalse() {

    assertThat(Weekday.MONDAY.isWeekend()).isFalse();
    assertThat(Weekday.TUESDAY.isWeekend()).isFalse();
    assertThat(Weekday.WEDNESDAY.isWeekend()).isFalse();
    assertThat(Weekday.THURSDAY.isWeekend()).isFalse();
    assertThat(Weekday.FRIDAY.isWeekend()).isFalse();
  }
}
