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

import java.util.Arrays;
import java.util.Calendar;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.StringUtils;
import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Month}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see java.time.Month
 * @see java.util.Calendar
 * @see org.cp.elements.time.Month
 * @since 1.0.0
 */
public class MonthUnitTests {

  private static final int[] CALENDAR_MONTHS = {
    Calendar.JANUARY,
    Calendar.FEBRUARY,
    Calendar.MARCH,
    Calendar.APRIL,
    Calendar.MAY,
    Calendar.JUNE,
    Calendar.JULY,
    Calendar.AUGUST,
    Calendar.SEPTEMBER,
    Calendar.OCTOBER,
    Calendar.NOVEMBER,
    Calendar.DECEMBER
  };

  @Test
  public void valueOfReturnsMonthEnum() {

    AtomicInteger index = new AtomicInteger(0);

    Arrays.stream(Month.values()).forEach(month -> {
      assertThat(Month.valueOf(month.name())).isEqualTo(month);
      assertThat(month.ordinal()).isEqualTo(index.get());
      assertThat(month.getAbbreviation()).isEqualTo(StringUtils.capitalize(month.name().toLowerCase().substring(0, 3)));
      assertThat(month.getCalendarMonth()).isEqualTo(CALENDAR_MONTHS[index.get()]);
      assertThat(month.getJavaTimeMonth()).isEqualTo(java.time.Month.values()[index.get()]);
      assertThat(month.getName()).isEqualTo(StringUtils.capitalize(month.name().toLowerCase()));
      assertThat(month.getPosition()).isEqualTo(index.incrementAndGet());
    });
  }

  @Test
  public void valueOfAbbreviationReturnsMonthEnum() {

    Arrays.stream(Month.values()).forEach(month ->
      assertThat(Month.valueOfAbbreviation(month.getAbbreviation())).isEqualTo(month));
  }

  @Test
  public void valueOfAbbreviationIgnoresCaseAndReturnsMonthEnum() {

    assertThat(Month.valueOfAbbreviation("jan")).isEqualTo(Month.JANUARY);
    assertThat(Month.valueOfAbbreviation("May")).isEqualTo(Month.MAY);
    assertThat(Month.valueOfAbbreviation("AUG")).isEqualTo(Month.AUGUST);
  }

  @Test
  public void valueOfInvalidAbbreviationReturnsNull() {

    assertThat(Month.valueOfAbbreviation("")).isNull();
    assertThat(Month.valueOfAbbreviation("  ")).isNull();
    assertThat(Month.valueOfAbbreviation("nil")).isNull();
    assertThat(Month.valueOfAbbreviation("null")).isNull();
    assertThat(Month.valueOfAbbreviation("Maybe")).isNull();
    assertThat(Month.valueOfAbbreviation("June")).isNull();
    assertThat(Month.valueOfAbbreviation("Sept")).isNull();
  }

  @Test
  public void valueOfNullAbbreviationIsNullSafeReturnsNull() {
    assertThat(Month.valueOfAbbreviation(null)).isNull();
  }

  @Test
  public void valueOfCalendarMonthReturnsMonthEnum() {

    AtomicInteger index = new AtomicInteger(0);

    Arrays.stream(Month.values()).forEach(month -> {
      assertThat(Month.valueOfCalendarMonth(month.getCalendarMonth())).isEqualTo(month);
      assertThat(Month.valueOfCalendarMonth(CALENDAR_MONTHS[index.getAndIncrement()])).isEqualTo(month);
    });

    assertThat(index.get()).isEqualTo(12);
  }

  @Test
  public void valueOfInvalidCalendarMonthReturnsNull() {

    assertThat(Month.valueOfCalendarMonth(-11)).isNull();
    assertThat(Month.valueOfCalendarMonth(12)).isNull();
    assertThat(Month.valueOfCalendarMonth(13)).isNull();
  }

  @Test
  public void valueOfJavaTimeMonthReturnsMonthEnum() {

    AtomicInteger index = new AtomicInteger(0);

    Arrays.stream(Month.values()).forEach(month -> {
      assertThat(Month.valueOfJavaTimeMonth(month.getJavaTimeMonth())).isEqualTo(month);
      assertThat(Month.valueOfJavaTimeMonth(java.time.Month.values()[index.getAndIncrement()])).isEqualTo(month);
    });

    assertThat(index.get()).isEqualTo(12);
  }

  @Test
  public void valueOfNullJavaTimeMonthIsNullSafeAndReturnsNull() {
    assertThat(Month.valueOfJavaTimeMonth(null)).isNull();
  }

  @Test
  public void valueOfNameReturnsMonth() {

    Arrays.stream(Month.values()).forEach(month ->
      assertThat(Month.valueOfName(month.getName())).isEqualTo(month));
  }

  @Test
  public void valueOfNameIgnoresCaseAndReturnsMonthEnum() {

    assertThat(Month.valueOfName("january")).isEqualTo(Month.JANUARY);
    assertThat(Month.valueOfName("May")).isEqualTo(Month.MAY);
    assertThat(Month.valueOfName("AUGUST")).isEqualTo(Month.AUGUST);
  }

  @Test
  public void valueOfInvalidNamesReturnsNull() {

    assertThat(Month.valueOfName("")).isNull();
    assertThat(Month.valueOfName("  ")).isNull();
    assertThat(Month.valueOfName("nil")).isNull();
    assertThat(Month.valueOfName("null")).isNull();
    assertThat(Month.valueOfName("Jun")).isNull();
    assertThat(Month.valueOfName("Julie")).isNull();
    assertThat(Month.valueOfName("Octobre")).isNull();
    assertThat(Month.valueOfName("Daycember")).isNull();
  }

  @Test
  public void valueOfNullNameIsNullSafeReturnsNull() {
    assertThat(Month.valueOfName(null)).isNull();
  }

  @Test
  public void valueOfPositionReturnsMonthEnum() {

    AtomicInteger position = new AtomicInteger(0);

    Arrays.stream(Month.values()).forEach(month ->
      assertThat(Month.valueOfPosition(position.incrementAndGet())).isEqualTo(month));

    assertThat(position.get()).isEqualTo(12);
  }

  @Test
  public void valueOfInvalidPositionReturnsNull() {

    assertThat(Month.valueOfPosition(0)).isNull();
    assertThat(Month.valueOfPosition(-7)).isNull();
    assertThat(Month.valueOfPosition(13)).isNull();
  }

  @Test
  public void getNextMonthIsCorrect() {

    assertThat(Month.JANUARY.getNextMonth()).isEqualTo(Month.FEBRUARY);
    assertThat(Month.FEBRUARY.getNextMonth()).isEqualTo(Month.MARCH);
    assertThat(Month.MARCH.getNextMonth()).isEqualTo(Month.APRIL);
    assertThat(Month.APRIL.getNextMonth()).isEqualTo(Month.MAY);
    assertThat(Month.MAY.getNextMonth()).isEqualTo(Month.JUNE);
    assertThat(Month.JUNE.getNextMonth()).isEqualTo(Month.JULY);
    assertThat(Month.JULY.getNextMonth()).isEqualTo(Month.AUGUST);
    assertThat(Month.AUGUST.getNextMonth()).isEqualTo(Month.SEPTEMBER);
    assertThat(Month.SEPTEMBER.getNextMonth()).isEqualTo(Month.OCTOBER);
    assertThat(Month.OCTOBER.getNextMonth()).isEqualTo(Month.NOVEMBER);
    assertThat(Month.NOVEMBER.getNextMonth()).isEqualTo(Month.DECEMBER);
    assertThat(Month.DECEMBER.getNextMonth()).isEqualTo(Month.JANUARY);
  }

  @Test
  public void getPreviousMonthIsCorrect() {

    assertThat(Month.JANUARY.getPreviousMonth()).isEqualTo(Month.DECEMBER);
    assertThat(Month.FEBRUARY.getPreviousMonth()).isEqualTo(Month.JANUARY);
    assertThat(Month.MARCH.getPreviousMonth()).isEqualTo(Month.FEBRUARY);
    assertThat(Month.APRIL.getPreviousMonth()).isEqualTo(Month.MARCH);
    assertThat(Month.MAY.getPreviousMonth()).isEqualTo(Month.APRIL);
    assertThat(Month.JUNE.getPreviousMonth()).isEqualTo(Month.MAY);
    assertThat(Month.JULY.getPreviousMonth()).isEqualTo(Month.JUNE);
    assertThat(Month.AUGUST.getPreviousMonth()).isEqualTo(Month.JULY);
    assertThat(Month.SEPTEMBER.getPreviousMonth()).isEqualTo(Month.AUGUST);
    assertThat(Month.OCTOBER.getPreviousMonth()).isEqualTo(Month.SEPTEMBER);
    assertThat(Month.NOVEMBER.getPreviousMonth()).isEqualTo(Month.OCTOBER);
    assertThat(Month.DECEMBER.getPreviousMonth()).isEqualTo(Month.NOVEMBER);
  }

  private void testIsNotMonth(Function<Month, Boolean> monthFunction) {

    for (Month month : Month.values()) {
      if (FunctionUtils.toPredicate(monthFunction).negate().test(month)) {
        assertThat(monthFunction.apply(month)).isFalse();
      }
      else {
        assertThat(monthFunction.apply(month)).isTrue();
      }
    }
  }
  @Test
  public void isJanuaryReturnsTrue() {
    assertThat(Month.JANUARY.isJanuary()).isTrue();
  }

  @Test
  public void isJanuaryReturnsFalse() {
    testIsNotMonth(Month::isJanuary);
  }

  @Test
  public void isFebruaryReturnsTrue() {
    assertThat(Month.FEBRUARY.isFebruary()).isTrue();
  }

  @Test
  public void isFebruaryReturnFalse() {
    testIsNotMonth(Month::isFebruary);
  }

  @Test
  public void isMarchReturnsTrue() {
    assertThat(Month.MARCH.isMarch()).isTrue();
  }

  @Test
  public void isMarchReturnsFalse() {
    testIsNotMonth(Month::isMarch);
  }

  @Test
  public void isAprilReturnsTrue() {
    assertThat(Month.APRIL.isApril()).isTrue();
  }

  @Test
  public void isAprilReturnsFalse() {
    testIsNotMonth(Month::isApril);
  }

  @Test
  public void isMayReturnsTrue() {
    assertThat(Month.MAY.isMay()).isTrue();
  }

  @Test
  public void isMayReturnFalse() {
    testIsNotMonth(Month::isMay);
  }

  @Test
  public void isJuneReturnsTrue() {
    assertThat(Month.JUNE.isJune()).isTrue();
  }

  @Test
  public void isJuneReturnsFalse() {
    testIsNotMonth(Month::isJune);
  }

  @Test
  public void isJulyReturnsTrue() {
    assertThat(Month.JULY.isJuly()).isTrue();
  }

  @Test
  public void isJulyReturnsFalse() {
    testIsNotMonth(Month::isJuly);
  }

  @Test
  public void isAugustReturnsTrue() {
    assertThat(Month.AUGUST.isAugust()).isTrue();
  }

  @Test
  public void isAugustReturnsFalse() {
    testIsNotMonth(Month::isAugust);
  }

  @Test
  public void isSeptemberReturnsTrue() {
    assertThat(Month.SEPTEMBER.isSeptember()).isTrue();
  }

  @Test
  public void isSeptemberReturnsFalse() {
    testIsNotMonth(Month::isSeptember);
  }

  @Test
  public void isOctoberReturnsTrue() {
    assertThat(Month.OCTOBER.isOctober()).isTrue();
  }

  @Test
  public void isOctoberReturnsFalse() {
    testIsNotMonth(Month::isOctober);
  }

  @Test
  public void isNovemberReturnsTrue() {
    assertThat(Month.NOVEMBER.isNovember()).isTrue();
  }

  @Test
  public void isNovemberReturnsFalse() {
    testIsNotMonth(Month::isNovember);
  }

  @Test
  public void isDecemberReturnsTrue() {
    assertThat(Month.DECEMBER.isDecember()).isTrue();
  }

  @Test
  public void isDecemberReturnsFalse() {
    testIsNotMonth(Month::isDecember);
  }
}
