/*
 * Copyright 2017-Present Author or Authors.
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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.time.Year;
import java.time.YearMonth;
import java.util.Arrays;
import java.util.function.Supplier;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Timespan}
 *
 * @author John Blum
 * @see java.time.LocalDateTime
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.time.Timespan
 * @since 2.0.0
 */
class TimespanUnitTests {

  private void assertBeginning(Timespan timespan, int year, Month month, int dayOfMonth) {
    assertBeginning(timespan, year, month, dayOfMonth, 0, 0, 0);
  }

  private void assertBeginning(Timespan timespan, int year, Month month, int dayOfMonth,
      int hour, int minute, int second) {

    assertThat(timespan).isNotNull();
    assertThat(timespan.getEnd()).isNull();
    assertThat(timespan.getOptionalEnd()).isNotPresent();
    assertTime(year, month, dayOfMonth, hour, minute, second, timespan::getBegin);
    assertThat(timespan.getOptionalBegin().orElse(null)).isEqualTo(timespan.getBegin());
  }

  private void assertEnding(Timespan timespan, int year, Month month, int dayOfMonth) {
    LocalTime time = LocalTime.MAX;
    assertEnding(timespan, year, month, dayOfMonth, time.getHour(), time.getMinute(), time.getSecond());
  }

  private void assertEnding(Timespan timespan, int year, Month month, int dayOfMonth,
      int hour, int minute, int second) {

    assertThat(timespan).isNotNull();
    assertThat(timespan.getBegin()).isNull();
    assertThat(timespan.getOptionalBegin()).isNotPresent();
    assertTime(year, month, dayOfMonth, hour, minute, second, timespan::getEnd);
    assertThat(timespan.getOptionalEnd().orElse(null)).isEqualTo(timespan.getEnd());
  }

  private void assertTime(int year, Month month, int dayOfMonth, int hour, int minute, int second,
      Supplier<LocalDateTime> dateTimeSupplier) {

    LocalDateTime dateTime = dateTimeSupplier.get();

    assertThat(dateTime).isNotNull();
    assertThat(dateTime).hasYear(year);
    assertThat(dateTime).hasMonth(month);
    assertThat(dateTime).hasDayOfMonth(dayOfMonth);
    assertThat(dateTime).hasHour(hour);
    assertThat(dateTime).hasMinute(minute);
    assertThat(dateTime).hasSecond(second);
  }

  @Test
  void timespanBetweenIsCorrect() {

    LocalDateTime begin = LocalDateTime.of(2023, Month.OCTOBER, 30, 15, 4, 30);
    LocalDateTime end = LocalDateTime.now();

    Timespan timespan = Timespan.between(begin, end);

    assertThat(timespan).isNotNull();
    assertThat(timespan.getBegin()).isEqualTo(begin);
    assertThat(timespan.getOptionalBegin().orElse(null)).isEqualTo(begin);
    assertThat(timespan.getEnd()).isEqualTo(end);
    assertThat(timespan.getOptionalEnd().orElse(null)).isEqualTo(end);
  }

  @Test
  void timespanBetweenWithNullBegin() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.between(null, LocalDateTime.now()))
      .withMessage("Begin date/time is required")
      .withNoCause();
  }

  @Test
  void timespanBetweenWithNullEnd() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.between(LocalDateTime.now(), null))
      .withMessage("End date/time is required")
      .withNoCause();
  }

  @Test
  void timespanBetweenWithBeginAfterEnd() {

    LocalDateTime end = LocalDateTime.of(2023, Month.OCTOBER, 30, 15, 30, 15);
    LocalDateTime future = LocalDateTime.of(2024, Month.MARCH, 21, 13, 26, 52);

    Arrays.asList(end, LocalDateTime.now(), future).forEach(begin ->
      assertThatIllegalArgumentException()
        .isThrownBy(() ->  Timespan.between(begin, end))
        .withMessageMatching("Beginning \\[.*] must occur before the Ending \\[.*]")
        .withNoCause());
  }

  @Test
  void beginningInYear() {

    Timespan timespan = Timespan.beginning(Year.of(2023));

    assertBeginning(timespan, 2023, Month.JANUARY, 1);
  }

  @Test
  void beginningInNullYear() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.beginning((Year) null))
      .withMessage("Beginning year is required")
      .withNoCause();
  }

  @Test
  void beginningInYearAndMonth() {

    Timespan timespan = Timespan.beginning(YearMonth.of(2020, Month.JULY));

    assertBeginning(timespan, 2020, Month.JULY, 1);
  }

  @Test
  void beginningInNullYearAndMonth() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.beginning((YearMonth) null))
      .withMessage("Beginning year and month are required")
      .withNoCause();
  }

  @Test
  void beginningOnDate() {

    Timespan timespan = Timespan.beginning(LocalDate.of(2011, Month.MAY, 31));

    assertBeginning(timespan, 2011, Month.MAY, 31);
  }

  @Test
  void beginningOnNullDate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.beginning((LocalDate) null))
      .withMessage("Beginning date is required")
      .withNoCause();
  }

  @Test
  void beginningOnDateAndTime() {

    Timespan timespan = Timespan.beginning(LocalDateTime.of(1998, Month.MAY, 15, 13, 18, 30, 0));

    assertBeginning(timespan, 1998, Month.MAY, 15, 13, 18, 30);
  }

  @Test
  void beginningOnNullDateTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.beginning((LocalDateTime) null))
      .withMessage("Beginning date and time is required")
      .withNoCause();
  }

  @Test
  void beginningAtTime() {

    LocalDate now = LocalDate.now();
    LocalTime time = LocalTime.of(16, 55, 30, 0);

    Timespan timespan = Timespan.beginning(time);

    assertBeginning(timespan, now.getYear(), now.getMonth(), now.getDayOfMonth(), 16, 55, 30);
  }

  @Test
  void beginningAtNullTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.beginning((LocalTime) null))
      .withMessage("Start time is required")
      .withNoCause();
  }

  @Test
  void endingInYear() {

    Timespan timespan = Timespan.ending(Year.of(1999));

    assertEnding(timespan, 1999, Month.DECEMBER, 31);
  }

  @Test
  void endingInNullYear() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.ending((Year) null))
      .withMessage("End year is required")
      .withNoCause();
  }

  @Test
  void endingInYearAndMonth() {

    Timespan timespan = Timespan.ending(YearMonth.of(2020, Month.JUNE));

    assertEnding(timespan, 2020, Month.JUNE, 30);
  }

  @Test
  void endingInNullYearAndMonth() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.ending((YearMonth) null))
      .withMessage("End year and month are required")
      .withNoCause();
  }

  @Test
  void endingOnDate() {

    Timespan timespan = Timespan.ending(LocalDate.of(2013, Month.MARCH, 31));

    assertEnding(timespan, 2013, Month.MARCH, 31);
  }

  @Test
  void endingOnNullDate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.ending((LocalDate) null))
      .withMessage("End date is required")
      .withNoCause();
  }

  @Test
  void endingOnDateAndTime() {

    Timespan timespan = Timespan.ending(LocalDateTime.of(2019, Month.DECEMBER, 31, 16, 59, 59));

    assertEnding(timespan, 2019, Month.DECEMBER, 31, 16, 59, 59);
  }

  @Test
  void endingOnNullDateAndTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.ending((LocalDateTime) null))
      .withMessage("End date and time are required")
      .withNoCause();
  }

  @Test
  void endingAtTime() {

    LocalDate now = LocalDate.now();
    LocalTime time = LocalTime.of(22, 36, 15);

    Timespan timespan = Timespan.ending(time);

    assertEnding(timespan, now.getYear(), now.getMonth(), now.getDayOfMonth(), 22, 36, 15);
  }

  @Test
  void endingAtNullTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.ending((LocalTime) null))
      .withMessage("End time is required")
      .withNoCause();
  }

  @Test
  void fromNow() {

    LocalDateTime now = LocalDateTime.now();
    Timespan timespan = Timespan.fromNow();

    assertThat(timespan).isNotNull();
    assertThat(timespan.getBegin()).isAfterOrEqualTo(now);
    assertThat(timespan.getEnd()).isNull();
    assertThat(timespan.getOptionalEnd()).isNotPresent();
    assertThat(timespan.isFinite()).isFalse();
    assertThat(timespan.isInfinite()).isTrue();
  }

  @Test
  void fromYearToYear() {

    Timespan timespan = Timespan.from(Year.of(2021)).to(Year.of(2023)).build();

    assertThat(timespan).isNotNull();
    assertThat(timespan.getBegin()).isEqualTo(LocalDateTime.of(2021, Month.JANUARY, 1, 0, 0, 0));
    assertThat(timespan.getEnd()).isEqualTo(LocalDate.of(2023, Month.DECEMBER, 31).atTime(LocalTime.MAX));
    assertThat(timespan.isFinite()).isTrue();
    assertThat(timespan.isInfinite()).isFalse();
  }

  @Test
  void fromYearToNullYear() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from(Year.of(2023)).to((Year) null).build())
      .withMessage("End date and time is required")
      .withNoCause();
  }

  @Test
  void fromNullYearToYear() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from((Year) null).to(Year.of(2023)).build())
      .withMessage("Begin date and time is required")
      .withNoCause();
  }

  @Test
  void fromBeginYearAfterEndYear() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from(Year.of(2023)).to(Year.of(2020)).build())
      .withMessageMatching("Beginning \\[.*] of Timespan must not be after the ending \\[.*]")
      .withNoCause();
  }

  @Test
  void fromYearMonthToYearMonth() {

    Timespan timespan = Timespan.from(YearMonth.of(2020, Month.JULY)).to(YearMonth.of(2023, Month.JUNE)).build();

    assertThat(timespan).isNotNull();
    assertThat(timespan.getBegin()).isEqualTo(LocalDate.of(2020, Month.JULY, 1).atStartOfDay());
    assertThat(timespan.getEnd()).isEqualTo(LocalDate.of(2023, Month.JUNE, 30).atTime(LocalTime.MAX));
    assertThat(timespan.isFinite()).isTrue();
    assertThat(timespan.isInfinite()).isFalse();
  }

  @Test
  void fromYearMonthToNullYearMonth() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from(YearMonth.of(2020, Month.JULY)).to((YearMonth) null).build())
      .withMessage("End date and time is required")
      .withNoCause();
  }

  @Test
  void fromNullYearMonthToYearMonth() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from((YearMonth) null).to(YearMonth.of(2022, Month.DECEMBER)).build())
      .withMessage("Begin date and time is required")
      .withNoCause();
  }

  @Test
  void fromBeginYearMonthAfterEndYearMonth() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from(YearMonth.of(2023, Month.DECEMBER)).to(YearMonth.of(2000, Month.JANUARY)).build())
      .withMessageMatching("Beginning \\[.*] of Timespan must not be after the ending \\[.*]")
      .withNoCause();
  }

  @Test
  void infiniteTimespan() {

    Timespan timespan = Timespan.infinite();

    assertThat(timespan).isNotNull();
    assertThat(timespan.getBegin()).isNull();
    assertThat(timespan.getOptionalBegin()).isNotPresent();
    assertThat(timespan.getEnd()).isNull();
    assertThat(timespan.getOptionalEnd()).isNotPresent();
    assertThat(timespan.isFinite()).isFalse();
    assertThat(timespan.isInfinite()).isTrue();
  }

  @Test
  void untilNow() {

    Timespan timespan = Timespan.untilNow();
    LocalDateTime now = LocalDateTime.now();

    assertThat(timespan).isNotNull();
    assertThat(timespan.getBegin()).isNull();
    assertThat(timespan.getOptionalBegin()).isNotPresent();
    assertThat(timespan.getEnd()).isBeforeOrEqualTo(now);
    assertThat(timespan.isFinite()).isFalse();
    assertThat(timespan.isInfinite()).isTrue();
  }
}
