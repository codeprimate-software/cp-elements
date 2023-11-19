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

  private void assertFinite(Timespan timespan) {

    assertThat(timespan).isNotNull();
    assertThat(timespan.isFinite()).isTrue();
    assertThat(timespan.isInfinite()).isFalse();
  }

  private void assertInfinite(Timespan timespan) {

    assertThat(timespan).isNotNull();
    assertThat(timespan.isFinite()).isFalse();
    assertThat(timespan.isInfinite()).isTrue();
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
    assertInfinite(timespan);
  }

  @Test
  void fromYearToYear() {

    Timespan timespan = Timespan.from(Year.of(2021)).to(Year.of(2023)).build();

    assertThat(timespan).isNotNull();
    assertThat(timespan.getBegin()).isEqualTo(LocalDateTime.of(2021, Month.JANUARY, 1, 0, 0, 0));
    assertThat(timespan.getEnd()).isEqualTo(LocalDate.of(2023, Month.DECEMBER, 31).atTime(LocalTime.MAX));
    assertFinite(timespan);
  }

  @Test
  void fromYearToNullYear() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from(Year.of(2023)).to((Year) null).build())
      .withMessage("End date and time are required")
      .withNoCause();
  }

  @Test
  void fromNullYearToYear() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from((Year) null).to(Year.of(2023)).build())
      .withMessage("Begin date and time are required")
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
    assertFinite(timespan);
  }

  @Test
  void fromYearMonthToNullYearMonth() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from(YearMonth.of(2020, Month.JULY)).to((YearMonth) null).build())
      .withMessage("End date and time are required")
      .withNoCause();
  }

  @Test
  void fromNullYearMonthToYearMonth() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from((YearMonth) null).to(YearMonth.of(2022, Month.DECEMBER)).build())
      .withMessage("Begin date and time are required")
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
  void fromBeginDateToEndDate() {

    Timespan timespan = Timespan.from(LocalDate.of(2000, Month.DECEMBER, 4))
      .to(LocalDate.of(2023, Month.NOVEMBER, 15))
      .build();

    assertThat(timespan).isNotNull();
    assertThat(timespan.getBegin()).isEqualTo(LocalDate.of(2000, Month.DECEMBER, 4).atStartOfDay());
    assertThat(timespan.getEnd()).isEqualTo(LocalDate.of(2023, Month.NOVEMBER, 15).atTime(LocalTime.MAX));
    assertFinite(timespan);
  }

  @Test
  void fromBeginDateToNullEndDate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from(LocalDate.now()).to((LocalDate) null).build())
      .withMessage("End date and time are required")
      .withNoCause();
  }

  @Test
  void fromNullBeginDateToEndDate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from((LocalDate) null).to(LocalDate.now()).build())
      .withMessage("Begin date and time are required")
      .withNoCause();
  }

  @Test
  void fromBeginDateAfterEndDate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from(LocalDate.now().plusDays(2)).to(LocalDate.now()).build())
      .withMessageMatching("Beginning \\[.*] of Timespan must not be after the ending \\[.*]")
      .withNoCause();
  }

  @Test
  void fromBeginDateTimeToEndDateTime() {

    LocalDateTime begin = LocalDateTime.of(2023, Month.NOVEMBER, 15, 23, 9, 0, 0);
    LocalDateTime end = LocalDateTime.of(2023, Month.NOVEMBER, 15, 23, 10, 0, 0);

    Timespan timespan = Timespan.from(begin).to(end).build();

    assertThat(timespan).isNotNull();
    assertThat(timespan.getBegin()).isEqualTo(begin);
    assertThat(timespan.getEnd()).isEqualTo(end);
    assertFinite(timespan);
  }

  @Test
  void fromBeginDateTimeToNullEndDateTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from(LocalDateTime.now()).to((LocalDateTime) null).build())
      .withMessage("End date and time are required")
      .withNoCause();
  }

  @Test
  void fromNullBeginDateTimeToEndDateTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from((LocalDateTime) null).to(LocalDateTime.now()).build())
      .withMessage("Begin date and time are required")
      .withNoCause();
  }

  @Test
  void fromBeginDateTimeAfterEndDateTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from(LocalDateTime.now().plusHours(1)).to(LocalDateTime.now()).build())
      .withMessageMatching("Beginning \\[.*] of Timespan must not be after the ending \\[.*]")
      .withNoCause();
  }

  @Test
  void fromBeginTimeToEndTime() {

    Timespan timespan = Timespan.from(LocalTime.MIDNIGHT).to(LocalTime.MAX).build();

    assertThat(timespan).isNotNull();
    assertThat(timespan.getBegin()).isEqualTo(LocalDate.now().atStartOfDay());
    assertThat(timespan.getEnd()).isEqualTo(LocalDate.now().atTime(LocalTime.MAX));
    assertFinite(timespan);
  }

  @Test
  void fromBeginTimeToNullEndTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from(LocalTime.now()).to((LocalTime) null).build())
      .withMessage("End date and time are required")
      .withNoCause();
  }

  @Test
  void fromNullBeginTimeToEndTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from((LocalTime) null).to(LocalTime.now()).build())
      .withMessage("Begin date and time are required")
      .withNoCause();
  }

  @Test
  void fromBeginTimeAfterEndTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.from(LocalTime.now()).to(LocalTime.MIDNIGHT).build())
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
    assertInfinite(timespan);
  }

  @Test
  void untilNow() {

    Timespan timespan = Timespan.untilNow();
    LocalDateTime now = LocalDateTime.now();

    assertThat(timespan).isNotNull();
    assertThat(timespan.getBegin()).isNull();
    assertThat(timespan.getOptionalBegin()).isNotPresent();
    assertThat(timespan.getEnd()).isBeforeOrEqualTo(now);
    assertInfinite(timespan);
  }

  @Test
  void isAfterYearReturnsTrue() {
    assertThat(Timespan.ending(Year.of(2023)).isAfter(Year.of(2024))).isTrue();
  }

  @Test
  void isAfterYearReturnsFalse() {
    assertThat(Timespan.ending(Year.of(2023)).isAfter(Year.of(2022))).isFalse();
    assertThat(Timespan.ending(Year.of(2023)).isAfter(Year.of(1999))).isFalse();
  }

  @Test
  void isAfterYearIsNullSafe() {
    assertThat(Timespan.ending(Year.of(2020)).isAfter((Year) null)).isFalse();
  }

  @Test
  void isAfterYearMonthReturnsTrue() {
    assertThat(Timespan.ending(YearMonth.of(2023, Month.MARCH)).isAfter(YearMonth.of(2024, Month.FEBRUARY)))
      .isTrue();
  }

  @Test
  void isAfterYearMonthReturnsFalse() {
    assertThat(Timespan.ending(YearMonth.of(2023, Month.APRIL)).isAfter(YearMonth.of(2022, Month.NOVEMBER)))
      .isFalse();
  }

  @Test
  void isAfterYearMonthIsNullSafe() {
    assertThat(Timespan.ending(YearMonth.of(2023, Month.NOVEMBER)).isAfter((YearMonth) null)).isFalse();
  }

  @Test
  void isAfterDateReturnsTrue() {
    assertThat(Timespan.ending(LocalDate.of(1974, Month.MAY, 27)).isAfter(LocalDate.of(1975, Month.JANUARY, 22)))
      .isTrue();
  }

  @Test
  void isAfterDateReturnsFalse() {
    assertThat(Timespan.ending(LocalDate.of(2023, Month.MAY, 27)).isAfter(LocalDate.of(2008, Month.AUGUST, 25)))
      .isFalse();
  }

  @Test
  void isAfterDateIsNullSafe() {
    assertThat(Timespan.ending(LocalDate.of(2023, Month.MAY, 27)).isAfter((LocalDate) null)).isFalse();
  }

  @Test
  void isAfterDateTimeReturnsTrue() {
    assertThat(Timespan.ending(LocalDateTime.of(1974, Month.MAY, 27, 1, 15, 30))
      .isAfter(LocalDateTime.of(1975, Month.JANUARY, 22, 18, 45, 30, 0))).isTrue();
  }

  @Test
  void isAfterDateTimeReturnsFalse() {
    assertThat(Timespan.ending(LocalDateTime.of(1974, Month.MAY, 27, 1, 15, 30))
      .isAfter(LocalDateTime.of(2008, Month.AUGUST, 25, 5, 5, 30, 512))).isTrue();
  }

  @Test
  void isAfterDateTimeIsNullSafe() {
    assertThat(Timespan.ending(LocalDateTime.of(2023, Month.MAY, 27, 1, 15, 30, 0)).isAfter((LocalDateTime) null))
      .isFalse();
  }

  @Test
  void isAferTimeReturnsTrue() {
    assertThat(Timespan.ending(LocalTime.of(22, 23, 24, 512)).isAfter(LocalTime.MAX)).isTrue();
  }

  @Test
  void isAferTimeReturnsFalse() {
    assertThat(Timespan.ending(LocalTime.MAX).isAfter(LocalTime.of(22, 24, 30, 512))).isFalse();
  }

  @Test
  void isAferTimeIsNullSafe() {
    assertThat(Timespan.ending(LocalTime.MAX).isAfter((LocalTime) null)).isFalse();
  }

  @Test
  void isBeforeYearReturnsTrue() {
    assertThat(Timespan.beginning(Year.of(2023)).isBefore(Year.of(2022))).isTrue();
  }

  @Test
  void isBeforeYearReturnsFalse() {
    assertThat(Timespan.beginning(Year.of(2023)).isBefore(Year.of(2023))).isFalse();
    assertThat(Timespan.beginning(Year.of(2023)).isBefore(Year.of(2024))).isFalse();
    assertThat(Timespan.beginning(Year.of(2023)).isBefore(Year.of(2100))).isFalse();
  }

  @Test
  void isBeforeYearIsNullSafe() {
    assertThat(Timespan.beginning(Year.of(2023)).isBefore((Year) null)).isFalse();
  }

  @Test
  void isBeforeYearMonthReturnsTrue() {
    assertThat(Timespan.beginning(YearMonth.of(2023, Month.JULY)).isBefore(YearMonth.of(2023, Month.JUNE))).isTrue();
    assertThat(Timespan.beginning(YearMonth.of(2023, Month.MAY)).isBefore(YearMonth.of(2022, Month.AUGUST))).isTrue();
    assertThat(Timespan.beginning(YearMonth.of(2023, Month.MAY)).isBefore(YearMonth.of(2021, Month.APRIL))).isTrue();
  }

  @Test
  void isBeforeYearMonthReturnsFalse() {
    assertThat(Timespan.beginning(YearMonth.of(2023, Month.JULY)).isBefore(YearMonth.of(2023, Month.AUGUST))).isFalse();
    assertThat(Timespan.beginning(YearMonth.of(2023, Month.JULY)).isBefore(YearMonth.of(2024, Month.JANUARY))).isFalse();
    assertThat(Timespan.beginning(YearMonth.of(2023, Month.MAY)).isBefore(YearMonth.of(2100, Month.MARCH))).isFalse();
  }

  @Test
  void isBeforeYearMonthIsNullSafe() {
    assertThat(Timespan.beginning(YearMonth.of(2023, Month.NOVEMBER)).isBefore((YearMonth) null)).isFalse();
  }

  @Test
  void isBeforeDateReturnsTrue() {

    assertThat(Timespan.beginning(LocalDate.of(2023, Month.NOVEMBER, 19))
      .isBefore(LocalDate.of(2023, Month.NOVEMBER, 18))).isTrue();
    assertThat(Timespan.beginning(LocalDate.of(2023, Month.NOVEMBER, 19))
      .isBefore(LocalDate.of(2022, Month.DECEMBER, 25))).isTrue();
  }

  @Test
  void isBeforeDateReturnsFalse() {

    assertThat(Timespan.beginning(LocalDate.of(2023, Month.NOVEMBER, 19))
      .isBefore(LocalDate.of(2023, Month.NOVEMBER, 19))).isFalse();
    assertThat(Timespan.beginning(LocalDate.of(2023, Month.NOVEMBER, 19))
      .isBefore(LocalDate.of(2023, Month.DECEMBER, 12))).isFalse();
    assertThat(Timespan.beginning(LocalDate.of(2023, Month.NOVEMBER, 19))
      .isBefore(LocalDate.of(2024, Month.JANUARY, 1))).isFalse();
  }

  @Test
  void isBeforeDateIsNullSafe() {
    assertThat(Timespan.beginning(LocalDate.of(2023, Month.NOVEMBER, 19)).isBefore((LocalDate) null)).isFalse();
  }

  @Test
  void isBeforeDateTimeReturnsTrue() {

    assertThat(Timespan.beginning(LocalDateTime.of(2023, Month.NOVEMBER, 19, 12, 24, 48, 0))
      .isBefore(LocalDateTime.of(2022, Month.JANUARY, 1, 6, 12, 24, 0))).isTrue();
    assertThat(Timespan.beginning(LocalDateTime.of(2023, Month.NOVEMBER, 19, 12, 24, 48, 0))
      .isBefore(LocalDateTime.of(2023, Month.NOVEMBER, 19, 12, 24, 47, 0))).isTrue();
  }

  @Test
  void isBeforeDateTimeReturnsFalse() {

    assertThat(Timespan.beginning(LocalDateTime.of(2023, Month.NOVEMBER, 19, 12, 24, 48, 0))
      .isBefore(LocalDateTime.of(2023, Month.NOVEMBER, 19, 12, 24, 48, 0))).isFalse();
    assertThat(Timespan.beginning(LocalDateTime.of(2023, Month.NOVEMBER, 19, 12, 24, 48, 0))
      .isBefore(LocalDateTime.of(2024, Month.FEBRUARY, 28, 11, 59, 59, 999))).isFalse();
  }

  @Test
  void isBeforeDateTimeIsNullSafe() {
    assertThat(Timespan.beginning(LocalDateTime.of(2023, Month.NOVEMBER, 19, 0, 9, 18, 0))
      .isBefore((LocalDateTime) null)).isFalse();
  }

  @Test
  void isBeforeTimeReturnsTrue() {
    assertThat(Timespan.beginning(LocalTime.of(12, 25, 50, 500)).isBefore(LocalTime.of(6, 13, 30, 999))).isTrue();
  }

  @Test
  void isBeforeTimeReturnsFalse() {
    assertThat(Timespan.beginning(LocalTime.of(12, 25, 50, 500)).isBefore(LocalTime.of(12, 25, 50, 500))).isFalse();
    assertThat(Timespan.beginning(LocalTime.of(12, 25, 50, 500)).isBefore(LocalTime.MAX)).isFalse();
  }

  @Test
  void isBeforeTimeIsNullSafe() {
    assertThat(Timespan.beginning(LocalTime.of(0, 10, 20, 0)).isBefore((LocalTime) null)).isFalse();
  }

  @Test
  void isDuringYearReturnsTrue() {
    assertThat(Timespan.from(Year.of(2023)).to(Year.of(2023)).build().isDuring(Year.of(2023))).isTrue();
    assertThat(Timespan.from(Year.of(2022)).to(Year.of(2024)).build().isDuring(Year.of(2023))).isTrue();
  }

  @Test
  void isDuringYearReturnsFalse() {
    assertThat(Timespan.from(Year.of(2023)).to(Year.of(2023)).build().isDuring(Year.of(2022))).isFalse();
    assertThat(Timespan.from(Year.of(2023)).to(Year.of(2023)).build().isDuring(Year.of(2024))).isFalse();
    assertThat(Timespan.from(Year.of(2022)).to(Year.of(2024)).build().isDuring(Year.of(1923))).isFalse();
  }

  @Test
  void isDuringYearWithNullIsNullSafe() {
    assertThat(Timespan.from(Year.of(2023)).to(Year.of(2023)).build().isDuring((Year) null)).isFalse();
  }

  @Test
  void isDuringYearMonthReturnsTrue() {

    YearMonth yearMonth = YearMonth.of(2023, Month.NOVEMBER);

    assertThat(Timespan.from(yearMonth).to(yearMonth).build().isDuring(yearMonth)).isTrue();
    assertThat(Timespan.from(yearMonth).to(yearMonth).build()
      .isDuring(LocalDate.of(yearMonth.getYear(), yearMonth.getMonth(), 19))).isTrue();
  }

  @Test
  void isDuringYearMonthReturnsFalse() {

    YearMonth yearMonth = YearMonth.of(2023, Month.NOVEMBER);

    assertThat(Timespan.from(yearMonth).to(yearMonth).build()
      .isDuring(LocalDate.of(2022, yearMonth.getMonth(), 19))).isFalse();
    assertThat(Timespan.from(yearMonth).to(yearMonth).build()
      .isDuring(LocalDate.of(2024, yearMonth.getMonth(), 19))).isFalse();
  }

  @Test
  void isDuringYearMonthWithNullIsNullSafe() {
    YearMonth november2023 = YearMonth.of(2023, Month.NOVEMBER);
    assertThat(Timespan.from(november2023).to(november2023).build().isDuring((YearMonth) null)).isFalse();
  }

  @Test
  void isDuringDateReturnsTrue() {
    LocalDate date = LocalDate.of(2023, Month.NOVEMBER, 19);
    assertThat(Timespan.from(date).to(date).build().isDuring(date)).isTrue();
    assertThat(Timespan.from(date).to(date).build().isDuring(LocalTime.now().atDate(date))).isTrue();
  }

  @Test
  void isDuringDateReturnsFalse() {
    LocalDate date = LocalDate.of(2023, Month.NOVEMBER, 19);
    assertThat(Timespan.from(date).to(date).build().isDuring(LocalDate.of(2022, Month.NOVEMBER, 19))).isFalse();
    assertThat(Timespan.from(date).to(date).build().isDuring(LocalDate.of(2024, Month.NOVEMBER, 19))).isFalse();
  }

  @Test
  void isDuringDateWithNullIsNullSafe() {
    LocalDate date = LocalDate.of(2023, Month.NOVEMBER, 19);
    assertThat(Timespan.from(date).to(date).build().isDuring((LocalDate) null)).isFalse();
  }

  @Test
  void isDuringDateTimeReturnsTrue() {
    LocalDateTime now = LocalDateTime.now();
    assertThat(Timespan.from(now).to(now).build().isDuring(now)).isTrue();
  }

  @Test
  void isDuringDateTimeReturnsFalse() {
    LocalDateTime dateTime = LocalDateTime.of(2023, Month.NOVEMBER, 19, 14, 41, 55, 999);
    assertThat(Timespan.from(dateTime).to(dateTime).build().isDuring(LocalDateTime.now())).isFalse();
  }

  @Test
  void isDuringDateTimeWithNullIsNullSafe() {
    LocalDateTime dateTime = LocalDateTime.of(2023, Month.NOVEMBER, 19, 13, 30, 45, 500);
    assertThat(Timespan.from(dateTime).to(dateTime).build().isDuring((LocalDateTime) null)).isFalse();
  }

  @Test
  void isDuringTimeReturnsTrue() {
    LocalTime now = LocalTime.now();
    assertThat(Timespan.from(now).to(now).build().isDuring(now)).isTrue();
    assertThat(Timespan.from(LocalTime.MIN).to(LocalTime.MAX).build().isDuring(now)).isTrue();
  }

  @Test
  void isDuringTimeReturnsFalse() {

    LocalTime time = LocalTime.of(14, 2, 30, 999);

    assertThat(Timespan.from(time).to(time).build().isDuring(LocalTime.of(2, 2, 30, 999))).isFalse();
    assertThat(Timespan.from(time).to(time).build().isDuring(time.atDate(LocalDate.of(2020, Month.NOVEMBER, 19))))
      .isFalse();
  }

  @Test
  void isDuringTimeWithNullIsNullSafe() {
    LocalTime time = LocalTime.of(13, 30, 45, 500);
    assertThat(Timespan.from(time).to(time).build().isDuring((LocalDateTime) null)).isFalse();
  }
}
