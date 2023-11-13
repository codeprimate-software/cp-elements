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

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Timespan}
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.time.Timespan
 * @since 2.0.0
 */
class TimespanUnitTests {

  private void assertBeginning(Timespan timespan, int year, Month month, int dayOfMonth) {
    assertBeginning(timespan, year, month, dayOfMonth, 0, 0, 0);
  }

  private void assertBeginning(Timespan timespan, int year, Month month, int dayOfMonth,
      int hour, int minute, int seconds) {

    assertThat(timespan).isNotNull();
    assertThat(timespan.getEnd()).isNull();
    assertThat(timespan.getOptionalEnd()).isNotPresent();

    LocalDateTime begin = timespan.getBegin();

    assertThat(begin).isNotNull();
    assertThat(begin).hasYear(year);
    assertThat(begin).hasMonth(month);
    assertThat(begin).hasDayOfMonth(dayOfMonth);
    assertThat(begin).hasHour(hour);
    assertThat(begin).hasMinute(minute);
    assertThat(begin).hasSecond(seconds);
    assertThat(timespan.getOptionalBegin().orElse(null)).isEqualTo(begin);
  }

  @Test
  void timespanBetweenIsSuccessful() {

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

    YearMonth yearMonth = YearMonth.of(2020, Month.MAY);

    Timespan timespan = Timespan.beginning(yearMonth);

    assertBeginning(timespan, 2020, Month.MAY, 1);
  }

  @Test
  void beginningInNullYearnAndMonth() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.beginning((YearMonth) null))
      .withMessage("Beginning year and month are required")
      .withNoCause();
  }

  @Test
  void beginningOnDate() {

    LocalDate date = LocalDate.of(2011, Month.MAY, 31);

    Timespan timespan = Timespan.beginning(date);

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

    LocalDateTime dateTime = LocalDateTime.of(1998, Month.MAY, 15, 13, 18, 30, 0);

    Timespan timespan = Timespan.beginning(dateTime);

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

    LocalTime time = LocalTime.of(16, 55, 30, 0);
    LocalDate date = LocalDate.now();

    Timespan timespan = Timespan.beginning(time);

    assertBeginning(timespan, date.getYear(), date.getMonth(), date.getDayOfMonth(), 16, 55, 30);
  }

  @Test
  void beginningAtNullTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.beginning((LocalTime) null))
      .withMessage("Start time is required")
      .withNoCause();
  }
}
