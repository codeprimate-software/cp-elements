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

import java.time.LocalDateTime;
import java.time.Month;
import java.time.Year;
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
  void beginningYearIsCorrect() {

    Timespan timespan = Timespan.beginning(Year.of(2023));

    assertThat(timespan).isNotNull();
    assertThat(timespan.getEnd()).isNull();
    assertThat(timespan.getOptionalEnd()).isNotPresent();

    LocalDateTime begin = timespan.getBegin();

    assertThat(begin).isNotNull();
    assertThat(begin).hasYear(2023);
    assertThat(begin).hasMonth(Month.JANUARY);
    assertThat(begin).hasDayOfMonth(1);
    assertThat(begin).hasHour(0);
    assertThat(begin).hasMinute(0);
    assertThat(begin).hasSecond(0);
    assertThat(timespan.getOptionalBegin().orElse(null)).isEqualTo(begin);
  }

  @Test
  void beginningInNullYear() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Timespan.beginning((Year) null))
      .withMessage("Begin Year is required")
      .withNoCause();
  }
}
