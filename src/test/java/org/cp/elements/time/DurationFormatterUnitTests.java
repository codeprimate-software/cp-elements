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

import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.LangExtensions.assertThat;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link DurationFormatter}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.time.DurationFormatter
 * @since 2.0.0
 */
class DurationFormatterUnitTests {

  private Duration durationOf(long days, long hours, long minutes, long seconds,
      long milliseconds, long microseconds, long nanoseconds) {

    return Duration.ofDays(days)
      .plus(Duration.ofHours(hours))
      .plus(Duration.ofMinutes(minutes))
      .plus(Duration.ofSeconds(seconds))
      .plus(Duration.ofMillis(milliseconds))
      .plus(Duration.of(microseconds, ChronoUnit.MICROS))
      .plus(Duration.ofNanos(nanoseconds));
  }

  @Test
  void daysHoursMinutesSecondsMillisecondsMicrosecondsNanosecondsIsCorrect() {

    Duration actual = DurationFormatter.TIME_UNIT.parse("2d16h29m10s451ms256125us8192156ns");
    Duration expected = durationOf(2, 16, 29, 10, 451, 256_125, 8_192_156);

    assertThat(actual).isEqualTo(expected);
  }

  @Test
  void daysHoursMinutesSecondsIsCorrect() {

    Duration actual = DurationFormatter.TIME_UNIT.parse("5d12h30m15s");
    Duration expected = durationOf(5, 12, 30, 15, 0, 0, 0);

    assertThat(actual).isEqualTo(expected);
  }

  @Test
  void daysMinutesMillisecondsNanosecondsIsCorrect() {

    Duration actual = DurationFormatter.TIME_UNIT.parse("21d42m999ms2124480ns");
    Duration expected = durationOf(21, 0, 42, 0, 999, 0, 2_124_480);

    assertThat(actual).isEqualTo(expected);
  }

  @Test
  void daysSecondsNanosecondsIsCorrect() {

    Duration actual = DurationFormatter.TIME_UNIT.parse("8d22s16156248ns");
    Duration expected = durationOf(8, 0, 0, 22, 0, 0, 16_156_248);

    assertThat(actual).isEqualTo(expected);
  }

  @Test
  void daysNanosecondsIsCorrect() {

    Duration actual = DurationFormatter.TIME_UNIT.parse("16d8ns");
    Duration expected = durationOf(16, 0, 0, 0, 0, 0, 8);

    assertThat(actual).isEqualTo(expected);
  }

  @Test
  void hoursSecondsMicrosecondsIsCorrect() {

    Duration actual = DurationFormatter.TIME_UNIT.parse("18h59s512125us");
    Duration expected = durationOf(0, 18, 0, 59, 0, 512_125, 0);

    assertThat(actual).isEqualTo(expected);
  }

  @Test
  void minutesSecondsMillisecondsIsCorrect() {

    Duration actual = DurationFormatter.TIME_UNIT.parse("30m29s999ms");
    Duration expected = durationOf(0, 0, 30, 29, 999, 0, 0);

    assertThat(actual).isEqualTo(expected);
  }

  @Test
  void secondsMilliseconds() {

    Duration actual = DurationFormatter.TIME_UNIT.parse("45s555ms");
    Duration expected = durationOf(0, 0, 0, 45, 555, 0, 0);

    assertThat(actual).isEqualTo(expected);
  }

  @Test
  void millisecondsMicrosecondsNanosecondsIsCorrect() {

    Duration actual = DurationFormatter.TIME_UNIT.parse("456ms128654us1248842ns");
    Duration expected = durationOf(0, 0, 0, 0, 456, 128_654, 1_248_842);

    assertThat(actual).isEqualTo(expected);
  }

  @Test
  void durationFormatterUnitValueOfChronoUnitIsCorrect() {

    Arrays.stream(DurationFormatter.Unit.values()).forEach(unit ->
      assertThat(DurationFormatter.Unit.valueOfChronoUnit(unit.getUnit())).isEqualTo(unit));
  }

  @Test
  void durationFormatterUnitValueOfInvalidChronoUnitThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> DurationFormatter.Unit.valueOfChronoUnit(ChronoUnit.ERAS))
      .withMessage("No Unit %s matches the given ChronoUnit [%s]",
        Arrays.toString(DurationFormatter.Unit.values()), ChronoUnit.ERAS)
      .withNoCause();
  }

  @Test
  void durationFormatterUnitValueOfSymbolIsCorrect() {

    Arrays.stream(DurationFormatter.Unit.values()).forEach(unit ->
      assertThat(DurationFormatter.Unit.valueOfSymbol(unit.getSymbol())).isEqualTo(unit));
  }

  @Test
  void durationFormatterUnitValueOfInvalidSymbolThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> DurationFormatter.Unit.valueOfSymbol("yr"))
      .withMessage("No Unit %s matches the given Symbol [yr]", Arrays.toString(DurationFormatter.Unit.values()))
      .withNoCause();
  }
}
