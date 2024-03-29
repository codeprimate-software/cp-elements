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

import java.time.temporal.ChronoUnit;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link TimeUnit}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.time.TimeUnit
 * @since 1.0.0
 */
class TimeUnitUnitTests {

  @Test
  void valueOfReturnsTimeUnit() {

    Arrays.stream(TimeUnit.values()).forEach(timeUnit ->
      assertThat(TimeUnit.valueOf(timeUnit.name())).isEqualTo(timeUnit));
  }

  @Test
  void valueOfAbbreviationIsCorrect() {

    Arrays.stream(TimeUnit.values()).forEach(timeUnit ->
      assertThat(TimeUnit.valueOfAbbreviation(timeUnit.getAbbreviation())).isEqualTo(timeUnit));
  }

  @Test
  void valueOfAbbreviationIsLenient() {

    assertThat(TimeUnit.valueOfAbbreviation("ms")).isEqualTo(TimeUnit.MILLISECOND);
    assertThat(TimeUnit.valueOfAbbreviation("Mi")).isEqualTo(TimeUnit.MINUTE);
    assertThat(TimeUnit.valueOfAbbreviation("DAY")).isEqualTo(TimeUnit.DAY);
  }

  @Test
  void valueOfAbbreviationIsNullSafeReturnsNull() {
    assertThat(TimeUnit.valueOfAbbreviation(null)).isNull();
  }

  @Test
  void valueOfAbbreviationUsingNameReturnsNull() {
    assertThat(TimeUnit.valueOfAbbreviation("Week")).isNull();
  }

  @Test
  void valueOfInvalidAbbreviationReturnsNull() {

    assertThat(TimeUnit.valueOfAbbreviation("")).isNull();
    assertThat(TimeUnit.valueOfAbbreviation("  ")).isNull();
    assertThat(TimeUnit.valueOfAbbreviation("our")).isNull();
    assertThat(TimeUnit.valueOfAbbreviation("sent")).isNull();
    assertThat(TimeUnit.valueOfAbbreviation("wknd")).isNull();
  }

  @Test
  void valueOfNameIsCorrect() {

    Arrays.stream(TimeUnit.values()).forEach(timeUnit -> {
      assertThat(TimeUnit.valueOfName(timeUnit.name())).isEqualTo(timeUnit);
      assertThat(TimeUnit.valueOfName(timeUnit.getName())).isEqualTo(timeUnit);
    });
  }

  @Test
  void valueOfNameIsLenient() {

    assertThat(TimeUnit.valueOfName("MicroSecond")).isEqualTo(TimeUnit.MICROSECOND);
    assertThat(TimeUnit.valueOfName("milliSecond")).isEqualTo(TimeUnit.MILLISECOND);
    assertThat(TimeUnit.valueOfName("second")).isEqualTo(TimeUnit.SECOND);
    assertThat(TimeUnit.valueOfName("Minute")).isEqualTo(TimeUnit.MINUTE);
    assertThat(TimeUnit.valueOfName("hOUR")).isEqualTo(TimeUnit.HOUR);
    assertThat(TimeUnit.valueOfName("DAY")).isEqualTo(TimeUnit.DAY);
    assertThat(TimeUnit.valueOfName("yEAR")).isEqualTo(TimeUnit.YEAR);
  }

  @Test
  void valueOfNameIsNullSafeReturnsNull() {
    assertThat(TimeUnit.valueOfName(null)).isNull();
  }

  @Test
  void valueOfNameUsingAbbreviationReturnsNull() {
    assertThat(TimeUnit.valueOfName("us")).isNull();
  }

  @Test
  void valueOfInvalidNameReturnsNull() {

    assertThat(TimeUnit.valueOfName("")).isNull();
    assertThat(TimeUnit.valueOfName("  ")).isNull();
    assertThat(TimeUnit.valueOfName("daily")).isNull();
    assertThat(TimeUnit.valueOfName("weekend")).isNull();
  }

  @Test
  void valueOfChronoUnitIsCorrect() {

    Arrays.stream(TimeUnit.values()).forEach(timeUnit ->
      assertThat(TimeUnit.valueOfChronoUnit(timeUnit.getChronoUnit())).isEqualTo(timeUnit));
  }

  @Test
  void valeOfNonExistingChronoUnitReturnsNull() {
    assertThat(TimeUnit.valueOfChronoUnit(ChronoUnit.ERAS)).isNull();
  }
}
