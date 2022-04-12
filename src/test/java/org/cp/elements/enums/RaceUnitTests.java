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
package org.cp.elements.enums;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;

import org.junit.Test;

/**
 * Unit Tests for {@link Race}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.enums.Race
 * @since 1.0.0
 */
public class RaceUnitTests {

  @Test
  public void valueOfReturnsRace() {

    Arrays.stream(Race.values()).forEach(race ->
      assertThat(Race.valueOf(race.name())).isEqualTo(race));
  }

  @Test
  public void valueOfAbbreviationIsCorrect() {

    Arrays.stream(Race.values()).forEach(race ->
      assertThat(Race.valueOfAbbreviation(race.getAbbreviation())).isEqualTo(race));
  }

  @Test
  public void valueOfAbbreviationIsLenient() {

    assertThat(Race.valueOfAbbreviation("BLACK")).isEqualTo(Race.AFRICAN_AMERICAN);
    assertThat(Race.valueOfAbbreviation("Indian")).isEqualTo(Race.AMERICAN_INDIAN);
    assertThat(Race.valueOfAbbreviation("white")).isEqualTo(Race.WHITE);
  }

  @Test
  public void valueOfAbbreviationIsNullSafeReturnsNull() {
    assertThat(Race.valueOfAbbreviation(null)).isNull();
  }

  @Test
  public void valueOfAbbreviationUsingNameReturnsNull() {
    assertThat(Race.valueOfAbbreviation(Race.AFRICAN_AMERICAN.getName())).isNull();
  }

  @Test
  public void valueOfInvalidAbbreviationReturnsNull() {

    assertThat(Race.valueOfAbbreviation("")).isNull();
    assertThat(Race.valueOfAbbreviation("  ")).isNull();
    assertThat(Race.valueOfAbbreviation("alien")).isNull();
    assertThat(Race.valueOfAbbreviation("EURO")).isNull();
    assertThat(Race.valueOfAbbreviation("heBrew")).isNull();
    assertThat(Race.valueOfAbbreviation("muslim")).isNull();
    assertThat(Race.valueOfAbbreviation("Spanish")).isNull();
  }

  @Test
  public void valueOfNameIsCorrect() {

    Arrays.stream(Race.values()).forEach(race ->
      assertThat(Race.valueOfName(race.getName())).isEqualTo(race));
  }

  @Test
  public void valueOfNameIsLenient() {

    assertThat(Race.valueOfName("African American")).isEqualTo(Race.AFRICAN_AMERICAN);
    assertThat(Race.valueOfName("ASIAN")).isEqualTo(Race.ASIAN);
    assertThat(Race.valueOfName("white")).isEqualTo(Race.WHITE);
  }

  @Test
  public void valueOfNameIsNullSafeReturnsNull() {
    assertThat(Race.valueOfName(null)).isNull();
  }

  @Test
  public void valueOfNameUsingAbbreviationReturnsNull() {

    assertThat(Race.valueOfName("Alaskan")).isNull();
    assertThat(Race.valueOfName("Hawaiian")).isNull();
    assertThat(Race.valueOfName("Indian")).isNull();
  }

  @Test
  public void valueOfInvalidNameReturnsNull() {

    assertThat(Race.valueOfName("")).isNull();
    assertThat(Race.valueOfName("  ")).isNull();
    assertThat(Race.valueOfName("Eskimo")).isNull();
    assertThat(Race.valueOfName("Negro")).isNull();
    assertThat(Race.valueOfName("Redman")).isNull();
  }
}
