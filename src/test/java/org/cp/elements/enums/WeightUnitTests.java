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
import java.util.Locale;
import java.util.Optional;

import org.cp.elements.lang.StringUtils;
import org.junit.Test;

/**
 * Unit tests for {@link WeightUnit}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.enums.WeightUnit
 * @since 1.0.0
 */
public class WeightUnitTests {

  @Test
  public void defaultWeightUnitIsCorrect() {

    assertThat(WeightUnit.getDefault())
      .isEqualTo(Optional.of(Locale.getDefault().getCountry())
        .filter(Locale.US.getCountry()::equals)
        .map(it -> WeightUnit.OUNCE)
        .orElse(WeightUnit.GRAM));
  }

  @Test
  public void valueOfReturnsWeightUnit() {

    Arrays.stream(WeightUnit.values()).forEach(it ->
      assertThat(WeightUnit.valueOf(it.name())).isEqualTo(it));
  }

  @Test
  public void valueOfAbbreviationReturnsWeightUnit() {

    Arrays.stream(WeightUnit.values()).forEach(it -> {
      assertThat(WeightUnit.valueOfAbbreviation(it.getAbbreviation().toLowerCase())).isEqualTo(it);
      assertThat(WeightUnit.valueOfAbbreviation(StringUtils.capitalize(it.getAbbreviation().toLowerCase()))).isEqualTo(it);
      assertThat(WeightUnit.valueOfAbbreviation(it.getAbbreviation().toUpperCase())).isEqualTo(it);
    });
  }

  @Test
  public void valueOfInvalidAbbreviationsReturnsNull() {

    assertThat(WeightUnit.valueOfAbbreviation("GR")).isNull();
    assertThat(WeightUnit.valueOfAbbreviation("GRM")).isNull();
    assertThat(WeightUnit.valueOfAbbreviation("ounce")).isNull();
    assertThat(WeightUnit.valueOfAbbreviation("one")).isNull();
    assertThat(WeightUnit.valueOfAbbreviation("pond")).isNull();
    assertThat(WeightUnit.valueOfAbbreviation("TT")).isNull();
  }

  @Test
  public void valueOfNullAbbreviationReturnsNull() {
    assertThat(WeightUnit.valueOfAbbreviation(null)).isNull();
  }

  @Test
  public void valueOfNameReturnsWeightUnit() {

    Arrays.stream(WeightUnit.values()).forEach(it -> {
      assertThat(WeightUnit.valueOfName(it.name().toLowerCase())).isEqualTo(it);
      assertThat(WeightUnit.valueOfName(StringUtils.capitalize(it.name().toLowerCase()))).isEqualTo(it);
      assertThat(WeightUnit.valueOfName(it.name().toUpperCase())).isEqualTo(it);
    });
  }

  @Test
  public void valueOfInvalidNamesReturnsNull() {

    assertThat(WeightUnit.valueOfName("milliram")).isNull();
    assertThat(WeightUnit.valueOfName("gramcracker")).isNull();
    assertThat(WeightUnit.valueOfName("killogram")).isNull();
    assertThat(WeightUnit.valueOfName("one")).isNull();
    assertThat(WeightUnit.valueOfName("pond")).isNull();
    assertThat(WeightUnit.valueOfName("t")).isNull();
  }

  @Test
  public void valueOfNullNameReturnsNull() {
    assertThat(WeightUnit.valueOfName(null)).isNull();
  }

  @Test
  public void getPluralNamesIsCorrect() {

    Arrays.stream(WeightUnit.values()).forEach(it ->
      assertThat(it.getPluralName()).isEqualTo(it.name().concat("S")));
  }

  @Test
  public void toStringIsNameInTitleCase() {

    Arrays.stream(WeightUnit.values()).forEach(it ->
      assertThat(it.toString()).isEqualTo(StringUtils.capitalize(it.name().toLowerCase())));
  }
}
