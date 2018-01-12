/*
 * Copyright 2016 Author or Authors.
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

package org.cp.elements.data.conversion.converters;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit tests for {@link BooleanConverter}.
 *
 * @author John Blum
 * @see java.lang.Boolean
 * @see org.junit.Test
 * @see org.cp.elements.data.conversion.converters.BooleanConverter
 * @since 1.0.0
 */
public class BooleanConverterTests {

  private final BooleanConverter converter = new BooleanConverter();

  @Test
  public void canConvertReturnsTrue() {

    assertThat(this.converter.canConvert(Boolean.class, Boolean.class)).isTrue();
    assertThat(this.converter.canConvert(Boolean.class, Boolean.TYPE)).isTrue();
    assertThat(this.converter.canConvert(Boolean.TYPE, Boolean.class)).isTrue();
    assertThat(this.converter.canConvert(Boolean.TYPE, Boolean.TYPE)).isTrue();
    assertThat(this.converter.canConvert(Character.class, Boolean.class)).isTrue();
    assertThat(this.converter.canConvert(Double.class, Boolean.class)).isTrue();
    assertThat(this.converter.canConvert(Integer.class, Boolean.class)).isTrue();
    assertThat(this.converter.canConvert(Object.class, Boolean.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Boolean.class)).isTrue();
  }

  @Test
  public void canConverterNullToBooleanReturnsTrue() {
    assertThat(this.converter.canConvert(null, Boolean.class)).isTrue();
  }

  @Test
  public void cannotConvertReturnsFalse() {

    assertThat(this.converter.canConvert(Boolean.class, null)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, Character.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, Double.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, Integer.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, Object.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, String.class)).isFalse();
  }

  @Test
  public void convertBooleanValues() {

    assertThat(this.converter.convert(Boolean.TRUE)).isTrue();
    assertThat(this.converter.convert(true)).isTrue();
    assertThat(this.converter.convert(Boolean.FALSE)).isFalse();
    assertThat(this.converter.convert(false)).isFalse();
  }

  @Test
  public void convertFalseStringsReturnsFalse() {

    assertThat(this.converter.convert("false")).isFalse();
    assertThat(this.converter.convert("False")).isFalse();
    assertThat(this.converter.convert("FALSE")).isFalse();
  }

  @Test
  public void convertNullReturnsFalse() {
    assertThat(this.converter.convert(null)).isFalse();
  }

  @Test
  public void convertNullWithTrueDefaultValueReturnsTrue() {
    assertThat(this.converter.withDefaultValue(true).convert(null)).isTrue();
  }

  @Test
  public void convertTrueStringsReturnsTrue() {

    assertThat(this.converter.convert("true")).isTrue();
    assertThat(this.converter.convert("True")).isTrue();
    assertThat(this.converter.convert("TRUE")).isTrue();
  }

  @Test
  public void convertUnspecifiedStringsReturnsFalse() {

    assertThat(this.converter.convert("yes")).isFalse();
    assertThat(this.converter.convert("y")).isFalse();
    assertThat(this.converter.convert("positive")).isFalse();
    assertThat(this.converter.convert("1")).isFalse();
    assertThat(this.converter.convert("0")).isFalse();
    assertThat(this.converter.convert("negative")).isFalse();
    assertThat(this.converter.convert("n")).isFalse();
    assertThat(this.converter.convert("no")).isFalse();
  }

  @Test
  public void convertSpecifiedStringsReturnsTrue() {

    BooleanConverter converter = new BooleanConverter("yes", "y", "1");

    assertThat(converter.convert(Boolean.TRUE)).isTrue();
    assertThat(converter.convert(true)).isTrue();
    assertThat(converter.convert("YES")).isTrue();
    assertThat(converter.convert("Yes")).isTrue();
    assertThat(converter.convert("yes")).isTrue();
    assertThat(converter.convert("Y")).isTrue();
    assertThat(converter.convert("y")).isTrue();
    assertThat(converter.convert("1")).isTrue();
  }

  @Test
  public void convertNonSpecifiedStringsReturnsFalse() {

    BooleanConverter converter = new BooleanConverter("yes", "y", "1");

    assertThat(converter.convert("yesterday")).isFalse();
    assertThat(converter.convert("yeah")).isFalse();
    assertThat(converter.convert("sey")).isFalse();
    assertThat(converter.convert("si")).isFalse();
    assertThat(converter.convert("one")).isFalse();
    assertThat(converter.convert("-1")).isFalse();
    assertThat(converter.convert("no")).isFalse();
    assertThat(converter.convert("nil")).isFalse();
    assertThat(converter.convert("null")).isFalse();
  }
}
