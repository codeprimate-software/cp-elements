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

package org.cp.elements.data.conversion.converters;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.cp.elements.data.conversion.ConversionException;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link IntegerConverter}.
 *
 * @author John J. Blum
 * @see java.lang.Integer
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.conversion.converters.IntegerConverter
 * @since 1.0.0
 */
public class IntegerConverterTests {

  private final IntegerConverter converter = new IntegerConverter();

  @Test
  public void canConvertToIntegerReturnsTrue() {

    assertThat(this.converter.canConvert(BigDecimal.class, Integer.class)).isTrue();
    assertThat(this.converter.canConvert(BigInteger.class, Integer.class)).isTrue();
    assertThat(this.converter.canConvert(Byte.class, Integer.class)).isTrue();
    assertThat(this.converter.canConvert(Short.class, Integer.class)).isTrue();
    assertThat(this.converter.canConvert(Integer.class, Integer.class)).isTrue();
    assertThat(this.converter.canConvert(Long.class, Integer.class)).isTrue();
    assertThat(this.converter.canConvert(Float.class, Integer.class)).isTrue();
    assertThat(this.converter.canConvert(Double.class, Integer.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Integer.class)).isTrue();
  }

  @Test
  public void canConvertNullToIntegerReturnsTrue() {
    assertThat(this.converter.canConvert(null, Integer.class)).isTrue();
  }

  @Test
  public void testCannotConvert() {

    assertThat(this.converter.canConvert(Integer.class, null)).isFalse();
    assertThat(this.converter.canConvert(Integer.class, Integer.TYPE)).isFalse();
    assertThat(this.converter.canConvert(Integer.TYPE, Integer.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.class, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.class, BigInteger.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.class, Byte.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.class, Double.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.class, Float.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.class, Long.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.class, Number.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.class, Short.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, Integer.class));
    assertThat(this.converter.canConvert(Character.class, Integer.class));
    assertThat(this.converter.canConvert(Object.class, Integer.class));
  }

  @Test
  public void convertBigDecimalToInteger() {
    assertThat(this.converter.convert(new BigDecimal("123.45"))).isEqualTo(123);
  }

  @Test
  public void convertBigIntegerToInteger() {
    assertThat(this.converter.convert(new BigDecimal("12345"))).isEqualTo(12345);
  }

  @Test
  public void convertByteToInteger() {
    assertThat(this.converter.convert((byte) 64)).isEqualTo(64);
  }

  @Test
  public void convertShortToInteger() {
    assertThat(this.converter.convert((short) 256)).isEqualTo(256);
  }

  @Test
  public void convertIntegerToInteger() {
    assertThat(this.converter.convert(1024000)).isEqualTo(1024000);
  }

  @Test
  public void convertLongToInteger() {
    assertThat(this.converter.convert(1024000000L)).isEqualTo(1024000000);
  }

  @Test
  public void convertFloatToInteger() {
    assertThat(this.converter.convert(1.9f)).isEqualTo(1);
  }

  @Test
  public void convertDoubleToInteger() {
    assertThat(this.converter.convert(3.14159d)).isEqualTo(3);
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidIntegerStringThrowsException() {

    try {
      this.converter.convert("oneTwoThree");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [oneTwoThree] to [java.lang.Integer]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidNumericStringThrowsException() {

    try {
      this.converter.convert("$1OO");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[$1OO] is not a valid Integer");
      assertThat(expected).hasCauseInstanceOf(NumberFormatException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertNullToIntegerWithDefaultValueReturnsDefaultValue() {
    assertThat(this.converter.withDefaultValue(2).convert(null)).isEqualTo(2);
  }

  @Test(expected = ConversionException.class)
  public void convertNullToIntegerWithoutDefaultValueThrowsException() {

    try {
      this.converter.convert(null);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [null] to [java.lang.Integer]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertStringToInteger() {
    assertThat(this.converter.convert("123")).isEqualTo(123);
  }

  @Test
  public void convertNegativeIntegerStringToInteger() {
    assertThat(this.converter.convert("-123")).isEqualTo(-123);
  }
}
