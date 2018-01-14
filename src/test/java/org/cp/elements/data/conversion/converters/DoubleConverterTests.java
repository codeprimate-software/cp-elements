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

import java.math.BigDecimal;
import java.math.BigInteger;

import org.cp.elements.data.conversion.ConversionException;
import org.junit.Test;

/**
 * Unit tests for {@link DoubleConverter}.
 *
 * @author John J. Blum
 * @see java.lang.Double
 * @see org.junit.Test
 * @see org.cp.elements.data.conversion.converters.DoubleConverter
 * @since 1.0.0
 */
public class DoubleConverterTests {

  private final DoubleConverter converter = new DoubleConverter();

  @Test
  public void canConvertToDoubleReturnsTrue() {

    assertThat(this.converter.canConvert(Double.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(BigDecimal.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(BigInteger.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(Byte.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(Float.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(Integer.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(Long.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(Number.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(Short.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Double.class)).isTrue();
  }

  @Test
  public void canConvertNullToDoubleReturnsTrue() {
    assertThat(this.converter.canConvert(null, Double.class)).isTrue();
  }

  @Test
  public void cannotConvertToDoubleReturnsFalse() {

    assertThat(converter.canConvert(Double.class, null)).isFalse();
    assertThat(converter.canConvert(Double.class, Double.TYPE)).isFalse();
    assertThat(converter.canConvert(Double.TYPE, Double.class)).isFalse();
    assertThat(converter.canConvert(Double.class, String.class)).isFalse();
    assertThat(converter.canConvert(Double.class, Byte.class)).isFalse();
    assertThat(converter.canConvert(Double.class, Short.class)).isFalse();
    assertThat(converter.canConvert(Double.class, Integer.class)).isFalse();
    assertThat(converter.canConvert(Double.class, Long.class)).isFalse();
    assertThat(converter.canConvert(Double.class, Float.class)).isFalse();
    assertThat(converter.canConvert(Boolean.class, Double.class)).isFalse();
    assertThat(converter.canConvert(Character.class, Double.class)).isFalse();
    assertThat(converter.canConvert(Object.class, Double.class)).isFalse();
  }

  @Test
  public void convertBigDecimalToDouble() {
    assertThat(this.converter.convert(new BigDecimal("3.14"))).isEqualTo(3.14d);
  }

  @Test
  public void convertBigIntegerToDouble() {
    assertThat(this.converter.convert(new BigInteger("42"))).isEqualTo(42.0d);
  }

  @Test
  public void convertByteToDouble() {
    assertThat(this.converter.convert((byte) 16)).isEqualTo(16.0d);
  }

  @Test
  public void convertShortToDouble() {
    assertThat(this.converter.convert((short) 1024)).isEqualTo(1024.0d);
  }

  @Test
  public void convertIntegerToDouble() {
    assertThat(this.converter.convert(1024000)).isEqualTo(1024000.0d);
  }

  @Test
  public void convertLongToDouble() {
    assertThat(this.converter.convert(10240000000L)).isEqualTo(10240000000.0d);
  }

  @Test
  public void convertFloatToDouble() {
    assertThat(Math.round(this.converter.convert(3.14159f) * 100000.0d) / 100000.0d)
      .isEqualTo(3.14159d);
  }

  @Test
  public void convertDoubleToDouble() {
    assertThat(this.converter.convert(Math.PI)).isEqualTo(Math.PI);
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidDoubleStringThrowException() {

    try {
      this.converter.convert("oneTwentyThreePointFortyFive");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [oneTwentyThreePointFortyFive] to [java.lang.Double]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidNumericStringThrowException() {

    try {
      this.converter.convert("$100 & 51/100 cents");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[$100 & 51/100 cents] is not a valid double");
      assertThat(expected).hasCauseInstanceOf(NumberFormatException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertNullToDoubleWithDefaultValueReturnsDefaultValue() {
    assertThat(this.converter.withDefaultValue(1.21d).convert(null)).isEqualTo(1.21d);
  }

  @Test(expected = ConversionException.class)
  public void convertNullToDoubleWithNoDefaultValueThrowsException() {

    try {
      this.converter.convert(null);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [null] to [java.lang.Double]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertStringToDouble() {
    assertThat(this.converter.convert("3.14159")).isEqualTo(3.14159d);
  }

  @Test
  public void convertNegativeDoubleStringToDouble() {
    assertThat(this.converter.convert("-12.34")).isEqualTo(-12.34d);
  }
}
