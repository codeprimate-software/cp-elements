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
import org.junit.Test;

/**
 * Unit tests for {@link FloatConverter}.
 *
 * @author John J. Blum
 * @see java.lang.Float
 * @see org.junit.Test
 * @see org.cp.elements.data.conversion.converters.FloatConverter
 * @since 1.0.0
 */
public class FloatConverterTests {

  private final FloatConverter converter = new FloatConverter();

  @Test
  public void canConvertToFloatReturnsTrue() {

    assertThat(this.converter.canConvert(Float.class, Float.class)).isTrue();
    assertThat(this.converter.canConvert(BigDecimal.class, Float.class)).isTrue();
    assertThat(this.converter.canConvert(BigInteger.class, Float.class)).isTrue();
    assertThat(this.converter.canConvert(Byte.class, Float.class)).isTrue();
    assertThat(this.converter.canConvert(Double.class, Float.class)).isTrue();
    assertThat(this.converter.canConvert(Integer.class, Float.class)).isTrue();
    assertThat(this.converter.canConvert(Long.class, Float.class)).isTrue();
    assertThat(this.converter.canConvert(Number.class, Float.class)).isTrue();
    assertThat(this.converter.canConvert(Short.class, Float.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Float.class)).isTrue();
  }

  @Test
  public void canConvertNullToFloatReturnsTrue() {
    assertThat(this.converter.canConvert(null, Float.class)).isTrue();
  }

  @Test
  public void cannotConvertToFloatReturnsFalse() {

    assertThat(this.converter.canConvert(Float.class, null)).isFalse();
    assertThat(this.converter.canConvert(Float.class, Float.TYPE)).isFalse();
    assertThat(this.converter.canConvert(Float.TYPE, Float.class)).isFalse();
    assertThat(this.converter.canConvert(Float.class, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(Float.class, BigInteger.class)).isFalse();
    assertThat(this.converter.canConvert(Float.class, Byte.class)).isFalse();
    assertThat(this.converter.canConvert(Float.class, Double.class)).isFalse();
    assertThat(this.converter.canConvert(Float.class, Integer.class)).isFalse();
    assertThat(this.converter.canConvert(Float.class, Long.class)).isFalse();
    assertThat(this.converter.canConvert(Float.class, Number.class)).isFalse();
    assertThat(this.converter.canConvert(Float.class, Short.class)).isFalse();
    assertThat(this.converter.canConvert(Float.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, Float.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, Float.class)).isFalse();
    assertThat(this.converter.canConvert(Object.class, Float.class)).isFalse();
  }

  @Test
  public void convertBigDecimalToFloat() {
    assertThat(this.converter.convert(new BigDecimal("3.14159"))).isEqualTo(3.14159f);
  }

  @Test
  public void convertBigIntegerToFloat() {
    assertThat(this.converter.convert(new BigInteger("12345"))).isEqualTo(12345.0f);
  }

  @Test
  public void convertByteToFloat() {
    assertThat(this.converter.convert((byte) 2)).isEqualTo(2.0f);
  }

  @Test
  public void convertShortToFloat() {
    assertThat(this.converter.convert((short) 1024)).isEqualTo(1024.0f);
  }

  @Test
  public void convertIntegerToFloat() {
    assertThat(this.converter.convert(1024000)).isEqualTo(1024000.0f);
  }

  @Test
  public void convertLongToFloat() {
    assertThat(this.converter.convert(1024000000L)).isEqualTo(1024000000.0f);
  }

  @Test
  public void convertFloatToFloat() {
    assertThat(this.converter.convert(3.14159f)).isEqualTo(3.14159f);
  }

  @Test
  public void convertDoubleToFloat() {
    assertThat(this.converter.convert(Math.PI)).isEqualTo((float) Math.PI);
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidFloatStringThrowsException() {

    try {
      this.converter.convert("one.twothree");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [one.twothree] to [java.lang.Float]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidNumericStringThrowsException() {

    try {
      this.converter.convert("$101.23.45");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[$101.23.45] is not a valid float");
      assertThat(expected).hasCauseInstanceOf(NumberFormatException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertNullToFloatWithDefaultValueReturnsDefaultValue() {
    assertThat(this.converter.withDefaultValue(1.23f).convert(null)).isEqualTo(1.23f);
  }

  @Test(expected = ConversionException.class)
  public void convertNullToFloatWithoutDefaultValueThrowsException() {

    try {
      this.converter.convert(null);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [null] to [java.lang.Float]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertStringToFloat() {
    assertThat(this.converter.convert("3.14159")).isEqualTo(3.14159f);
  }

  @Test
  public void convertNegativeFloatStringToFloat() {
    assertThat(this.converter.convert("-123.456")).isEqualTo(-123.456f);
  }
}
