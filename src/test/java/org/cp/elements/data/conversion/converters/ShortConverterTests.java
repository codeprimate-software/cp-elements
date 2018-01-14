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
 * Unit tests for {@link ShortConverter}.
 *
 * @author John J. Blum
 * @see java.lang.Short
 * @see org.junit.Test
 * @see org.cp.elements.data.conversion.converters.ShortConverter
 * @since 1.0.0
 */
public class ShortConverterTests {

  private final ShortConverter converter = new ShortConverter();

  @Test
  public void canConvertToShortReturnsTrue() {

    assertThat(this.converter.canConvert(BigDecimal.class, Short.class)).isTrue();
    assertThat(this.converter.canConvert(BigInteger.class, Short.class)).isTrue();
    assertThat(this.converter.canConvert(Byte.class, Short.class)).isTrue();
    assertThat(this.converter.canConvert(Double.class, Short.class)).isTrue();
    assertThat(this.converter.canConvert(Float.class, Short.class)).isTrue();
    assertThat(this.converter.canConvert(Integer.class, Short.class)).isTrue();
    assertThat(this.converter.canConvert(Long.class, Short.class)).isTrue();
    assertThat(this.converter.canConvert(Number.class, Short.class)).isTrue();
    assertThat(this.converter.canConvert(Short.class, Short.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Short.class)).isTrue();
  }

  @Test
  public void canConvertNullToShortReturnsTrue() {
    assertThat(this.converter.canConvert(null, Short.class)).isTrue();
  }

  @Test
  public void cannotConvertToShortReturnsFalse() {

    assertThat(this.converter.canConvert(Short.class, null)).isFalse();
    assertThat(this.converter.canConvert(Short.class, Short.TYPE)).isFalse();
    assertThat(this.converter.canConvert(Short.TYPE, Short.class)).isFalse();
    assertThat(this.converter.canConvert(Short.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(Short.class, Number.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, Short.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, Short.class)).isFalse();
    assertThat(this.converter.canConvert(Object.class, Short.class)).isFalse();
  }

  @Test
  public void convertBigDecimalToShort() {
    assertThat(this.converter.convert(new BigDecimal("3.14159"))).isEqualTo((short) 3);
  }

  @Test
  public void converterBigIntegerToShort() {
    assertThat(this.converter.convert(new BigInteger("42"))).isEqualTo((short) 42);
  }

  @Test
  public void convertByteToShort() {
    assertThat(this.converter.convert((byte) 64)).isEqualTo((short) 64);
  }

  @Test
  public void convertShortToShort() {
    assertThat(this.converter.convert((short) 1024)).isEqualTo((short) 1024);
  }

  @Test
  public void convertIntegerToShort() {
    assertThat(this.converter.convert(42)).isEqualTo((short) 42);
  }

  @Test
  public void convertLongToShort() {
    assertThat(this.converter.convert(16384L)).isEqualTo((short) 16384);
  }

  @Test
  public void convertFloatToShort() {
    assertThat(this.converter.convert(3.14159f)).isEqualTo((short) 3);
  }

  @Test
  public void convertDoubleToShort() {
    assertThat(this.converter.convert(Math.PI)).isEqualTo((short) 3);
  }

  @Test
  public void convertNullToShortWithDefaultValueReturnsDefaultValue() {
    assertThat(this.converter.withDefaultValue((short) 124).convert(null)).isEqualTo((short) 124);
  }

  @Test(expected = ConversionException.class)
  public void convertNullToShortWithNoDefaultValueThrowsException() {

    try {
      this.converter.withDefaultValue(null).convert(null);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [null] to [java.lang.Short]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertOverflowIntegerToShort() {
    assertThat(this.converter.convert(1024000)).isEqualTo((short) 1024000);
  }

  @Test
  public void convertStringToShort() {
    assertThat(this.converter.convert("1248")).isEqualTo((short) 1248);
  }

  @Test(expected = ConversionException.class)
  public void convertFloatingPointNumberStringToShortThrowsException() {

    try {
      this.converter.convert("12.34");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[12.34] is not a valid Short");
      assertThat(expected).hasCauseInstanceOf(NumberFormatException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidNumericStringToShortThrowsException() {

    try {
      this.converter.convert("oneTwoThree");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [oneTwoThree] to [java.lang.Short]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertNegativeShortStringToShort() {
    assertThat(this.converter.convert("-8421")).isEqualTo((short) -8421);
  }
}
