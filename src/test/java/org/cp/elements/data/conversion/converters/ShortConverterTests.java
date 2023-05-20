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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.lang.ThrowableAssertions;

/**
 * Unit Tests for {@link ShortConverter}.
 *
 * @author John J. Blum
 * @see java.lang.Short
 * @see org.junit.jupiter.api.Test
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

  @Test
  public void convertNullToShortWithNoDefaultValueThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.withDefaultValue(null).convert(null))
      .withMessage("Cannot convert [null] to [java.lang.Short]")
        .withNoCause();
  }

  @Test
  public void convertOverflowIntegerToShort() {
    assertThat(this.converter.convert(1024000)).isEqualTo((short) 1024000);
  }

  @Test
  public void convertStringToShort() {
    assertThat(this.converter.convert("1248")).isEqualTo((short) 1248);
  }

  @Test
  public void convertFloatingPointNumberStringToShortThrowsException() {

    ThrowableAssertions.assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> this.converter.convert("12.34"))
      .havingMessage("[12.34] is not a valid Short")
      .withNoCause();
  }

  @Test
  public void convertInvalidNumericStringToShortThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.convert("oneTwoThree"))
      .withMessage("Cannot convert [oneTwoThree] to [java.lang.Short]")
      .withNoCause();
  }

  @Test
  public void convertNegativeShortStringToShort() {
    assertThat(this.converter.convert("-8421")).isEqualTo((short) -8421);
  }
}
