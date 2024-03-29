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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Date;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.lang.ThrowableAssertions;

/**
 * Unit Tests for {@link NumberConverter}.
 *
 * @author John J. Blum
 * @see java.lang.Number
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.conversion.converters.NumberConverter
 * @since 1.0.0
 */
public class NumberConverterTests {

  private final NumberConverter converter = new NumberConverter();

  @Test
  public void canConvertToNumberReturnsTrue() {

    assertThat(this.converter.canConvert(Number.class, Number.class)).isTrue();
    assertThat(this.converter.canConvert(AtomicInteger.class, Number.class)).isTrue();
    assertThat(this.converter.canConvert(AtomicLong.class, Number.class)).isTrue();
    assertThat(this.converter.canConvert(BigDecimal.class, Number.class)).isTrue();
    assertThat(this.converter.canConvert(BigInteger.class, Number.class)).isTrue();
    assertThat(this.converter.canConvert(Byte.class, Number.class)).isTrue();
    assertThat(this.converter.canConvert(Short.class, Number.class)).isTrue();
    assertThat(this.converter.canConvert(Integer.class, Number.class)).isTrue();
    assertThat(this.converter.canConvert(Long.class, Number.class)).isTrue();
    assertThat(this.converter.canConvert(Float.class, Number.class)).isTrue();
    assertThat(this.converter.canConvert(Double.class, Number.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Number.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, AtomicInteger.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, AtomicLong.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, BigDecimal.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, BigInteger.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Byte.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Short.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Integer.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Float.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(Byte.class, Integer.class)).isTrue();
    assertThat(this.converter.canConvert(Integer.class, Byte.class)).isTrue();
    assertThat(this.converter.canConvert(Integer.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(Long.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(Float.class, Double.class)).isTrue();
    assertThat(this.converter.canConvert(Double.class, Integer.class)).isTrue();
    assertThat(this.converter.canConvert(Double.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(Double.class, Float.class)).isTrue();
  }

  @Test
  public void canConvertNullToNumberReturnsFalse() {
    assertThat(this.converter.canConvert(null, Number.class)).isFalse();
  }

  @Test
  public void cannotConvertToNumberReturnFalse() {

    assertThat(this.converter.canConvert(Number.class, null)).isFalse();
    assertThat(this.converter.canConvert(Number.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.TYPE, Number.class)).isFalse();
    assertThat(this.converter.canConvert(Double.TYPE, Number.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, Integer.TYPE)).isFalse();
    assertThat(this.converter.canConvert(String.class, Double.TYPE)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, Number.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, Number.class)).isFalse();
    assertThat(this.converter.canConvert(Date.class, Number.class)).isFalse();
  }

  @Test
  public void convertDoubleToDouble() {
    assertThat(this.converter.convert(Math.PI, Double.class)).isEqualTo(Math.PI);
  }

  @Test
  public void convertDoubleToFloat() {
    assertThat(this.converter.convert(3.14159d, Float.class)).isEqualTo(3.14159f);
  }

  @Test
  public void convertFloatToInteger() {
    assertThat(this.converter.convert(3.14159f, Integer.class)).isEqualTo(3);
  }

  @Test
  public void convertIntegerToNumberThrowsException() {
    assertThat(this.converter.convert(124, Number.class)).isEqualTo(124);
  }

  @Test
  public void convertIntegerToInteger() {
    assertThat(this.converter.convert(42, Integer.class)).isEqualTo(42);
  }

  @Test
  public void convertIntegerToDouble() {
    assertThat(this.converter.convert(42, Double.class)).isEqualTo(42.0d);
  }

  @Test
  public void convertIntegerToAtomicInteger() {

    Number value = this.converter.convert(42, AtomicInteger.class);

    assertThat(value).isInstanceOf(AtomicInteger.class);
    assertThat(value.intValue()).isEqualTo(42);
  }

  @Test
  public void convertAtomicIntegerToLong() {
    assertThat(this.converter.convert(new AtomicInteger(1024000), Long.class)).isEqualTo(1024000L);
  }

  @Test
  public void convertLongToByte() {
    assertThat(this.converter.convert(64L, Byte.class)).isEqualTo((byte) 64);
  }

  @Test
  public void convertStringToDouble() {
    assertThat(this.converter.convert("3.14159", Double.class)).isEqualTo(3.14159d);
  }

  @Test
  public void convertStringToFloat() {
    assertThat(this.converter.convert("-99.9999", Float.class)).isEqualTo(-99.9999f);
  }

  @Test
  public void convertStringToInteger() {
    assertThat(this.converter.convert("1248", Integer.class)).isEqualTo(1248);
  }

  @Test
  public void convertStringFloatingPointNumberToIntegerThrowsException() {

    ThrowableAssertions.assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> this.converter.convert("9.99", Integer.class))
      .havingMessage("[9.99] is not a valid number of the qualifying type [java.lang.Integer]")
      .causedBy(NumberFormatException.class)
      .withNoCause();
  }

  @Test
  public void convertStringToNumberThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.convert("123456789", Number.class))
      .withMessage("[java.lang.Number] is not a valid Number type")
      .withNoCause();
  }

  @Test
  public void convertCharacterToNumberThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.convert('1', Integer.class))
      .withMessage("[1] is not a valid number of the qualifying type [java.lang.Integer]")
      .withNoCause();
  }

  @Test
  public void convertCurrencyToBigDecimalThrowsException() {

    ThrowableAssertions.assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> this.converter.convert("$100.00", BigDecimal.class))
      .havingMessage("[$100.00] is not a valid number of the qualifying type [java.math.BigDecimal]")
      .causedBy(NumberFormatException.class)
      .withNoCause();
  }

  @Test
  public void convertNullToNumberThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.convert(null, Number.class))
      .withMessage("[null] is not a valid number of the qualifying type [java.lang.Number]")
      .withNoCause();
  }

  @Test
  public void convertWithNullQualifyingTypeThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.converter.convert(1, null))
      .withMessage("Qualifying type is required")
      .withNoCause();
  }

  @Test
  public void fromNumberToUnqualifiedTypeThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.toQualifyingNumber(1, Number.class))
      .withMessage("[java.lang.Number] is not a valid Number type")
      .withNoCause();
  }
}
