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
import java.util.Date;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.enums.Gender;
import org.cp.elements.lang.ThrowableAssertions;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link ByteConverter}.
 *
 * @author John J. Blum
 * @see java.lang.Byte
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.conversion.converters.ByteConverter
 * @since 1.0.0
 */
public class ByteConverterTests {

  private final ByteConverter converter = new ByteConverter();

  @Test
  public void canConvertReturnsTrue() {

    assertThat(this.converter.canConvert(BigDecimal.class, Byte.class)).isTrue();
    assertThat(this.converter.canConvert(BigInteger.class, Byte.class)).isTrue();
    assertThat(this.converter.canConvert(Byte.class, Byte.class)).isTrue();
    assertThat(this.converter.canConvert(Double.class, Byte.class)).isTrue();
    assertThat(this.converter.canConvert(Float.class, Byte.class)).isTrue();
    assertThat(this.converter.canConvert(Integer.class, Byte.class)).isTrue();
    assertThat(this.converter.canConvert(Long.class, Byte.class)).isTrue();
    assertThat(this.converter.canConvert(Number.class, Byte.class)).isTrue();
    assertThat(this.converter.canConvert(Short.class, Byte.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Byte.class)).isTrue();
  }

  @Test
  public void canConvertNullToByteIsTrue() {
    assertThat(this.converter.canConvert(null, Byte.class)).isTrue();
  }

  @Test
  public void cannotConvertReturnsFalse() {

    assertThat(this.converter.canConvert(Byte.class, null)).isFalse();
    assertThat(this.converter.canConvert(Byte.TYPE, Byte.class)).isFalse();
    assertThat(this.converter.canConvert(Byte.class, Byte.TYPE)).isFalse();
    assertThat(this.converter.canConvert(Byte.class, Number.class)).isFalse();
    assertThat(this.converter.canConvert(Byte.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, Byte.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, Byte.class)).isFalse();
    assertThat(this.converter.canConvert(Date.class, Byte.class)).isFalse();
    assertThat(this.converter.canConvert(Gender.class, Byte.class)).isFalse();
    assertThat(this.converter.canConvert(Object.class, Byte.class)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void convertByte() {
    assertThat(this.converter.convert(Byte.valueOf((byte) 1))).isEqualTo(Byte.valueOf((byte) 1));
  }

  @Test
  public void convertDoubleToByte() {
    assertThat(this.converter.convert(12.3d)).isEqualTo((byte) 12);
  }

  @Test
  public void convertIntegerToByte() {
    assertThat(this.converter.convert(-9)).isEqualTo(Byte.valueOf((byte) -9));
  }

  @Test
  public void convertPrimitiveByte() {
    assertThat(this.converter.convert((byte) 2)).isEqualTo((byte) 2);
  }

  @Test
  public void convertStringToByte() {
    assertThat(this.converter.convert(" 64  ")).isEqualTo(Byte.valueOf((byte) 64));
  }

  @Test
  public void convertBooleanThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.convert(true))
      .withMessage("Cannot convert [true] to [java.lang.Byte]")
      .withNoCause();
  }

  @Test
  public void convertNamedNumberThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.convert("two"))
      .withMessage("Cannot convert [two] to [java.lang.Byte]")
      .withNoCause();
  }

  @Test
  public void convertNullWithDefaultValueReturnsDefaultValue() {
    assertThat(this.converter.withDefaultValue((byte) 2).convert(null)).isEqualTo((byte) 2);
  }

  @Test
  public void convertNullWithoutDefaultValueThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.convert(null))
      .withMessage("Cannot convert [null] to [java.lang.Byte]")
      .withNoCause();
  }

  @Test
  public void convertOverSizedByteThrowsException() {

    ThrowableAssertions.assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> this.converter.convert("1234"))
      .havingMessage("[1234] is not a valid byte")
      .causedBy(NumberFormatException.class)
      .withNoCause();
  }
}
