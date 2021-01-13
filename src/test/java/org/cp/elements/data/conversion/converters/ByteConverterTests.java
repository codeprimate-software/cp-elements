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
import java.util.Date;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.enums.Gender;
import org.junit.Test;

/**
 * Unit tests for {@link ByteConverter}.
 *
 * @author John J. Blum
 * @see java.lang.Byte
 * @see org.junit.Test
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
    assertThat(this.converter.convert(new Byte((byte) 1))).isEqualTo(new Byte((byte) 1));
  }

  @Test
  public void convertDoubleToByte() {
    assertThat(this.converter.convert(12.3d)).isEqualTo((byte) 12);
  }

  @Test
  public void convertIntegerToByte() {
    assertThat(this.converter.convert(-9)).isEqualTo(new Byte((byte) -9));
  }

  @Test
  public void convertPrimitiveByte() {
    assertThat(this.converter.convert((byte) 2)).isEqualTo((byte) 2);
  }

  @Test
  public void convertStringToByte() {
    assertThat(this.converter.convert(" 64  ")).isEqualTo(new Byte((byte) 64));
  }

  @Test(expected = ConversionException.class)
  public void convertBooleanThrowsException() {

    try {
      this.converter.convert(true);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [true] to [java.lang.Byte]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertNamedNumberThrowsException() {

    try {
      this.converter.convert("two");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [two] to [java.lang.Byte]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertNullWithDefaultValueReturnsDefaultValue() {
    assertThat(this.converter.withDefaultValue((byte) 2).convert(null)).isEqualTo((byte) 2);
  }

  @Test(expected = ConversionException.class)
  public void convertNullWithoutDefaultValueThrowsException() {

    try {
      this.converter.convert(null);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [null] to [java.lang.Byte]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertOverSizedByteThrowsException() {

    try {
      this.converter.convert("1234");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[1234] is not a valid byte");
      assertThat(expected).hasCauseInstanceOf(NumberFormatException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }
}
