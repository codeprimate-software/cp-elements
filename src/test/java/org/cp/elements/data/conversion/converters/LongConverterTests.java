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
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Date;

import org.cp.elements.data.conversion.ConversionException;
import org.junit.Test;

/**
 * Unit tests for {@link LongConverter}.
 *
 * @author John J. Blum
 * @see java.lang.Long
 * @see org.junit.Test
 * @see org.cp.elements.data.conversion.converters.LongConverter
 * @since 1.0.0
 */
public class LongConverterTests {

  private final LongConverter converter = new LongConverter();

  @Test
  public void canConvertToLongReturnsTrue() {

    assertThat(this.converter.canConvert(BigDecimal.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(BigInteger.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(Byte.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(Double.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(Float.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(Integer.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(Long.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(Number.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(Short.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(Calendar.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(Date.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Long.class)).isTrue();
    assertThat(this.converter.canConvert(Timestamp.class, Long.class)).isTrue();
  }

  @Test
  public void canConvertNullToLongReturnsTrue() {
    assertThat(this.converter.canConvert(null, Long.class)).isTrue();
  }

  @Test
  public void cannotConvertToLongReturnsFalse() {

    assertThat(this.converter.canConvert(Long.class, null)).isFalse();
    assertThat(this.converter.canConvert(Long.class, Long.TYPE)).isFalse();
    assertThat(this.converter.canConvert(Long.TYPE, Long.class)).isFalse();
    assertThat(this.converter.canConvert(Long.class, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(Long.class, BigInteger.class)).isFalse();
    assertThat(this.converter.canConvert(Long.class, Byte.class)).isFalse();
    assertThat(this.converter.canConvert(Long.class, Short.class)).isFalse();
    assertThat(this.converter.canConvert(Long.class, Integer.class)).isFalse();
    assertThat(this.converter.canConvert(Long.class, Number.class)).isFalse();
    assertThat(this.converter.canConvert(Long.class, Float.class)).isFalse();
    assertThat(this.converter.canConvert(Long.class, Double.class)).isFalse();
    assertThat(this.converter.canConvert(Long.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, Number.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, Long.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, Long.class)).isFalse();
    assertThat(this.converter.canConvert(Object.class, Long.class)).isFalse();
  }

  @Test
  public void convertBigDecimalToLong() {
    assertThat(this.converter.convert(new BigDecimal("123.45"))).isEqualTo(123L);
  }

  @Test
  public void convertBigIntegerToLong() {
    assertThat(this.converter.convert(new BigInteger("12345"))).isEqualTo(12345L);
  }

  @Test
  public void convertByteToLong() {
    assertThat(this.converter.convert((byte) 64)).isEqualTo(64L);
  }

  @Test
  public void convertShortToLong() {
    assertThat(this.converter.convert((short) 1024)).isEqualTo(1024L);
  }

  @Test
  public void convertIntegerToLong() {
    assertThat(this.converter.convert(1024000)).isEqualTo(1024000L);
  }

  @Test
  public void convertLongToLong() {
    assertThat(this.converter.convert(Long.MAX_VALUE)).isEqualTo(Long.MAX_VALUE);
  }

  @Test
  public void convertFloatToLong() {
    assertThat(this.converter.convert(1.9f)).isEqualTo(1L);
  }

  @Test
  public void convertDoubleToLong() {
    assertThat(this.converter.convert(Math.PI)).isEqualTo(3L);
  }

  @Test
  public void convertCalendarToLong() {

    Calendar now = Calendar.getInstance();

    assertThat(this.converter.convert(now)).isEqualTo(now.getTimeInMillis());
  }

  @Test
  public void convertDateToLong() {

    Date date = new Date();

    assertThat(this.converter.convert(date)).isEqualTo(date.getTime());
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidNumericValueToLongThrowsException() {

    try {
      this.converter.convert("$123.45");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[$123.45] is not a valid Long");
      assertThat(expected).hasCauseInstanceOf(NumberFormatException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidValueToLongThrowsException() {

    try {
      this.converter.convert("test");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [test] to [java.lang.Long]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertNullToLongWithDefaultValueReturnDefaultValue() {
    assertThat(this.converter.withDefaultValue(2L).convert(null)).isEqualTo(2L);
  }

  @Test(expected = ConversionException.class)
  public void convertNullToLongWithNoDefaultValueThrowsException() {

    try {
      this.converter.withDefaultValue(null).convert(null);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [null] to [java.lang.Long]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertStringToLong() {
    assertThat(this.converter.convert("1123581321345589")).isEqualTo(1123581321345589L);
  }

  @Test
  public void convertNegativeLongStringToLong() {
    assertThat(this.converter.convert("-123456789")).isEqualTo(-123456789L);
  }
}
