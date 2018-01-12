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
 * Unit tests for {@link BigDecimalConverter}.
 *
 * @author John J. Blum
 * @see java.math.BigDecimal
 * @see org.junit.Test
 * @see org.cp.elements.data.conversion.converters.BigDecimalConverter
 * @since 1.0.0
 */
public class BigDecimalConverterTests {

  private BigDecimalConverter converter = new BigDecimalConverter();

  @Test
  public void canConvertStringToBigDecimalReturnsTrue() {
    assertThat(this.converter.canConvert(String.class, BigDecimal.class)).isTrue();
  }

  @Test
  public void canConvertNullToBigDecimalReturnsFalse() {
    assertThat(this.converter.canConvert(null, BigDecimal.class)).isFalse();
  }

  @Test
  public void cannotConvertReturnsFalse() {

    assertThat(this.converter.canConvert(BigDecimal.class, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(BigDecimal.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(BigDecimal.class, null)).isFalse();
    assertThat(this.converter.canConvert(BigInteger.class, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(char[].class, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(Double.class, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(Double.TYPE, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.class, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.TYPE, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(Long.class, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(Long.TYPE, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, null)).isFalse();
    assertThat(this.converter.canConvert(String.class, char[].class)).isFalse();
    assertThat(this.converter.canConvert(String.class, Double.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, Integer.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, Long.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, BigInteger.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, String.class)).isFalse();
  }

  @Test
  public void convertStringToBigDecimalIsSuccessful() {

    String expected = "123.45";

    BigDecimal actual = this.converter.convert(expected);

    assertThat(actual).isNotNull();
    assertThat(actual).isNotEqualTo(expected);
    assertThat(actual).isEqualTo(new BigDecimal(expected));
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidValueToBigDecimalThrowsException() {

    try {
      this.converter.convert("test");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[test] is not a valid BigDecimal");
      assertThat(expected).hasCauseInstanceOf(NumberFormatException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }
}
