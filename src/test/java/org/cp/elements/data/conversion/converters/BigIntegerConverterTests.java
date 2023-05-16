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
 * Unit tests for {@link BigIntegerConverter}.
 *
 * @author John J. Blum
 * @see java.math.BigInteger
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.conversion.converters.BigIntegerConverter
 * @since 1.0.0
 */
public class BigIntegerConverterTests {

  private BigIntegerConverter converter = new BigIntegerConverter();

  @Test
  public void canConvertStringToBigIntegerReturnsTrue() {
    assertThat(this.converter.canConvert(String.class, BigInteger.class)).isTrue();
  }

  @Test
  public void canConvertNullToBigIntegerReturnsFalse() {
    assertThat(this.converter.canConvert(null, BigInteger.class)).isFalse();
  }

  @Test
  public void cannotConvertReturnsFalse() {

    assertThat(this.converter.canConvert(BigDecimal.class, BigInteger.class)).isFalse();
    assertThat(this.converter.canConvert(BigInteger.class, BigInteger.class)).isFalse();
    assertThat(this.converter.canConvert(BigInteger.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(BigInteger.class, null)).isFalse();
    assertThat(this.converter.canConvert(byte[].class, BigInteger.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.class, BigInteger.class)).isFalse();
    assertThat(this.converter.canConvert(Integer.TYPE, BigInteger.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, null)).isFalse();
    assertThat(this.converter.canConvert(String.class, byte[].class)).isFalse();
    assertThat(this.converter.canConvert(String.class, BigDecimal.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, Integer.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, String.class)).isFalse();
  }

  @Test
  public void convertStringToBigIntegerIsSuccessful() {

    String expected = "123";

    BigInteger actual = this.converter.convert(expected);

    assertThat(actual).isNotNull();
    assertThat(actual).isNotEqualTo(expected);
    assertThat(actual).isEqualTo(new BigInteger(expected));
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidValueToBigIntegerThrowsException() {

    try {
      this.converter.convert("test");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[test] is not a valid BigInteger");
      assertThat(expected).hasCauseInstanceOf(NumberFormatException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }
}
