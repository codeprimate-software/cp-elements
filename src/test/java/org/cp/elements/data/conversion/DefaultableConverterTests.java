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

package org.cp.elements.data.conversion;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit tests for {@link DefaultableConverter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.data.conversion.DefaultableConverter
 * @since 1.0.0
 */
public class DefaultableConverterTests {

  protected DefaultableConverter<Object, Object> newConverter() {
    return new TestConverter();
  }

  @Test
  public void canConvertWithNullTypeAndNonNullDefaultValueReturnsTrue() {
    assertThat(newConverter().withDefaultValue("test").canConvert(null, Object.class)).isTrue();
  }

  @Test
  public void canConvertWithNullTypeAndNullDefaultValueReturnsFalse() {
    assertThat(newConverter().canConvert(null, String.class)).isFalse();
  }

  @Test
  public void canConvertWithNonNullTypeAndDefaultValueReturnsFalse() {
    assertThat(newConverter().withDefaultValue("test").canConvert(Object.class, String.class)).isFalse();
  }

  @Test
  public void convertReturnsDefaultValue() {
    assertThat(newConverter().withDefaultValue("test").convert(null)).isEqualTo("test");
  }

  @Test(expected = ConversionException.class)
  public void convertWithNonNullValueThrowsException() {

    try {
      newConverter().convert("test");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [test] to [java.lang.Object]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertWithNullValueAndNullDefaultValueThrowsException() {

    try {
      newConverter().convert(null);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [null] to [java.lang.Object]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void setAndGetDefaultValue() {

    DefaultableConverter<Object, Object> converter = newConverter();

    assertThat(converter.getDefaultValue()).isNull();

    converter.setDefaultValue("test");

    assertThat(converter.getDefaultValue()).isEqualTo("test");

    converter.setDefaultValue(null);

    assertThat(converter.getDefaultValue()).isNull();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void withDefaultValue() {

    DefaultableConverter<Object, Object> converter = newConverter();

    assertThat(converter.getDefaultValue()).isNull();
    assertThat(converter.<DefaultableConverter>withDefaultValue("test")).isEqualTo(converter);
    assertThat(converter.getDefaultValue()).isEqualTo("test");
    assertThat(converter.<DefaultableConverter>withDefaultValue(null)).isEqualTo(converter);
    assertThat(converter.getDefaultValue()).isNull();
  }

  static class TestConverter extends DefaultableConverter<Object, Object> {
  }
}
