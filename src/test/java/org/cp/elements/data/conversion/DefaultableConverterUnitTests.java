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
package org.cp.elements.data.conversion;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link DefaultableConverter}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.conversion.DefaultableConverter
 * @since 1.0.0
 */
public class DefaultableConverterUnitTests {

  private DefaultableConverter<Object, Object> newConverter() {
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

  @Test
  public void convertWithNonNullValueThrowsConversionException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> newConverter().convert("test"))
      .withMessage("Cannot convert [test] to [java.lang.Object]")
      .withNoCause();
  }

  @Test
  public void convertWithNullValueAndNullDefaultValueThrowsConversionException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> newConverter().convert(null))
      .withMessage("Cannot convert [null] to [java.lang.Object]")
      .withNoCause();
  }

  @Test
  public void setAndGetDefaultValue() {

    DefaultableConverter<Object, Object> converter = newConverter();

    assertThat(converter.getDefaultValue()).isNull();

    converter.setDefaultValue("test");

    assertThat(converter.getDefaultValue()).isEqualTo("test");

    converter.setDefaultValue(null);

    assertThat(converter.getDefaultValue()).isNull();

    converter.setDefaultValue("mock");

    assertThat(converter.getDefaultValue()).isEqualTo("mock");
  }

  @Test
  public void withDefaultValue() {

    DefaultableConverter<Object, Object> converter = newConverter();

    assertThat(converter.getDefaultValue()).isNull();
    assertThat(converter.<DefaultableConverter<Object, Object>>withDefaultValue("test")).isEqualTo(converter);
    assertThat(converter.getDefaultValue()).isEqualTo("test");
    assertThat(converter.<DefaultableConverter<Object, Object>>withDefaultValue(null)).isEqualTo(converter);
    assertThat(converter.getDefaultValue()).isNull();
    assertThat(converter.<DefaultableConverter<Object, Object>>withDefaultValue("mock")).isEqualTo(converter);
    assertThat(converter.getDefaultValue()).isEqualTo("mock");
  }

  static class TestConverter extends DefaultableConverter<Object, Object> { }

}
