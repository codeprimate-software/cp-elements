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
import static org.mockito.Mockito.mock;

import java.net.URL;
import java.sql.Date;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Calendar;

import org.junit.Test;

/**
 * Unit tests for {@link AbstractConverter}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractConverterTests {

  protected AbstractConverter<Object, Object> newConverter() {
    return new TestConverter();
  }

  @Test
  public void setAndGetConversionService() {

    AbstractConverter<?, ?> converter = newConverter();

    ConversionService mockConversionService = mock(ConversionService.class);

    converter.setConversionService(mockConversionService);

    assertThat(converter.getConversionService().orElse(null)).isEqualTo(mockConversionService);
  }

  @Test
  public void getConversionServiceWhenUnsetIsNotPresent() {
    assertThat(newConverter().getConversionService().isPresent()).isFalse();
  }

  @Test
  public void resolveConversionServiceWhenSet() {

    AbstractConverter<?, ?> converter = newConverter();

    ConversionService mockConversionService = mock(ConversionService.class);

    converter.setConversionService(mockConversionService);

    assertThat(converter.resolveConversionService()).isEqualTo(mockConversionService);
  }

  @Test(expected = IllegalStateException.class)
  public void resolveConversionServiceWhenUnsetThrowsException() {

    try {
      newConverter().resolveConversionService();
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("No ConversionService was configured");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void isAssignableToReturnsTrue() {

    AbstractConverter converter = newConverter();

    assertThat(converter.isAssignableTo(Character.class, Short.class, String.class, Object.class)).isTrue();
    assertThat(converter.isAssignableTo(Boolean.class, Boolean.class, Byte.class, Character.class, String.class)).isTrue();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void isAssignableToReturnsFalse() {

    AbstractConverter converter = newConverter();

    assertThat(converter.isAssignableTo(Timestamp.class, Calendar.class, Date.class, LocalDate.class,
      LocalDateTime.class, Long.class, String.class)) .isFalse();

    assertThat(converter.isAssignableTo(Object.class, Boolean.class, Integer.class, String.class)).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void isAssignableToWithEmptyClassTypeArrayReturnsFalse() {

    AbstractConverter converter = newConverter();

    assertThat(converter.isAssignableTo(Object.class)).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void isAssignableToWithNullClassTypeArrayIsNullSafeAndReturnsFalse() {

    AbstractConverter converter = newConverter();

    assertThat(converter.isAssignableTo(Object.class, (Class[]) null)).isFalse();
  }

  @Test
  public void getSourceAndTargetTypesWithObjectToStringConverter() {

    AbstractConverter<Object, String> converter = new ObjectToStringConverter();

    assertThat(converter.getSourceType()).isEqualTo(Object.class);
    assertThat(converter.getTargetType()).isEqualTo(String.class);
  }

  @Test
  public void getSourceAndTargetTypesWithRawTypeConverter() {

    AbstractConverter<?, ?> converter = new RawTypeConverter();

    assertThat(converter.getSourceType()).isEqualTo(Object.class);
    assertThat(converter.getTargetType()).isEqualTo(Object.class);
  }

  @Test
  public void getSourceAndTargetTypesWithStringToUrlConverter() {

    AbstractConverter<?, ?> converter = new StringToUrlConverter();

    assertThat(converter.getSourceType()).isEqualTo(String.class);
    assertThat(converter.getTargetType()).isEqualTo(URL.class);
  }

  @Test
  public void getSourceAndTargetTypesWithTimestampToLocalDateTimeConverter() {

    AbstractConverter<?, ?> converter = new TimestampToLocalDateTimeConverter();

    assertThat(converter.getSourceType()).isEqualTo(Timestamp.class);
    assertThat(converter.getTargetType()).isEqualTo(LocalDateTime.class);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void canConvertThrowsUnsupportedOperationException() {
    newConverter().canConvert(Object.class, String.class);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void convertThrowsUnsupportedOperationException() {
    newConverter().convert("test");
  }

  @Test(expected = UnsupportedOperationException.class)
  public void convertWithQualifyingTypeThrowsUnsupportedOperationException() {
    newConverter().convert(1, Long.class);
  }

  static class ObjectToStringConverter extends AbstractConverter<Object, String> { }

  static class RawTypeConverter extends AbstractConverter { }

  static class StringToUrlConverter extends AbstractConverter<String, URL> { }

  static class TimestampToLocalDateTimeConverter extends AbstractConverter<Timestamp, LocalDateTime> { }

  static class TestConverter extends AbstractConverter<Object, Object> { }

}
