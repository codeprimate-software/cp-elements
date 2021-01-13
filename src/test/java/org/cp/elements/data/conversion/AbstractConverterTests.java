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
import static org.mockito.Mockito.mock;

import java.net.URL;
import java.sql.Date;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.function.Function;

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

  private AbstractConverter<Object, Object> newConverter() {
    return new TestConverter();
  }

  private AbstractConverter<Object, Object> newConverter(Class<?> type) {
    return new TestConverter(type);
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
  public void resolveConversionServiceWhenUnsetThrowsIllegalStateException() {

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
    assertThat(converter.isAssignableTo(Boolean.class, Boolean.class, Byte.class, Character.class, String.class,
      Object.class)).isTrue();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void isAssignableToReturnsFalse() {

    AbstractConverter converter = newConverter();

    assertThat(converter.isAssignableTo(Timestamp.class, Calendar.class, Date.class, LocalDate.class,
      LocalDateTime.class, Long.class, String.class)) .isFalse();

    assertThat(converter.isAssignableTo(Object.class, Boolean.class, Double.class, Integer.class,
      String.class)).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void isAssignableToWithEmptyClassTypeArrayReturnsFalse() {
    assertThat(newConverter().isAssignableTo(Object.class)).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void isAssignableToWithNullClassTypeArrayIsNullSafeAndReturnsFalse() {
    assertThat(newConverter().isAssignableTo(Object.class, (Class[]) null)).isFalse();
  }

  @Test
  public void isParameterizedFunctionTypeWithParameterizedConverterReturnsTrue() {

    AbstractConverter<?, ?> converter = newConverter();

    assertThat(converter.isParameterizedFunctionType(ObjectToStringConverter.class.getGenericSuperclass())).isTrue();
    assertThat(converter.isParameterizedFunctionType(StringToUrlConverter.class.getGenericSuperclass())).isTrue();
    assertThat(converter.isParameterizedFunctionType(TestConverter.class.getGenericSuperclass())).isTrue();
    assertThat(converter.isParameterizedFunctionType(TimestampToLocalDateTimeConverter.class.getGenericSuperclass()))
      .isTrue();
  }

  @Test
  public void isParameterizedFunctionTypeWithParameterizedFunctionReturnsTrue() {
    assertThat(newConverter().isParameterizedFunctionType(ObjectToStringFunction.class.getGenericInterfaces()[0]))
      .isTrue();
  }

  @Test
  public void isParameterizedFunctionTypeWithRawTypeReturnsFalse() {

    AbstractConverter<?, ?> converter = newConverter();

    assertThat(converter.isParameterizedFunctionType(ObjectToStringConverter.class)).isFalse();
    assertThat(converter.isParameterizedFunctionType(ObjectToStringFunction.class)).isFalse();
    assertThat(converter.isParameterizedFunctionType(RawTypeConverter.class)).isFalse();
    assertThat(converter.isParameterizedFunctionType(RawTypeConverter.class.getGenericSuperclass())).isFalse();
  }

  @Test
  public void isParameterizedFunctionTypeWithObjectClassReturnsFalse() {
    assertThat(newConverter().isParameterizedFunctionType(Object.class)).isFalse();
  }

  @Test
  public void isParameterizedFunctionTypeWithNullReturnsFalse() {
    assertThat(newConverter().isParameterizedFunctionType(null)).isFalse();
  }

  @Test
  public void getSourceAndTargetTypesWithObjectToStringConverter() {

    AbstractConverter<Object, String> converter = new ObjectToStringConverter();

    assertThat(converter.getSourceType().orElse(null)).isEqualTo(Object.class);
    assertThat(converter.getTargetType().orElse(null)).isEqualTo(String.class);
  }

  @Test
  public void getSourceAndTargetTypesWithObjectToStringFunction() {

    AbstractConverter<Object, Object> function = newConverter(ObjectToStringFunction.class);

    assertThat(function).isNotNull();
    assertThat(function.getSourceType().orElse(null)).isEqualTo(Object.class);
    assertThat(function.getTargetType().orElse(null)).isEqualTo(String.class);
  }

  @Test
  public void getSourceAndTargetTypesWithRawTypeConverter() {

    AbstractConverter<?, ?> converter = new RawTypeConverter();

    assertThat(converter.getSourceType().orElse(null)).isEqualTo(Object.class);
    assertThat(converter.getTargetType().orElse(null)).isEqualTo(Object.class);
  }

  @Test
  public void getSourceAndTargetTypesWithStringToUrlConverter() {

    AbstractConverter<?, ?> converter = new StringToUrlConverter();

    assertThat(converter.getSourceType().orElse(null)).isEqualTo(String.class);
    assertThat(converter.getTargetType().orElse(null)).isEqualTo(URL.class);
  }

  @Test
  public void getSourceAndTargetTypesWithTimestampToLocalDateTimeConverter() {

    AbstractConverter<?, ?> converter = new TimestampToLocalDateTimeConverter();

    assertThat(converter.getSourceType().orElse(null)).isEqualTo(Timestamp.class);
    assertThat(converter.getTargetType().orElse(null)).isEqualTo(LocalDateTime.class);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void canConvertStringToAnythingUsingRawTypeConverterReturnsTrue() {

    RawTypeConverter converter = new RawTypeConverter();

    assertThat(converter.canConvert(Object.class, String.class)).isTrue();
    assertThat(converter.canConvert(String.class, Boolean.class)).isTrue();
    assertThat(converter.canConvert(String.class, Character.class)).isTrue();
    assertThat(converter.canConvert(String.class, Double.class)).isTrue();
    assertThat(converter.canConvert(String.class, Integer.class)).isTrue();
    assertThat(converter.canConvert(String.class, Timestamp.class)).isTrue();
  }

  @Test
  public void canConvertObjectToStringUsingObjectToStringConverterReturnsTrue() {
    assertThat(new ObjectToStringConverter().canConvert(Object.class, String.class)).isTrue();
  }

  @Test
  public void canConvertObjectToStringUsingObjectToStringFunctionReturnsTrue() {
    assertThat(newConverter(ObjectToStringFunction.class).canConvert(Object.class, String.class)).isTrue();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void canConvertObjectToStringUsingRawTypeConverterReturnsTrue() {
    assertThat(new RawTypeConverter().canConvert(Object.class, String.class)).isTrue();
  }

  @Test
  public void canConvertStringToNumberUsingStringToIntegerConverterReturnsTrue() {
    assertThat(new StringToIntegerConverter().canConvert(String.class, Number.class)).isTrue();
  }

  @Test
  public void canConvertStringToIntegerUsingStringToNumberConverterReturnsFalse() {
    assertThat(new StringToNumberConverter().canConvert(String.class, Integer.class)).isFalse();
  }

  @Test
  public void canConverterStringToCharacterUsingObjectToStringConverterReturnsFalse() {
    assertThat(new ObjectToStringConverter().canConvert(String.class, Character.class)).isFalse();
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

  static class ObjectToStringFunction implements Function<Object, String> {

    @Override
    public String apply(Object obj) {
      return String.valueOf(obj);
    }
  }

  static class RawTypeConverter extends AbstractConverter { }

  static class StringToIntegerConverter extends AbstractConverter<String, Integer> { }

  static class StringToNumberConverter extends AbstractConverter<String, Number> { }

  static class StringToUrlConverter extends AbstractConverter<String, URL> { }

  static class TimestampToLocalDateTimeConverter extends AbstractConverter<Timestamp, LocalDateTime> { }

  static class TestConverter extends AbstractConverter<Object, Object> {

    TestConverter() { }

    TestConverter(Class<?> type) {
      super(type);
    }
  }
}
