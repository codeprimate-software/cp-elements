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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.function.Function;

import org.cp.elements.lang.Constants;
import org.junit.Test;

/**
 * Unit tests for {@link Converter}.
 *
 * @author John Blum
 * @see java.util.function.Function
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.conversion.Converter
 * @since 1.0.0
 */
public class ConverterTests {

  @Test
  public void canConvertObjectReturnsTrue() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    when(mockConverter.canConvert(any(Class.class), any(Class.class))).thenReturn(true);
    when(mockConverter.canConvert(any(Object.class), any(Class.class))).thenCallRealMethod();

    assertThat(mockConverter.canConvert(new Object(), String.class)).isTrue();

    verify(mockConverter, times(1)).canConvert(eq(Object.class), eq(String.class));
  }

  @Test
  public void canConvertObjectReturnsFalse() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    when(mockConverter.canConvert(any(Class.class), any(Class.class))).thenReturn(false);
    when(mockConverter.canConvert(any(Object.class), any(Class.class))).thenCallRealMethod();

    assertThat(mockConverter.canConvert(new Object(), String.class)).isFalse();

    verify(mockConverter, times(1)).canConvert(eq(Object.class), eq(String.class));
  }

  @Test
  public void canConvertNullObjectIsNullSafeAndReturnsTrue() {

    Converter<?, ?> mockConverter = spy(new TestConverter());

    doReturn(true).when(mockConverter).canConvert(any(), any(Class.class));

    assertThat(mockConverter.canConvert((Object) null, String.class)).isTrue();

    verify(mockConverter, times(1)).canConvert(isNull(), eq(String.class));
  }

  @Test
  public void canConvertNullObjectIsNullSafeAndReturnsFalse() {

    Converter<?, ?> mockConverter = spy(new TestConverter());

    doReturn(false).when(mockConverter).canConvert(any(), any(Class.class));

    assertThat(mockConverter.canConvert((Object) null, String.class)).isFalse();

    verify(mockConverter, times(1)).canConvert(isNull(), eq(String.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void convertWithQualifyingTypeCallsConvertWithUnqualifiedType() {

    Converter<Object, String> mockConverter = mock(Converter.class);

    // NOTE: Mockito has a bug!!! The following does not work...
    // `invocation -> String.valueOf(invocation.getArgument(0))`
    // Throws java.lang.ClassCastException: org.cp.elements.data.conversion.ConverterTests$TestObject cannot be cast to [C
    when(mockConverter.convert(any())).thenAnswer(invocation -> {
      Object argument = invocation.getArgument(0);
      return String.valueOf(argument);
    });

    when(mockConverter.convert(any(), any(Class.class))).thenCallRealMethod();

    assertThat(mockConverter.convert(new TestObject(), String.class)).isEqualTo("test");

    verify(mockConverter, times(1)).convert(isA(TestObject.class));
  }

  @Test(expected = IllegalArgumentException.class)
  @SuppressWarnings("unchecked")
  public void convertWithNullQualifyingTypeThrowsIllegalArgumentException() {

    Converter<Object, ?> mockConverter = mock(Converter.class);

    when(mockConverter.convert(any(), any())).thenCallRealMethod();

    try {
      mockConverter.convert("test", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Qualifying type is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockConverter, never()).convert(any());
    }
  }

  @Test(expected = ConversionException.class)
  @SuppressWarnings("unchecked")
  public void convertHandlesClassCastExceptionByThrowingConversionException() {

    Converter<String, TestObject> mockConverter = mock(Converter.class);

    when(mockConverter.convert(any())).thenAnswer(invocation -> invocation.getArgument(0));
    when(mockConverter.convert(any(), any(Class.class))).thenCallRealMethod();

    try {
      mockConverter.convert("test", TestObject.class);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [test] into an Object of type [%s]",
        TestObject.class.getName());

      assertThat(expected).hasCauseInstanceOf(ClassCastException.class);

      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockConverter, times(1)).convert(eq("test"));
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void functionApplyCallsConverterConvert() {

    Converter<String, Number> mockConverter = mock(Converter.class);

    when(mockConverter.apply(any())).thenCallRealMethod();
    when(mockConverter.convert(any())).thenReturn(1);

    assertThat(mockConverter.apply("one")).isEqualTo(1);

    verify(mockConverter, times(1)).apply(eq("one"));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void functionAndThenIsCorrect() {

    Converter<String, Integer> toIntegerConverter = mock(Converter.class);
    Converter<Number, Double> andThenConverter = mock(Converter.class);

    when(toIntegerConverter.apply(anyString())).thenCallRealMethod();
    when(toIntegerConverter.andThen(any(Function.class))).thenCallRealMethod();

    when(toIntegerConverter.convert(anyString()))
      .thenAnswer(invocation -> Integer.parseInt(invocation.getArgument(0)));

    when(andThenConverter.apply(any(Number.class))).thenCallRealMethod();

    when(andThenConverter.convert(any(Number.class)))
      .thenAnswer(invocation -> invocation.<Number>getArgument(0).doubleValue() * 2.0d);

    assertThat(toIntegerConverter.andThen(andThenConverter).apply("1")).isEqualTo(2.0d);

    verify(toIntegerConverter, times(1)).apply(eq("1"));
    verify(andThenConverter, times(1)).apply(eq(1));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void functionComposeIsCorrect() {

    Converter<String, Integer> beforeConverter = mock(Converter.class);
    Converter<Number, Double> doublingConverter = mock(Converter.class);

    when(beforeConverter.apply(anyString())).thenCallRealMethod();

    when(beforeConverter.convert(anyString()))
      .thenAnswer(invocation -> Integer.parseInt(invocation.getArgument(0)));

    when(doublingConverter.apply(isA(Number.class))).thenCallRealMethod();
    when(doublingConverter.compose(any(Function.class))).thenCallRealMethod();

    when(doublingConverter.convert(any(Integer.class)))
      .thenAnswer(invocation -> invocation.<Integer>getArgument(0).doubleValue() * 2.0d);

    assertThat(doublingConverter.compose(beforeConverter).apply("1")).isEqualTo(2.0d);

    verify(beforeConverter, times(1)).apply(eq("1"));
    verify(doublingConverter, times(1)).apply(eq(1));
  }

  static class TestConverter implements Converter<Object, Object> {

    @Override
    public void setConversionService(ConversionService conversionService) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }

    @Override
    public boolean canConvert(Class<?> fromType, Class<?> toType) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }

    @Override
    public Object convert(Object value) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }

  static class TestObject {

    @Override
    public String toString() {
      return "test";
    }
  }
}
