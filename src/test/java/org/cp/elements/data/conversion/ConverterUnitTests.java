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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.function.Function;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Constants;

import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link Converter}.
 *
 * @author John Blum
 * @see java.util.function.Function
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.conversion.Converter
 * @since 1.0.0
 */
public class ConverterUnitTests {

  @Test
  public void canConvertFromClassTypeToAssignableClassType() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    doCallRealMethod().when(mockConverter).canConvert(any(Class.class), any(Class.class));

    assertThat(mockConverter.canConvert(Object.class, Object.class)).isTrue();
    assertThat(mockConverter.canConvert(String.class, Object.class)).isTrue();
    assertThat(mockConverter.canConvert(Integer.class, Number.class)).isTrue();
    assertThat(mockConverter.canConvert(Double .class, Number.class)).isTrue();
    assertThat(mockConverter.canConvert(java.sql.Date.class, java.util.Date.class)).isTrue();

    verify(mockConverter, times(5)).canConvert(isA(Class.class), isA(Class.class));
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  public void canConvertFromClassTypeToNonAssignableClassType() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    doCallRealMethod().when(mockConverter).canConvert(any(Class.class), any(Class.class));

    assertThat(mockConverter.canConvert(Object.class, String.class)).isFalse();
    assertThat(mockConverter.canConvert(Character.class, String.class)).isFalse();
    assertThat(mockConverter.canConvert(Float.class, Double.class)).isFalse();
    assertThat(mockConverter.canConvert(Integer.class, Long.class)).isFalse();
    assertThat(mockConverter.canConvert(java.time.ZonedDateTime.class, java.util.Date.class)).isFalse();

    verify(mockConverter, times(5)).canConvert(isA(Class.class), isA(Class.class));
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  public void canConvertFromClassTypeToNullClassTypeReturnsFalse() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    doCallRealMethod().when(mockConverter).canConvert(any(Class.class), any());

    assertThat(mockConverter.canConvert(Object.class, null)).isFalse();

    verify(mockConverter, times(1)).canConvert(eq(Object.class), isNull());
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  public void canConvertFromNullClassTypeToClassTypeReturnsTrue() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    doCallRealMethod().when(mockConverter).canConvert(any(), any(Class.class));

    assertThat(mockConverter.canConvert(null, java.util.Date.class)).isTrue();

    verify(mockConverter, times(1)).canConvert(isNull(), eq(java.util.Date.class));
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  public void canConvertObjectReturnsTrue() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    doReturn(true).when(mockConverter).canConvert(any(Class.class), any(Class.class));
    doCallRealMethod().when(mockConverter).canConvert(any(Object.class), any(Class.class));

    Object target = new Object();

    assertThat(mockConverter.canConvert(target, String.class)).isTrue();

    verify(mockConverter, times(1)).canConvert(eq(target), eq(String.class));
    verify(mockConverter, times(1)).canConvert(eq(Object.class), eq(String.class));
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  public void canConvertObjectReturnsFalse() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    doReturn(false).when(mockConverter).canConvert(any(Class.class), any(Class.class));
    doCallRealMethod().when(mockConverter).canConvert(any(Object.class), any(Class.class));

    Object target = new Object();

    assertThat(mockConverter.canConvert(target, String.class)).isFalse();

    verify(mockConverter, times(1)).canConvert(eq(target), eq(String.class));
    verify(mockConverter, times(1)).canConvert(eq(Object.class), eq(String.class));
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  public void canConvertNullObjectIsNullSafeAndReturnsTrue() {

    Converter<?, ?> mockConverter = spy(new TestConverter());

    doReturn(true).when(mockConverter).canConvert(any(), any(Class.class));
    doCallRealMethod().when(mockConverter).canConvert(any(Object.class), any(Class.class));

    assertThat(mockConverter.canConvert((Object) null, String.class)).isTrue();

    verify(mockConverter, times(1))
      .canConvert(ArgumentMatchers.<Object>eq(null), eq(String.class));
    verify(mockConverter, times(1)).canConvert(isNull(), eq(String.class));
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  public void canConvertNullObjectIsNullSafeAndReturnsFalse() {

    Converter<?, ?> mockConverter = spy(new TestConverter());

    doReturn(false).when(mockConverter).canConvert(any(), any(Class.class));
    doCallRealMethod().when(mockConverter).canConvert(any(Object.class), any(Class.class));

    assertThat(mockConverter.canConvert((Object) null, String.class)).isFalse();

    verify(mockConverter, times(1))
      .canConvert(ArgumentMatchers.<Object>eq(null), eq(String.class));
    verify(mockConverter, times(1)).canConvert(isNull(), eq(String.class));
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void convertWithQualifyingTypeCallsConvertWithUnqualifiedType() {

    Converter<Object, String> mockConverter = mock(Converter.class);

    // NOTE: Mockito has a bug!!! The following does not work...
    // `invocation -> String.valueOf(invocation.getArgument(0))`
    // Throws java.lang.ClassCastException: org.cp.elements.data.conversion.ConverterUnitTests$TestObject cannot be cast to [C
    doAnswer(invocation -> {
      Object argument = invocation.getArgument(0);
      return String.valueOf(argument);
    }).when(mockConverter).convert(any());

    doCallRealMethod().when(mockConverter).convert(any(), any(Class.class));

    assertThat(mockConverter.convert(new TestObject(), String.class)).isEqualTo("test");

    verify(mockConverter, times(1)).convert(isA(TestObject.class), eq(String.class));
    verify(mockConverter, times(1)).convert(isA(TestObject.class));
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void convertWithNullQualifyingTypeThrowsIllegalArgumentException() {

    Converter<Object, ?> mockConverter = mock(Converter.class);

    doCallRealMethod().when(mockConverter).convert(any(), any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockConverter.convert("test", null))
      .withMessage("Qualifying type is required")
      .withNoCause();

    verify(mockConverter, times(1)).convert(eq("test"), isNull());
    verify(mockConverter, never()).convert(any());
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void convertHandlesClassCastExceptionByThrowingConversionException() {

    Converter<String, TestObject> mockConverter = mock(Converter.class);

    doAnswer(invocation -> invocation.getArgument(0)).when(mockConverter).convert(any());
    doCallRealMethod().when(mockConverter).convert(any(), any(Class.class));

    assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> mockConverter.convert("test", TestObject.class))
      .havingMessage("Cannot convert value [test] into an Object of type [%s]", TestObject.class.getName())
      .causedBy(ClassCastException.class)
      .withNoCause();

    verify(mockConverter, times(1)).convert(eq("test"), eq(TestObject.class));
    verify(mockConverter, times(1)).convert(eq("test"));
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void functionApplyCallsConverterConvert() {

    Converter<String, Number> mockConverter = mock(Converter.class);

    doCallRealMethod().when(mockConverter).apply(any());
    doReturn(1).when(mockConverter).convert(any());

    assertThat(mockConverter.apply("one")).isEqualTo(1);

    verify(mockConverter, times(1)).apply(eq("one"));
    verify(mockConverter, times(1)).convert(eq("one"));
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void functionAndThenIsCorrect() {

    Converter<String, Integer> toIntegerConverter = mock(Converter.class);
    Converter<Number, Double> andThenConverter = mock(Converter.class);

    doCallRealMethod().when(toIntegerConverter).apply(anyString());
    doCallRealMethod().when(toIntegerConverter).andThen(any(Function.class));

    doAnswer(invocation -> Integer.parseInt(invocation.getArgument(0)))
      .when(toIntegerConverter).convert(anyString());

    doCallRealMethod().when(andThenConverter).apply(any(Number.class));

    doAnswer(invocation -> invocation.<Number>getArgument(0).doubleValue() * 2.0d)
      .when(andThenConverter).convert(any(Number.class));

    assertThat(toIntegerConverter.andThen(andThenConverter).apply("2")).isEqualTo(4.0d);

    verify(toIntegerConverter, times(1)).andThen(eq(andThenConverter));
    verify(toIntegerConverter, times(1)).apply(eq("2"));
    verify(toIntegerConverter, times(1)).convert(eq("2"));
    verify(andThenConverter, times(1)).apply(eq(2));
    verify(andThenConverter, times(1)).convert(eq(2));
    verifyNoMoreInteractions(toIntegerConverter, andThenConverter);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void functionComposeIsCorrect() {

    Converter<String, Integer> beforeConverter = mock(Converter.class);
    Converter<Number, Double> powerOfTwoConverter = mock(Converter.class);

    doCallRealMethod().when(beforeConverter).apply(anyString());

    doAnswer(invocation -> Integer.parseInt(invocation.getArgument(0)))
      .when(beforeConverter).convert(anyString());

    doCallRealMethod().when(powerOfTwoConverter).apply(isA(Number.class));
    doCallRealMethod().when(powerOfTwoConverter).compose(any(Function.class));

    doAnswer(invocation -> Math.pow(invocation.<Integer>getArgument(0).doubleValue(), 2.0d))
      .when(powerOfTwoConverter).convert(any(Integer.class));

    assertThat(powerOfTwoConverter.compose(beforeConverter).apply("3")).isEqualTo(9.0d);

    verify(powerOfTwoConverter, times(1)).compose(eq(beforeConverter));
    verify(powerOfTwoConverter, times(1)).apply(eq(3));
    verify(powerOfTwoConverter, times(1)).convert(eq(3));
    verify(beforeConverter, times(1)).apply(eq("3"));
    verify(beforeConverter, times(1)).convert(eq("3"));
    verifyNoMoreInteractions(beforeConverter, powerOfTwoConverter);
  }

  static class TestConverter implements Converter<Object, Object> {

    @Override
    public void setConversionService(ConversionService conversionService) {
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
