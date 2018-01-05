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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;

/**
 * Unit tests for {@link Converter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.conversion.Converter
 * @since 1.0.0
 */
public class ConverterTests {

  @Test
  public void canConvertWithObjectAndClassTypeReturnsTrue() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    when(mockConverter.canConvert(any(Class.class), any(Class.class))).thenReturn(true);
    when(mockConverter.canConvert(any(Object.class), any(Class.class))).thenCallRealMethod();

    assertThat(mockConverter.canConvert(new Object(), String.class)).isTrue();

    verify(mockConverter, times(1)).canConvert(eq(Object.class), eq(String.class));
  }

  @Test
  public void canConvertWithObjectAndClassTypeReturnsFalse() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    when(mockConverter.canConvert(any(Class.class), any(Class.class))).thenReturn(false);
    when(mockConverter.canConvert(any(Object.class), any(Class.class))).thenCallRealMethod();

    assertThat(mockConverter.canConvert(new Object(), String.class)).isFalse();

    verify(mockConverter, times(1)).canConvert(eq(Object.class), eq(String.class));
  }

  @Test
  public void canConvertWithNullObjectAndClassTypeReturnsFalse() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    when(mockConverter.canConvert(any(Class.class), any(Class.class))).thenReturn(false);
    when(mockConverter.canConvert(any(Object.class), any(Class.class))).thenCallRealMethod();

    assertThat(mockConverter.canConvert((Object) null, String.class)).isFalse();

    verify(mockConverter, never()).canConvert(eq(Object.class), eq(String.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void convertWithValueAndQualifyTypeCallsConvertValue() {

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

  static class TestObject {

    @Override
    public String toString() {
      return "test";
    }
  }
}
