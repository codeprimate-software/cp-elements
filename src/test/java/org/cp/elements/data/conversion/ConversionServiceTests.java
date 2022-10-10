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
import static org.cp.elements.util.ArrayUtils.asIterator;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Iterator;

import org.junit.Test;

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.Registry;

/**
 * Unit Tests for {@link ConversionService}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.conversion.ConversionService
 * @since 1.0.0
 */
public class ConversionServiceTests {

  @Test
  public void getLoaderReturnsSingleInstance() {

    ConversionService.Loader conversionServiceLoader = ConversionService.getLoader();

    assertThat(conversionServiceLoader).isNotNull();
    assertThat(conversionServiceLoader).isSameAs(ConversionService.getLoader());
  }

  @Test
  public void canConvertObjectReturnsTrue() {

    ConversionService mockConversionService = mock(ConversionService.class);

    when(mockConversionService.canConvert(any(Class.class), any(Class.class))).thenReturn(true);
    when(mockConversionService.canConvert(any(Object.class), any(Class.class))).thenCallRealMethod();

    assertThat(mockConversionService.canConvert(new Object(), String.class)).isTrue();

    verify(mockConversionService, times(1)).canConvert(eq(Object.class), eq(String.class));
  }

  @Test
  public void canConverterObjectReturnsFalse() {

    ConversionService mockConversionService = mock(ConversionService.class);

    when(mockConversionService.canConvert(any(Class.class), any(Class.class))).thenReturn(false);
    when(mockConversionService.canConvert(any(Object.class), any(Class.class))).thenCallRealMethod();

    assertThat(mockConversionService.canConvert("test", String.class)).isFalse();

    verify(mockConversionService, times(1)).canConvert(eq(String.class), eq(String.class));
  }

  @Test
  public void canConvertNullObjectIsNullSafeAndReturnsTrue() {

    ConversionService testConversionService = spy(new TestConversionService());

    doReturn(true).when(testConversionService).canConvert(any(), any(Class.class));

    assertThat(testConversionService.canConvert((Object) null, Object.class)).isTrue();

    verify(testConversionService, times(1)).canConvert(isNull(), any(Class.class));
  }

  @Test
  public void canConvertNullObjectIsNullSafeAndReturnsFalse() {

    ConversionService testConversionService = spy(new TestConversionService());

    doReturn(false).when(testConversionService).canConvert(any(), any(Class.class));

    assertThat(testConversionService.canConvert((Object) null, Object.class)).isFalse();

    verify(testConversionService, times(1)).canConvert(isNull(), any(Class.class));
  }

  @Test
  public void canConvertWithTypeReturnsTrue() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    ConversionService mockConversionService = mock(ConversionService.class);

    when(mockConversionService.canConvert(any(Class.class), any(Class.class))).thenCallRealMethod();
    when(mockConversionService.iterator()).thenReturn(asIterator(mockConverter));
    when(mockConverter.canConvert(any(Class.class), any(Class.class))).thenReturn(true);

    assertThat(mockConversionService.canConvert(String.class, Enum.class)).isTrue();

    verify(mockConversionService, times(1)).iterator();
    verify(mockConverter, times(1)).canConvert(eq(String.class), eq(Enum.class));
  }

  @Test
  public void canConvertWithTypeWhenNoConverterCanConvertReturnsFalse() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    ConversionService mockConversionService = mock(ConversionService.class);

    when(mockConversionService.canConvert(any(Class.class), any(Class.class))).thenCallRealMethod();
    when(mockConversionService.iterator()).thenReturn(asIterator(mockConverter));
    when(mockConverter.canConvert(any(Class.class), any(Class.class))).thenReturn(false);

    assertThat(mockConversionService.canConvert(String.class, Enum.class)).isFalse();

    verify(mockConversionService, times(1)).iterator();
    verify(mockConverter, times(1)).canConvert(eq(String.class), eq(Enum.class));
  }

  @Test
  public void canConvertWithTypeWhenNoConvertersAreRegisteredReturnsFalse() {

    ConversionService mockConversionService = mock(ConversionService.class);

    when(mockConversionService.canConvert(any(Class.class), any(Class.class))).thenCallRealMethod();
    when(mockConversionService.iterator()).thenReturn(Collections.emptyIterator());

    assertThat(mockConversionService.canConvert(String.class, Enum.class)).isFalse();

    verify(mockConversionService, times(1)).iterator();
  }

  static class TestConversionService implements ConversionService {

    @Override
    public <T> T convert(Object value, Class<T> toType) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }

    @Override
    public Iterator<Converter<?, ?>> iterator() {
      return Collections.emptyIterator();
    }

    @Override
    public <R extends Registry<Converter<?, ?>>> R register(Converter<?, ?> obj) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }
}
