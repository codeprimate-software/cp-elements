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
import static org.cp.elements.data.conversion.AbstractConverterRegistry.ConverterDescriptor;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Map;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.annotation.NotNull;

import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link AbstractConverterRegistry}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.conversion.AbstractConverterRegistry
 * @since 1.0.0
 */
public class AbstractConverterRegistryTests {

  private @NotNull AbstractConverterRegistry newConverterRegistry() {
    return new AbstractConverterRegistry() { };
  }

  @SuppressWarnings("unchecked")
  private @NotNull <T extends ConversionService> T newConversionService() {
    return (T) new TestConversionService();
  }

  @Test
  public void getRegistryReturnsNonNullEmptyMap() {

    AbstractConverterRegistry converterRegistry = newConverterRegistry();

    assertThat(converterRegistry.getRegistry()).isInstanceOf(Map.class);
    assertThat(converterRegistry.getRegistry()).isEmpty();
  }

  @Test
  public void registerConverterWithConverterRegistry() {

    TestConverter testConverter = new TestConverter();

    AbstractConverterRegistry converterRegistry = newConverterRegistry();

    assertThat(converterRegistry.<ConverterRegistry>register(testConverter)).isEqualTo(converterRegistry);
    assertThat(converterRegistry.getRegistry()).hasSize(1);
    assertThat(converterRegistry).containsExactlyInAnyOrder(testConverter);
    assertThat(testConverter.getConversionService().orElse(null)).isNull();
  }

  @Test
  public void registerConverterWithConversionService() {

    TestConverter testConverter = new TestConverter();

    TestConversionService testConversionService = newConversionService();

    assertThat(testConversionService.<ConversionService>register(testConverter)).isEqualTo(testConversionService);
    assertThat(testConversionService.getRegistry()).hasSize(1);
    assertThat(testConversionService).containsExactlyInAnyOrder(testConverter);
    assertThat(testConverter.getConversionService().orElse(null)).isEqualTo(testConversionService);
  }

  @Test
  public void registerNullConverterThrowsException() {

    AbstractConverterRegistry converterRegistry = newConverterRegistry();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> converterRegistry.register(null))
      .withMessage("Converter is required")
      .withNoCause();

    assertThat(converterRegistry).isEmpty();
  }

  @Test
  public void unregisterRegisteredConverter() {

    AbstractConverterRegistry converterRegistry = newConverterRegistry();

    ConversionService mockConversionService = mock(ConversionService.class);

    TestConverter testConverter = new TestConverter();

    testConverter.setConversionService(mockConversionService);

    converterRegistry.getRegistry().put(ConverterDescriptor.describe(testConverter), testConverter);

    assertThat(converterRegistry).hasSize(1);
    assertThat(testConverter.getConversionService().orElse(null)).isEqualTo(mockConversionService);
    assertThat(converterRegistry.<ConverterRegistry>unregister(testConverter)).isEqualTo(converterRegistry);
    assertThat(converterRegistry).isEmpty();
    assertThat(testConverter.getConversionService().orElse(null)).isNull();
  }

  @Test
  public void unregisterUnregisteredConverter() {

    AbstractConverterRegistry converterRegistry = newConverterRegistry();

    ConversionService mockConversionService = mock(ConversionService.class);

    TestConverter testConverter = new TestConverter();

    testConverter.setConversionService(mockConversionService);

    assertThat(converterRegistry).isEmpty();
    assertThat(testConverter.getConversionService().orElse(null)).isEqualTo(mockConversionService);
    assertThat(converterRegistry.<ConverterRegistry>unregister(testConverter)).isEqualTo(converterRegistry);
    assertThat(converterRegistry).isEmpty();
    assertThat(testConverter.getConversionService().orElse(null)).isEqualTo(mockConversionService);
  }

  @Test
  public void unregisterNullConverter() {
    newConverterRegistry().unregister(null);
  }

  @Test
  public void constructConverterDescriptor() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    ConverterDescriptor converterDescriptor = new ConverterDescriptor(mockConverter, String.class, Enum.class);

    assertThat(converterDescriptor).isNotNull();
    assertThat(converterDescriptor.getConverter()).isEqualTo(mockConverter);
    assertThat(converterDescriptor.getFromType()).isEqualTo(String.class);
    assertThat(converterDescriptor.getToType()).isEqualTo(Enum.class);
  }

  @Test
  public void constructConverterDescriptorByDescribingConverter() {

    TestConverter testConverter = new TestConverter();

    ConverterDescriptor converterDescriptor = ConverterDescriptor.describe(testConverter);

    assertThat(converterDescriptor).isNotNull();
    assertThat(converterDescriptor.getConverter()).isEqualTo(testConverter);
    assertThat(converterDescriptor.getFromType()).isEqualTo(Object.class);
    assertThat(converterDescriptor.getToType()).isEqualTo(String.class);
  }

  @Test
  public void constructConverterDescriptorWithNullConverterThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ConverterDescriptor(null, String.class, Enum.class))
      .withMessage("Converter is required")
      .withNoCause();
  }

  @Test
  public void constructConverterDescriptorWithNullFromTypeThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ConverterDescriptor(mock(Converter.class), null, Enum.class))
      .withMessage("From type is required")
      .withNoCause();
  }

  @Test
  public void constructConverterDescriptorWithNullToTypeThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ConverterDescriptor(mock(Converter.class), String.class, null))
      .withMessage("To type is required")
      .withNoCause();
  }

  @Test
  public void describeNonParameterizedConverterThrowsIllegalArgumentException() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ConverterDescriptor.describe(mockConverter))
      .withMessage("Converter [%s] was not properly parameterized", mockConverter.getClass().getName())
      .withNoCause();

    verifyNoInteractions(mockConverter);
  }

  @Test
  public void describeNullConverterThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ConverterDescriptor.describe(null))
      .withMessage("Converter is required")
      .withNoCause();
  }

  @Test
  public void isExactConversionReturnsTrue() {

    ConverterDescriptor converterDescriptor =
      new AbstractConverterRegistry.ConverterDescriptor(mock(Converter.class), Object.class, Integer.class);

    assertThat(converterDescriptor.isExactConversion(Integer.class)).isTrue();
  }

  @Test
  public void isExactConversionWithGenericTypeArgumentReturnsFalse() {

    ConverterDescriptor converterDescriptor =
      new AbstractConverterRegistry.ConverterDescriptor(mock(Converter.class), Object.class, Integer.class);

    assertThat(converterDescriptor.isExactConversion(Number.class)).isFalse();
  }

  @Test
  public void isExactConversionWithSpecificTypeArgumentReturnsFalse() {

    ConverterDescriptor converterDescriptor =
      new AbstractConverterRegistry.ConverterDescriptor(mock(Converter.class), Object.class, Number.class);

    assertThat(converterDescriptor.isExactConversion(Integer.class)).isFalse();
    assertThat(converterDescriptor.isExactConversion(Double.class)).isFalse();
  }

  @Test
  public void canConvertFromObjectToTypeDelegatesToConverter() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    doReturn(true).when(mockConverter).canConvert(ArgumentMatchers.<Object>any(), any(Class.class));

    ConverterDescriptor converterDescriptor =
      new AbstractConverterRegistry.ConverterDescriptor(mockConverter, Object.class, Number.class);

    assertThat(converterDescriptor.canConvert("123.45", Double.class)).isTrue();

    verify(mockConverter, times(1)).canConvert(eq("123.45"), eq(Double.class));
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  public void canConvertFromTypeToTypeDelegatesToConverter() {

    Converter<?, ?> mockConverter = mock(Converter.class);

    doReturn(true).when(mockConverter).canConvert(any(Class.class), any(Class.class));

    ConverterDescriptor converterDescriptor =
      new AbstractConverterRegistry.ConverterDescriptor(mockConverter, Object.class, Number.class);

    assertThat(converterDescriptor.canConvert(String.class, Integer.class)).isTrue();

    verify(mockConverter, times(1)).canConvert(eq(String.class), eq(Integer.class));
    verifyNoMoreInteractions(mockConverter);
  }

  @Test
  public void equalConverterDescriptorsAreEqual() {

    ConverterDescriptor converterDescriptorOne =
      new ConverterDescriptor(mock(Converter.class), String.class, Enum.class);

    ConverterDescriptor converterDescriptorTwo =
      new ConverterDescriptor(mock(Converter.class), String.class, Enum.class);

    assertThat(converterDescriptorOne).isNotSameAs(converterDescriptorTwo);
    assertThat(converterDescriptorOne).isEqualTo(converterDescriptorTwo);
  }

  static class TestConversionService extends AbstractConverterRegistry implements ConversionService {

    @Override
    public <T> T convert(Object value, Class<T> toType) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }

  static class TestConverter extends AbstractConverter<Object, String> {

    @Override
    public boolean canConvert(Class<?> fromType, Class<?> toType) {
      return String.class.equals(toType);
    }

    @Override
    public String convert(Object value) {
      return String.valueOf(value);
    }
  }
}
