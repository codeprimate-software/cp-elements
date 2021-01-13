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
import static org.cp.elements.data.conversion.AbstractConverterRegistry.ConverterDescriptor;
import static org.mockito.Mockito.mock;

import java.util.Map;

import org.cp.elements.lang.Constants;
import org.junit.Test;

/**
 * Unit tests for {@link AbstractConverterRegistry}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.data.conversion.AbstractConverterRegistry
 * @since 1.0.0
 */
public class AbstractConverterRegistryTests {

  protected AbstractConverterRegistry newConverterRegistry() {
    return new AbstractConverterRegistry() { };
  }

  @SuppressWarnings("unchecked")
  protected <T extends ConversionService> T newConversionService() {
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

  @Test(expected = IllegalArgumentException.class)
  public void registerNullConverterThrowsException() {

    AbstractConverterRegistry converterRegistry = newConverterRegistry();

    try {
      converterRegistry.register(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Converter is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(converterRegistry).isEmpty();
    }
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

  @Test(expected = IllegalArgumentException.class)
  public void constructConverterDescriptionWithNullConverterThrowsException() {

    try {
      new ConverterDescriptor(null, String.class, Enum.class);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Converter is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructConverterDescriptionWithNullFromTypeThrowsException() {

    try {
      new ConverterDescriptor(mock(Converter.class), null, Enum.class);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("From type is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructConverterDescriptionWithNullToTypeThrowsException() {

    try {
      new ConverterDescriptor(mock(Converter.class), String.class, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("To type is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void describeNullConverterThrowsException() {

    try {
      ConverterDescriptor.describe(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Converter is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void describeNonParameterizedConverter() {

    Converter mockConverter = mock(Converter.class);

    try {
      ConverterDescriptor.describe(mockConverter);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Converter [%s] was not properly parameterized",
        mockConverter.getClass().getName());

      assertThat(expected).hasNoCause();

      throw expected;
    }
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
