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
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.conversion.converters.BooleanConverter;
import org.cp.elements.data.conversion.converters.CharacterConverter;
import org.cp.elements.data.conversion.converters.DoubleConverter;
import org.cp.elements.data.conversion.converters.EnumConverter;
import org.cp.elements.data.conversion.converters.IntegerConverter;
import org.cp.elements.data.conversion.converters.NumberConverter;
import org.cp.elements.data.conversion.converters.StringConverter;
import org.cp.elements.enums.Gender;
import org.cp.elements.enums.Race;

/**
 * Unit Tests for {@link AbstractConversionService}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.conversion.AbstractConversionService
 * @since 1.0.0
 */
public class AbstractConversionServiceTests {

  protected static <T> T add(Set<T> set, T element) {

    if (set.add(element)) {
      return element;
    }

    throw newIllegalArgumentException("Could not add element [%1$s] to Set [%2$s]", element, set);
  }

  protected AbstractConversionService newConversionService() {
    return new AbstractConversionService() { };
  }

  @Test
  public void convert() {

    AbstractConversionService conversionService = newConversionService();

    conversionService.register(new ObjectToStringConverter());

    assertThat(conversionService.convert(null, String.class)).isEqualTo("null");
    assertThat(conversionService.convert(true, String.class)).isEqualTo("true");
    assertThat(conversionService.convert('A', String.class)).isEqualTo("A");
    assertThat(conversionService.convert(42, String.class)).isEqualTo("42");
    assertThat(conversionService.convert(Math.PI, String.class)).isEqualTo(String.valueOf(Math.PI));
    assertThat(conversionService.convert("test", String.class)).isEqualTo("test");
  }

  @Test
  public void convertExact() {

    AbstractConversionService conversionService = newConversionService();

    conversionService.register(new GenderConverter());

    assertThat(conversionService.canConvert(Object.class, Gender.class)).isTrue();
    assertThat(conversionService.canConvert(String.class, Gender.class)).isTrue();
    assertThat(conversionService.canConvert(Gender.class, String.class)).isFalse();
    assertThat(conversionService.convert("female", Gender.class)).isEqualTo(Gender.FEMALE);
    assertThat(conversionService.convert("MALE", Gender.class)).isEqualTo(Gender.MALE);
  }

  @Test
  public void convertInexactWithEnums() {

    AbstractConversionService conversionService = newConversionService();

    conversionService.register(new EnumConverter());

    assertThat(conversionService.canConvert(String.class, Enum.class)).isTrue();
    assertThat(conversionService.canConvert(String.class, Gender.class)).isTrue();
    assertThat(conversionService.canConvert(String.class, Race.class)).isTrue();
    assertThat(conversionService.convert("FEMALE", Gender.class)).isEqualTo(Gender.FEMALE);
    assertThat(conversionService.convert("WHITE", Race.class)).isEqualTo(Race.WHITE);
  }

  @Test
  public void convertInexactWithNumbers() {

    AbstractConversionService conversionService = newConversionService();

    conversionService.register(new NumberConverter());

    assertThat(conversionService.canConvert(String.class, Number.class)).isTrue();
    assertThat(conversionService.canConvert(String.class, Integer.class)).isTrue();
    assertThat(conversionService.canConvert(String.class, Long.class)).isTrue();
    assertThat(conversionService.convert("1", Integer.class)).isEqualTo(1);
    assertThat(conversionService.convert("1", Long.class)).isEqualTo(1L);
  }

  @Test
  public void convertThrowsExceptionDuringConversion() {

    AbstractConversionService conversionService = newConversionService();

    assertThat(conversionService.canConvert(Object.class, String.class)).isFalse();

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> conversionService.convert("test", String.class))
      .withMessage("Cannot convert [test] into Object of type [%s]")
      .withNoCause();
  }

  @Test
  public void convertThrowsExceptionForUnsupportedConversion() {

    AbstractConversionService conversionService = newConversionService();

    conversionService.register(new ObjectToStringConverter());

    assertThat(conversionService.canConvert(null, String.class)).isTrue();
    assertThat(conversionService.canConvert(null, Character.class)).isFalse();

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> conversionService.convert("X", Character.class))
      .withMessage("Cannot convert [X] into Object of type [%s]", Character.class.getName())
      .withNoCause();
  }

  @Test
  @SuppressWarnings("rawtypes")
  public void iteration() {

    AbstractConversionService conversionService = newConversionService();

    Set<Converter> expectedConverters = new HashSet<>(5);

    conversionService.register(add(expectedConverters, new BooleanConverter()));
    conversionService.register(add(expectedConverters, new CharacterConverter()));
    conversionService.register(add(expectedConverters, new DoubleConverter()));
    conversionService.register(add(expectedConverters, new IntegerConverter()));
    conversionService.register(add(expectedConverters, new StringConverter()));

    assertThat(expectedConverters.size()).isEqualTo(5);
    assertThat(conversionService.getRegistry().size()).isEqualTo(expectedConverters.size());

    for (Converter converter : conversionService) {
      assertThat(expectedConverters.remove(converter)).isTrue();
    }

    assertThat(expectedConverters.isEmpty()).isTrue();
  }

  static class GenderConverter extends AbstractConverter<String, Gender> {

    @Override
    public boolean canConvert(Class<?> fromType, Class<?> toType) {
      return Gender.class.equals(toType);
    }

    @Override
    public Gender convert(String value) {
      return Gender.valueOf(value.toUpperCase());
    }
  }

  static class ObjectToStringConverter extends AbstractConverter<Object, String> {

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
