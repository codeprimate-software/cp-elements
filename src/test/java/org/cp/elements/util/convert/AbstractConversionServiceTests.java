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

package org.cp.elements.util.convert;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.HashSet;
import java.util.Set;

import org.cp.elements.enums.Gender;
import org.cp.elements.enums.Race;
import org.cp.elements.util.convert.support.BooleanConverter;
import org.cp.elements.util.convert.support.CharacterConverter;
import org.cp.elements.util.convert.support.DoubleConverter;
import org.cp.elements.util.convert.support.EnumConverter;
import org.cp.elements.util.convert.support.IntegerConverter;
import org.cp.elements.util.convert.support.StringConverter;
import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link AbstractConversionService} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.convert.AbstractConversionService
 * @since 1.0.0
 */
public class AbstractConversionServiceTests {

  protected static <T> T add(Set<T> set, T element) {
    if (set.add(element)) {
      return element;
    }

    throw new IllegalArgumentException(String.format("Failed to add element [%1$s] to Set [%2$s]", element, set));
  }

  @Test
  public void canConvert() {
    AbstractConversionService conversionService = new TestConversionService();

    assertFalse(conversionService.canConvert(null, String.class));
    assertFalse(conversionService.canConvert("test", String.class));

    conversionService.register(new TestConverter());

    assertFalse(conversionService.canConvert((Object) null, String.class));
    assertTrue(conversionService.canConvert(null, String.class));
    assertTrue(conversionService.canConvert("test", String.class));
    assertFalse(conversionService.canConvert(String.class, Character.class));
  }

  @Test
  public void convert() {
    AbstractConversionService conversionService = new TestConversionService();

    conversionService.register(new TestConverter());

    assertEquals("null", conversionService.convert(null, String.class));
    assertEquals("true", conversionService.convert(true, String.class));
    assertEquals("A", conversionService.convert('A', String.class));
    assertEquals("42", conversionService.convert(42, String.class));
    assertEquals(String.valueOf(Math.PI), conversionService.convert(Math.PI, String.class));
    assertEquals("test", conversionService.convert("test", String.class));
  }

  @Test
  public void convertExact() {
    AbstractConversionService conversionService = new TestConversionService();

    conversionService.register(new EnumConverter());

    assertTrue(conversionService.canConvert(String.class, Enum.class));
    assertTrue(conversionService.canConvert(String.class, Gender.class));
    assertTrue(conversionService.canConvert(String.class, Race.class));

    assertEquals(Gender.FEMALE, conversionService.convert("FEMALE", Gender.class));
    assertEquals(Race.WHITE, conversionService.convert("WHITE", Race.class));
  }

  @Test(expected = ConversionException.class)
  public void convertThrowsConversionException() {
    AbstractConversionService conversionService = new TestConversionService();

    assertFalse(conversionService.canConvert(null, String.class));

    try {
      conversionService.convert(null, String.class);
    }
    catch (ConversionException expected) {
      assertEquals(String.format("Failed to convert value (%1$s) into an object of type (%2$s)!", null,
        String.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertThrowsConversionExceptionForUnsupportedConversion() {
    AbstractConversionService conversionService = new TestConversionService();

    conversionService.register(new TestConverter());

    assertTrue(conversionService.canConvert(null, String.class));

    try {
      conversionService.convert(null, Character.class);
    }
    catch (ConversionException expected) {
      assertEquals(String.format("Failed to convert value (%1$s) into an object of type (%2$s)!", null,
        Character.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void describe() {
    AbstractConversionService conversionService = new TestConversionService();

    Converter converter = new TestConverter();

    AbstractConversionService.ConverterDescriptor descriptor = conversionService.describe(converter);

    assertNotNull(descriptor);
    assertSame(converter, descriptor.getConverter());
    assertEquals(Object.class, descriptor.getFromType());
    assertEquals(String.class, descriptor.getToType());
  }

  @Test
  public void iteration() {
    AbstractConversionService conversionService = new TestConversionService();
    Set<Converter> expectedConverters = new HashSet<>(5);

    conversionService.register(add(expectedConverters, new BooleanConverter()));
    conversionService.register(add(expectedConverters, new CharacterConverter()));
    conversionService.register(add(expectedConverters, new DoubleConverter()));
    conversionService.register(add(expectedConverters, new IntegerConverter()));
    conversionService.register(add(expectedConverters, new StringConverter()));

    assertEquals(5, expectedConverters.size());
    assertEquals(expectedConverters.size(), conversionService.getRegistry().size());

    for (Converter converter : conversionService) {
      assertTrue(expectedConverters.remove(converter));
    }

    assertTrue(expectedConverters.isEmpty());
  }

  @Test
  public void register() {
    AbstractConversionService conversionService = new TestConversionService();
    Converter<Object, String> testConverter = new TestConverter();

    assertTrue(conversionService.getRegistry().isEmpty());

    conversionService.register(testConverter);

    assertFalse(conversionService.getRegistry().isEmpty());
    assertSame(testConverter, conversionService.iterator().next());
  }

  @Test(expected = IllegalArgumentException.class)
  public void registerNull() {
    try {
      new TestConversionService().register(null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals(String.format("The Converter to register with this ConversionService (%1$s) cannot be null!",
        TestConversionService.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void unregister() {
    AbstractConversionService conversionService = new TestConversionService();
    Converter testConverter = new TestConverter();

    conversionService.register(testConverter);

    assertFalse(conversionService.getRegistry().isEmpty());
    assertSame(testConverter, conversionService.iterator().next());

    conversionService.unregister(null);

    assertFalse(conversionService.getRegistry().isEmpty());
    assertSame(testConverter, conversionService.iterator().next());

    conversionService.unregister(mock(Converter.class));

    assertFalse(conversionService.getRegistry().isEmpty());
    assertSame(testConverter, conversionService.iterator().next());

    conversionService.unregister(testConverter);

    assertTrue(conversionService.getRegistry().isEmpty());
  }

  protected static class TestConversionService extends AbstractConversionService {
  }

  protected static class TestConverter extends ConverterAdapter<Object, String> {

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
