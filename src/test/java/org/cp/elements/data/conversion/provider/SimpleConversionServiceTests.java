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

package org.cp.elements.data.conversion.provider;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.data.conversion.converters.BigDecimalConverter;
import org.cp.elements.data.conversion.converters.BigIntegerConverter;
import org.cp.elements.data.conversion.converters.BooleanConverter;
import org.cp.elements.data.conversion.converters.ByteConverter;
import org.cp.elements.data.conversion.converters.CalendarConverter;
import org.cp.elements.data.conversion.converters.CharacterConverter;
import org.cp.elements.data.conversion.converters.DoubleConverter;
import org.cp.elements.data.conversion.converters.EnumConverter;
import org.cp.elements.data.conversion.converters.FloatConverter;
import org.cp.elements.data.conversion.converters.IdentifiableConverter;
import org.cp.elements.data.conversion.converters.IntegerConverter;
import org.cp.elements.data.conversion.converters.LongConverter;
import org.cp.elements.data.conversion.converters.NumberConverter;
import org.cp.elements.data.conversion.converters.ShortConverter;
import org.cp.elements.data.conversion.converters.StringConverter;
import org.cp.elements.data.conversion.converters.URIConverter;
import org.cp.elements.data.conversion.converters.URLConverter;
import org.cp.elements.enums.Gender;
import org.cp.elements.enums.Race;
import org.junit.Test;

/**
 * Unit tests for {@link SimpleConversionService}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.conversion.provider.SimpleConversionService
 * @since 1.0.0
 */
public class SimpleConversionServiceTests {

  private final SimpleConversionService conversionService = new SimpleConversionService();

  @Test
  public void testRegisteredSupportConverters() {
    Set<Class> expectedRegisteredSupportConverters = new HashSet<>(17);

    expectedRegisteredSupportConverters.add(BigDecimalConverter.class);
    expectedRegisteredSupportConverters.add(BigIntegerConverter.class);
    expectedRegisteredSupportConverters.add(BooleanConverter.class);
    expectedRegisteredSupportConverters.add(ByteConverter.class);
    expectedRegisteredSupportConverters.add(CalendarConverter.class);
    expectedRegisteredSupportConverters.add(CharacterConverter.class);
    expectedRegisteredSupportConverters.add(DoubleConverter.class);
    expectedRegisteredSupportConverters.add(EnumConverter.class);
    expectedRegisteredSupportConverters.add(FloatConverter.class);
    expectedRegisteredSupportConverters.add(IdentifiableConverter.class);
    expectedRegisteredSupportConverters.add(IntegerConverter.class);
    expectedRegisteredSupportConverters.add(LongConverter.class);
    expectedRegisteredSupportConverters.add(NumberConverter.class);
    expectedRegisteredSupportConverters.add(ShortConverter.class);
    expectedRegisteredSupportConverters.add(StringConverter.class);
    expectedRegisteredSupportConverters.add(URIConverter.class);
    expectedRegisteredSupportConverters.add(URLConverter.class);

    for (Converter converter : conversionService) {
      assertTrue(String.format("Expected the Converter (%1$s) to registered in the SimpleConversionService!",
        converter.getClass().getName()), expectedRegisteredSupportConverters.remove(converter.getClass()));
    }

    assertTrue(String.format("Expected the registered, support Converters Set to be empty; but was (%1$s)!",
      expectedRegisteredSupportConverters), expectedRegisteredSupportConverters.isEmpty());
  }

  @Test
  public void testDefaultValuesInitialized() {
    assertFalse(conversionService.isDefaultValuesEnabled());
    assertEquals(new BigDecimal(0.0d), conversionService.getDefaultValue(BigDecimal.class));
    assertEquals(new BigInteger("0"), conversionService.getDefaultValue(BigInteger.class));
    assertEquals(Boolean.FALSE, conversionService.getDefaultValue(Boolean.class));
    assertNotNull(conversionService.getDefaultValue(Calendar.class));
    assertEquals(new Character('\0'), conversionService.getDefaultValue(Character.class));
    assertEquals(new Byte((byte) 0), conversionService.getDefaultValue(Byte.class));
    assertEquals(new Double(0.0d), conversionService.getDefaultValue(Double.class));
    assertEquals(new Float(0.0f), conversionService.getDefaultValue(Float.class));
    assertEquals(new Integer(0), conversionService.getDefaultValue(Integer.class));
    assertEquals(new Long(0l), conversionService.getDefaultValue(Long.class));
    assertNull(conversionService.getDefaultValue(Number.class));
    assertEquals(new Short((short) 0), conversionService.getDefaultValue(Short.class));
    assertNull(conversionService.getDefaultValue(String.class));
    assertFalse(conversionService.isDefaultValuesEnabled());
  }

  @Test
  public void testSetGetAndUnsetDefaultValue() {
    assertNull(conversionService.getDefaultValue(String.class));

    conversionService.setDefaultValue(String.class, "nil");

    assertEquals("nil", conversionService.getDefaultValue(String.class));

    conversionService.setDefaultValue(String.class, (String) null);

    assertNull(conversionService.getDefaultValue(String.class));
    assertNull(conversionService.getDefaultValue(Gender.class));

    conversionService.setDefaultValue(Gender.class, Gender.FEMALE);

    assertEquals(Gender.FEMALE, conversionService.getDefaultValue(Gender.class));
    assertNull(conversionService.getDefaultValue(Race.class));

    conversionService.unsetDefaultValue(Gender.class);
    conversionService.unsetDefaultValue(Race.class);

    assertNull(conversionService.getDefaultValue(Gender.class));
    assertNull(conversionService.getDefaultValue(Race.class));
  }

  @Test
  public void testSetAndGetDefaultValue() {
    conversionService.setDefaultValue(Number.class, new SimpleConversionService.ValueGenerator<Number>() {
      private final AtomicLong value = new AtomicLong();
      @Override public Number generateValue() {
        return value.getAndIncrement();
      }
    });

    assertEquals(0l, conversionService.getDefaultValue(Number.class));
    assertEquals(1l, conversionService.getDefaultValue(Number.class));
    assertEquals(2l, conversionService.getDefaultValue(Number.class));
    assertEquals(3l, conversionService.getDefaultValue(Number.class));

    conversionService.setDefaultValue(Number.class, (Number) null);

    assertNull(conversionService.getDefaultValue(Number.class));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetDefaultValueWithNullType() {
    try {
      conversionService.setDefaultValue(null, "test");
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The Class type to set the default value for cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testSetDefaultValueWithNullValue() {
    assertEquals(new Integer(0), conversionService.getDefaultValue(Integer.class));

    conversionService.setDefaultValue(Integer.class, (Integer) null);

    assertNull(conversionService.getDefaultValue(Integer.class));

    conversionService.setDefaultValue(Integer.class, 0);

    assertEquals(new Integer(0), conversionService.getDefaultValue(Integer.class));
  }

  @Test
  public void testDefaultValuesEnabled() {
    assertFalse(conversionService.isDefaultValuesEnabled());

    conversionService.setDefaultValuesEnabled(true);

    assertTrue(conversionService.isDefaultValuesEnabled());

    conversionService.setDefaultValuesEnabled(false);

    assertFalse(conversionService.isDefaultValuesEnabled());
  }

  @Test
  public void testUseDefault() {
    assertFalse(conversionService.isDefaultValuesEnabled());
    assertFalse(conversionService.useDefault("test", String.class));
    assertFalse(conversionService.useDefault(null, Gender.class));
    assertFalse(conversionService.useDefault(null, Race.class));
    assertFalse(conversionService.useDefault(null, String.class));

    conversionService.setDefaultValuesEnabled(true);

    assertTrue(conversionService.isDefaultValuesEnabled());
    assertFalse(conversionService.useDefault("null", String.class));
    assertFalse(conversionService.useDefault(null, Gender.class));
    assertFalse(conversionService.useDefault(null, Race.class));
    assertTrue(conversionService.useDefault(null, Boolean.class));
    assertTrue(conversionService.useDefault(null, Double.class));
    assertTrue(conversionService.useDefault(null, Integer.class));
    assertTrue(conversionService.useDefault(null, Number.class));
    assertTrue(conversionService.useDefault(null, String.class));

    conversionService.setDefaultValue(Gender.class, Gender.FEMALE);

    assertTrue(conversionService.useDefault(null, Gender.class));
    assertFalse(conversionService.useDefault("MALE", Gender.class));
    assertFalse(conversionService.useDefault(null, Race.class));

    conversionService.setDefaultValuesEnabled(false);
    conversionService.unsetDefaultValue(Gender.class);

    assertFalse(conversionService.isDefaultValuesEnabled());
    assertNull(conversionService.getDefaultValue(Gender.class));
  }

  @Test
  public void testConvert() {
    assertEquals(true, conversionService.convert("true", Boolean.class));
    assertEquals(new Character('X'), conversionService.convert("X", Character.class));
    assertEquals(new Integer(1), conversionService.convert("1", Integer.class));
    assertEquals(new Double(Math.PI), conversionService.convert(String.valueOf(Math.PI), Double.class));
    assertEquals("test", conversionService.convert("test", String.class));
    assertEquals("nil", conversionService.convert("nil", String.class));
    assertEquals("null", conversionService.convert("null", String.class));
    assertEquals("null", conversionService.convert(null, String.class));
  }

  @Test(expected = ConversionException.class)
  public void testConvertWithDefaultsDisabled() {
    try {
      assertFalse(conversionService.isDefaultValuesEnabled());
      conversionService.convert(null, Integer.class);
    }
    catch (ConversionException expected) {
      assertEquals(String.format("Failed to convert value (%1$s) into an object of type (%2$s)!", null,
        Integer.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testConvertWithDefaultsEnabled() {
    conversionService.setDefaultValuesEnabled(true);

    assertTrue(conversionService.isDefaultValuesEnabled());
    assertEquals(Boolean.FALSE, conversionService.convert(null, Boolean.class));
    assertEquals(new Character('\0'), conversionService.convert(null, Character.class));
    assertEquals(new Integer(0), conversionService.convert(null, Integer.class));
    assertEquals(new Double(0.0d), conversionService.convert(null, Double.class));
    assertNull(conversionService.convert(null, String.class));
  }
}
