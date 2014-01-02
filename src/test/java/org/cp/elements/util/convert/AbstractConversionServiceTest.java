/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util.convert;

import static org.junit.Assert.*;

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
import org.jmock.Mockery;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * The AbstractConversionServiceTest class is a test suite of test cases testing the contract and functionality of the
 * AbstractConversionService class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.AbstractConversionService
 * @see org.jmock.Mockery
 * @see org.junit.Test
 */
public class AbstractConversionServiceTest {

  private Mockery mockContext;

  @Before
  public void setup() {
    mockContext = new Mockery();
    mockContext.setImposteriser(ClassImposteriser.INSTANCE);
  }

  @After
  public void tearDown() {
    mockContext.assertIsSatisfied();
    mockContext = null;
  }

  protected static <T> T add(final Set<T> set, T element) {
    if (set.add(element)) {
      return element;
    }

    throw new IllegalArgumentException(String.format("Failed to add element (%1$s) to Set (%2$s)!", element, set));
  }

  @Test
  public void testCanConvert() {
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
  public void testConvert() {
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
  public void testConvertExact() {
    AbstractConversionService conversionService = new TestConversionService();

    conversionService.register(new EnumConverter());

    assertTrue(conversionService.canConvert(String.class, Enum.class));
    assertTrue(conversionService.canConvert(String.class, Gender.class));
    assertTrue(conversionService.canConvert(String.class, Race.class));

    assertEquals(Gender.FEMALE, conversionService.convert("FEMALE", Gender.class));
    assertEquals(Race.WHITE, conversionService.convert("WHITE", Race.class));
  }

  @Test(expected = ConversionException.class)
  public void testConvertThrowsConversionException() {
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
  public void testConvertThrowsConversionExceptionForUnsupportedConversion() {
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
  public void testDescribe() {
    AbstractConversionService conversionService = new TestConversionService();

    Converter converter = new TestConverter();

    AbstractConversionService.ConverterDescriptor descriptor = conversionService.describe(converter);

    assertNotNull(descriptor);
    assertSame(converter, descriptor.getConverter());
    assertEquals(Object.class, descriptor.getFromType());
    assertEquals(String.class, descriptor.getToType());
  }

  @Test
  public void testIteration() {
    AbstractConversionService conversionService = new TestConversionService();
    Set<Converter> expectedConverters = new HashSet<Converter>(5);

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
  public void testRegister() {
    AbstractConversionService conversionService = new TestConversionService();
    Converter<Object, String> testConverter = new TestConverter();

    assertTrue(conversionService.getRegistry().isEmpty());

    conversionService.register(testConverter);

    assertFalse(conversionService.getRegistry().isEmpty());
    assertSame(testConverter, conversionService.iterator().next());
  }

  @Test(expected = NullPointerException.class)
  public void testRegisterNull() {
    try {
      new TestConversionService().register(null);
    }
    catch (NullPointerException expected) {
      assertEquals(String.format("The Converter to register with this ConversionService (%1$s) cannot be null!",
        TestConversionService.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testUnregister() {
    AbstractConversionService conversionService = new TestConversionService();
    Converter testConverter = new TestConverter();

    conversionService.register(testConverter);

    assertFalse(conversionService.getRegistry().isEmpty());
    assertSame(testConverter, conversionService.iterator().next());

    conversionService.unregister(null);

    assertFalse(conversionService.getRegistry().isEmpty());
    assertSame(testConverter, conversionService.iterator().next());

    conversionService.unregister(mockContext.mock(Converter.class));

    assertFalse(conversionService.getRegistry().isEmpty());
    assertSame(testConverter, conversionService.iterator().next());

    conversionService.unregister(testConverter);

    assertTrue(conversionService.getRegistry().isEmpty());
  }

  protected static class TestConversionService extends AbstractConversionService {
  }

  protected static class TestConverter extends ConverterAdapter<Object, String> {

    @Override
    public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
      return String.class.equals(toType);
    }

    @Override
    public String convert(final Object value) {
      return String.valueOf(value);
    }
  }

}
