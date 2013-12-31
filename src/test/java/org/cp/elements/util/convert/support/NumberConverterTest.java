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

package org.cp.elements.util.convert.support;

import static org.junit.Assert.*;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Date;

import org.cp.elements.lang.ClassUtils;
import org.cp.elements.util.convert.ConversionException;
import org.junit.Test;

/**
 * The NumberConverterTest class is a test suite of test cases testing the contract and functionality of the
 * NumberConverter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.support.NumberConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class NumberConverterTest {

  private final NumberConverter converter = new NumberConverter();

  @Test
  public void test() {
  }

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Number.class, Number.class));
    assertTrue(converter.canConvert(Double.class, Number.class));
    assertTrue(converter.canConvert(Integer.class, Number.class));
    assertTrue(converter.canConvert(String.class, Number.class));
    assertTrue(converter.canConvert(String.class, BigDecimal.class));
    assertTrue(converter.canConvert(String.class, BigInteger.class));
    assertTrue(converter.canConvert(String.class, Byte.class));
    assertTrue(converter.canConvert(String.class, Short.class));
    assertTrue(converter.canConvert(String.class, Integer.class));
    assertTrue(converter.canConvert(String.class, Long.class));
    assertTrue(converter.canConvert(String.class, Float.class));
    assertTrue(converter.canConvert(String.class, Double.class));
    assertTrue(converter.canConvert(Byte.class, Integer.class));
    assertTrue(converter.canConvert(Integer.class, Byte.class));
    assertTrue(converter.canConvert(Integer.class, Double.class));
    assertTrue(converter.canConvert(Double.class, Long.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Number.class));
    assertFalse(converter.canConvert(Number.class, null));
    assertFalse(converter.canConvert(Number.class, String.class));
    assertFalse(converter.canConvert(String.class, Integer.TYPE));
    assertFalse(converter.canConvert(String.class, Double.TYPE));
    assertFalse(converter.canConvert(Date.class, Number.class));
    assertFalse(converter.canConvert(Character.class, Number.class));
    assertFalse(converter.canConvert(Boolean.class, Number.class));
  }

  @Test
  public void testConvert() {
    Integer expected = 42;
    Integer actual = converter.convert(expected, Integer.class);

    assertSame(expected, actual);
  }

  @Test
  public void testConvertIntegerToDouble() {
    Integer expected = 42;
    Double actual = converter.convert(expected, Double.class);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.doubleValue(), actual, 0.0d);
  }

  @Test
  public void testConvertByteToLong() {
    Byte expected = 2;
    Long actual = converter.convert(expected, Long.class);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.longValue(), actual.longValue());
  }

  @Test
  public void testConvertLongToByte() {
    Long expected = 64l;
    Byte actual = converter.convert(expected, Byte.class);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.byteValue(), actual.byteValue());
  }

  @Test
  public void testConvertStringToInteger() {
    String expected = "1248";
    Integer actual = converter.convert(expected, Integer.class);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(Integer.parseInt(expected), actual.intValue());
  }

  @Test
  public void testConvertStringToDouble() {
    String expected = "-3.14159";
    Double actual = converter.convert(expected, Double.class);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(Double.parseDouble(expected), actual, 0.0d);
  }

  @Test(expected = ConversionException.class)
  public void testConvertBooleanToNumber() {
    try {
      converter.convert(true, Integer.class);
    }
    catch (ConversionException expected) {
      assertEquals("The Object value (true) is not a valid number of the qualifying type (java.lang.Integer)!",
        expected.getMessage());
      assertNull(expected.getCause());
      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidNumber() {
    try {
      converter.convert("test123...", Integer.class);
    }
    catch (ConversionException expected) {
      assertEquals("The Object value (test123...) is not a valid number of the qualifying type (java.lang.Integer)!",
        expected.getMessage());
      assertTrue(String.format("Expected NumberFormatException; but was (%1$s)!",
        ClassUtils.getClassName(expected.getCause())), expected.getCause() instanceof NumberFormatException);
      throw expected;
    }
  }

}
