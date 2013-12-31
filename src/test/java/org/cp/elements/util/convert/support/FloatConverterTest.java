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

import org.cp.elements.util.convert.ConversionException;
import org.junit.Test;

/**
 * The FloatConverterTest class is a test suite of test cases testing the contract and functionality of the
 * FloatConverter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.support.FloatConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class FloatConverterTest {

  private final FloatConverter converter = new FloatConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Float.class, Float.class));
    assertTrue(converter.canConvert(Number.class, Float.class));
    assertTrue(converter.canConvert(Integer.class, Float.class));
    assertTrue(converter.canConvert(Double.class, Float.class));
    assertTrue(converter.canConvert(BigInteger.class, Float.class));
    assertTrue(converter.canConvert(BigDecimal.class, Float.class));
    assertTrue(converter.canConvert(String.class, Float.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Float.class));
    assertFalse(converter.canConvert(Float.class, null));
    assertFalse(converter.canConvert(Float.class, Float.TYPE));
    assertFalse(converter.canConvert(Float.TYPE, Float.class));
    assertFalse(converter.canConvert(Float.class, Number.class));
    assertFalse(converter.canConvert(Float.class, Double.class));
    assertFalse(converter.canConvert(Float.class, Integer.class));
    assertFalse(converter.canConvert(Float.class, String.class));
    assertFalse(converter.canConvert(Character.class, Float.class));
    assertFalse(converter.canConvert(Boolean.class, Float.class));
  }

  @Test
  public void testConvert() {
    Float expected = 3.14159f;
    Float actual = converter.convert(expected);

    assertNotNull(expected);
    assertNotSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertNumber() {
    Double expectedValue = Math.PI;
    Float actual = converter.convert(expectedValue);

    assertNotNull(actual);
    assertNotSame(expectedValue, actual);
    assertEquals(expectedValue.floatValue(), actual, 0.0f);

    Integer expected = 11235;

    actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.floatValue(), actual, 0.0f);
  }

  @Test
  public void testConvertNegativeNumber() {
    Float expected = -11.235813f;
    Float actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertString() {
    String expected = "123.45";
    Float actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(Float.parseFloat(expected), actual, 0.0f);
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidFloat() {
    try {
      converter.convert("one.twothree");
    }
    catch (ConversionException e) {
      assertEquals("The Object value (one.twothree) is not a valid float!", e.getMessage());
      throw e;
    }
  }

}
