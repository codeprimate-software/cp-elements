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
 * The DoubleConverterTest class is a test suite of test cases testing the contract and functionality of the
 * DoubleConverter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.support.DoubleConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class DoubleConverterTest {

  private final DoubleConverter converter = new DoubleConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Double.class, Double.class));
    assertTrue(converter.canConvert(Number.class, Double.class));
    assertTrue(converter.canConvert(Float.class, Double.class));
    assertTrue(converter.canConvert(Integer.class, Double.class));
    assertTrue(converter.canConvert(BigInteger.class, Double.class));
    assertTrue(converter.canConvert(BigDecimal.class, Double.class));
    assertTrue(converter.canConvert(String.class, Double.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Double.class));
    assertFalse(converter.canConvert(Double.class, null));
    assertFalse(converter.canConvert(Double.class, Double.TYPE));
    assertFalse(converter.canConvert(Double.TYPE, Double.class));
    assertFalse(converter.canConvert(Double.class, String.class));
    assertFalse(converter.canConvert(Double.class, Integer.class));
    assertFalse(converter.canConvert(Double.class, Float.class));
    assertFalse(converter.canConvert(Character.class, Double.class));
    assertFalse(converter.canConvert(Boolean.class, Double.class));
    assertFalse(converter.canConvert(Object.class, Double.class));
  }

  @Test
  public void testConvert() {
    Double expected = Math.PI;
    Double actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertNumber() {
    Float expectedValue = 3.14159f;
    Double actual = converter.convert(expectedValue);

    assertNotNull(actual);
    assertNotSame(expectedValue, actual);
    assertEquals(expectedValue.doubleValue(), actual, 0.0d);

    Integer expected = 42;

    actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.doubleValue(), actual, 0.0d);
  }

  @Test
  public void testConvertNegativeNumber() {
    Double expected = -11.23513d;
    Double actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertString() {
    String expected = "3.14159";
    Double actual = converter.convert(expected);

    assertNotNull(expected);
    assertNotSame(expected, actual);
    assertEquals(Double.parseDouble(expected), actual, 0.0d);
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidDouble() {
    try {
      converter.convert("oneTwentyThreePointFortyFive");
    }
    catch (ConversionException e) {
      assertEquals("The Object value (oneTwentyThreePointFortyFive) is not a valid double!", e.getMessage());
      throw e;
    }
  }

}
