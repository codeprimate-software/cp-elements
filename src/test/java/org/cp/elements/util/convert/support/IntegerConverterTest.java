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
 * The IntegerConverterTest class is a test suite of test cases testing the contract and functionality of the
 * IntegerConverter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.support.IntegerConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class IntegerConverterTest {

  private final IntegerConverter converter = new IntegerConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Integer.class, Integer.class));
    assertTrue(converter.canConvert(Number.class, Integer.class));
    assertTrue(converter.canConvert(Long.class, Integer.class));
    assertTrue(converter.canConvert(Double.class, Integer.class));
    assertTrue(converter.canConvert(BigInteger.class, Integer.class));
    assertTrue(converter.canConvert(BigDecimal.class, Integer.class));
    assertTrue(converter.canConvert(String.class, Integer.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Integer.class));
    assertFalse(converter.canConvert(Integer.class, null));
    assertFalse(converter.canConvert(Integer.class, Integer.TYPE));
    assertFalse(converter.canConvert(Integer.TYPE, Integer.class));
    assertFalse(converter.canConvert(Integer.class, String.class));
    assertFalse(converter.canConvert(Integer.class, Number.class));
    assertFalse(converter.canConvert(Integer.class, Long.class));
    assertFalse(converter.canConvert(Integer.class, Double.class));
    assertFalse(converter.canConvert(Integer.class, BigInteger.class));
    assertFalse(converter.canConvert(Character.class, Integer.class));
    assertFalse(converter.canConvert(Boolean.class, Integer.class));
  }

  @Test
  public void testConvert() {
    Integer expected = 2;
    Integer actual = converter.convert(expected);

    assertNotNull(actual);
    assertSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertBigInteger() {
    BigInteger expected = new BigInteger("123456789");
    Integer actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.intValue(), actual.intValue());
  }

  @Test
  public void testConvertDouble() {
    Double expected = 42.0d;
    Integer actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.intValue(), actual.intValue());
  }

  @Test
  public void testConvertNegativeNumber() {
    Double expected = -42.0d;
    Integer actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.intValue(), actual.intValue());
  }

  @Test
  public void testConvertString() {
    String expected = "123";
    Integer actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotNull(expected, actual);
    assertEquals(Integer.parseInt(expected), actual.intValue());
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidInteger() {
    try {
      converter.convert("one");
    }
    catch (ConversionException expected) {
      assertEquals("The Object value (one) is not a valid integer!", expected.getMessage());
      throw expected;
    }
  }

}
