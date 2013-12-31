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
 * The BigIntegerConverterTest class is a test suite of test cases testing the contract and functionality of the
 * BigIntegerConverter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.support.BigIntegerConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class BigIntegerConverterTest {

  private BigIntegerConverter converter = new BigIntegerConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(String.class, BigInteger.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(BigInteger.class, BigInteger.class));
    assertFalse(converter.canConvert(null, BigInteger.class));
    assertFalse(converter.canConvert(BigInteger.class, null));
    assertFalse(converter.canConvert(BigInteger.class, String.class));
    assertFalse(converter.canConvert(BigInteger.class, Integer.class));
    assertFalse(converter.canConvert(BigInteger.class, Long.class));
    assertFalse(converter.canConvert(BigInteger.class, Float.class));
    assertFalse(converter.canConvert(BigInteger.class, Double.class));
    assertFalse(converter.canConvert(BigInteger.class, BigDecimal.class));
    assertFalse(converter.canConvert(String.class, String.class));
    assertFalse(converter.canConvert(null, String.class));
    assertFalse(converter.canConvert(String.class, null));
    assertFalse(converter.canConvert(String.class, Integer.class));
    assertFalse(converter.canConvert(String.class, Long.class));
    assertFalse(converter.canConvert(String.class, Float.class));
    assertFalse(converter.canConvert(String.class, Double.class));
    assertFalse(converter.canConvert(String.class, BigDecimal.class));
    assertFalse(converter.canConvert(Character.class, BigInteger.class));
    assertFalse(converter.canConvert(Boolean.class, BigInteger.class));
  }

  @Test
  public void testConvert() {
    String expected = "123";
    BigInteger actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(new BigInteger(expected), actual);
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidBigInteger() {
    try {
      converter.convert("test");
    }
    catch (ConversionException expected) {
      assertEquals("The String value (test) is not a valid BigInteger!", expected.getMessage());
      throw expected;
    }
  }

}
