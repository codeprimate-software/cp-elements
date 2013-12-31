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

import org.cp.elements.util.convert.ConversionException;
import org.junit.Test;

/**
 * The ByteConverterTest class is a test suite of test cases testing the contract and functionality of the ByteConverter
 * class.
 * <p/>
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.util.convert.support.ByteConverter
 * @since 1.0.0
 */
public class ByteConverterTest {

  private final ByteConverter converter = new ByteConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Byte.class, Byte.class));
    assertTrue(converter.canConvert(Number.class, Byte.class));
    assertTrue(converter.canConvert(Integer.class, Byte.class));
    assertTrue(converter.canConvert(Double.class, Byte.class));
    assertTrue(converter.canConvert(BigInteger.class, Byte.class));
    assertTrue(converter.canConvert(BigDecimal.class, Byte.class));
    assertTrue(converter.canConvert(String.class, Byte.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Byte.class));
    assertFalse(converter.canConvert(Byte.class, null));
    assertFalse(converter.canConvert(Byte.TYPE, Byte.class));
    assertFalse(converter.canConvert(Byte.class, Byte.TYPE));
    assertFalse(converter.canConvert(Byte.class, Number.class));
    assertFalse(converter.canConvert(Byte.class, String.class));
    assertFalse(converter.canConvert(Boolean.class, Byte.class));
    assertFalse(converter.canConvert(Character.class, Byte.class));
    assertFalse(converter.canConvert(Date.class, Byte.class));
    assertFalse(converter.canConvert(Object.class, Byte.class));
  }

  @Test
  public void testConvertByte() {
    assertEquals(new Byte((byte) 1), converter.convert((byte) 1));
  }

  @Test
  public void testConvertNegativeNumber() {
    assertEquals(new Byte((byte) -9), converter.convert(-9));
  }

  @Test
  public void testConvertString() {
    assertEquals(new Byte((byte) 64), converter.convert(" 64  "));
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidByte() {
    try {
      converter.convert("two");
    }
    catch (ConversionException e) {
      assertEquals("The Object value (two) is not a valid byte!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = ConversionException.class)
  public void testConvertBoolean() {
    try {
      converter.convert(true);
    }
    catch (ConversionException e) {
      assertEquals("The Object value (true) is not a valid byte!", e.getMessage());
      throw e;
    }
  }

}
