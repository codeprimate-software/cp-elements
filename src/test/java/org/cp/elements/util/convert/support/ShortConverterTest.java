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

import org.cp.elements.util.convert.ConversionException;
import org.junit.Test;

/**
 * The ShortConverterTest class is a test suite of test cases testing the contract and functionality of the
 * ShortConverter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.support.ShortConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ShortConverterTest {

  private final ShortConverter converter = new ShortConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Short.class, Short.class));
    assertTrue(converter.canConvert(Number.class, Short.class));
    assertTrue(converter.canConvert(Integer.class, Short.class));
    assertTrue(converter.canConvert(Double.class, Short.class));
    assertTrue(converter.canConvert(String.class, Short.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Short.class));
    assertFalse(converter.canConvert(Short.class, null));
    assertFalse(converter.canConvert(Short.class, Short.TYPE));
    assertFalse(converter.canConvert(Short.TYPE, Short.class));
    assertFalse(converter.canConvert(Short.class, String.class));
    assertFalse(converter.canConvert(Short.class, Number.class));
    assertFalse(converter.canConvert(Character.class, Short.class));
    assertFalse(converter.canConvert(Boolean.class, Short.class));
  }

  @Test
  public void testConvert() {
    Short expected = (short) 1024;
    Short actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertNumber() {
    Integer expected = 8192;
    Short actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.shortValue(), actual.shortValue());
  }

  @Test
  public void testConvertNegativeNumber() {
    Integer expected = -99;
    Short actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.shortValue(), actual.shortValue());
  }

  @Test
  public void testConvertString() {
    String expected = "4096";
    Short actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(Short.parseShort(expected), actual.shortValue());
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidShort() {
    try {
      converter.convert("test");
    }
    catch (ConversionException expected) {
      assertEquals("The Object value (test) is not a valid short!", expected.getMessage());
      throw expected;
    }
  }

}
