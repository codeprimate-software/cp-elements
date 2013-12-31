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

import java.util.Calendar;

import org.junit.Test;

/**
 * The StringConverterTest class is a test suite of test cases testing the contract and functionality of the
 * StringConverter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.support.StringConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class StringConverterTest {

  private final StringConverter converter = new StringConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(null, String.class));
    assertTrue(converter.canConvert(String.class, String.class));
    assertTrue(converter.canConvert(Object.class, String.class));
    assertTrue(converter.canConvert(Number.class, String.class));
    assertTrue(converter.canConvert(Calendar.class, String.class));
    assertTrue(converter.canConvert(Boolean.class, String.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(String.class, null));
    assertFalse(converter.canConvert(String.class, Character.class));
    assertFalse(converter.canConvert(String.class, Object.class));
  }

  @Test
  public void testConvert() {
    assertEquals("null", converter.convert(null));
    assertEquals("true", converter.convert(Boolean.TRUE));
    assertEquals("X", converter.convert('X'));
    assertEquals("42", converter.convert(42));
    assertEquals("3.14159", converter.convert(3.14159d));
    assertEquals("test", converter.convert("test"));
  }

  @Test
  public void testConvertObject() {
    Object obj = new Object();
    assertEquals(obj.toString(), converter.convert(obj));
  }

}
