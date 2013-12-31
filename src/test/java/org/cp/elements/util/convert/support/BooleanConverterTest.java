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

import org.junit.Test;

/**
 * The BooleanConverterTest class is a test suite of test cases testing the contract and functionality of the
 * BooleanConverter class.
 * <p/>
 * @author John Blum
 * @see org.cp.elements.util.convert.support.BooleanConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class BooleanConverterTest {

  private final BooleanConverter converter = new BooleanConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Boolean.class, Boolean.class));
    assertTrue(converter.canConvert(Boolean.class, Boolean.TYPE));
    assertTrue(converter.canConvert(Object.class, Boolean.TYPE));
    assertTrue(converter.canConvert(Object.class, Boolean.class));
    assertTrue(converter.canConvert(Integer.class, Boolean.class));
    assertTrue(converter.canConvert(String.class, Boolean.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Boolean.class));
    assertFalse(converter.canConvert(Boolean.class, null));
    assertFalse(converter.canConvert(Boolean.class, Object.class));
    assertFalse(converter.canConvert(Boolean.class, Integer.class));
    assertFalse(converter.canConvert(Boolean.class, String.class));
  }

  @Test
  public void testConvertBoolean() {
    assertTrue(converter.convert(Boolean.TRUE));
    assertTrue(converter.convert(true));
    assertFalse(converter.convert(Boolean.FALSE));
    assertFalse(converter.convert(false));
  }

  @Test
  public void testConvertBooleanStrings() {
    assertTrue(converter.convert("true"));
    assertTrue(converter.convert("True"));
    assertTrue(converter.convert("TRUE"));
    assertFalse(converter.convert("false"));
    assertFalse(converter.convert("False"));
    assertFalse(converter.convert("FALSE"));
  }

  @Test
  public void testConvertString() {
    assertFalse(converter.convert("yes"));
    assertFalse(converter.convert("y"));
    assertFalse(converter.convert("positive"));
    assertFalse(converter.convert("1"));
    assertFalse(converter.convert("0"));
    assertFalse(converter.convert("negative"));
    assertFalse(converter.convert("n"));
    assertFalse(converter.convert("no"));
  }

  @Test
  public void testConvertTrueAndFalseStrings() {
    final BooleanConverter converter = new BooleanConverter("yes", "y", "1");

    assertTrue(converter.convert(Boolean.TRUE));
    assertTrue(converter.convert(true));
    assertTrue(converter.convert("YES"));
    assertTrue(converter.convert("Yes"));
    assertTrue(converter.convert("yes"));
    assertTrue(converter.convert("Y"));
    assertTrue(converter.convert("y"));
    assertTrue(converter.convert("1"));
    assertFalse(converter.convert("yeah"));
    assertFalse(converter.convert("si"));
    assertFalse(converter.convert("why"));
    assertFalse(converter.convert("one"));
    assertFalse(converter.convert("-1"));
    assertFalse(converter.convert("no"));
    assertFalse(converter.convert("null"));
    assertFalse(converter.convert("nil"));
    assertFalse(converter.convert(null));
  }

}
