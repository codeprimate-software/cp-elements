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

import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.convert.ConversionException;
import org.junit.Test;

/**
 * The CharacterConverterTest class is a test suite of test cases testing the contract and functionality of the
 * CharacterConverter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.support.CharacterConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class CharacterConverterTest {

  private CharacterConverter converter = new CharacterConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Character.class, Character.class));
    assertTrue(converter.canConvert(String.class, Character.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Character.class));
    assertFalse(converter.canConvert(Character.class, null));
    assertFalse(converter.canConvert(Character.TYPE, Character.class));
    assertFalse(converter.canConvert(Character.class, Character.TYPE));
    assertFalse(converter.canConvert(Character.class, String.class));
    assertFalse(converter.canConvert(Character.class, Object.class));
    assertFalse(converter.canConvert(Character.class, Number.class));
    assertFalse(converter.canConvert(Object.class, Character.class));
    assertFalse(converter.canConvert(Number.class, Character.class));
  }

  @Test
  public void testConvertCharacter() {
    final Character expected = new Character('A');
    final Character actual = converter.convert(expected);

    assertNotNull(actual);
    assertSame(expected, actual);
  }

  @Test
  public void testConvertChar() {
    final Character actual = converter.convert('0');

    assertNotNull(actual);
    assertEquals('0', actual.charValue());
  }

  @Test
  public void testConvertStringCharacter() {
    final Character actual = converter.convert("x");

    assertNotNull(actual);
    assertEquals('x', actual.charValue());
  }

  @Test
  public void testConvertStringWord() {
    final Character actual = converter.convert("word");

    assertNotNull(actual);
    assertEquals('w', actual.charValue());
  }

  @Test
  public void testConvertEmptyString() {
    final Character actual = converter.convert(StringUtils.EMPTY_STRING);

    assertNotNull(actual);
    assertEquals('\0', actual.charValue());
  }

  @Test
  public void testConvertBlankString() {
    final Character actual = converter.convert(" ");

    assertNotNull(actual);
    assertEquals(String.format("Expected ' '; but was (%1$s)!", actual), new Character(' '), actual);
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidCharacter() {
    try {
      converter.convert(true);
    }
    catch (ConversionException e) {
      assertEquals("The Object value (true) is not a valid character!", e.getMessage());
      throw e;
    }
  }

}
