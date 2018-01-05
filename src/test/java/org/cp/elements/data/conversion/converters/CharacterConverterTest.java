/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.cp.elements.data.conversion.converters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.lang.StringUtils;
import org.junit.Test;

/**
 * The CharacterConverterTest class is a test suite of test cases testing the contract and functionality of the
 * CharacterConverter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.conversion.converters.CharacterConverter
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
