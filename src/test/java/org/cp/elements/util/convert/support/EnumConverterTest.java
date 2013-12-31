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

import org.cp.elements.enums.Gender;
import org.cp.elements.enums.Race;
import org.cp.elements.enums.TimeUnit;
import org.cp.elements.util.convert.ConversionException;
import org.junit.Test;

/**
 * The EnumConverterTest class is a test suite of test cases testing the contract and functionality of the
 * EnumConverter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.support.EnumConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class EnumConverterTest {

  private EnumConverter converter = new EnumConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(String.class, Enum.class));
    assertTrue(converter.canConvert(String.class, Gender.class));
    assertTrue(converter.canConvert(String.class, Race.class));
    assertTrue(converter.canConvert(String.class, TimeUnit.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(Enum.class, Enum.class));
    assertFalse(converter.canConvert(null, Enum.class));
    assertFalse(converter.canConvert(Enum.class, null));
    assertFalse(converter.canConvert(Enum.class, String.class));
    assertFalse(converter.canConvert(Enum.class, Race.class));
    assertFalse(converter.canConvert(Enum.class, Gender.class));
    assertFalse(converter.canConvert(Character.class, Enum.class));
    assertFalse(converter.canConvert(Boolean.class, Enum.class));
  }

  @Test
  public void testConvert() {
    Gender gender = converter.convert("FEMALE", Gender.class);

    assertNotNull(gender);
    assertEquals(Gender.FEMALE, gender);
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidEnum() {
    try {
      converter.convert("BLACK", Gender.class);
    }
    catch (ConversionException expected) {
      assertEquals(String.format("The String (BLACK) is not a valid enumerated value of Enum (%1$s)!", Gender.class),
        expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidEnumValue() {
    try {
      converter.convert("IT", Gender.class);
    }
    catch (ConversionException expected) {
      assertEquals(String.format("The String (IT) is not a valid enumerated value of Enum (%1$s)!", Gender.class),
        expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void testConvertWithEnum() {
    try {
      converter.convert("test", Enum.class);
    }
    catch (ConversionException expected) {
      assertEquals(String.format("The String (test) is not a valid enumerated value of Enum (%1$s)!", Enum.class),
        expected.getMessage());
      throw expected;
    }
  }

}
