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

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

import org.cp.elements.enums.Gender;
import org.cp.elements.enums.Race;
import org.cp.elements.enums.TimeUnit;
import org.cp.elements.util.convert.ConversionException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The EnumConverterTest class is a test suite of test cases testing the contract and functionality of the
 * EnumConverter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.convert.support.EnumConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class EnumConverterTest {

  private EnumConverter converter = new EnumConverter();

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  @Test
  public void canConvert() {
    assertThat(converter.canConvert(String.class, Enum.class), is(true));
    assertThat(converter.canConvert(String.class, Gender.class), is(true));
    assertThat(converter.canConvert(String.class, Race.class), is(true));
    assertThat(converter.canConvert(String.class, TimeUnit.class), is(true));
  }

  @Test
  public void cannotConvert() {
    assertThat(converter.canConvert(Enum.class, Enum.class), is(false));
    assertThat(converter.canConvert(null, Enum.class), is(false));
    assertThat(converter.canConvert(Enum.class, null), is(false));
    assertThat(converter.canConvert(Enum.class, String.class), is(false));
    assertThat(converter.canConvert(Enum.class, Race.class), is(false));
    assertThat(converter.canConvert(Enum.class, Gender.class), is(false));
    assertThat(converter.canConvert(Character.class, Enum.class), is(false));
    assertThat(converter.canConvert(Boolean.class, Enum.class), is(false));
  }

  @Test
  public void convert() {
    Gender gender = converter.convert("FEMALE", Gender.class);

    assertThat(gender, is(notNullValue()));
    assertThat(gender, is(equalTo(Gender.FEMALE)));
  }

  @Test
  public void convertInvalidEnum() {
    expectedException.expect(ConversionException.class);
    expectedException.expectCause(is(instanceOf(IllegalArgumentException.class)));
    expectedException.expectMessage(String.format("(BLACK) is not a valid enumerated value of Enum (%1$s)", Gender.class));
    converter.convert("BLACK", Gender.class);
  }

  @Test
  public void convertInvalidEnumValue() {
    expectedException.expect(ConversionException.class);
    expectedException.expectCause(is(instanceOf(IllegalArgumentException.class)));
    expectedException.expectMessage(String.format("(IT) is not a valid enumerated value of Enum (%1$s)", Gender.class));
    converter.convert("IT", Gender.class);
  }

  @Test
  public void convertWithEnum() {
    expectedException.expect(ConversionException.class);
    expectedException.expectCause(is(instanceOf(IllegalArgumentException.class)));
    expectedException.expectMessage(String.format("(test) is not a valid enumerated value of Enum (%1$s)", Enum.class));
    converter.convert("test", Enum.class);
  }

}
