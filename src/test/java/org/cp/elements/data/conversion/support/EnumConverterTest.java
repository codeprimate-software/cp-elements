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

package org.cp.elements.data.conversion.support;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.enums.Gender;
import org.cp.elements.enums.Race;
import org.cp.elements.enums.TimeUnit;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The EnumConverterTest class is a test suite of test cases testing the contract and functionality of the
 * EnumConverter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.conversion.support.EnumConverter
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
