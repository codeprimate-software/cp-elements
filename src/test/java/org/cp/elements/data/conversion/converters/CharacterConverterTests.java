/*
 * Copyright 2011-Present Author or Authors.
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.lang.StringUtils;

/**
 * Unit Tests for {@link CharacterConverter}.
 *
 * @author John J. Blum
 * @see java.lang.Character
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.conversion.converters.CharacterConverter
 * @since 1.0.0
 */
public class CharacterConverterTests {

  private final CharacterConverter converter = new CharacterConverter();

  @Test
  public void canConvertToCharacterReturnsTrue() {

    assertThat(this.converter.canConvert(Character.class, Character.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Character.class)).isTrue();
  }

  @Test
  public void canConvertNullToCharacterReturnsTrue() {
    assertThat(this.converter.canConvert(null, Character.class)).isTrue();
  }

  @Test
  public void cannotConvertToCharacterReturnsFalse() {

    assertThat(this.converter.canConvert(Character.class, null)).isFalse();
    assertThat(this.converter.canConvert(Character.TYPE, Character.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, Character.TYPE)).isFalse();
    assertThat(this.converter.canConvert(Character.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, Object.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, Number.class)).isFalse();
    assertThat(this.converter.canConvert(Object.class, Character.class)).isFalse();
    assertThat(this.converter.canConvert(Number.class, Character.class)).isFalse();
  }

  @Test
  public void convertBooleanToCharacterThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.convert(true))
      .withMessage("Cannot convert [true] to [java.lang.Character]")
      .withNoCause();
  }

  @Test
  public void convertCharacterToCharacter() {
    assertThat(this.converter.convert('X')).isEqualTo('X');
  }

  @Test
  public void convertNumberToCharacterThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.convert(123))
      .withMessage("Cannot convert [123] to [java.lang.Character]")
      .withNoCause();
  }

  @Test
  public void convertNullToCharacterWithDefaultValueReturnsDefaultValue() {
    assertThat(this.converter.withDefaultValue('X').convert(null)).isEqualTo('X');
  }

  @Test
  public void convertNullToCharacterWithoutDefaultValueThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.convert(null))
      .withMessage("Cannot convert [null] to [java.lang.Character]")
      .withNoCause();
  }

  @Test
  public void convertStringLetterToCharacter() {
    assertThat(this.converter.convert("X")).isEqualTo('X');
  }

  @Test
  public void convertStringWordToCharacter() {
    assertThat(this.converter.convert("word")).isEqualTo('w');
  }

  @Test
  public void convertBlankStringToCharacter() {
    assertThat(this.converter.convert(" ")).isEqualTo(' ');
  }

  @Test
  public void convertEmptyStringToCharacter() {
    assertThat(this.converter.convert(StringUtils.EMPTY_STRING)).isEqualTo('\0');
  }
}
