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

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.enums.Gender;
import org.cp.elements.enums.Race;
import org.cp.elements.time.TimeUnit;
import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link EnumConverter}.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.conversion.converters.EnumConverter
 * @since 1.0.0
 */
public class EnumConverterTests {

  private final EnumConverter converter = new EnumConverter();

  @Test
  public void canConvertToEnumReturnsTrue() {

    assertThat(this.converter.canConvert(String.class, Enum.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Gender.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Race.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, TimeUnit.class)).isTrue();
  }

  @Test
  public void canConvertNullToEnumReturnsFalse() {
    assertThat(this.converter.canConvert(null, Enum.class)).isFalse();
  }

  @Test
  public void cannotConvertToEnumReturnsFalse() {

    assertThat(this.converter.canConvert(Enum.class, null)).isFalse();
    assertThat(this.converter.canConvert(Enum.class, Enum.class)).isFalse();
    assertThat(this.converter.canConvert(Enum.class, Gender.class)).isFalse();
    assertThat(this.converter.canConvert(Enum.class, Race.class)).isFalse();
    assertThat(this.converter.canConvert(Enum.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, Enum.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, Enum.class)).isFalse();
    assertThat(this.converter.canConvert(Object.class, Enum.class)).isFalse();
  }

  @Test
  public void convertStringToGenderEnum() {
    assertThat(this.converter.convert("FEMALE", Gender.class)).isEqualTo(Gender.FEMALE);
  }

  @Test
  public void convertStringToRaceEnum() {
    assertThat(this.converter.convert("WHITE", Race.class)).isEqualTo(Race.WHITE);
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidEnumeratedValueOfEnum() {

    try {
      this.converter.convert("IT", Gender.class);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[IT] is not a valid enumerated value of Enum [%s]",
        Gender.class.getName());

      assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidStringToEnumThrowsException() {

    try {
      this.converter.convert("BLACK", Gender.class);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[BLACK] is not a valid enumerated value of Enum [%s]",
        Gender.class.getName());

      assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertWithEnumThrowsException () {

    try {
      this.converter.convert("test", Enum.class);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[test] is not a valid enumerated value of Enum [%s]",
        Enum.class.getName());

      assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }
}
