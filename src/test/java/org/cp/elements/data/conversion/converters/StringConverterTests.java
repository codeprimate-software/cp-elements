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

import java.util.Date;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Identifiable;

/**
 * Unit tests for {@link StringConverter}.
 *
 * @author John J. Blum
 * @see java.lang.String
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.conversion.converters.StringConverter
 * @since 1.0.0
 */
public class StringConverterTests {

  private final StringConverter converter = new StringConverter();

  @Test
  public void canConvertToStringReturnsTrue() {

    assertThat(this.converter.canConvert(Boolean.class, String.class)).isTrue();
    assertThat(this.converter.canConvert(Character.class, String.class)).isTrue();
    assertThat(this.converter.canConvert(Date.class, String.class)).isTrue();
    assertThat(this.converter.canConvert(Enum.class, String.class)).isTrue();
    assertThat(this.converter.canConvert(Identifiable.class, String.class)).isTrue();
    assertThat(this.converter.canConvert(Number.class, String.class)).isTrue();
    assertThat(this.converter.canConvert(Object.class, String.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, String.class)).isTrue();
  }

  @Test
  public void canConverterNullToStringReturnsTrue() {
    assertThat(this.converter.canConvert(null, String.class)).isTrue();
  }

  @Test
  public void cannotConvertStringToNullReturnsFalse() {
    assertThat(converter.canConvert(String.class, null)).isFalse();
  }

  @Test
  public void cannotConvertToStringReturnsFalse() {

    assertThat(this.converter.canConvert(String.class, Character.class));
    assertThat(this.converter.canConvert(String.class, Date.class));
    assertThat(this.converter.canConvert(String.class, Enum.class));
    assertThat(this.converter.canConvert(String.class, Identifiable.class));
    assertThat(this.converter.canConvert(String.class, Number.class));
    assertThat(this.converter.canConvert(String.class, Object.class));
  }

  @Test
  public void convertNullToString() {
    assertThat(this.converter.convert(null)).isEqualTo("null");
  }

  @Test
  public void convertToString() {

    Date now = new Date();

    assertThat(this.converter.convert(Boolean.TRUE)).isEqualTo("true");
    assertThat(this.converter.convert('X')).isEqualTo("X");
    assertThat(this.converter.convert(now)).isEqualTo(now.toString());
    assertThat(this.converter.convert(Gender.FEMALE)).isEqualTo("FEMALE");
    assertThat(this.converter.convert(42)).isEqualTo("42");
    assertThat(this.converter.convert(3.14159d)).isEqualTo("3.14159");
    assertThat(this.converter.convert("test")).isEqualTo("test");
  }

  @Test
  public void convertObjectToString() {

    Object obj = new Object();

    assertThat(this.converter.convert(obj)).isEqualTo(obj.toString());
  }

  private enum Gender {
    FEMALE, MALE
  }
}
