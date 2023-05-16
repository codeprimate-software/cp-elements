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
package org.cp.elements.enums;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Gender}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.enums.Gender
 * @since 1.0.0
 */
public class GenderUnitTests {

  @Test
  public void valueOfReturnsGender() {

    Arrays.stream(Gender.values()).forEach(gender ->
      assertThat(Gender.valueOf(gender.name())).isEqualTo(gender));
  }

  @Test
  public void valueOfAbbreviationReturnsGender() {

    Arrays.stream(Gender.values()).forEach(gender ->
      assertThat(Gender.valueOfAbbreviation(gender.getAbbreviation())).isEqualTo(gender));
  }

  @Test
  public void valueOfAbbreviationUsingNameReturnsNull() {

    assertThat(Gender.valueOfAbbreviation("Female")).isNull();
    assertThat(Gender.valueOfAbbreviation("Girl")).isNull();
    assertThat(Gender.valueOfAbbreviation("Woman")).isNull();
    assertThat(Gender.valueOfAbbreviation("Women")).isNull();
  }

  @Test
  public void valueOfInvalidAbbreviationReturnsNull() {

    assertThat(Gender.valueOfAbbreviation("B")).isNull(); // Boy
    assertThat(Gender.valueOfAbbreviation("G")).isNull(); // Guy / Girl
    assertThat(Gender.valueOfAbbreviation("W")).isNull(); // Woman
  }

  @Test
  public void valueOfNullAbbreviationIsNullSafeAndReturnsNull() {
    assertThat(Gender.valueOfAbbreviation(null)).isNull();
  }

  @Test
  public void valueOfNameReturnsGender() {

    Arrays.stream(Gender.values()).forEach(gender ->
      assertThat(Gender.valueOfName(gender.getName())).isEqualTo(gender));
  }

  @Test
  public void valueOfNameUsingAbbreviationReturnsNull() {

    assertThat(Gender.valueOfName("B")).isNull();
    assertThat(Gender.valueOfName("F")).isNull();
    assertThat(Gender.valueOfName("G")).isNull();
    assertThat(Gender.valueOfName("M")).isNull();
    assertThat(Gender.valueOfName("W")).isNull();
  }

  @Test
  public void valueOfInvalidNameReturnsNull() {

    assertThat(Gender.valueOfName("Boy")).isNull();
    assertThat(Gender.valueOfName("Girl")).isNull();
    assertThat(Gender.valueOfName("Man")).isNull();
    assertThat(Gender.valueOfName("Men")).isNull();
    assertThat(Gender.valueOfName("Woman")).isNull();
    assertThat(Gender.valueOfName("Women")).isNull();
  }

  @Test
  public void valueOfNullNameIsNullSafeReturnsNull() {
    assertThat(Gender.valueOfName(null)).isNull();
  }
}
