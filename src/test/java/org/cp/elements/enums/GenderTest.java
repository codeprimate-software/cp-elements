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

package org.cp.elements.enums;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

/**
 * The GenderTest class is a test suite of test cases testing the contract and functionality of the Gender Enum.
 *
 * @author John J. Blum
 * @see org.cp.elements.enums.Gender
 * @see org.junit.Test
 * @since 1.0.0
 */
public class GenderTest {

  @Test
  public void testValueOf() {
    for (Gender gender : Gender.values()) {
      assertEquals(gender, Gender.valueOf(gender.name()));
    }
  }

  @Test
  public void testValueOfAbbreviation() {
    for (Gender gender : Gender.values()) {
      assertEquals(gender, Gender.valueOfAbbreviation(gender.getAbbreviation()));
    }

    assertEquals(Gender.FEMALE, Gender.valueOfAbbreviation("f"));
    assertNull(Gender.valueOfAbbreviation("Female"));
    assertNull(Gender.valueOfAbbreviation("G"));
    assertNull(Gender.valueOfAbbreviation("W"));
  }

  @Test
  public void testValueOfName() {
    for (Gender gender : Gender.values()) {
      assertEquals(gender, Gender.valueOfName(gender.getName()));
    }

    assertEquals(Gender.FEMALE, Gender.valueOfName("female"));
    assertNull(Gender.valueOfName("F"));
    assertNull(Gender.valueOfName("Girl"));
    assertNull(Gender.valueOfName("Woman"));
  }

}
