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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

/**
 * The RaceTest class is a test suite of test cases testing the contract and functionality of the Race Enum.
 *
 * @author John J. Blum
 * @see org.cp.elements.enums.Race
 * @see org.junit.Test
 * @since 1.0.0
 */
public class RaceTest {

  @Test
  public void testValueOf() {
    for (Race race : Race.values()) {
      assertEquals(race, Race.valueOf(race.name()));
    }
  }

  @Test
  public void testValueOfAbbreviation() {
    for (Race race : Race.values()) {
      assertEquals(race, Race.valueOfAbbreviation(race.getAbbreviation()));
    }

    assertEquals(Race.WHITE, Race.valueOfAbbreviation("white"));
    assertEquals(Race.NATIVE_AMERICAN, Race.valueOfAbbreviation("Indian"));
    assertEquals(Race.AFRICAN_AMERICAN, Race.valueOfAbbreviation("BLACK"));
    assertNull(Race.valueOfAbbreviation("African American"));
    assertNull(Race.valueOfAbbreviation("alien"));
    assertNull(Race.valueOfAbbreviation("Euro"));
    assertNull(Race.valueOfAbbreviation("hebrew"));
    assertNull(Race.valueOfAbbreviation("muslim"));
    assertNull(Race.valueOfAbbreviation("Spanish"));
  }

  @Test
  public void testValueOfName() {
    for (Race race : Race.values()) {
      assertEquals(race, Race.valueOfName(race.getName()));
    }

    assertEquals(Race.WHITE, Race.valueOfName("white"));
    assertEquals(Race.EUROPEAN, Race.valueOfName("European"));
    assertEquals(Race.AFRICAN_AMERICAN, Race.valueOfName("African American"));
    assertNull(Race.valueOfName("Black"));
  }

}
