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
 * The TimeUnitTest class is a test suite of test cases testing the contract and functionality of the TimeUnit Enum.
 *
 * @author John J. Blum
 * @see org.cp.elements.enums.TimeUnit
 * @see org.junit.Test
 * @since 1.0.0
 */
public class TimeUnitTest {

  @Test
  public void testValueOf() {
    for (TimeUnit timeUnit : TimeUnit.values()) {
      assertEquals(timeUnit, TimeUnit.valueOf(timeUnit.name()));
    }
  }

  @Test
  public void testValueOfAbbreviation() {
    for (TimeUnit timeUnit : TimeUnit.values()) {
      assertEquals(timeUnit, TimeUnit.valueOfAbbreviation(timeUnit.getAbbreviation()));
    }

    assertEquals(TimeUnit.MILLISECOND, TimeUnit.valueOfAbbreviation("ms"));
    assertEquals(TimeUnit.MINUTE, TimeUnit.valueOfAbbreviation("Mi"));
    assertEquals(TimeUnit.DAY, TimeUnit.valueOfAbbreviation("DAY"));
    assertNull(TimeUnit.valueOfAbbreviation("Week"));
  }

  @Test
  public void testValueOfName() {
    for (TimeUnit timeUnit : TimeUnit.values()) {
      assertEquals(timeUnit, TimeUnit.valueOfName(timeUnit.getName()));
    }

    assertEquals(TimeUnit.MILLISECOND, TimeUnit.valueOfName("millisecond"));
    assertEquals(TimeUnit.MINUTE, TimeUnit.valueOfName("Minute"));
    assertEquals(TimeUnit.DAY, TimeUnit.valueOfName("DAY"));
    assertNull(TimeUnit.valueOfName("ns"));
  }

}
