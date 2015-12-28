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
 * The StateTest class is a test suite of test cases testing the contract and functionality of the State Enum.
 *
 * @author John J. Blum
 * @see org.cp.elements.enums.State
 * @see org.junit.Test
 * @since 1.0.0
 */
public class StateTest {

  @Test
  public void testValueOf() {
    for (State state : State.values()) {
      assertEquals(state, State.valueOf(state.name()));
    }
  }

  @Test
  public void testValueOfAbbreviation() {
    for (State state : State.values()) {
      assertEquals(state, State.valueOfAbbreviation(state.getAbbreviation()));
    }

    assertEquals(State.WISCONSIN, State.valueOfAbbreviation("wi"));
    assertEquals(State.IOWA, State.valueOfAbbreviation("Ia"));
    assertEquals(State.MONTANA, State.valueOfAbbreviation("MT"));
    assertEquals(State.OREGON, State.valueOfAbbreviation("or"));
    assertNull(State.valueOfAbbreviation("Oregon"));
    assertNull(State.valueOfAbbreviation("Wash"));
    assertNull(State.valueOfAbbreviation("Cali"));
  }

  @Test
  public void testValueOfName() {
    for (State state : State.values()) {
      assertEquals(state, State.valueOfName(state.getName()));
    }

    assertEquals(State.WISCONSIN, State.valueOfName("wisconsin"));
    assertEquals(State.IOWA, State.valueOfName("Iowa"));
    assertEquals(State.MONTANA, State.valueOfName("MONTANA"));
    assertEquals(State.OREGON, State.valueOfName("Oregon"));
    assertNull(State.valueOfName("OR"));
    assertNull(State.valueOfName("Oregano"));
    assertNull(State.valueOfName("Wash"));
  }

}
