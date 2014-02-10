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

package org.cp.elements.enums;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The StateTest class is a test suite of test cases testing the contract and functionality of the State Enum.
 * <p/>
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
