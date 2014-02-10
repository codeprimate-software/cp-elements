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
 * The RaceTest class is a test suite of test cases testing the contract and functionality of the Race Enum.
 * <p/>
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
