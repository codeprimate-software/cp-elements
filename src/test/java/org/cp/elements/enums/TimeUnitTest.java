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
 * The TimeUnitTest class is a test suite of test cases testing the contract and functionality of the TimeUnit Enum.
 * <p/>
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
