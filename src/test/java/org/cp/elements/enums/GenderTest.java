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
 * The GenderTest class is a test suite of test cases testing the contract and functionality of the Gender Enum.
 * <p/>
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
