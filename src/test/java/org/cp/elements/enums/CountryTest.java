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
 * The CountryTest class is a test suite of test cases testing the contract and functionality of the Country Enum.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.enums.Country
 * @see org.junit.Test
 * @since 1.0.0
 */
public class CountryTest {

  @Test
  public void testValueOf() {
    for (Country country : Country.values()) {
      assertEquals(country, Country.valueOf(country.name()));
    }
  }

  @Test
  public void testValueOfAbbreviation() {
    for (Country country : Country.values()) {
      assertEquals(country, Country.valueOfAbbreviation(country.getAbbreviation()));
    }

    assertEquals(Country.UNITED_STATES_OF_AMERICA, Country.valueOfAbbreviation("usa"));
    assertNull(Country.valueOfAbbreviation("US"));
    assertNull(Country.valueOfAbbreviation("USSR"));
    assertNull(Country.valueOfAbbreviation("GER"));
  }

  @Test
  public void testValueOfName() {
    for (Country country : Country.values()) {
      assertEquals(country, Country.valueOfName(country.getName()));
    }

    assertEquals(Country.UNITED_STATES_OF_AMERICA, Country.valueOfName("UNITED STATES OF AMERICA"));
    assertNull(Country.valueOfName("United States"));
    assertNull(Country.valueOfName("Russia"));
    assertNull(Country.valueOfName("Germany"));
  }

}
