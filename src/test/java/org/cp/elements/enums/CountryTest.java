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
 * The CountryTest class is a test suite of test cases testing the contract and functionality of the Country Enum.
 *
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
