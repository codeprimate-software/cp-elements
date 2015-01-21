/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * 
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * 
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * 
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * 
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util.search;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The SearchTypeTest class is a test suite of test cases testing the contract and functionality of the SearchType
 * enumerated type.
 * 
 * @author John J. Blum
 * @see org.cp.elements.util.search.SearchType
 * @see org.junit.Test
 * @since 1.0.0
 */
public class SearchTypeTest {

  @Test
  public void testValueOf() {
    for (SearchType value : SearchType.values()) {
      assertEquals(value, SearchType.valueOf(value.name()));
    }
  }

  @Test
  public void testValueOfAbbreviation() {
    for (SearchType value : SearchType.values()) {
      assertEquals(value, SearchType.valueOfAbbreviation(value.getAbbreviation()));
    }

    assertEquals(SearchType.BINARY_SEARCH, SearchType.valueOfAbbreviation("binary"));
    assertEquals(SearchType.INDEX_SEARCH, SearchType.valueOfAbbreviation("Index"));
    assertEquals(SearchType.LINEAR_SEARCH, SearchType.valueOfAbbreviation("LINEAR"));
    assertNull(SearchType.valueOfAbbreviation("octal"));
    assertNull(SearchType.valueOfAbbreviation("HashIndex"));
    assertNull(SearchType.valueOfAbbreviation("NONLINEAR"));
  }

  @Test
  public void testValueOfName() {
    for (SearchType value : SearchType.values()) {
      assertEquals(value, SearchType.valueOfName(value.getName()));
    }

    assertEquals(SearchType.BINARY_SEARCH, SearchType.valueOfName("binary search"));
    assertEquals(SearchType.INDEX_SEARCH, SearchType.valueOfName("Index Search"));
    assertEquals(SearchType.LINEAR_SEARCH, SearchType.valueOfName("LINEAR SEARCH"));
    assertNull(SearchType.valueOfName("octal search"));
    assertNull(SearchType.valueOfName("HashIndex Search"));
    assertNull(SearchType.valueOfName("NONLINEAR SEARCH"));
  }

}
