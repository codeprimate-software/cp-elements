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

package org.cp.elements.util.search;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.jupiter.api.Test;

/**
 * The SearchTypeTest class is a test suite of test cases testing the contract and functionality of the SearchType
 * enumerated type.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.search.SearchType
 * @see org.junit.jupiter.api.Test
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
