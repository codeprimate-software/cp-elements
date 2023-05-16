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

package org.cp.elements.util.sort;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.jupiter.api.Test;

/**
 * The SortTypeTest class is a test suite of test cases testing the contract and functionality of the
 * SorterType enumeration type.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @see org.cp.elements.util.sort.SortType
 * @see org.junit.jupiter.api.Test
 * @since 1.0.0
 */
public class SortTypeTest {

  @Test
  public void testValueOf() {
    for (SortType value : SortType.values()) {
      assertEquals(value, SortType.valueOf(value.name()));
    }
  }

  @Test
  public void testValueOfAbbreviation() {
    for (SortType value : SortType.values()) {
      assertEquals(value, SortType.valueOfAbbreviation(value.getAbbreviation()));
    }

    assertEquals(SortType.HEAP_SORT, SortType.valueOfAbbreviation("heap"));
    assertEquals(SortType.MERGE_SORT, SortType.valueOfAbbreviation("Merge"));
    assertEquals(SortType.QUICK_SORT, SortType.valueOfAbbreviation("QUICK"));
    assertNull(SortType.valueOfAbbreviation(SortType.QUICK_SORT.getName()));
    assertNull(SortType.valueOfAbbreviation(null));
    assertNull(SortType.valueOfAbbreviation("bubbles"));
    assertNull(SortType.valueOfAbbreviation("OffHeap"));
    assertNull(SortType.valueOfAbbreviation("DELETION"));
    assertNull(SortType.valueOfAbbreviation("separate"));
    assertNull(SortType.valueOfAbbreviation("Slow"));
    assertNull(SortType.valueOfAbbreviation("SELECT"));
    assertNull(SortType.valueOfAbbreviation("fossil"));
  }

  @Test
  public void testValueOfName() {
    for (SortType value : SortType.values()) {
      assertEquals(value, SortType.valueOfName(value.getName()));
    }

    assertEquals(SortType.HEAP_SORT, SortType.valueOfName("heap sort"));
    assertEquals(SortType.MERGE_SORT, SortType.valueOfName("Merge Sort"));
    assertEquals(SortType.QUICK_SORT, SortType.valueOfName("QUICK SORT"));
    assertNull(SortType.valueOfName(SortType.QUICK_SORT.getAbbreviation()));
    assertNull(SortType.valueOfName(null));
    assertNull(SortType.valueOfName("offheap sort"));
    assertNull(SortType.valueOfName("Select Sort"));
    assertNull(SortType.valueOfName("NO SORT"));
  }

}
