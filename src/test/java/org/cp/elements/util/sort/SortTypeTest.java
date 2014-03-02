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

package org.cp.elements.util.sort;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The SortTypeTest class is a test suite of test cases testing the contract and functionality of the
 * SorterType enumeration type.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.sort.SortType
 * @see org.junit.Test
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
    assertEquals(SortType.BUBBLE_SORT, SortType.valueOfAbbreviation(SortType.BUBBLE_SORT.getAbbreviation()));
    assertEquals(SortType.COMB_SORT, SortType.valueOfAbbreviation(SortType.COMB_SORT.getAbbreviation()));
    assertEquals(SortType.HEAP_SORT, SortType.valueOfAbbreviation(SortType.HEAP_SORT.getAbbreviation()));
    assertEquals(SortType.INSERTION_SORT, SortType.valueOfAbbreviation(SortType.INSERTION_SORT.getAbbreviation()));
    assertEquals(SortType.MERGE_SORT, SortType.valueOfAbbreviation(SortType.MERGE_SORT.getAbbreviation()));
    assertEquals(SortType.QUICK_SORT, SortType.valueOfAbbreviation(SortType.QUICK_SORT.getAbbreviation()));
    assertEquals(SortType.SELECTION_SORT, SortType.valueOfAbbreviation(SortType.SELECTION_SORT.getAbbreviation()));
    assertEquals(SortType.SHELL_SORT, SortType.valueOfAbbreviation(SortType.SHELL_SORT.getAbbreviation()));
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
    assertEquals(SortType.BUBBLE_SORT, SortType.valueOfName(SortType.BUBBLE_SORT.getName()));
    assertEquals(SortType.COMB_SORT, SortType.valueOfName(SortType.COMB_SORT.getName()));
    assertEquals(SortType.HEAP_SORT, SortType.valueOfName(SortType.HEAP_SORT.getName()));
    assertEquals(SortType.INSERTION_SORT, SortType.valueOfName(SortType.INSERTION_SORT.getName()));
    assertEquals(SortType.MERGE_SORT, SortType.valueOfName(SortType.MERGE_SORT.getName()));
    assertEquals(SortType.QUICK_SORT, SortType.valueOfName(SortType.QUICK_SORT.getName()));
    assertEquals(SortType.SELECTION_SORT, SortType.valueOfName(SortType.SELECTION_SORT.getName()));
    assertEquals(SortType.SHELL_SORT, SortType.valueOfName(SortType.SHELL_SORT.getName()));
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
