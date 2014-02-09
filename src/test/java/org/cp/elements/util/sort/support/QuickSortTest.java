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

package org.cp.elements.util.sort.support;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The QuickSortTest class is a test suite of test cases testing the contract and functionality of the QuickSort
 * class and the Quick Sort algorithm.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.sort.Sorter
 * @see org.cp.elements.util.sort.support.CommonSortTestSuite
 * @see org.cp.elements.util.sort.support.QuickSort
 * @since 1.0.0
 */
public class QuickSortTest extends CommonSortTestSuite {

  @Override
  protected QuickSort getSorter() {
    return new QuickSort();
  }

  @Test
  public void testSetAndGetSizeThreshold() {
    QuickSort sorter = getSorter();

    assertNotNull(sorter);
    assertEquals(QuickSort.DEFAULT_SIZE_THRESHOLD, sorter.getSizeThreshold());

    sorter.setSizeThreshold(QuickSort.DEFAULT_SIZE_THRESHOLD + 100);

    assertEquals(QuickSort.DEFAULT_SIZE_THRESHOLD + 100, sorter.getSizeThreshold());

    sorter.setSizeThreshold(QuickSort.DEFAULT_SIZE_THRESHOLD - 5);

    assertEquals(QuickSort.DEFAULT_SIZE_THRESHOLD, sorter.getSizeThreshold());

    sorter.setSizeThreshold(QuickSort.DEFAULT_SIZE_THRESHOLD - QuickSort.DEFAULT_SIZE_THRESHOLD - 5);

    assertEquals(QuickSort.DEFAULT_SIZE_THRESHOLD, sorter.getSizeThreshold());

    sorter.setSizeThreshold(QuickSort.DEFAULT_SIZE_THRESHOLD * -2);

    assertEquals(QuickSort.DEFAULT_SIZE_THRESHOLD, sorter.getSizeThreshold());
  }

  @Test
  public void testSetAndGetSorter() {
    QuickSort sorter = getSorter();

    assertNotNull(sorter);
    assertSame(QuickSort.DEFAULT_SORTER, sorter.getSorter());
    assertFalse(sorter.getSorter() instanceof HeapSort);

    sorter.setSorter(new HeapSort());

    assertTrue(sorter.getSorter() instanceof HeapSort);

    sorter.setSorter(null);

    assertSame(QuickSort.DEFAULT_SORTER, sorter.getSorter());
  }

}
