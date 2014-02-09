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

import org.cp.elements.util.sort.support.BubbleSort;
import org.cp.elements.util.sort.support.HeapSort;
import org.cp.elements.util.sort.support.InsertionSort;
import org.cp.elements.util.sort.support.MergeSort;
import org.cp.elements.util.sort.support.QuickSort;
import org.cp.elements.util.sort.support.SelectionSort;
import org.cp.elements.util.sort.support.ShellSort;
import org.junit.Test;

/**
 * The SorterFactoryTest class is a test suite of test cases testing the contract and functionality of the
 * SorterFactory class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.sort.SorterFactory
 * @see org.cp.elements.util.sort.SortType
 * @since 1.0.0
 */
public class SorterFactoryTest {

  @Test
  public void testCreate() {
    assertTrue(SorterFactory.createSorter(SortType.BUBBLE_SORT) instanceof BubbleSort);
    assertTrue(SorterFactory.createSorter(SortType.HEAP_SORT) instanceof HeapSort);
    assertTrue(SorterFactory.createSorter(SortType.INSERTION_SORT) instanceof InsertionSort);
    assertTrue(SorterFactory.createSorter(SortType.MERGE_SORT) instanceof MergeSort);
    assertTrue(SorterFactory.createSorter(SortType.QUICK_SORT) instanceof QuickSort);
    assertTrue(SorterFactory.createSorter(SortType.SELECTION_SORT) instanceof SelectionSort);
    assertTrue(SorterFactory.createSorter(SortType.SHELL_SORT) instanceof ShellSort);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testCreateWithNull() {
    try {
      SorterFactory.createSorter(null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals(String.format("The SortType (null) is not supported by the %1$s!",
        SorterFactory.class.getSimpleName()), expected.getMessage());
      throw expected;
    }
  }

}
