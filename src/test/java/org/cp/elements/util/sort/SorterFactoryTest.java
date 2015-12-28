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

package org.cp.elements.util.sort;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.cp.elements.test.AbstractMockingTestSuite;
import org.cp.elements.util.sort.support.BubbleSort;
import org.cp.elements.util.sort.support.CombSort;
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
 *
 * @author John J. Blum
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.cp.elements.util.sort.SorterFactory
 * @see org.cp.elements.util.sort.SortType
 * @since 1.0.0
 */
public class SorterFactoryTest extends AbstractMockingTestSuite {

  @Test
  public void testCreate() {
    assertTrue(SorterFactory.createSorter(SortType.BUBBLE_SORT) instanceof BubbleSort);
    assertTrue(SorterFactory.createSorter(SortType.COMB_SORT) instanceof CombSort);
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

  @Test
  public void testCreateSorterElseDefault() {
    assertTrue(SorterFactory.createSorterElseDefault(SortType.HEAP_SORT, null) instanceof HeapSort);
  }

  @Test
  public void testCreateSorterElseDefaultWithUnknownSortAlgorithm() {
    Sorter mockDefaultSorter = mockContext.mock(Sorter.class, "testCreateSorterElseDefaultWithUnknownSortAlgorithm");

    assertSame(mockDefaultSorter, SorterFactory.createSorterElseDefault(SortType.UNKONWN, mockDefaultSorter));
    assertNull(SorterFactory.createSorterElseDefault(SortType.UNKONWN, null));
  }

}
