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

package org.cp.elements.util.sort.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * The QuickSortTest class is a test suite of test cases testing the contract and functionality of the QuickSort
 * class and the Quick Sort algorithm.
 *
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
  @SuppressWarnings("all")
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
