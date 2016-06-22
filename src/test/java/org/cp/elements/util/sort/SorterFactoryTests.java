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

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import org.cp.elements.util.sort.support.BubbleSort;
import org.cp.elements.util.sort.support.CombSort;
import org.cp.elements.util.sort.support.HeapSort;
import org.cp.elements.util.sort.support.InsertionSort;
import org.cp.elements.util.sort.support.MergeSort;
import org.cp.elements.util.sort.support.QuickSort;
import org.cp.elements.util.sort.support.SelectionSort;
import org.cp.elements.util.sort.support.ShellSort;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Test suite of test cases testing the contract and functionality of the {@link SorterFactory} class.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.sort.SorterFactory
 * @see org.cp.elements.util.sort.SortType
 * @since 1.0.0
 */
public class SorterFactoryTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void create() {
    assertTrue(SorterFactory.createSorter(SortType.BUBBLE_SORT) instanceof BubbleSort);
    assertTrue(SorterFactory.createSorter(SortType.COMB_SORT) instanceof CombSort);
    assertTrue(SorterFactory.createSorter(SortType.HEAP_SORT) instanceof HeapSort);
    assertTrue(SorterFactory.createSorter(SortType.INSERTION_SORT) instanceof InsertionSort);
    assertTrue(SorterFactory.createSorter(SortType.MERGE_SORT) instanceof MergeSort);
    assertTrue(SorterFactory.createSorter(SortType.QUICK_SORT) instanceof QuickSort);
    assertTrue(SorterFactory.createSorter(SortType.SELECTION_SORT) instanceof SelectionSort);
    assertTrue(SorterFactory.createSorter(SortType.SHELL_SORT) instanceof ShellSort);
  }

  @Test
  public void createWithNull() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(String.format("The SortType (null) is not supported by the %1$s!",
      SorterFactory.class.getSimpleName()));

    SorterFactory.createSorter(null);
  }

  @Test
  public void createSorterElseDefault() {
    assertTrue(SorterFactory.createSorterElseDefault(SortType.HEAP_SORT, null) instanceof HeapSort);
  }

  @Test
  public void createSorterElseDefaultWithUnknownSortAlgorithm() {
    Sorter mockDefaultSorter = mock(Sorter.class);

    assertSame(mockDefaultSorter, SorterFactory.createSorterElseDefault(SortType.UNKONWN, mockDefaultSorter));
    assertNull(SorterFactory.createSorterElseDefault(SortType.UNKONWN, null));
  }
}
