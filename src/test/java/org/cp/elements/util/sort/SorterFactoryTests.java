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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import org.cp.elements.test.TestUtils;
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
 * Unit Tests for {@link SorterFactory}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.sort.SorterFactory
 * @see org.cp.elements.util.sort.SortType
 * @see org.cp.elements.util.sort.support.BubbleSort
 * @see org.cp.elements.util.sort.support.CombSort
 * @see org.cp.elements.util.sort.support.HeapSort
 * @see org.cp.elements.util.sort.support.InsertionSort
 * @see org.cp.elements.util.sort.support.MergeSort
 * @see org.cp.elements.util.sort.support.QuickSort
 * @see org.cp.elements.util.sort.support.SelectionSort
 * @see org.cp.elements.util.sort.support.ShellSort
 * @since 1.0.0
 */
public class SorterFactoryTests {

  @Test
  public void create() {

    assertThat(SorterFactory.<Sorter>createSorter(SortType.BUBBLE_SORT)).isInstanceOf(BubbleSort.class);
    assertThat(SorterFactory.<Sorter>createSorter(SortType.COMB_SORT)).isInstanceOf(CombSort.class);
    assertThat(SorterFactory.<Sorter>createSorter(SortType.HEAP_SORT)).isInstanceOf(HeapSort.class);
    assertThat(SorterFactory.<Sorter>createSorter(SortType.INSERTION_SORT)).isInstanceOf(InsertionSort.class);
    assertThat(SorterFactory.<Sorter>createSorter(SortType.MERGE_SORT)).isInstanceOf(MergeSort.class);
    assertThat(SorterFactory.<Sorter>createSorter(SortType.QUICK_SORT)).isInstanceOf(QuickSort.class);
    assertThat(SorterFactory.<Sorter>createSorter(SortType.SELECTION_SORT)).isInstanceOf(SelectionSort.class);
    assertThat(SorterFactory.<Sorter>createSorter(SortType.SHELL_SORT)).isInstanceOf(ShellSort.class);
  }

  @Test(expected = IllegalArgumentException.class)
  public void createWithNull() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> SorterFactory.createSorter(null),
      () -> String.format("The SortType (null) is not supported by the %s!", SorterFactory.class.getSimpleName()));
  }

  @Test
  public void createSorterElseDefault() {
    assertThat(SorterFactory.<Sorter>createSorterElseDefault(SortType.HEAP_SORT, null))
      .isInstanceOf(HeapSort.class);
  }

  @Test
  public void createSorterElseDefaultWithUnknownSortAlgorithm() {

    Sorter mockDefaultSorter = mock(Sorter.class);

    assertThat(SorterFactory.createSorterElseDefault(SortType.UNKONWN, mockDefaultSorter)).isSameAs(mockDefaultSorter);
    assertThat(SorterFactory.<Sorter>createSorterElseDefault(SortType.UNKONWN, null)).isNull();
  }
}
