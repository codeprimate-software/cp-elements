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

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.util.sort.support.BubbleSort;
import org.cp.elements.util.sort.support.CombSort;
import org.cp.elements.util.sort.support.HeapSort;
import org.cp.elements.util.sort.support.InsertionSort;
import org.cp.elements.util.sort.support.MergeSort;
import org.cp.elements.util.sort.support.QuickSort;
import org.cp.elements.util.sort.support.SelectionSort;
import org.cp.elements.util.sort.support.ShellSort;

/**
 * The SorterFactory class is a factory for creating instances of different Sorter implementations that implement
 * different sorting algorithms.
 *
 * @author John J. Blum
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
@SuppressWarnings("unused")
public class SorterFactory {

  /**
   * Creates an instance of the Sorter interface implementing the sorting algorithm based on the SortType.
   *
   * @param <T> the Class type of the actual Sorter implementation based on the SortType.
   * @param type the type of sorting algorithm Sorter implementation to create.
   * @return a Sorter implementation subclass that implements the sorting algorithm based on the SortType.
   * @see org.cp.elements.util.sort.Sorter
   * @see org.cp.elements.util.sort.SortType
   */
  @SuppressWarnings("unchecked")
  public static <T extends Sorter> T createSorter(final SortType type) {
    switch (ObjectUtils.defaultIfNull(type, SortType.UNKONWN)) {
      case BUBBLE_SORT:
        return (T) new BubbleSort();
      case COMB_SORT:
        return (T) new CombSort();
      case HEAP_SORT:
        return (T) new HeapSort();
      case INSERTION_SORT:
        return (T) new InsertionSort();
      case MERGE_SORT:
        return (T) new MergeSort();
      case QUICK_SORT:
        return (T) new QuickSort();
      case SELECTION_SORT:
        return (T) new SelectionSort();
      case SHELL_SORT:
        return (T) new ShellSort();
      default:
        throw new IllegalArgumentException(String.format("The SortType (%1$s) is not supported by the %2$s!", type,
          SorterFactory.class.getSimpleName()));
    }
  }

  /**
   * Creates an instance of the Sorter interface implementing the sorting algorithm based on the SortType,
   * otherwise returns the provided default Sorter implementation if a Sorter based on the specified SortType
   * is not available.
   *
   * @param <T> the Class type of the actual Sorter implementation based on the SortType.
   * @param type the type of sorting algorithm Sorter implementation to create.
   * @param defaultSorter the default Sorter implementation to use if a Sorter based on the specified SortType
   * is not available.
   * @return a Sorter implementation subclass that implements the sorting algorithm based on the SortType,
   * or the provided default Sorter implementation if the Sorter based on the SortType is not available.
   * @see #createSorter(SortType)
   * @see org.cp.elements.util.sort.Sorter
   * @see org.cp.elements.util.sort.SortType
   */
  public static <T extends Sorter> T createSorterElseDefault(final SortType type, final T defaultSorter) {
    try {
      return createSorter(type);
    }
    catch (IllegalArgumentException ignore) {
      return defaultSorter;
    }
  }

}
