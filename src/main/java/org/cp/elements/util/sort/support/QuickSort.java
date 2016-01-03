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

import java.util.List;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.util.sort.AbstractSorter;
import org.cp.elements.util.sort.Sorter;

/**
 * The QuickSort class is an implementation of the Quick Sort algorithm
 * and the {@link org.cp.elements.util.sort.Sorter} interface.
 *
 * @author John J. Blum
 * @see java.util.List
 * @see org.cp.elements.util.sort.AbstractSorter
 * @see org.cp.elements.util.sort.Sorter
 * @see org.cp.elements.util.sort.support.InsertionSort
 * @see <a href="http://en.wikipedia.org/wiki/Quicksort">Quicksort</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class QuickSort extends AbstractSorter {

  protected static final int DEFAULT_SIZE_THRESHOLD = 7;

  protected static final Sorter DEFAULT_SORTER = new InsertionSort();

  private int sizeThreshold = DEFAULT_SIZE_THRESHOLD;

  private Sorter sorter = DEFAULT_SORTER;

  /**
   * Gets the size threshold used to stop the recursion of the Quick Sort and resort to another sorting algorithm
   * as determined by the Sorter property.
   *
   * @return an integer value indicating the size threshold in terms of the remaining unsorted elements from the
   * collection to use a non-recursive sorting algorithm on.  Returns the DEFAULT_THRESHOLD value if the user
   * specified size threshold is less than the default.
   * @see #DEFAULT_SIZE_THRESHOLD
   * @see #setSizeThreshold(int)
   * @see #getSorter()
   */
  public int getSizeThreshold() {
    return Math.max(sizeThreshold, DEFAULT_SIZE_THRESHOLD);
  }

  /**
   * Sets the size threshold used to stop the recursion of the Quick Sort and resort to another sorting algorithm
   * as determined by the Sorter property.
   *
   * @param sizeThreshold an integer value indicating the size threshold in terms of the remaining unsorted elements
   * from the collection to use a non-recursive sorting algorithm on.
   * @see #getSizeThreshold()
   */
  public void setSizeThreshold(final int sizeThreshold) {
    this.sizeThreshold = sizeThreshold;
  }

  /**
   * Gets the Sorter implementation to use when the remaining elements from the collection to sort using the Quick Sort
   * algorithm is less than equal to the size threshold.
   *
   * @return the alternate sorting algorithm implementation, or the default sorting algorithm when the user defined
   * Sorter is unspecified, or has not been configured.
   * @see #DEFAULT_SORTER
   * @see #setSorter(org.cp.elements.util.sort.Sorter)
   * @see #getSizeThreshold()
   * @see org.cp.elements.util.sort.Sorter
   */
  public Sorter getSorter() {
    return ObjectUtils.defaultIfNull(sorter, DEFAULT_SORTER);
  }

  /**
   * Sets the Sorter implementation to use when the remaining elements from the collection to sort using the Quick Sort
   * algorithm is less than equal to the size threshold.
   *
   * @param sorter an alternate sorting algorithm implementation to use when the size threshold is reached during the
   * Quick Sort.
   * @see #getSorter()
   * @see org.cp.elements.util.sort.Sorter
   */
  public void setSorter(final Sorter sorter) {
    this.sorter = sorter;
  }

  /**
   * Uses the Quick Sort algorithm to sort a List of elements as defined by the Comparator, or as determined
   * by the elements in the collection if the elements are Comparable.
   *
   * @param <E> the type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the collection of elements sorted.
   * @see java.util.List
   */
  @Override
  public <E> List<E> sort(final List<E> elements) {
    int elementsSize = elements.size();

    if (elementsSize <= getSizeThreshold()) {
      return getSorter().sort(elements);
    }
    else {
      int beginIndex = 1;
      int endIndex = (elementsSize - 1);

      E pivotElement = elements.get(0);

      while (beginIndex < endIndex) {
        while (beginIndex < endIndex && getOrderBy().compare(elements.get(beginIndex), pivotElement) <= 0) {
          beginIndex++;
        }

        while (endIndex >= beginIndex && getOrderBy().compare(elements.get(endIndex), pivotElement) >= 0) {
          endIndex--;
        }

        if (beginIndex < endIndex) {
          swap(elements, beginIndex, endIndex);
        }
      }

      swap(elements, 0, endIndex);
      sort(elements.subList(0, endIndex));
      sort(elements.subList(endIndex + 1, elementsSize));

      return elements;
    }
  }

}
