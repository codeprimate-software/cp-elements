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

package org.cp.elements.util.sort.support;

import java.util.List;

import org.cp.elements.util.sort.AbstractSorter;

/**
 * The SelectionSort class is a implementation of the Selection Sort algorithm
 * and the {@link org.cp.elements.util.sort.Sorter} interface.
 *
 * @author John J. Blum
 * @see java.util.List
 * @see org.cp.elements.util.sort.AbstractSorter
 * @see org.cp.elements.util.sort.Sorter
 * @see <a href="http://en.wikipedia.org/wiki/Selection_sort">Selection Sort</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SelectionSort extends AbstractSorter {

  /**
   * Uses the Selection Sort algorithm to sort a List of elements as defined by the Comparator, or as determined
   * by the elements in the collection if the elements are Comparable.
   *
   * @param <E> the type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the collection of elements sorted.
   * @see java.util.List
   */
  @Override
  public <E> List<E> sort(final List<E> elements) {
    for (int index = 0, size = elements.size(), length = (size - 1); index < length; index++) {
      int minIndex = index;

      for (int j = (index + 1); j < size; j++) {
        if (getOrderBy().compare(elements.get(j), elements.get(minIndex)) < 0) {
          minIndex = j;
        }
      }

      if (minIndex != index) {
        swap(elements, minIndex, index);
      }
    }

    return elements;
  }

}
