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

import org.cp.elements.util.sort.AbstractSorter;

/**
 * The ShellSort class is an implementation of the Shell Sort algorithm and Sorter interface.
 *
 * @author John J. Blum
 * @see java.util.List
 * @see org.cp.elements.util.sort.AbstractSorter
 * @link http://en.wikipedia.org/wiki/Shellsort
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ShellSort extends AbstractSorter {

  /**
   * Calculates the gap for the Shell Sort algorithm based on the size of the List of elements.
   *
   * @param elements the List of elements to base the gap on.
   * @return an integer value defining the gap to use in the Shell Sort algorithm.
   * @see java.util.List
   */
  protected int getGap(final List elements) {
    return (elements.size() / 3);
  }

  /**
   * Uses the Shell (Insertion) Sort algorithm to sort a List of elements as defined by the Comparator, or as determined
   * by the elements in the collection if the elements are Comparable.
   *
   * @param <E> the type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the collection of elements sorted.
   * @see java.util.List
   */
  @Override
  public <E> List<E> sort(final List<E> elements) {
    for (int gap = getGap(elements); gap > 0; gap /= 2) {
      for (int i = gap, size = elements.size(); i < size; i++) {
        E elementAtI = elements.get(i);
        int j = i;

        for ( ; j >= gap && getOrderBy().compare(elements.get(j - gap), elementAtI) > 0; j -= gap) {
          elements.set(j, elements.get(j - gap));
        }

        elements.set(j, elementAtI);
      }
    }

    return elements;
  }

}
