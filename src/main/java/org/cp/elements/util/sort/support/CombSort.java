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
 * The CombSort class is an implementation of the Comb Sort algorithm
 * and the {@link org.cp.elements.util.sort.Sorter} interface.
 *
 * @author John J. Blum
 * @see java.util.List
 * @see org.cp.elements.util.sort.AbstractSorter
 * @see org.cp.elements.util.sort.Sorter
 * @see <a href="http://en.wikipedia.org/wiki/Comb_sort">Comb Sort</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CombSort extends AbstractSorter {

  protected static final double SHRINK = 1.3d;

  /**
   * Uses the Comb (Bubble) Sort algorithm to sort a List of elements as defined by the Comparator, or as determined
   * by the elements in the collection if the elements are Comparable.
   *
   * @param <E> the type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the collection of elements sorted.
   * @see java.util.List
   */
  @Override
  public <E> List<E> sort(final List<E> elements) {
    boolean swapped = true;

    for (int size = elements.size(), gap = size; gap > 0 && swapped; ) {
      gap = Math.max(1, (int) Math.floor(gap / SHRINK));
      swapped = false;

      for (int index = 0; index + gap < size; index++) {
        if (getOrderBy().compare(elements.get(index), elements.get(index + gap)) > 0) {
          swap(elements, index, index + gap);
          swapped = true;
        }
      }
    }

    return elements;
  }

}
