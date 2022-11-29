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

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.sort.AbstractSorter;

/**
 * {@link AbstractSorter} implementation of the {@literal Comb Sort algorithm}.
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
  public @NotNull <E> List<E> sort(@NotNull List<E> elements) {

    boolean swapped = true;

    int size = elements.size();
    int gap = size;

    while (gap > 0 && swapped) {

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
