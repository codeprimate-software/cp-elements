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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.cp.elements.util.sort.AbstractSorter;

/**
 * The JavaMergeSort class is an implementation of the Merge Sort algorithm implemented using the Java API.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.sort.AbstractSorter
 * @see java.util.Arrays#sort(Object[], java.util.Comparator)
 * @see java.util.Collections#sort(java.util.List, java.util.Comparator)
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class JavaMergeSort extends AbstractSorter {

  /**
   * Uses Java's modified Merge Sort to sort an array of elements as defined by the Comparator, or as determined
   * by the elements in the array if the elements are Comparable.
   *
   * @param <E> the type of elements in the array.
   * @param elements the array of elements to sort.
   * @return the array of elements sorted.
   * @see #getOrderBy()
   * @see java.util.Arrays#sort(Object[], java.util.Comparator)
   */
  @Override
  @SuppressWarnings({ "unchecked", "varargs" })
  public <E> E[] sort(final E... elements) {
    Arrays.sort(elements, getOrderBy());
    return elements;
  }

  /**
   * Uses Java's modified Merge Sort algorithm to sort a List of elements as defined by the Comparator, or as determined
   * by the elements in the collection if the elements are Comparable.
   *
   * @param <E> the type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the collection of elements sorted.
   * @see #getOrderBy()
   * @see java.util.Collections#sort(java.util.List, java.util.Comparator)
   * @see java.util.List
   */
  @Override
  public <E> List<E> sort(final List<E> elements) {
    Collections.sort(elements, getOrderBy());
    return elements;
  }

}
