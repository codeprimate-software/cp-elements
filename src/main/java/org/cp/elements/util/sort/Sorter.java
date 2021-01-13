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

import java.util.Comparator;
import java.util.List;

/**
 * The Sorter interface defines a contract for implementing objects that sort a collection of elements.
 *
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see java.util.Comparator
 * @see java.util.List
 * @see org.cp.elements.util.sort.Sortable
 * @see org.cp.elements.util.sort.SorterFactory
 * @see org.cp.elements.util.sort.SortType
 * @see org.cp.elements.util.sort.annotation.Sortable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Sorter {

  /**
   * Gets the Comparator used to order the elements in the collection.
   *
   * @param <E> the type of elements in the collection to compare.
   * @return the Comparator used to order the collection elements.
   * @see java.lang.Comparable
   * @see java.util.Comparator
   */
  <E> Comparator<E> getOrderBy();

  /**
   * Sorts an array of elements as defined by the 'orderBy' Comparator, or as determined by the elements in the array
   * if the elements are Comparable.
   *
   * @param <E> the Class type of elements in the array.
   * @param elements the array of elements to sort.
   * @return the array of elements sorted.
   */
  @SuppressWarnings({ "unchecked", "varargs "})
  <E> E[] sort(E... elements);

  /**
   * Sorts a List of elements as defined by the 'orderBy' Comparator, or as determined by the elements in the collection
   * if the elements are Comparable.
   *
   * @param <E> the Class type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the List of elements sorted.
   * @see java.util.List
   */
  <E> List<E> sort(List<E> elements);

  /**
   * Sorts the List representation of the Sortable implementing object as defined by the 'orderBy' Comparator, or as
   * determined by elements in the Sortable collection if the elements are Comparable.
   *
   * @param <E> the Class type of elements in the Sortable.
   * @param sortable the Sortable implementing object containing the collection of elements to sort.
   * @return the Sortable implementing object sorted.
   * @see org.cp.elements.util.sort.Sortable
   */
  <E> Sortable<E> sort(Sortable<E> sortable);

  /**
   * Sorts the List representation of the @Sortable annotated object as defined by the 'orderBy' Comparator, or as
   * determined by elements in the Sortable collection if the elements are Comparable.
   *
   * @param <T> the Class type of object annotated with the @Sortable annotation.
   * @param sortableAnnotatedObject the @Sortable annotated object containing the collection of elements to sort.
   * @return the @Sortable annotated object sorted.
   * @see org.cp.elements.util.sort.annotation.Sortable
   */
  <T> T sort(T sortableAnnotatedObject);

}
