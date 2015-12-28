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

import java.util.Comparator;
import java.util.List;

/**
 * The Sortable interface defines a contract for objects of implementing classes to provide a means (java.util.List)
 * by which to sort the object.
 *
 * @author John J. Blum
 * @param <T> the Class type of the elements in the List.
 * @see org.cp.elements.util.sort.Sorter
 * @see org.cp.elements.util.sort.annotation.Sortable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Sortable<T> {

  /**
   * Returns a List representation of the implementing object in order to perform the sort.
   *
   * @return a List representation of the implementing object used to perform the sort.
   * @see java.util.List
   */
  List<T> asList();

  /**
   * Property defining the Comparator class to use when sorting and ordering the elements in the collection (List).
   *
   * @return a Comparator class type used to sort and order the elements in the collection (List) during the sort.
   * @see java.util.Comparator
   */
  Comparator<T> getOrderBy();

}
