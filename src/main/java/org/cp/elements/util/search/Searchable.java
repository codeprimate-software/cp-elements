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

package org.cp.elements.util.search;

import java.util.List;

/**
 * The Searchable interface defines a contract for objects of implementing classes to provide a means (java.util.List)
 * by which to search the object.
 *
 * @author John J. Blum
 * @param <T> the Class type of the elements in the List.
 * @see org.cp.elements.util.search.Searcher
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Searchable<T> {

  /**
   * Returns a List representation of the implementing object in order to perform the search.
   *
   * @return a List representation of the implementing object used to perform the search.
   * @see java.util.List
   */
  List<T> asList();

  /**
   * Gets the desired Matcher to use during the search operation performed by the Searcher to match and find elements
   * in the collection.
   *
   * @return the desired Matcher to match and find elements in the collection during the search operation.
   * @see org.cp.elements.util.search.Matcher
   */
  Matcher<T> getMatcher();

}
