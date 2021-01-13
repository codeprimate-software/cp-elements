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

package org.cp.elements.util.search.support;

import java.util.Collection;

import org.cp.elements.util.search.AbstractSearcher;

/**
 * The LinearSearch class is an implementation of the Searcher interface iterating over elements in the collection
 * in a linear manner in search of the first element satisfying the Matcher's criteria.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.search.AbstractSearcher
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class LinearSearch extends AbstractSearcher {

  /**
   * Searches the Collection of elements in order to find the element or elements matching the criteria defined
   * by the Matcher.
   *
   * @param <E> the Class type of elements in the Collection.
   * @param collection the Collection of elements to search.
   * @return the element in the Collection matching the search criteria defined by the Matcher.
   * @see #getMatcher()
   */
  @Override
  public <E> E search(final Collection<E> collection) {
    for (E element : collection) {
      if (getMatcher().isMatch(element)) {
        return element;
      }
    }

    return null;
  }

}
