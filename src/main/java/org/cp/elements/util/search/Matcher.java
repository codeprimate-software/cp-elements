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
package org.cp.elements.util.search;

import org.cp.elements.lang.Filter;

/**
 * Interface defining a contract for objects used to match {@link Object objects} based on search criteria
 * defined by the {@link Matcher} implementation.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of the {@link Object objects} to match while searching.
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.util.search.Searcher
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Matcher<T> extends Filter<T> {

  /**
   * Determines whether the specified object is an exact match to the criteria defined by this Matcher.
   *
   * @param obj the Object being evaluated as a possible match to the criteria of this Matcher.
   * @return a boolean value indicating whether the specified object is an exact match to the criteria
   * defined by this Matcher.
   * @see #match(Object)
   */
  boolean isMatch(T obj);

  /**
   * Determines the value of the specified object relative to the criteria defined by this Matcher.
   *
   * @param obj the Object being evaluated as a possible match to the criteria of this Matcher.
   * @return an integer indicating the object's relative value to the criteria of this Matcher.  Returns zero for an
   * exact match, a negative number if the object is undervalued, and a positive number if the object exceeds the value
   * of the criteria.
   * @see #isMatch(Object)
   */
  int match(T obj);

}
