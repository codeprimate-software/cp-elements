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

/**
 * Abstract base class encapsulating functionality common to all {@link Matcher} implementations.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of the {@link Object objects} to match while searching.
 * @see org.cp.elements.util.search.Matcher
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractMatcher<T> implements Matcher<T> {

  /**
   * Determines whether the specified object is "accepted", or matched by the criteria defined by this Matcher.
   *
   * @param obj the Object being evaluated as a possible match to the criteria of this Matcher.
   * @return a boolean value indicating whether the specified object is "accepted", or matched by the criteria
   * defined by this Matcher.
   * @see #isMatch(Object)
   */
  @Override
  public boolean accept(final T obj) {
    return isMatch(obj);
  }

  /**
   * Determines whether the specified object is an exact match to the criteria defined by this Matcher.
   *
   * @param obj the Object being evaluated as a possible match to the criteria of this Matcher.
   * @return a boolean value indicating whether the specified object is an exact match to the criteria
   * defined by this Matcher.
   * @see #match(Object)
   */
  @Override
  public boolean isMatch(final T obj) {
    return (match(obj) == 0);
  }
}
