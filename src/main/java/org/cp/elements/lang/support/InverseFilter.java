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

package org.cp.elements.lang.support;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;

/**
 * The InverseFilter class negates the outcome of the target Filter wrapped by an instance of this class.
 *
 * @author John J. Blum
 * @param <T> the Class type of Objects evaluated by this Filter.
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.ComposableFilter
 * @see org.cp.elements.lang.support.DefaultFilter
 * @since 1.0.0
 */
public final class InverseFilter<T> implements Filter<T> {

  private final Filter<T> filter;

  /**
   * Constructs an instance of the InverseFilter wrapping the specified Filter object in order to negate the
   * Filter's outcome.
   *
   * @param filter the Filter object being wrapped by an instance of the InverseFilter.
   */
  public InverseFilter(final Filter<T> filter) {
    Assert.notNull(filter, "The target Filter being wrapped by the InverseFilter cannot be null!");
    this.filter = filter;
  }

  /**
   * Gets the target Filter object wrapped by this InverseFilter.  This package-private method is provided for
   * testing purposes.
   *
   * @return the Filter object wrapped by this InverseFilter.
   */
  final Filter<T> getFilter() {
    return this.filter;
  }

  /**
   * Determines whether the specified object meets the criteria (rules) defined by this Filter.
   *
   * @param obj the Object being evaluated by this Filter.
   * @return a boolean value indicating whether the specified Object satisfies the criteria (rules) of this Filter.
   */
  public boolean accept(final T obj) {
    return !getFilter().accept(obj);
  }

}
