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

package org.cp.elements.lang.support;

import org.cp.elements.lang.Filter;

/**
 * The DefaultFilter class is a Filter implementation that allows the user to pre-define the outcome of the Filter's
 * evaluation (as determined by the accept method).
 *
 * @author John J. Blum
 * @param <T> the Class type of Objects evaluated by this Filter.
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.ComposableFilter
 * @see org.cp.elements.lang.support.InverseFilter
 * @since 1.0.0
 */
public final class DefaultFilter<T> implements Filter<T> {

  public static final boolean DEFAULT_ACCEPT_RESULT = true;

  private final boolean acceptResult;

  /**
   * Constructs an instance of the DefaultFilter class with the default accept result of true.
   */
  public DefaultFilter() {
    this(DEFAULT_ACCEPT_RESULT);
  }

  /**
   * Constructs an instance of the DefaultFilter class with the given boolean value for the accept's methods result.
   *
   * @param acceptResult a boolean value defining the result of the accept method.
   */
  public DefaultFilter(final boolean acceptResult) {
    this.acceptResult = acceptResult;
  }

  /**
   * Determines the result of calling the accept method on any type of object.
   *
   * @return a boolean value indicating the default return result for the accept method.
   */
  final boolean isAccepting() {
    return this.acceptResult;
  }

  /**
   * Determines whether the specified object meets the criteria (rules) defined by this Filter.
   *
   * @param obj the Object being evaluated by this Filter.
   * @return a boolean value indicating whether the specified Object satisfies the criteria (rules) of this Filter.
   */
  public boolean accept(final T obj) {
    return acceptResult;
  }

}
