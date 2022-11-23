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

  @SuppressWarnings("rawtypes")
  public static final DefaultFilter ACCEPT = new DefaultFilter<>(true);

  @SuppressWarnings("rawtypes")
  public static final DefaultFilter REJECT = new DefaultFilter<>(false);

  private final boolean acceptReturnValue;

  /**
   * Returns an instance of {@link DefaultFilter} initialized with the specified, default return value
   * for the {@link DefaultFilter#accept(Object)} method.
   *
   * @param <T> the Class type of the object to filter.
   * @param acceptReturnValue the result of call the {@link DefaultFilter#accept(Object)} method.
   * @return an instance of the {@link DefaultFilter} initialized with the default return value
   * for the {@link DefaultFilter#accept(Object)} method.
   */
  @SuppressWarnings("unchecked")
  public static <T> DefaultFilter<T> getInstance(boolean acceptReturnValue) {
    return acceptReturnValue ? ACCEPT : REJECT;
  }

  /**
   * Constructs an instance of the DefaultFilter class with the given boolean value for the accept's methods result.
   *
   * @param acceptReturnValue a boolean value defining the result of the accept method.
   */
  private DefaultFilter(boolean acceptReturnValue) {
    this.acceptReturnValue = acceptReturnValue;
  }

  /**
   * Determines the result of calling the {@link DefaultFilter#accept(Object)} method on any type of object.
   *
   * @return a boolean value indicating the result of the accept method.
   */
  boolean isAccepting() {
    return this.acceptReturnValue;
  }

  /**
   * Determines whether the given object satisfies the criteria (rules) defined by this Filter.
   *
   * @param obj the Object being evaluated by this {@link Filter}.
   * @return a boolean value indicating whether the given Object satisfies the criteria (rules) of this {@link Filter}.
   * @see #isAccepting()
   */
  public boolean accept(T obj) {
    return isAccepting();
  }
}
