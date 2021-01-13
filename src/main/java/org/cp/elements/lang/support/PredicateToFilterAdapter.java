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

import java.util.function.Predicate;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;

/**
 * {@link PredicateToFilterAdapter} is an Adapter to adapt an instance of the {@link Predicate} interface
 * into an instance of the {@link Filter} interface.
 *
 * @author John Blum
 * @see java.util.function.Predicate
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PredicateToFilterAdapter<T> implements Filter<T> {

  /**
   * Factory method to construct an instance of the {@link PredicateToFilterAdapter} class initialized with
   * the given {@link Predicate} to adapt into an instance of the {@link Filter} interface.
   *
   * @param <T> {@link Class type} of object evaluated by the {@link Predicate}.
   * @param predicate {@link Predicate} to adapt; must not be {@literal null}.
   * @return a new instance of the {@link PredicateToFilterAdapter} wrapping the given {@link Predicate}
   * as an instance of the {@link Filter} interface.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @see #PredicateToFilterAdapter(Predicate)
   * @see java.util.function.Predicate
   */
  public static <T> PredicateToFilterAdapter<T> of(Predicate<T> predicate) {
    return new PredicateToFilterAdapter<>(predicate);
  }

  private final Predicate<T> predicate;

  /**
   * Constructs an instance of the {@link PredicateToFilterAdapter} class initialized with the given {@link Predicate}
   * to adapt into an instance of the {@link Filter} interface.
   *
   * @param predicate {@link Predicate} to adapt; must not be {@literal null}.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @see java.util.function.Predicate
   */
  public PredicateToFilterAdapter(Predicate<T> predicate) {

    Assert.notNull(predicate, "Predicate is required");

    this.predicate = predicate;
  }

  /**
   * Returns a reference to the adapted {@link Predicate}.
   *
   * @return a reference to the adapted {@link Predicate}.
   * @see java.util.function.Predicate
   */
  protected Predicate<T> getPredicate() {
    return this.predicate;
  }

  /**
   * Applied the {@link Predicate} of this {@link Filter} to evaluate the given {@link Object}.
   *
   * @param obj {@link Object} evaluated by this {@link Filter}.
   * @return a boolean value indicating whether the given {@link Object} satisfies the {@link Predicate}
   * of this {@link Filter}.
   * @see java.util.function.Predicate#test(Object)
   * @see #getPredicate()
   */
  @Override
  public boolean accept(T obj) {
    return getPredicate().test(obj);
  }
}
