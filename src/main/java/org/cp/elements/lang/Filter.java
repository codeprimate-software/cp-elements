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
package org.cp.elements.lang;

import java.util.function.Predicate;

/**
 * The {@link Filter} interface is a {@link Predicate} and defines a contract for an {@link Object} that functions as
 * a {@literal filter} for other {@link Object objects}.
 *
 * @author John J. Blum
 * @see java.lang.FunctionalInterface
 * @see java.util.function.Predicate
 * @since 1.0.0
 */
@FunctionalInterface
public interface Filter<T> extends Predicate<T> {

  /**
   * Returns a {@link Filter} implementation that accepts all {@link Object objects}.
   *
   * @param <T> {@link Class type} of {@link Object objects} being accepted.
   * @return a {@link Filter} implementation accepting all {@link Object objects}.
   * @see org.cp.elements.lang.Filter
   */
  static <T> Filter<T> accepting() {
    return obj -> true;
  }

  /**
   * Returns a {@link Filter} implementation that rejects all {@link Object objects}.
   *
   * @param <T> {@link Class type} of {@link Object objects} being rejected.
   * @return a {@link Filter} implementation rejecting all {@link Object objects}.
   * @see org.cp.elements.lang.Filter
   */
  static <T> Filter<T> rejecting() {
    return obj -> false;
  }

  /**
   * Determines whether the specified object meets the criteria (rules) defined by this Filter.
   *
   * @param obj the Object being evaluated by this Filter.
   * @return a boolean value indicating whether the specified Object satisfies the criteria (rules) of this Filter.
   */
  boolean accept(T obj);

  /**
   * Evaluates this {@link Filter} / {@link Predicate} on the given {@link T argument}.
   *
   * @param obj {@link Object} to evaluate.
   * @return a boolean value indicating whether the given {@link Object argument} matches the criteria
   * defined by this {@link Filter} / {@link Predicate}.
   * @see java.util.function.Predicate#test(Object)
   * @see #accept(Object)
   */
  @Override
  default boolean test(T obj) {
    return accept(obj);
  }
}
