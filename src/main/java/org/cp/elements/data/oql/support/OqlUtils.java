/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.data.oql.support;

import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Predicate;

import org.cp.elements.data.oql.QueryArguments;
import org.cp.elements.data.oql.QueryContext;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * Abstract utility class for OQL.
 *
 * @author John Blum
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public abstract class OqlUtils {

  public static final BiPredicate<QueryArguments, ?> ACCEPT_ALL_QUERY_PREDICATE = (queryArguments, target) -> true;

  /**
   * Converts the given {@link Function} into a {@link BiFunction} with no regard for a {@link QueryContext}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param function {@link Function} being converted to a {@link BiFunction}; required.
   * @return a new {@link BiFunction} converted from the given {@link Function}.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.BiFunction
   * @see java.util.function.Function
   */
  public static <S, T> BiFunction<QueryContext<S, T>, S, T> asBiFunction(@NotNull Function<S, T> function) {
    Assert.notNull(function, "Function is required");
    return (queryContext, target) -> function.apply(target);
  }

  /**
   * Converts the given {@link BiFunction} into a {@link Function} called with the given {@link QueryContext}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param function {@link BiFunction} being converted to a {@link Function}; required.
   * @param queryContext {@link QueryContext} passed to the given {@link BiFunction}
   * when the returned {@link Function} is called; required
   * @return a new {@link Function} converted from the given {@link BiFunction}.
   * @throws IllegalArgumentException if {@link BiFunction} or {@link QueryContext} are {@literal null}.
   * @see java.util.function.BiFunction
   * @see java.util.function.Function
   * @see QueryContext
   */
  public static <S, T> Function<S, T> asFunction(@NotNull BiFunction<QueryContext<S, T>, S, T> function,
      @NotNull QueryContext<S, T> queryContext) {

    Assert.notNull(function, "Function is required");
    Assert.notNull(queryContext, "QueryContext is required");

    return target -> function.apply(queryContext, target);
  }

  /**
   * Converts the given {@link Predicate} into a {@link BiPredicate} with no {@link QueryArguments}.
   *
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param predicate {@link Predicate} to convert into a {@link BiPredicate}; required.
   * @return a new {@link BiPredicate} converted from the given {@link Predicate}.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @see java.util.function.BiPredicate
   * @see java.util.function.Predicate
   * @see QueryArguments
   */
  public static <T> BiPredicate<QueryArguments, T> asBiPredicate(@NotNull Predicate<T> predicate) {
    Assert.notNull(predicate, "Predicate is required");
    return (queryArguments, target) -> predicate.test(target);
  }

  /**
   * Converts the given {@link BiPredicate} into a {@link Predicate} with no {@link QueryArguments}.
   *
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param predicate {@link BiPredicate} to convert into a {@link Predicate}; required.
   * @return a new {@link Predicate} converted from the given {@link BiPredicate}.
   * @throws IllegalArgumentException if {@link BiPredicate} is {@literal null}.
   * @see #asPredicate(BiPredicate, QueryArguments)
   * @see java.util.function.BiPredicate
   * @see java.util.function.Predicate
   * @see QueryArguments
   */
  public static <T> Predicate<T> asPredicate(@NotNull BiPredicate<QueryArguments, T> predicate) {
    return asPredicate(predicate, QueryArguments.empty());
  }

  /**
   * Converts the given {@link BiPredicate} into a {@link Predicate} called with the given {@link QueryArguments}.
   *
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param predicate {@link BiPredicate} to convert into a {@link Predicate}; required.
   * @param queryArguments {@link QueryArguments} passed to the {@link BiPredicate}
   * when the {@link Predicate} is called; required.
   * @return a new {@link Predicate} converted from the given {@link BiPredicate}.
   * @throws IllegalArgumentException if {@link BiPredicate} or {@link QueryArguments} are {@literal null}.
   * @see java.util.function.BiPredicate
   * @see java.util.function.Predicate
   * @see #asPredicate(BiPredicate)
   * @see QueryArguments
   */
  public static <T> Predicate<T> asPredicate(@NotNull BiPredicate<QueryArguments, T> predicate,
      @NotNull QueryArguments queryArguments) {

    Assert.notNull(predicate, "Predicate is required");
    Assert.notNull(queryArguments, "QueryArguments are required");

    return target -> predicate.test(queryArguments, target);
  }

  /**
   * Returns the given {@link BiPredicate} if not {@literal null}, otherwise returns a default,
   * accepting {@link BiPredicate}.
   *
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param predicate {@link BiPredicate} to guard against {@literal null}.
   * @return the given {@link BiPredicate} if not {@literal null}, otherwise returns a default,
   * accepting {@link BiPredicate}.
   * @see java.util.function.BiPredicate
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  public static <T> BiPredicate<QueryArguments, T> nullSafePredicate(BiPredicate<QueryArguments, T> predicate) {
    return predicate != null ? predicate : (BiPredicate<QueryArguments, T>) ACCEPT_ALL_QUERY_PREDICATE;
  }
}
