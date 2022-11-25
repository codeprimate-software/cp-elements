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
package org.cp.elements.function;

import java.util.function.Function;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link FunctionalInterface} defining a contract for functions that accept 3 arguments.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the first input value.
 * @param <U> {@link Class type} of the second input value.
 * @param <V> {@link Class type} of the third input value.
 * @param <R> {@link Class type} of the return value.
 * @see java.lang.FunctionalInterface
 * @see java.util.function.BiFunction
 * @see java.util.function.Function
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface TriFunction<T, U, V, R> {

  /**
   * Applies this function to the 3 arguments and returns a computed value.
   *
   * @param t {@link Object first argument} to this function.
   * @param u {@link Object second argument} to this function.
   * @param v {@link Object third argument} to this function.
   * @return the {@link Object computed value} of this function.
   */
  R apply(T t, U u, V v);

  /**
   * Builder method used to {@literal compose} this {@link TriFunction} with the given, required {@link Function}
   * by invoking this function with the given arguments and then processing result applied to the given,
   * required {@link Function}.
   *
   * @param <S> {@link Class type} of the return value after applying the given, required {@link Function}
   * to this function's {@link Object result}.
   * @param after {@link Function} used to apply to the result of calling this function to process the arguments;
   * must not be {@literal null}.
   * @return a composed {@link TriFunction}.
   * @throws IllegalArgumentException if the {@link Function} is {@literal null}.
   * @see <a href="https://en.wikipedia.org/wiki/Composite_pattern">Composite Software Design Pattern</a>
   * @see java.util.function.Function
   */
  @NotNull default <S> TriFunction<T, U, V, S> andThen(@NotNull Function<? super R, ? extends S> after) {
    Assert.notNull(after, "The Function to apply to the result of this TriFunction is required");
    return (t, u, v) -> after.apply(this.apply(t, u, v));
  }
}
