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

import java.util.function.BiFunction;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link BiFeederFunction} {@link FunctionalInterface} defines a {@link BiFunction} type that accepts two arguments,
 * where the first argument is an arbitrary value and the second argument is a possible, merged return value
 * from a {@link BiFeederFunction} proceeding {@literal this} {@link BiFeederFunction} in a composition of
 * {@link BiFeederFunction BiFeederFunctions}.
 * <p>
 * The first {@link BiFeederFunction} in the composition can be passed an initial value or {@literal null}
 * as the second argument. The second argument to all downstream {@link BiFeederFunction BiFeederFunctions}
 * will be the composite value of the second argument to {@literal this} {@link BiFeederFunction}
 * {@link #merge(Object, Object) merged} with the return value from {@link BiFeederFunction} immediately proceeding
 * {@literal this} {@link BiFeederFunction}.
 * <p>
 * Implementors of {@literal this} interface must implement the {@link #merge(Object, Object)} operation.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the input value.
 * @param <R> {@link Class type} of the return value.
 * @see java.lang.FunctionalInterface
 * @see java.util.function.BiFunction
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface BiFeederFunction<T, R> extends BiFunction<T, R, R> {

  /**
   * Applies the logic in {@literal this} {@link BiFunction} to the given arguments, returning the {@link R result}
   * of the computation.
   *
   * @param arg0 first argument of type {@link T} passed to {@literal this} {@link BiFunction}.
   * @param arg1 second argument of type {@link R} passed to {@literal this} {@link BiFunction}.
   * @return the {@link R result} of {@literal this} {@link BiFunction BiFunction's} computation.
   */
  R apply(T arg0, R arg1);

  /**
   * Composes {@literal this} {@link BiFeederFunction} with the given, required {@link BiFunction}.
   * <p>
   * The given {@link BiFunction} will be executed after {@literal this} {@link BiFeederFunction}
   * and the {@link R return value} will be used as input, or the second argument to
   * the given {@link BiFunction}.
   *
   * @param function {@link BiFunction} to compose with {@literal this} {@link BiFeederFunction};
   * must not be {@literal null}.
   * @return a new {@link BiFeederFunction} composed of {@literal this} {@link BiFeederFunction}
   * with the given, required {@link BiFunction}.
   * @throws IllegalArgumentException if the given {@link BiFunction} is {@literal null}.
   * @see java.util.function.BiFunction
   */
  default BiFeederFunction<T, R> andThen(@NotNull BiFunction<T, R, R> function) {

    Assert.notNull(function,
      "The BiFunction to compose with and apply after this BiFeederFunction [%s] is required",
      getClass().getName());

    return (argument, returnValue) ->
      function.apply(argument, this.merge(returnValue, this.apply(argument, returnValue)));
  }

  /**
   * Merges two {@link R argument} values into a single {@link R value}.
   * <p>
   * This method return {@literal this} {@link BiFeederFunction BiFeederFunction's}
   * {@link R return value} by default.
   *
   * @param upstreamFunctionReturnValue first {@link R argument} in the {{@literal merge} operation.
   * @param thisFunctionReturnValue second {@link R argument} in the {{@literal merge} operation.
   * @return the {@literal merged} {@link R argument} values.
   */
  default R merge(R upstreamFunctionReturnValue, R thisFunctionReturnValue) {
    return thisFunctionReturnValue;
  }
}
