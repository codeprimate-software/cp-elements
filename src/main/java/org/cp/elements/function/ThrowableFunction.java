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

import static org.cp.elements.lang.ElementsExceptionsFactory.newFunctionExecutionException;

import java.util.function.Function;

/**
 * Java {@link Function} implementation capable of throwing an {@link Throwable exception} during execution.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the {@link Function Function's} input argument.
 * @param <R> {@link Class type} of the {@link Function Function's} computed result and return value.
 * @see java.lang.FunctionalInterface
 * @see java.util.function.Function
 * @since 2.0.0
 */
@FunctionalInterface
public interface ThrowableFunction<T, R> extends Function<T, R> {

  @Override
  default R apply(T input) {

    try {
      return applyWithThrows(input);
    }
    catch (Throwable cause) {
      throw newFunctionExecutionException(cause, "Failed to execute Function [%s]", getClass().getSimpleName());
    }
  }

  /**
   * Executes the logic of this {@link Function} with the possibility that an {@link Throwable exception} may be thrown.
   *
   * @param input {@link Object} of {@link T type T} passed as the argument to this {@link Function}.
   * @return the {@link R computation (result)} of this {@link Function}.
   * @throws Throwable if the {@link Function} execution fails.
   * @see #apply(Object)
   */
  R applyWithThrows(T input) throws Throwable;

}
