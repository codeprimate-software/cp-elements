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

import java.util.concurrent.Callable;

/**
 * Interface defining a contract for {@link Object Objects} that can be executed.
 * <p>
 * This {@link Class interface} is similar in purpose and function to the {@link java.lang.Runnable}
 * and {@link java.util.concurrent.Callable} Java {@link Class interfaces}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the execution's {@link Object return value}.
 * @see java.lang.FunctionalInterface
 * @see java.lang.Runnable
 * @see java.util.concurrent.Callable
 * @since 1.0.0
 */
@FunctionalInterface
public interface Executable<T> extends Callable<T>, Runnable {

  /**
   * Indicates whether {@literal this} {@link Object} is running.
   *
   * @return a boolean value indicating whether {@literal this} {@link Object} is running.
   * @throws IllegalStateException if the {@link Object Object's} runnable state cannot be determined.
   */
  default boolean isRunning() {
    throw new IllegalStateException("The runnable state of this object cannot be determined");
  }

  /**
   * Callback method used to execute the intended function encapsulating the logic of the execution.
   *
   * @param args optional array of {@link Object arguments} passed to the execution to perform its function.
   * @return the result of the execution.
   */
  T execute(Object... args);

  /**
   * Computes a result or throws an {@link Exception} if the result could not be computed.
   *
   * @return the computed result.
   * @see #execute(Object...)
   */
  @Override
  default T call() {
    return execute();
  }

  /**
   * Runs this execution to compute a result.
   *
   * @see #execute(Object...)
   */
  @Override
  default void run() {
    execute();
  }
}
