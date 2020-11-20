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
package org.cp.elements.lang;

import java.util.concurrent.Callable;

/**
 * The {@link Executable} interface defines a contract for objects that can be executed.
 *
 * This {@link Class interface} is similar in purpose and function to the {@link java.lang.Runnable}
 * and {@link java.util.concurrent.Callable} {@link Class interfaces}.
 *
 * @author John Blum
 * @param <T> {@link Class} type of the executions return value.
 * @see java.lang.FunctionalInterface
 * @see java.lang.Runnable
 * @see java.util.concurrent.Callable
 * @since 1.0.0
 */
@FunctionalInterface
public interface Executable<T> extends Callable<T>, Runnable {

  /**
   * Indicates whether this object is running.
   *
   * @return a boolean value indicating whether this object is running.
   */
  default boolean isRunning() {
    throw new IllegalStateException("The runnable state of this object cannot be determined");
  }

  /**
   * Callback method to execute the intended function/logic encapsulated in the execution.
   *
   * @param args optional array of {@link Object arguments} passed to the execution to carry out its function.
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
