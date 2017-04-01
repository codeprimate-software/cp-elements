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

/**
 * The {@link Executable} interface defines a contract for implementing objects enabling them to be executed.
 * This {@link Class interface} is similar in purpose and function to the {@link Runnable}
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
public interface Executable<T> {

  /**
   * Callback method to execute the intended function/logic encapsulated in the execution.
   *
   * @return the result of the execution.
   */
  T execute();

}
