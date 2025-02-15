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
package org.cp.elements.function;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;

import java.util.function.Consumer;

/**
 * {@link Consumer} implementation capable of throwing an {@link Exception} when processing a {@link T target}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} to process.
 * @see java.lang.FunctionalInterface
 * @see java.util.function.Consumer
 * @since 2.0.0
 */
@FunctionalInterface
public interface ThrowableConsumer<T> extends Consumer<T> {

  @Override
  default void accept(T target) {

    try {
      acceptThrowingException(target);
    }
    catch (Exception cause) {
      throw newIllegalStateException(cause, "Failed to consume object [%s]", target);
    }
  }

  /**
   * Consumes (accepts) the given {@link T object} to process.
   *
   * @param target {@link Object} to process.
   * @throws Exception if processing of the {@link T object} fails.
   * @see #accept(Object)
   */
  void acceptThrowingException(T target) throws Exception;

}
