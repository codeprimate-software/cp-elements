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

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * {@link Supplier} implementation capable of throwing an {@link Exception} when supplying a value.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object value} supplied by this {@link Supplier}.
 * @see java.lang.FunctionalInterface
 * @see java.util.function.Supplier
 * @since 2.0.0
 */
@FunctionalInterface
public interface ThrowableSupplier<T> extends Supplier<T> {

  /**
   * Factory method used to {@link Supplier#get} the {@link T value} safely.
   *
   * @param <T> {@link Class type} of the {@link Object value} returned by the {@link ThrowableSupplier}.
   * @param supplier {@link ThrowableSupplier} to get the {@link T value} from.
   * @param exceptionHandler {@link Function} used to handle the {@link Exception}
   * thrown by the {@link ThrowableSupplier}.
   * @return the {@link T value} from the {@link ThrowableSupplier}.
   * @see java.util.function.Function
   */
  static <T> T getSafely(ThrowableSupplier<T> supplier, Function<Exception, T> exceptionHandler) {

    try {
      return supplier.get();
    }
    catch (Exception cause) {
      return exceptionHandler.apply(cause);
    }
  }

  @Override
  default T get() {

    try {
      return getThrowingException();
    }
    catch (Exception cause) {
      throw newIllegalStateException(cause, "Failed to get supplied value");
    }
  }

  /**
   * Return a {@link T value} computed by this {@link Supplier}.
   *
   * @return a {@link T value} computed by this {@link Supplier}.
   * @throws Exception if {@link T value} cannot be computed.
   * @see #get()
   */
  T getThrowingException() throws Exception;

}
