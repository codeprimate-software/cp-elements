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

/**
 * Java {@link RuntimeException} thrown when an {@lin Object} is not of the expected {@link Class type}.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class IllegalTypeException extends RuntimeException {

  /**
   * Constructs a new, uninitialized instance of {@link IllegalTypeException}.
   */
  public IllegalTypeException() {
  }

  /**
   * Constructs a new {@link IllegalTypeException} initialized with the given {@link String message}
   * describing this exception.
   *
   * @param message {@link String} describing this exception.
   */
  public IllegalTypeException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link IllegalTypeException} initialized with the given {@link Throwable}
   * used as the cause of this exception.
   *
   * @param cause a {@link Throwable} object used as caused this exception.
   */
  public IllegalTypeException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link IllegalTypeException} initialized with the given {@link String message}
   * describing this exception along with the {@link Throwable} used as the cause of this exception.
   *
   * @param message {@link String} describing this exception.
   * @param cause a {@link Throwable} object used as caused this exception.
   */
  public IllegalTypeException(String message, Throwable cause) {
    super(message, cause);
  }
}
