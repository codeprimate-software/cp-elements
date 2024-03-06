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
 * Java {@link IllegalArgumentException} thrown when two {@link Object objects} are not equal.
 *
 * @author John J. Blum
 * @see java.lang.IllegalArgumentException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class EqualityException extends IllegalArgumentException {

  /**
   * Constructs a new instance of {@link EqualityException}.
   */
  public EqualityException() {
  }

  /**
   * Constructs a new {@link EqualityException} initialized with the given {@link String message}
   * describing the equality error.
   *
   * @param message {@link String} describing the equality error.
   */
  public EqualityException(final String message) {
    super(message);
  }

  /**
   * Constructs a new {@link EqualityException} initialized with the given {@link Throwable}
   * used as the cause of this equality error.
   *
   * @param cause {@link Throwable} used as the cause of this equality error.
   */
  public EqualityException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link EqualityException} initialized with the given {@link String message}
   * describing the equality error along with the given {@link Throwable} used as the cause
   * of this equality error.
   *
   * @param message {@link String} describing the equality error.
   * @param cause {@link Throwable} used as the cause of this equality error.
   */
  public EqualityException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
