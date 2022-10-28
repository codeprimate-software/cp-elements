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
package org.cp.elements.dao;

/**
 * A Java {@link RuntimeException} indicating an error occurred during a data access operation.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DataAccessException extends RuntimeException {

  /**
   * Constructs a new, uninitialized instance of {@link DataAccessException} having no {@link String message}
   * and no {@link Throwable cause}.
   */
  public DataAccessException() { }

  /**
   * Constructs a new instance of {@link DataAccessException} initialized with the given {@link String message}
   * describing the data access error.
   *
   * @param message {@link String} describing the data access error.
   */
  public DataAccessException(String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link DataAccessException} initialized with the given {@link Throwable}
   * used as the {@literal cause} of the data access error.
   *
   * @param cause {@link Throwable} used as the cause of the data access error.
   * @see java.lang.Throwable
   */
  public DataAccessException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link DataAccessException} initialized with the given {@link String message}
   * describing the data access error along with the given {@link Throwable} used as the {@literal cause}
   * of the data access error.
   *
   * @param message {@link String} describing the data access error.
   * @param cause {@link Throwable} used as the cause of the data access error.
   * @see java.lang.Throwable
   */
  public DataAccessException(final String message, final Throwable cause) {
    super(message, cause);
  }
}
