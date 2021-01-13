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
 * The {@link DataAccessException} class is a {@link RuntimeException} indicating a data access operation failed.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DataAccessException extends RuntimeException {

  /**
   * Default constructor used to create a new instance of the {@link DataAccessException}.
   *
   * The {@link DataAccessException} is uninitialized, with no {@link String message} or {@link Throwable cause}.
   */
  public DataAccessException() {
  }

  /**
   * Constructs a new instance of {@link DataAccessException} initialized with the given {@link String message}
   * to describe the data access error.
   *
   * @param message {@link String} describing the cause of the data access error.
   * @see java.lang.String
   */
  public DataAccessException(String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link DataAccessException} initialized with the given {@link Throwable}
   * to indicate the {@literal cause} of the data access error.
   *
   * @param cause {@link Throwable} indicating the cause of the data access error.
   * @see java.lang.Throwable
   */
  public DataAccessException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link DataAccessException} initialized with the given {@link String message}
   * to describe the data access error and the given {@link Throwable} as the {@literal cause} of the data access error.
   *
   * @param message {@link String} describing the cause of the data access error.
   * @param cause {@link Throwable} indicating the cause of the data access error.
   * @see java.lang.Throwable
   * @see java.lang.String
   */
  public DataAccessException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
