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

package org.cp.elements.dao;

/**
 * The DataAccessException class is a RuntimeException indicating that a data access operation was unsuccessful.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DataAccessException extends RuntimeException {

  /**
   * Default constructor creating an instance of the DataAccessException.
   */
  public DataAccessException() {
  }

  /**
   * Constructor to create an instance of the DataAccessException with a given message to describe the
   * data access error.
   *
   * @param message a String value describing the nature of the data access error.
   */
  public DataAccessException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the DataAccessException with the given Throwable indicating the cause
   * of the data access error.
   *
   * @param cause a Throwable indicated as the cause of this data access error.
   */
  public DataAccessException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the DataAccessException with both a message to describe the
   * data access error along with a Throwable indicating the probably cause of the data access error.
   *
   * @param message a String value describing the nature of the data access error.
   * @param cause a Throwable indicated as the cause of this data access error.
   */
  public DataAccessException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
