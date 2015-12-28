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
 * CloneException is a RuntimeException type indicating a clone, or copy operation failure.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CloneException extends RuntimeException {

  /**
   * Constructs an uninitialized instance of the CloneException.
   */
  public CloneException() {
  }

  /**
   * Constructor to create an instance of the CloneException with a given message describing the clone error.
   *
   * @param message a String value describing the nature of the clone error.
   */
  public CloneException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the CloneException with the given Throwable to indicate the cause
   * of the clone error.
   *
   * @param cause the Throwable indicating the cause of the clone error.
   * @see java.lang.Throwable
   */
  public CloneException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the CloneException with a message describing the clone error
   * and a Throwable to indicate the probable cause of the clone error.
   *
   * @param message a String value describing the nature of the clone error.
   * @param cause the Throwable indicating the cause of the clone error.
   * @see java.lang.Throwable
   */
  public CloneException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
