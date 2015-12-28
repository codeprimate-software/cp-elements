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

package org.cp.elements.lang.reflect;

/**
 * The FieldAccessException class is a RuntimeException indicating a field access error when the field of a object
 * is either read or written.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class FieldAccessException extends RuntimeException {

  /**
   * Constructs an uninitialized instance of the FieldAccessException.
   */
  public FieldAccessException() {
  }

  /**
   * Constructs an instance of the FieldAccessException initialized with the given message describing
   * the field access error.
   *
   * @param message a String describing the nature of the field access error.
   */
  public FieldAccessException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the FieldAccessException initialized with the given Throwable to indicate
   * the cause of the field access error.
   *
   * @param cause the Throwable indicating the cause of the field access error.
   * @see java.lang.Throwable
   */
  public FieldAccessException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the FieldAccessException initialized with the given message describing
   * the field access error along with the Throwable indicating the probable cause of the field access error.
   *
   * @param message a String describing the nature of the field access error.
   * @param cause the Throwable indicating the cause of the field access error.
   * @see java.lang.Throwable
   */
  public FieldAccessException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
