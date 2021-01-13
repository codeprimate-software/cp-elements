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
 * The ComparisonException class is an IllegalArgumentException that indicates two Comparable objects failed
 * a relational comparison.
 *
 * @author John J. Blum
 * @see java.lang.IllegalArgumentException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComparisonException extends IllegalArgumentException {

  /**
   * Default constructor creating an uninitialized instance of ComparisonException.
   */
  public ComparisonException() {
  }

  /**
   * Constructs an instance of ComparisonException with the given message describing the relational comparison error.
   *
   * @param message a String value describing the nature of the relational comparison error.
   */
  public ComparisonException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of ComparisonException with the given Throwable indicating the cause
   * of the relational comparison error.
   *
   * @param cause the Throwable indicated as the cause of this relational comparison error.
   */
  public ComparisonException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of ComparisonException with a message describing the relational comparison error
   * and a Throwable indicating the probable cause of the relational comparison error.
   *
   * @param message a String value describing the nature of the relational comparison error.
   * @param cause the Throwable indicated as the cause of this relational comparison error.
   */
  public ComparisonException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
