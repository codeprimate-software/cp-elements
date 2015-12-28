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
 * The EqualityException class is an IllegalArgumentException indicating that two objects are not equal.
 * 
 * @author John J. Blum
 * @see java.lang.IllegalArgumentException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class EqualityException extends IllegalArgumentException {

  /**
   * Default constructor creating an instance of the EqualityException.
   */
  public EqualityException() {
  }

  /**
   * Constructor to create an instance of the EqualityException with the given message to describe the equality
   * comparison error.
   * 
   * @param message a String value describing the nature of the equality comparison error.
   */
  public EqualityException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the EqualityException with the given Throwable to indicate the cause
   * of the equality comparison error.
   * 
   * @param cause the Throwable indicated as the cause of this equality comparison error.
   */
  public EqualityException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the EqualityException with a message to describe the equality comparison error
   * and a Throwable to indicate the probable cause of the equality comparison error.
   * 
   * @param message a String value describing the nature of the equality comparison error.
   * @param cause the Throwable indicated as the cause of this equality comparison error.
   */
  public EqualityException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
