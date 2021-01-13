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

package org.cp.elements.text;

/**
 * The FormatException class is a RuntimeException that indicates a problem with formatting textual data.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class FormatException extends RuntimeException {

  /**
   * Default constructor to create an uninitialized instance of the FormatException class.
   */
  public FormatException() {
  }

  /**
   * Constructs an instance of the FormatException class with a message describing the formatting error.
   *
   * @param message a String describing the parsing error.
   */
  public FormatException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the FormatException class along with the underlying cause of the formatting error.
   *
   * @param cause a Throwable indicating the cause of the underlying formatting error.
   */
  public FormatException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the FormatException class with a message describing the formatting error along with
   * the underlying cause of the formatting error.
   *
   * @param message a String describing the formatting error.
   * @param cause a Throwable indicating the cause of the underlying formatting error.
   */
  public FormatException(final String message, Throwable cause) {
    super(message, cause);
  }

}
