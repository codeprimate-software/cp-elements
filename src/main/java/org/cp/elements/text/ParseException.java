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
 * The ParseException class is a RuntimeException that indicates a problem with parsing textual data.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ParseException extends RuntimeException {

  /**
   * Default constructor to create an uninitialized instance of the ParseException class.
   */
  public ParseException() {
  }

  /**
   * Constructs an instance of the ParseException class with a message describing the parse error.
   *
   * @param message a String describing the parsing error.
   */
  public ParseException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the ParseException class along with the underlying cause of the parse error.
   *
   * @param cause a Throwable indicating the cause of the underlying parsing error.
   */
  public ParseException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the ParseException class with a message describing the parse error along with
   * the underlying cause of the parsing error.
   *
   * @param message a String describing the parsing error.
   * @param cause a Throwable indicating the cause of the underlying parsing error.
   */
  public ParseException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
