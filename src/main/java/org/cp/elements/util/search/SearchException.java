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

package org.cp.elements.util.search;

/**
 * The SearchException class is a RuntimeException that is thrown when a search error occurs.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SearchException extends RuntimeException {

  /**
   * Default constructor to create an uninitialized instance of the SearchException class.
   */
  public SearchException() {
  }

  /**
   * Constructs an instance of the SearchException class with a message describing the searching failure.
   *
   * @param message a String describing the searching failure.
   */
  public SearchException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the SearchException class with the underlying cause of the searching failure.
   *
   * @param cause a Throwable indicating the underlying cause of the searching failure.
   */
  public SearchException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the SearchException class with a message describing the soring failure along
   * with the underlying cause of the searching failure.
   *
   * @param message a String describing the searching failure.
   * @param cause a Throwable indicating the underlying cause of the searching failure.
   */
  public SearchException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
