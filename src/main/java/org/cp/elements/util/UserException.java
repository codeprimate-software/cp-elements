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

package org.cp.elements.util;

/**
 * The UserException class is a RuntimeException indicating a user problem.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class UserException extends RuntimeException {

  /**
   * Default constructor to create an uninitialized instance of the UserException class.
   */
  public UserException() {
  }

  /**
   * Constructs an instance of the UserException class with a message describing the user's problem.
   *
   * @param message a String describing the user's problem.
   */
  public UserException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the UserException class with the underlying cause of the user's problem.
   *
   * @param cause a Throwable indicating the underlying cause of the user's problem.
   */
  public UserException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the UserException class with a message describing the user's problem and the underlying
   * cause of the user's problem.
   *
   * @param message a String describing the user's problem.
   * @param cause a Throwable indicating the underlying cause of the user's problem.
   */
  public UserException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
