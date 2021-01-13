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

package org.cp.elements.security;

/**
 * The AuthenticationException class is a AuthenticationException indicating the user could not be identified.
 *
 * @author John J. Blum
 * @see org.cp.elements.security.SecurityException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AuthenticationException extends SecurityException {

  /**
   * Constructs an uninitialized instance of the AuthenticationException class.
   */
  public AuthenticationException() {
  }

  /**
   * Constructs an instance of the AuthenticationException class with the specified message describing the security
   * violation.
   *
   * @param message a String describing the security violation.
   */
  public AuthenticationException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the AuthenticationException class with the underlying cause, or reason for the security
   * violation.
   *
   * @param cause a Throwable indicating the cause of the security violation.
   */
  public AuthenticationException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the AuthenticationException class with the specified message describing the security
   * violation along with the underlying cause, or reason of the security violation.
   *
   * @param message a String describing the security violation.
   * @param cause a Throwable indicating the cause of the security violation.
   */
  public AuthenticationException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
