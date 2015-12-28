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

package org.cp.elements.util;

/**
 * The ApplicationException class is a checked Exception indicating an application business rule violation occurred
 * that maybe possible to handle.
 *
 * @author jblum
 * @see java.lang.Exception
 * @see org.cp.elements.util.SystemException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ApplicationException extends Exception {

  /**
   * Creates an instance of the ApplicationException class.
   */
  public ApplicationException() {
  }

  /**
   * Creates an instance of the ApplicationException class initialized with a description of the problem.
   *
   * @param message a String describing the nature of the problem and reason this Exception was thrown.
   */
  public ApplicationException(final String message) {
    super(message);
  }

  /**
   * Creates an instance of the ApplicationException class initialized with the specified Throwable, which is also
   * the reason, or underlying cause for why this Exception was thrown.
   *
   * @param cause a Throwable indicating the reason this ApplicationException was thrown.
   */
  public ApplicationException(final Throwable cause) {
    super(cause);
  }

  /**
   * Creates an instance of the ApplicationException class initialized with a message describing the exceptional
   * condition and reason, or underlying cause for why this Exception was thrown.
   *
   * @param message a String describing the nature of the problem and reason this Exception was thrown.
   * @param cause a Throwable indicating the the reason this ApplicationException was thrown.
   */
  public ApplicationException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
