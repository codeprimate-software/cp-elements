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
 * The AssertionFailedException is a RuntimeException indicating an assertion failure using the Assert class.
 *
 * @author John J. Blum
 * @see java.lang.AssertionError
 * @see java.lang.RuntimeException
 * @see org.cp.elements.lang.AssertionFailedException
 * @since 1.0.0
 * @version 1.0.0
 */
@SuppressWarnings("unused")
public class AssertionFailedException extends RuntimeException {

  /**
   * Default constructor creating an instance of the AssertionFailedException.
   */
  public AssertionFailedException() {
  }

  /**
   * Constructor to create an instance of the AssertionFailedException with the given message to describe the
   * assertion failure.
   *
   * @param message a String value describing the nature of the assertion failure.
   */
  public AssertionFailedException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the AssertionFailedException with the given Throwable to indicated the cause
   * of the assertion failure.
   *
   * @param cause the Throwable indicated as the cause of this assertion failure.
   */
  public AssertionFailedException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the AssertionFailedException with a message to describe the assertion failure
   * and a Throwable to indicate the probable cause of the assertion failure.
   *
   * @param message a String value describing the nature of the assertion failure.
   * @param cause the Throwable indicated as the cause of this assertion failure.
   */
  public AssertionFailedException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
