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

package org.cp.elements.beans;

/**
 * The PropertyNotFoundException class is a RuntimeException to indicate that the specified property does not exist
 * on a JavaBean.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertyNotFoundException extends RuntimeException {

  /**
   * Creates an instance of the PropertyNotFoundException class.
   */
  public PropertyNotFoundException() {
  }

  /**
   * Creates an instance of the PropertyNotFoundException class initialized with a description of the problem.
   *
   * @param message a String describing the nature of the problem and reason this Exception was thrown.
   */
  public PropertyNotFoundException(final String message) {
    super(message);
  }

  /**
   * Creates an instance of the PropertyNotFoundException class initialized with the specified Throwable, which is also
   * the reason, or underlying cause for why this Exception was thrown.
   *
   * @param cause a Throwable indicating the reason this PropertyNotFoundException was thrown.
   */
  public PropertyNotFoundException(final Throwable cause) {
    super(cause);
  }

  /**
   * Creates an instance of the PropertyNotFoundException class initialized with a message describing the exceptional
   * condition and a reason, or underlying cause for why this Exception was thrown.
   *
   * @param message a String describing the nature of the problem and reason this Exception was thrown.
   * @param cause a Throwable indicating the reason this PropertyNotFoundException was thrown.
   */
  public PropertyNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
