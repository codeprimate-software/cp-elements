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

package org.cp.elements.beans;

/**
 * The RequiredPropertiesNotSetException class is a subclass of RequiredPropertyNotSetException indicating that a bean
 * property annotated with @Required was not properly set.
 *
 * @author John J. Blum
 * @see org.cp.elements.beans.IllegalPropertyValueException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class RequiredPropertyNotSetException extends IllegalPropertyValueException {

  /**
   * Constructs an uninitialized instance of the RequiredPropertyNotSetException class.
   */
  public RequiredPropertyNotSetException() {
  }

  /**
   * Constructs an instance of the RequiredPropertyNotSetException initialized with a message describing the problem.
   *
   * @param message a String describing the nature of the problem and reason this Exception was thrown.
   */
  public RequiredPropertyNotSetException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the RequiredPropertyNotSetException initialized with the specified Throwable
   * indicating the reason, or underlying cause for why this Exception was thrown.
   *
   * @param cause a Throwable indicating the reason this RequiredPropertyNotSetException was thrown.
   */
  public RequiredPropertyNotSetException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the RequiredPropertyNotSetException initialized with a message describing the exceptional
   * condition along with a Throwable indicating the reason, or underlying cause for why this Exception was thrown.
   *
   * @param message a String describing the nature of the problem and reason this Exception was thrown.
   * @param cause a Throwable indicating the reason this RequiredPropertyNotSetException was thrown.
   */
  public RequiredPropertyNotSetException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
