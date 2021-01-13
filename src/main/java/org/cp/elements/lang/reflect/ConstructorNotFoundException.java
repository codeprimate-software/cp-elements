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

package org.cp.elements.lang.reflect;

/**
 * ConstructorNotFoundException is a RuntimeException type indicating that a constructor with the specified signature
 * could not be found on a given class type.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.reflect.MethodNotFoundException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConstructorNotFoundException extends MethodNotFoundException {

  /**
   * Constructs an uninitialized instance of the ConstructorNotFoundException.
   */
  public ConstructorNotFoundException() {
  }

  /**
   * Constructs an instance of the ConstructorNotFoundException initialized with the given message describing
   * the constructor not found error.
   *
   * @param message a String describing the nature of the constructor not found error.
   */
  public ConstructorNotFoundException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the ConstructorNotFoundException initialized with the given Throwable to indicate
   * the cause of the constructor not found error.
   *
   * @param cause the Throwable indicating the cause of the constructor not found error.
   * @see java.lang.Throwable
   */
  public ConstructorNotFoundException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the ConstructorNotFoundException initialized with the given message describing
   * the constructor not found error along with the Throwable indicating the probable cause
   * of the constructor not found error.
   *
   * @param message a String describing the nature of the constructor not found error.
   * @param cause the Throwable indicating the cause of the constructor not found error.
   * @see java.lang.Throwable
   */
  public ConstructorNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
