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

package org.cp.elements.lang.reflect;

/**
 * The MethodNotFoundException class is a RuntimeException indicating that a specified method was not found on
 * a particular class type.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class MethodNotFoundException extends RuntimeException {

  /**
   * Constructs an uninitialized instance of the MethodNotFoundException.
   */
  public MethodNotFoundException() {
  }

  /**
   * Constructs an instance of the MethodNotFoundException initialized with the given message describing
   * the method not found error.
   *
   * @param message a String describing the nature of the method not found error.
   */
  public MethodNotFoundException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the MethodNotFoundException initialized with the given Throwable to indicate
   * the cause of the method not found error.
   *
   * @param cause the Throwable indicating the cause of the method not found error.
   * @see java.lang.Throwable
   */
  public MethodNotFoundException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the MethodNotFoundException initialized with the given message describing
   * the method not found error along with the Throwable indicating the probable cause of the method not found error.
   *
   * @param message a String describing the nature of the method not found error.
   * @param cause the Throwable indicating the cause of the method not found error.
   * @see java.lang.Throwable
   */
  public MethodNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
