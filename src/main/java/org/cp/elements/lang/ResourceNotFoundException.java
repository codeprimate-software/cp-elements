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

package org.cp.elements.lang;

/**
 * The ResourceNotFoundException class is a RuntimeException indicating that a specified Resource could not be found
 * on the running system.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ResourceNotFoundException extends RuntimeException {

  /**
   * Default constructor creating an instance of the ResourceNotFoundException.
   */
  public ResourceNotFoundException() {
  }

  /**
   * Constructor to create an instance of the ResourceNotFoundException with a given message to describe
   * the resource not found error.
   *
   * @param message a String value describing the nature of the resource not found error.
   */
  public ResourceNotFoundException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the ResourceNotFoundException with the given Throwable to indicate the cause
   * of the resource not found error.
   *
   * @param cause the Throwable indicating the cause of the resource not found error.
   * @see java.lang.Throwable
   */
  public ResourceNotFoundException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the ResourceNotFoundException with a message to describe the resource
   * not found error and a Throwable to indicate the probable cause of the resource not found error.
   *
   * @param message a String value describing the nature of the resource not found error.
   * @param cause the Throwable indicating the cause of the resource not found error.
   * @see java.lang.Throwable
   */
  public ResourceNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
