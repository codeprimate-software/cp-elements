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
 * The TypeNotFoundException class is a ResourceNotFoundException indicating that a class specified by name
 * cannot be found in the CLASSPATH.  This is the unchecked, runtime equivalent of the checked ClassNotFoundException.
 *
 * @author John J. Blum
 * @see java.lang.Class
 * @see java.lang.ClassNotFoundException
 * @see org.cp.elements.lang.ResourceNotFoundException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class TypeNotFoundException extends ResourceNotFoundException {

  /**
   * Default constructor creating an instance of the TypeNotFoundException.
   */
  public TypeNotFoundException() {
  }

  /**
   * Constructor to create an instance of the TypeNotFoundException with a given message to describe the class not found
   * error.
   *
   * @param message a String value describing the nature of the class not found error.
   */
  public TypeNotFoundException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the TypeNotFoundException with the given Throwable to indicate the cause
   * of the class not found error.
   *
   * @param cause the Throwable indicating the cause of the class not found error.
   * @see java.lang.Throwable
   */
  public TypeNotFoundException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the TypeNotFoundException with a message to describe the class not found error
   * and a Throwable to indicate the probable cause of the class not found error.
   *
   * @param message a String value describing the nature of the class not found error.
   * @param cause the Throwable indicated as the cause of the class not found error.
   * @see java.lang.Throwable
   */
  public TypeNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
