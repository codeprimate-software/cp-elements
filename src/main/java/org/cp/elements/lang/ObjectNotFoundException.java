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
 * The ObjectNotFoundException class is a ResourceNotFoundException indicating that a object could not be found
 * in Java heap memory.
 * 
 * @author John J. Blum
 * @see java.lang.Object
 * @see org.cp.elements.lang.ResourceNotFoundException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ObjectNotFoundException extends ResourceNotFoundException {

  /**
   * Default constructor creating an instance of the ObjectNotFoundException.
   */
  public ObjectNotFoundException() {
  }

  /**
   * Constructor to create an instance of the ObjectNotFoundException with a given message to describe the object
   * not found error.
   * 
   * @param message a String value describing the nature of the object not found error.
   */
  public ObjectNotFoundException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the ObjectNotFoundException with the given Throwable to indicate the cause
   * of the object not found error.
   * 
   * @param cause the Throwable indicating the cause of the object not found error.
   * @see java.lang.Throwable
   */
  public ObjectNotFoundException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the ObjectNotFoundException with a message to describe the object not found
   * error and a Throwable to indicate the probable cause of the object not found error.
   * 
   * @param message a String value describing the nature of the object not found error.
   * @param cause the Throwable indicated as the cause of the object not found error.
   * @see java.lang.Throwable
   */
  public ObjectNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
