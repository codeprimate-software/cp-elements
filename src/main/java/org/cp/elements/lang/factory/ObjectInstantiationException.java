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

package org.cp.elements.lang.factory;

/**
 * The ObjectInstantiationException is a RuntimeException indicating an error while instantiating an instance
 * of a specified class.  This class is the unchecked, runtime equivalent of the checked InstantiationException.
 *
 * @author John J. Blum
 * @see java.lang.InstantiationException
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ObjectInstantiationException extends RuntimeException {

  /**
   * Default constructor creating an instance of the ObjectInstantiationException.
   */
  public ObjectInstantiationException() {
  }

  /**
   * Constructor to create an instance of the ObjectInstantiationException with a given message to describe the object
   * instantiation error.
   *
   * @param message a String value describing the nature of the object instantiation error.
   */
  public ObjectInstantiationException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the ObjectInstantiationException with the given Throwable to indicate
   * the cause of the object instantiation error.
   *
   * @param cause the Throwable indicating the cause of the object instantiation error.
   */
  public ObjectInstantiationException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the ObjectInstantiationException with a message to describe the object
   * instantiation error and a Throwable to indicate the probable cause of the object instantiation error.
   *
   * @param message a String value describing the nature of the object instantiation error.
   * @param cause the Throwable indicated as the cause of the object instantiation error.
   */
  public ObjectInstantiationException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
