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

import org.cp.elements.lang.reflect.ConstructorNotFoundException;

/**
 * NoSuchConstructorException is a RuntimeException type indicating that a constructor with the specified signature
 * could not be found in the given class.  This class is the unchecked, runtime equivalent of the checked
 * NoSuchMethodException with emphasis on constructors.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.reflect.ConstructorNotFoundException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class NoSuchConstructorException extends ConstructorNotFoundException {

  /**
   * Constructs an uninitialized instance of the NoSuchConstructorException.
   */
  public NoSuchConstructorException() {
  }

  /**
   * Constructor to create an instance of the NoSuchConstructorException with a given message to describe the missing
   * constructor.
   *
   * @param message a String value describing the nature of the missing constructor.
   */
  public NoSuchConstructorException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the NoSuchConstructorException with the given Throwable to indicate the cause
   * of the missing constructor.
   *
   * @param cause the Throwable indicating the cause of the missing constructor.
   */
  public NoSuchConstructorException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the NoSuchConstructorException with a message to describe the missing
   * constructor and a Throwable to indicate the probable cause of the missing constructor.
   *
   * @param message a String value describing the nature of the missing constructor.
   * @param cause the Throwable indicated as the cause of the missing constructor.
   */
  public NoSuchConstructorException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
