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

package org.cp.elements.util.convert;

/**
 * The ConversionException class is a RuntimeException indicating that a type conversion or conversion process failed.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConversionException extends RuntimeException {

  /**
   * Default constructor to create an uninitialized instance of the ConversionException class.
   */
  public ConversionException() {
  }

  /**
   * Constructs an instance of the ConversionException class with a message describing the conversion failure.
   *
   * @param message a String describing the conversion failure.
   */
  public ConversionException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the ConversionException class with the underlying cause of the conversion failure.
   *
   * @param cause a Throwable indicating the underlying cause of the conversion failure.
   */
  public ConversionException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the ConversionException class with a message describing the conversion failure along
   * with the underlying cause of the conversion failure.
   *
   * @param message a String describing the conversion failure.
   * @param cause a Throwable indicating the underlying cause of the conversion failure.
   */
  public ConversionException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
