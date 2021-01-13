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
 * The IllegalTypeException class is a {@link RuntimeException} that indicates an object is not of the expected type.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class IllegalTypeException extends RuntimeException {

  /**
   * Constructs an uninitialized instance of IllegalTypeException.
   */
  public IllegalTypeException() {
  }

  /**
   * Constructs an instance of IllegalTypeException initialized with the given message to describe the error.
   *
   * @param message a String describing the cause of this exception.
   */
  public IllegalTypeException(String message) {
    super(message);
  }

  /**
   * Constructs an instance of IllegalTypeException initialized with the given {@link Throwable} indicating
   * the cause of this exception.
   *
   * @param cause a {@link Throwable} object that caused this exception.
   * @see java.lang.Throwable
   */
  public IllegalTypeException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of IllegalTypeException initialized with the given message to describe the error
   * along with the {@link Throwable} causing this exception.
   *
   * @param message a String describing the cause of this exception.
   * @param cause a {@link Throwable} object that caused this exception.
   * @see java.lang.Throwable
   */
  public IllegalTypeException(String message, Throwable cause) {
    super(message, cause);
  }
}
