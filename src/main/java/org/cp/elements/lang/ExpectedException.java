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
 * Java {@link RuntimeException} used to indicate a failed expectation.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ExpectedException extends RuntimeException {

  /**
   * Constructs a new {@link ExpectedException} initialized with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public ExpectedException() { }

  /**
   * Constructs a new {@link ExpectedException} initialized with the given {@link String message}
   * describing this exception.
   *
   * @param message {@link String} containing a messaged used to describe this exception.
   */
  public ExpectedException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link ExpectedException} initialized with the given {@link Throwable}
   * used as the cause of this exception.
   *
   * @param cause {@link Throwable} used as the cause for this exception.
   */
  public ExpectedException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link ExpectedException} initialized with the given {@link String message}
   * describing this exception along with the given {@link Throwable} used as the cause of this exception.
   *
   * @param message {@link String} containing a messaged used to describe this exception.
   * @param cause {@link Throwable} used as the cause for this exception.
   */
  public ExpectedException(String message, Throwable cause) {
    super(message, cause);
  }
}
