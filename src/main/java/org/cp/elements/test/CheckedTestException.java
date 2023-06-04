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
package org.cp.elements.test;

/**
 * Java Checked {@link Exception} used for testing.
 *
 * @author John Blum
 * @see java.lang.Exception
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CheckedTestException extends Exception {

  /**
   * Constructs a new {@link CheckedTestException} with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public CheckedTestException() { }

  /**
   * Constructs a new {@link CheckedTestException} initialized with the given {@link String message}
   * describing this {@link Exception}.
   *
   * @param message {@link String} containing a {@literal message} describing this {@link Exception}.
   * @see java.lang.String
   */
  public CheckedTestException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link CheckedTestException} initialized with the given {@link Throwable}
   * used as the {@literal cause} of this {@link Exception}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of this {@link Exception}.
   * @see java.lang.Throwable
   */
  public CheckedTestException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link CheckedTestException} initialized with the given {@link String message}
   * describing this {@link Exception} along with a {@link Throwable} used as the {@literal cause}
   * of this {@link Exception}.
   *
   * @param message {@link String} containing a {@literal message} describing this {@link Exception}.
   * @param cause {@link Throwable} used as the {@literal cause} of this {@link Exception}.
   * @see java.lang.Throwable
   */
  public CheckedTestException(String message, Throwable cause) {
    super(message, cause);
  }
}
