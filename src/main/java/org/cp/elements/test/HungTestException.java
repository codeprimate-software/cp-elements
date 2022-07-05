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
 * {@link TestException} used to indicate that a test case or test suite hung and failed to complete.
 *
 * @author John Blum
 * @see org.cp.elements.test.TestException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class HungTestException extends TestException {

  /**
   * Constructs a new, uninitialized instance of {@link HungTestException} with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public HungTestException() { }

  /**
   * Constructs a new instance of {@link HungTestException} initialized with the given {@link String message}
   * describing the test error.
   *
   * @param message {@link String} describing the test error.
   */
  public HungTestException(String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link HungTestException} initialized with the given {@link Throwable cause}
   * used and the reason why this test error occurred.
   *
   * @param cause {@link Throwable} used and the reason why this test error occurred.
   */
  public HungTestException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link HungTestException} initialized with the given {@link String message}
   * describing the test error along with the given {@link Throwable cause} used and the reason
   * why this test error occurred.
   *
   * @param message {@link String} describing the test error.
   * @param cause {@link Throwable} used and the reason why this test error occurred.
   */
  public HungTestException(String message, Throwable cause) {
    super(message, cause);
  }
}
