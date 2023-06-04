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
 * An {@link Error} representing a runtime issue while running tests.
 *
 * @author John Blum
 * @see java.lang.Error
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class TestError extends Error {

  /**
   * Constructs a new, uninitialized instance of {@link TestError}.
   */
  public TestError() { }

  /**
   * Constructs a new {@link TestError} initialized with the given {@link String message}
   * describing the error.
   *
   * @param message {@link String} containing a description of the test error.
   */
  public TestError(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link TestError} initialized with the given {@link Throwable}
   * used as the cause of this test error.
   *
   * @param cause {@link Throwable} used as the cause of this test error.
   */
  public TestError(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link TestError} initialized with the given {@link String message}
   * describing the error along with the given {@link Throwable} used as the cause of this test error.
   *
   * @param message {@link String} containing a description of the test error.
   * @param cause {@link Throwable} used as the cause of this test error.
   */
  public TestError(String message, Throwable cause) {
    super(message, cause);
  }
}
