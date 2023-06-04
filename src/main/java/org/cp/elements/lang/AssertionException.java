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
 * {@link RuntimeException} thrown to indicate that an assertion has failed.
 *
 * @author John J. Blum
 * @see java.lang.AssertionError
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AssertionException extends RuntimeException {

  /**
   * Constructs a new, default (uninitialized) instance of the {@link AssertionException}.
   */
  public AssertionException() { }

  /**
   * Constructs a new {@link AssertionException} initialized with the given {@link String message}
   * used to describe the assertion failure.
   *
   * @param message {@link String} describing the nature of the assertion failure.
   */
  public AssertionException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link AssertionException} initialized with the given {@link Throwable}
   * as the cause of the assertion failure.
   *
   * @param cause {@link Throwable} used as the cause of this assertion failure.
   */
  public AssertionException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link AssertionException} initialized with the given {@link String message}
   * used to describe the assertion failure and the given {@link Throwable} as the cause of the assertion failure.
   *
   * @param message {@link String} describing the nature of the assertion failure.
   * @param cause {@link Throwable} used as the cause of this assertion failure.
   */
  public AssertionException(String message, Throwable cause) {
    super(message, cause);
  }
}
