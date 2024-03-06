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
 * Java {@link RuntimeException} thrown on clone or copy operation failures.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CloneException extends RuntimeException {

  /**
   * Constructs a new, uninitialized instance of {@link CloneException}.
   */
  public CloneException() {
  }

  /**
   * Constructs a new {@link CloneException} initialized with the given {@link String message}
   * used to describe error.
   *
   * @param message {@link String} describing the clone error.
   */
  public CloneException(final String message) {
    super(message);
  }

  /**
   * Constructs a new {@link CloneException} initialized with the given {@link Throwable} used as the cause
   * of this clone error.
   *
   * @param cause {@link Throwable} used as the cause of this clone error.
   */
  public CloneException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link CloneException} initialized with the given {@link String message}
   * describing this clone error along with the {@link Throwable} used as the cause of this clone error.
   *
   * @param message {@link String} describing the clone error.
   * @param cause {@link Throwable} used as the cause of this clone error.
   */
  public CloneException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
