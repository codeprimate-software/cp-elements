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

package org.cp.elements.process;

/**
 * The {@link ProcessNotRespondingException} is a {@link ProcessException} indicating that a target {@link Process}
 * is not responding to inputs or control.
 *
 * @author John J. Blum
 * @see org.cp.elements.process.ProcessException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ProcessNotRespondingException extends ProcessException {

  /**
   * Constructs an uninitialized instance of {@link ProcessNotRespondingException} with no message or cause.
   */
  public ProcessNotRespondingException() {
  }

  /**
   * Constructs an instance of {@link ProcessNotRespondingException} initialized with the given message
   * describing this {@link ProcessException}.
   *
   * @param message {@link String} describing this {@link ProcessException}.
   */
  public ProcessNotRespondingException(String message) {
    super(message);
  }

  /**
   * Constructs an instance of {@link ProcessNotRespondingException} initialized with the given {@link Throwable}
   * indicating the cause of this {@link ProcessException}.
   *
   * @param cause {@link Throwable} object indicating the cause of this {@link ProcessException}.
   * @see java.lang.Throwable
   */
  public ProcessNotRespondingException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of {@link ProcessNotRespondingException} initialized with the given message
   * describing this {@link ProcessException} and cause to indicate the reason this {@link ProcessException}
   * was thrown.
   *
   * @param message {@link String} describing this {@link ProcessException}.
   * @param cause {@link Throwable} object indicating the cause of this {@link ProcessException}.
   * @see java.lang.Throwable
   */
  public ProcessNotRespondingException(String message, Throwable cause) {
    super(message, cause);
  }
}
