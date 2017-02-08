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

package org.cp.elements.process;

/**
 * The {@link EmbeddedProcessExecutionException} class is a special {@link ProcessExecutionException} to indicate
 * a runtime error occurred while executing a embedded program, which will run in the containing process.
 *
 * @author John Blum
 * @see org.cp.elements.process.ProcessExecutionException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class EmbeddedProcessExecutionException extends ProcessExecutionException {

  /**
   * Constructs an uninitialized instance of {@link EmbeddedProcessExecutionException} with no message or cause.
   */
  public EmbeddedProcessExecutionException() {
  }

  /**
   * Constructs an instance of {@link EmbeddedProcessExecutionException} initialized with the given message
   * describing this {@link ProcessException}.
   *
   * @param message {@link String} describing this {@link ProcessException}.
   */
  public EmbeddedProcessExecutionException(String message) {
    super(message);
  }

  /**
   * Constructs an instance of {@link EmbeddedProcessExecutionException} initialized with the given {@link Throwable}
   * indicating the cause of this {@link ProcessException}.
   *
   * @param cause {@link Throwable} object indicating the cause of this {@link ProcessException}.
   * @see java.lang.Throwable
   */
  public EmbeddedProcessExecutionException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of {@link EmbeddedProcessExecutionException} initialized with the given message
   * describing this {@link ProcessException} and cause to indicate the reason this {@link ProcessException}
   * was thrown.
   *
   * @param message {@link String} describing this {@link ProcessException}.
   * @param cause {@link Throwable} object indicating the cause of this {@link ProcessException}.
   * @see java.lang.Throwable
   */
  public EmbeddedProcessExecutionException(String message, Throwable cause) {
    super(message, cause);
  }
}
