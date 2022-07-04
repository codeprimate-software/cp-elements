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
 * A {@link ProcessExecutionException} used to signify that a target {@link Process} is not responding to
 * input or control.
 *
 * @author John J. Blum
 * @see org.cp.elements.process.ProcessExecutionException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ProcessNotRespondingException extends ProcessExecutionException {

  /**
   * Constructs a new, uninitialized instance of {@link ProcessNotRespondingException} with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public ProcessNotRespondingException() { }

  /**
   * Constructs a new instance of {@link ProcessNotRespondingException} initialized with the given {@link String message}
   * describing this {@link ProcessExecutionException}.
   *
   * @param message {@link String} describing this {@link ProcessExecutionException}.
   * @see java.lang.String
   */
  public ProcessNotRespondingException(String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link ProcessNotRespondingException} initialized with
   * the given {@link Throwable cause} used as the reason this {@link ProcessExecutionException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause and reason this {@link ProcessExecutionException} was thrown.
   * @see java.lang.Throwable
   */
  public ProcessNotRespondingException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link ProcessNotRespondingException} initialized with the given {@link String message}
   * describing this {@link ProcessExecutionException} along with the given {@link Throwable cause} used as the reason
   * this {@link ProcessExecutionException} was thrown.
   *
   * @param message {@link String} describing this {@link ProcessExecutionException}.
   * @param cause {@link Throwable} used as the cause and reason this {@link ProcessExecutionException} was thrown.
   * @see java.lang.String
   * @see java.lang.Throwable
   */
  public ProcessNotRespondingException(String message, Throwable cause) {
    super(message, cause);
  }
}
