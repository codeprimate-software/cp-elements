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
 * Java {@link RuntimeException} thrown when a runtime error occurs during the lifecycle of an executing program.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ProcessException extends RuntimeException {

  /**
   * Constructs a new {@link ProcessException} with no {@link String message} and no {@link Throwable cause}.
   */
  public ProcessException() { }

  /**
   * Constructs an instance of {@link ProcessException} initialized with the given message
   * describing this {@link RuntimeException}.
   *
   * @param message {@link String} describing this {@link RuntimeException}.
   */
  public ProcessException(String message) {
    super(message);
  }

  /**
   * Constructs an instance of {@link ProcessException} initialized with the given {@link Throwable}
   * indicating the cause of this {@link RuntimeException}.
   *
   * @param cause {@link Throwable} object indicating the cause of this {@link RuntimeException}.
   * @see java.lang.Throwable
   */
  public ProcessException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of {@link ProcessException} initialized with the given message
   * describing this {@link RuntimeException} and cause to indicate the reason this {@link RuntimeException}
   * was thrown.
   *
   * @param message {@link String} describing this {@link RuntimeException}.
   * @param cause {@link Throwable} object indicating the cause of this {@link RuntimeException}.
   * @see java.lang.Throwable
   */
  public ProcessException(String message, Throwable cause) {
    super(message, cause);
  }
}
