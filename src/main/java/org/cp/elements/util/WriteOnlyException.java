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

package org.cp.elements.util;

/**
 * The {@link WriteOnlyException} class is a {@link RuntimeException} that is thrown when a read operation is attempted
 * on a write-only resource.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class WriteOnlyException extends RuntimeException {

  /**
   * Constructs an instance of {@link WriteOnlyException} with no message and no cause.
   */
  public WriteOnlyException() {
  }

  /**
   * Constructs an instance of {@link WriteOnlyException} initialized with the given message explaining
   * the write-only error.
   *
   * @param message {@link String} describing the write-only error.
   */
  public WriteOnlyException(String message) {
    super(message);
  }

  /**
   * Constructs an instance of {@link WriteOnlyException} initialized with the given cause indicating
   * the reason this write-only error was thrown.
   *
   * @param cause {@link Throwable} indicating the cause of this write-only error.
   * @see java.lang.Throwable
   */
  public WriteOnlyException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of {@link WriteOnlyException} initialized with the given message explaining
   * the write-only error followed by the cause to indicate the reason this write-only error was thrown.
   *
   * @param message {@link String} describing the write-only error.
   * @param cause {@link Throwable} indicating the cause of this write-only error.
   * @see java.lang.Throwable
   */
  public WriteOnlyException(String message, Throwable cause) {
    super(message, cause);
  }
}
