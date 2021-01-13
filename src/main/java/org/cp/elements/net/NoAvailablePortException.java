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

package org.cp.elements.net;

/**
 * The {@link NoAvailablePortException} class is a {@link IllegalStateException} indicating that
 * no available system port could be found.
 *
 * @author John Blum
 * @see java.lang.IllegalStateException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class NoAvailablePortException extends IllegalStateException {

  /**
   * Constructs an uninitialized instance of {@link NoAvailablePortException}.
   */
  public NoAvailablePortException() {
  }

  /**
   * Constructs an instance of {@link NoAvailablePortException} initialized with the given message.
   *
   * @param message {@link String} containing a description of the error.
   */
  public NoAvailablePortException(String message) {
    super(message);
  }

  /**
   * Constructs an instance of {@link NoAvailablePortException} initialized with the given cause.
   *
   * @param cause {@link Throwable} object indicating the reason this error was thrown.
   * @see java.lang.Throwable
   */
  public NoAvailablePortException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of {@link NoAvailablePortException} initialized with the given message
   * and cause of this error.
   *
   * @param message {@link String} containing a description of the error.
   * @param cause {@link Throwable} object indicating the reason this error was thrown.
   * @see java.lang.Throwable
   */
  public NoAvailablePortException(String message, Throwable cause) {
    super(message, cause);
  }
}
