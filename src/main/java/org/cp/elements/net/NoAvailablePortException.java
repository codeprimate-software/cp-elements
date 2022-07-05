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
 * A {@link NetworkException} used to indicate that no available system port could be found.
 *
 * @author John Blum
 * @see org.cp.elements.net.NetworkException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class NoAvailablePortException extends NetworkException {

  /**
   * Constructs a new, uninitialized instance of {@link NoAvailablePortException} with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public NoAvailablePortException() { }

  /**
   * Constructs a new instance of {@link NoAvailablePortException} initialized with the given {@link String message}
   * to describe the port error.
   *
   * @param message {@link String} containing a description of the port error.
   */
  public NoAvailablePortException(String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link NoAvailablePortException} initialized with the given {@link Throwable cause}
   * used as the reason why this port error occurred.
   *
   * @param cause {@link Throwable} used as the reason why this port error occurred.
   */
  public NoAvailablePortException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link NoAvailablePortException} initialized with the given {@link String message}
   * to describe the port error along with the given {@link Throwable cause} used as the reason
   * why this port error occurred.
   *
   * @param message {@link String} containing a description of the port error.
   * @param cause {@link Throwable} used as the reason why this port error occurred.
   */
  public NoAvailablePortException(String message, Throwable cause) {
    super(message, cause);
  }
}
