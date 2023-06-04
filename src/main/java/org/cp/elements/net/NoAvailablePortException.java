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
 * {@link NetworkException} used to indicate that no system port was available for use by network services.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @see org.cp.elements.net.NetworkException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class NoAvailablePortException extends NetworkException {

  /**
   * Constructs a new {@link NoAvailablePortException} with no {@link String message} and no {@link Throwable cause}.
   */
  public NoAvailablePortException() { }

  /**
   * Constructs a new {@link NoAvailablePortException} initialized with the given {@link String message}
   * describing the no available network port error.
   *
   * @param message {@link String} containing a message describing the network port error.
   */
  public NoAvailablePortException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link NoAvailablePortException} initialized with the given {@link Throwable cause}
   * used as the reason a no available network port error was thrown.
   *
   * @param cause {@link Throwable} used as the reason this no available network port error ws thrown.
   */
  public NoAvailablePortException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link NoAvailablePortException} initialized with the given {@link String message}
   * describing the no available network port error along with the given {@link Throwable cause} used as the reason
   * a no available network port error was thrown.
   *
   * @param message {@link String} containing a message describing the network port error.
   * @param cause {@link Throwable} used as the reason this no available network port error ws thrown.
   */
  public NoAvailablePortException(String message, Throwable cause) {
    super(message, cause);
  }
}
