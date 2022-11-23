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
 * {@link RuntimeException} used to classify and indicate that a network error occurred.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class NetworkException extends RuntimeException {

  /**
   * Constructs a new instance of {@link NetworkException} with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public NetworkException() { }

  /**
   * Constructs a new instance of {@link NetworkException} initialized with the given {@link String message}
   * describing the network error.
   *
   * @param message {@link String} containing a message describing the network error.
   */
  public NetworkException(String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link NetworkException} initialized with the given {@link Throwable cause}
   * used as the reason this network error was thrown.
   *
   * @param cause {@link Throwable} used as the reason this network error was thrown.
   */
  public NetworkException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link NetworkException} initialized with the given {@link String message}
   * describing the network error along with the given {@link Throwable cause} used as the reason
   * this network error was thrown.
   *
   * @param message {@link String} containing a message describing the network error.
   * @param cause {@link Throwable} used as the reason this network error was thrown.
   */
  public NetworkException(String message, Throwable cause) {
    super(message, cause);
  }
}
