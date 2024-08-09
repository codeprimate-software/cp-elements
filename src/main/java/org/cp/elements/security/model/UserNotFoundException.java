/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.security.model;

/**
 * Java {@link RuntimeException} thrown when a {@link User} could not be found.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class UserNotFoundException extends RuntimeException {

  /**
   * Constructs a new {@link UserNotFoundException} with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public UserNotFoundException() { }

  /**
   * Constructs a new {@link UserNotFoundException} initialized with the given {@link String message}
   * describing the details for the missing {@link User}.
   *
   * @param message {@link String} describing the exception.
   */
  public UserNotFoundException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link UserNotFoundException} initialized with the given {@link Throwable cause}
   * used as the reason the {@link User} is missing.
   *
   * @param cause {@link Throwable} used as the reason the {@link User} was declared missing.
   */
  public UserNotFoundException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link UserNotFoundException} initialized with the given {@link String message}
   * describing the details for the missing {@link User} along with the given {@link Throwable cause}
   * used as the reason the {@link User} is missing.
   *
   * @param message {@link String} describing the exception.
   * @param cause {@link Throwable} used as the reason the {@link User} was declared missing.
   */
  public UserNotFoundException(String message, Throwable cause) {
    super(message, cause);
  }
}
