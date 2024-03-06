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
package org.cp.elements.lang;

/**
 * Java {@link RuntimeException} thrown when a {@literal resource} could not be found on the running system.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ResourceNotFoundException extends RuntimeException {

  /**
   * Constructs a new, initialized instance of {@link ResourceNotFoundException}.
   */
  public ResourceNotFoundException() {
  }

  /**
   * Constructs a new {@link ResourceNotFoundException} initialized with the given {@link String message}
   * describing the resource not found error.
   *
   * @param message {@link String} describing the resource not found error.
   */
  public ResourceNotFoundException(final String message) {
    super(message);
  }

  /**
   * Constructs a new {@link ResourceNotFoundException} initalized with the given {@link Throwable}
   * used as the cause of this resource not found error.
   *
   * @param cause {@link Throwable} used as the cause of the resource not found error.
   */
  public ResourceNotFoundException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link ResourceNotFoundException} initialized with the given {@link String message}
   * describing the resource not found error along with a {@link Throwable} used as the cause of this
   * resource not found error.
   *
   * @param message {@link String} describing the resource not found error.
   * @param cause {@link Throwable} used as the cause of the resource not found error.
   */
  public ResourceNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
