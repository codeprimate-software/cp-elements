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
 * Elements {@link ResourceNotFoundException} thrown when an object could not be found in Java heap memory.
 *
 * @author John J. Blum
 * @see java.lang.Object
 * @see org.cp.elements.lang.ResourceNotFoundException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ObjectNotFoundException extends ResourceNotFoundException {

  /**
   * Constructs a new, uninitialized {@link ObjectNotFoundException}.
   */
  public ObjectNotFoundException() {
  }

  /**
   * Constructs a new {@link ObjectNotFoundException} initialized with the given {@link String message}
   * describing the object not found error.
   *
   * @param message {@link String} describing the object not found error.
   */
  public ObjectNotFoundException(final String message) {
    super(message);
  }

  /**
   * Constructs a new {@link ObjectNotFoundException} initialized with the given {@link Throwable}
   * used as the cause of this object not found error.
   *
   * @param cause {@link Throwable} used as the cause of the object not found error.
   */
  public ObjectNotFoundException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link ObjectNotFoundException} initialized with the given {@link String message}
   * describing the object not found error along with the given {@link Throwable} used as the cause of this
   * object not found error.
   *
   * @param message {@link String} describing the object not found error.
   * @param cause {@link Throwable} used as the cause of the object not found error.
   */
  public ObjectNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }
}
