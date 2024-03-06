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
 * Elements {@link ResourceNotFoundException} thrown when {@link Class} by the specified by {@link Class#getName() name}
 * cannot be found in the {@literal CLASSPATH}.
 * <p>
 * This is the unchecked, Java {@link RuntimeException runtime exception} equivalent to
 * the checked Java {@link ClassNotFoundException}.
 *
 * @author John J. Blum
 * @see java.lang.Class
 * @see java.lang.ClassNotFoundException
 * @see org.cp.elements.lang.ResourceNotFoundException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class TypeNotFoundException extends ResourceNotFoundException {

  /**
   * Constructs a new, uninitialized {@link TypeNotFoundException}.
   */
  public TypeNotFoundException() {
  }

  /**
   * Constructs a new {@link TypeNotFoundException} initialized with the given {@link String message}
   * describing the type not found error.
   *
   * @param message {@link String} describing the type not found error.
   */
  public TypeNotFoundException(final String message) {
    super(message);
  }

  /**
   * Constructs a new {@link TypeNotFoundException} initialized with the given {@link Throwable}
   * used as the cause of this type not found error.
   *
   * @param cause {@link Throwable} used as the cause of the type not found error.
   */
  public TypeNotFoundException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link TypeNotFoundException} initialized with the given {@link String message}
   * describing the type not found error along with the given {@link Throwable} used as the cause of this
   * type not found error.
   *
   * @param message {@link String} describing the type not found error.
   * @param cause {@link Throwable} used as the cause of the type not found error.
   */
  public TypeNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }
}
