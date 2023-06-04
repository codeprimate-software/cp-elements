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

package org.cp.elements.data.mapping;

/**
 * The {@link MappingException} class is a {@link RuntimeException} thrown when a data mapping error occurs.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class MappingException extends RuntimeException {

  /**
   * Constructs a new, uninitialized instance of {@link MappingException}.
   */
  public MappingException() { }

  /**
   * Constructs a new {@link MappingException} initialized with a {@link String} describing this exception.
   *
   * @param message {@link String} containing a description of this exception.
   * @see java.lang.RuntimeException#RuntimeException(String)
   */
  public MappingException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link MappingException} initialized with a {@link Throwable}
   * object indicating the cause of this exception.
   *
   * @param cause {@link Throwable} indicating the cause of this exception.
   * @see java.lang.RuntimeException#RuntimeException(Throwable)
   */
  public MappingException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link MappingException} initialized with a {@link String}
   * describing this exception along with a {@link Throwable} object indicating the cause
   * of this exception.
   *
   * @param message {@link String} containing a description of this exception.
   * @param cause {@link Throwable} indicating the cause of this exception.
   * @see java.lang.RuntimeException#RuntimeException(String, Throwable)
   */
  public MappingException(String message, Throwable cause) {
    super(message, cause);
  }
}
