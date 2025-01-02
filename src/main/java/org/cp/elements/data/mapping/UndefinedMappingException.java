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
package org.cp.elements.data.mapping;

/**
 * {@link MappingException} thrown when an {@link Object} mapping is undefined.
 *
 * @author John Blum
 * @see org.cp.elements.data.mapping.MappingException
 * @see java.lang.RuntimeException
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class UndefinedMappingException extends MappingException {

  public static final UndefinedMappingException INSTANCE = new UndefinedMappingException("Mapping is undefined");

  /**
   * Constructs a new, uninitialized instance of {@link UndefinedMappingException}.
   */
  public UndefinedMappingException() { }

  /**
   * Constructs a new {@link UndefinedMappingException} initialized with a {@link String} describing this exception.
   *
   * @param message {@link String} containing a description of this exception.
   * @see java.lang.RuntimeException#RuntimeException(String)
   */
  public UndefinedMappingException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link UndefinedMappingException} initialized with a {@link Throwable}
   * object indicating the cause of this exception.
   *
   * @param cause {@link Throwable} indicating the cause of this exception.
   * @see java.lang.RuntimeException#RuntimeException(Throwable)
   */
  public UndefinedMappingException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link UndefinedMappingException} initialized with a {@link String}
   * describing this exception along with a {@link Throwable} object indicating the cause
   * of this exception.
   *
   * @param message {@link String} containing a description of this exception.
   * @param cause {@link Throwable} indicating the cause of this exception.
   * @see java.lang.RuntimeException#RuntimeException(String, Throwable)
   */
  public UndefinedMappingException(String message, Throwable cause) {
    super(message, cause);
  }
}
