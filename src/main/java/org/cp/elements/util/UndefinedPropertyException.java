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
package org.cp.elements.util;

/**
 * Java {@link RuntimeException} used to indicate that a {@link java.util.Properties Property} was declared
 * in Java application (program) configuration, but it was not defined (set).
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @see org.cp.elements.util.UndefinedPropertyException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class UndefinedPropertyException extends RuntimeException {

  /**
   * Constructs a new {@link UndefinedPropertyException}
   * with no {@link String message} and no {@link Throwable cause}.
   */
  public UndefinedPropertyException() { }

  /**
   * Constructs a new {@link UndefinedPropertyException} initialized with the given {@link String message}
   * used to description this {@link RuntimeException}.
   *
   * @param message {@link String} containing a {@literal message} to description this {@link RuntimeException}.
   */
  public UndefinedPropertyException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link UndefinedPropertyException} initialized with the given {@link Throwable}
   * used as the cause or reason this {@link RuntimeException} was thrown.
   *
   * @param cause {@link Throwable} used as the reason this {@link RuntimeException} was thrown.
   */
  public UndefinedPropertyException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link UndefinedPropertyException} initialized with the given {@link String message}
   * used to description this {@link RuntimeException} along with the given {@link Throwable} used as the cause
   * or reason this {@link RuntimeException} was thrown.
   *
   * @param message {@link String} containing a {@literal message} to description this {@link RuntimeException}.
   * @param cause {@link Throwable} used as the reason this {@link RuntimeException} was thrown.
   */
  public UndefinedPropertyException(String message, Throwable cause) {
    super(message, cause);
  }
}
