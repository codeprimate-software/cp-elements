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
package org.cp.elements.beans;

/**
 * {@link BeansException} extension indicating a failure to read a bean property.
 *
 * @author John Blum
 * @see org.cp.elements.beans.BeansException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertyReadException extends BeansException {

  /**
   * Constructs a new, uninitialized instance of {@link PropertyReadException}
   * with no {@link String message} or {@link Throwable cause}.
   */
  public PropertyReadException() { }

  /**
   * Constructs a new instance of {@link PropertyReadException} initialized with the given {@link String message}
   * describing the exception.
   *
   * @param message {@link String} describing the exception.
   */
  public PropertyReadException(String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link PropertyReadException} initialized with the given {@link Throwable}
   * used as the underlying cause of this exception.
   *
   * @param cause {@link Throwable} used as the cause of this exception.
   */
  public PropertyReadException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link PropertyReadException} initialized with the given {@link String message}
   * describing the exception and {@link Throwable} used as the underlying cause of this exception.
   *
   * @param message {@link String} describing the exception.
   * @param cause {@link Throwable} used as the cause of this exception.
   */
  public PropertyReadException(String message, Throwable cause) {
    super(message, cause);
  }
}
