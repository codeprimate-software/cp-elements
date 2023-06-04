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

import org.cp.elements.beans.annotation.Required;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link IllegalPropertyValueException} extension thrown to indicate that a bean property annotated with
 * {@link Required @Required} was not properly set.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @see org.cp.elements.beans.IllegalPropertyValueException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertyNotSetException extends IllegalPropertyValueException {

  /**
   * Constructs a new, default instance of {@link PropertyNotSetException} with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public PropertyNotSetException() { }

  /**
   * Constructs a new {@link PropertyNotSetException} initialized with
   * the given {@link String message} to describe this exception.
   *
   * @param message {@link String} describing this exception.
   */
  public PropertyNotSetException(@NotNull String message) {
    super(message);
  }

  /**
   * Constructs a new {@link PropertyNotSetException} initialized with the given {@link Throwable}
   * used as the cause and reason this exception was thrown.
   *
   * @param cause {@link Throwable} used as the cause and reason this exception was thrown.
   */
  public PropertyNotSetException(@NotNull Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link PropertyNotSetException} initialized with
   * the given {@link String message} to describe this exception along with the given {@link Throwable}
   * used as the cause and reason this exception was thrown.
   *
   * @param message {@link String} describing this exception.
   * @param cause {@link Throwable} used as the cause and reason this exception was thrown.
   */
  public PropertyNotSetException(@NotNull String message, @NotNull Throwable cause) {
    super(message, cause);
  }
}
