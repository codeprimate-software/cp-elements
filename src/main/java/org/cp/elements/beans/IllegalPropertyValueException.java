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

import org.cp.elements.lang.annotation.NotNull;

/**
 * A {@link BeansException} thrown to signify that the value being assigned to a bean property is not valid.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @see org.cp.elements.beans.BeansException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class IllegalPropertyValueException extends BeansException {

  /**
   * Constructs a new, default instance of {@link IllegalPropertyValueException} with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public IllegalPropertyValueException() { }

  /**
   * Constructs a new instance of {@link IllegalPropertyValueException} initialized with
   * the given {@link String message} to describe this exception.
   *
   * @param message {@link String} describing the illegal bean property value.
   */
  public IllegalPropertyValueException(@NotNull String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link IllegalPropertyValueException} initialized with the given {@link Throwable}
   * used as the cause or reason why this exception was thrown.
   *
   * @param cause {@link Throwable} used as the cause of this exception.
   */
  public IllegalPropertyValueException(@NotNull Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link IllegalPropertyValueException} initialized with
   * the given {@link String message} to describe this exception along with the given {@link Throwable}
   * used as the cause and reason this exception was thrown.
   *
   * @param message {@link String} describing the illegal bean property value.
   * @param cause {@link Throwable} used as the cause of this exception.
   */
  public IllegalPropertyValueException(@NotNull String message, @NotNull Throwable cause) {
    super(message, cause);
  }
}
