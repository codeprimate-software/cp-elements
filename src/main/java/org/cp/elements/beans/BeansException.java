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
 * Abstract {@link RuntimeException} thrown when an error occurs while accessing or processing a Java Bean.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class BeansException extends RuntimeException {

  /**
   * Constructs a new, default instance of {@link BeansException} with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public BeansException() { }

  /**
   * Constructs a new {@link BeansException} initialized with the given {@link String message}
   * describing this exception.
   *
   * @param message {@link String} containing a {@literal message} describing the exception.
   */
  public BeansException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link BeansException} initialized with a given {@link Throwable}
   * as the cause of this exception.
   *
   * @param cause {@link Throwable} used as the cause of this exception.
   */
  public BeansException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link BeansException} initialized with the given {@link String message}
   * describing this exception along with a {@link Throwable} used as the cause of this exception.
   *
   * @param message {@link String} containing a {@literal message} describing the exception.
   * @param cause {@link Throwable} used as the cause of this exception.
   */
  public BeansException(String message, Throwable cause) {
    super(message, cause);
  }
}
