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
 * {@link BeansException} thrown when an exception occurs while introspecting a {@link Bean}.
 *
 * @author John Blum
 * @see org.cp.elements.beans.BeansException
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class BeanIntrospectionException extends BeansException {

  /**
   * Constructs a new {@link BeanIntrospectionException} with no {@link String message} and no {@link Throwable cause}.
   */
  public BeanIntrospectionException() { }

  /**
   * Constructs a new {@link BeanIntrospectionException} initialized with the given {@link String message}
   * to describe this exception.
   *
   * @param message {@link String} containing a {@literal message} describing this exception.
   */
  public BeanIntrospectionException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link BeanIntrospectionException} initialized with a given {@link Throwable}
   * used as the cause of this exception.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of this exception.
   */
  public BeanIntrospectionException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link BeanIntrospectionException} initialized with the given {@link String message}
   * to describe this exception along with a {@link Throwable} used as the cause of this exception.
   *
   * @param message {@link String} containing a {@literal message} describing this exception.
   * @param cause {@link Throwable} used as the {@literal cause} of this exception.
   */
  public BeanIntrospectionException(String message, Throwable cause) {
    super(message, cause);
  }
}
