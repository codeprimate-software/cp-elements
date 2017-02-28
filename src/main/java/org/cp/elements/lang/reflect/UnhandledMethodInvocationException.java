/*
 * Copyright 2016 Author or Authors.
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

package org.cp.elements.lang.reflect;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

/**
 * The {@link UnhandledMethodInvocationException} class is a {@link MethodInvocationException} extension to indicate
 * that a {@link Method} invocation on a given target {@link Object} cannot be handled by
 * the current {@link InvocationHandler}.
 *
 * @author John Blum
 * @see java.lang.reflect.InvocationHandler
 * @see java.lang.reflect.Method
 * @see org.cp.elements.lang.reflect.MethodInvocationException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class UnhandledMethodInvocationException extends MethodInvocationException {

  /**
   * Constructs an uninitialized instance of {@link UnhandledMethodInvocationException}.
   */
  public UnhandledMethodInvocationException() {
  }

  /**
   * Constructs an instance of {@link UnhandledMethodInvocationException} initialized with
   * the given {@link String message} to describe the method handling error.
   *
   * @param message {@link String} describing the method handling error.
   * @see java.lang.String
   */
  public UnhandledMethodInvocationException(String message) {
    super(message);
  }

  /**
   * Constructs an instance of {@link UnhandledMethodInvocationException} initialized with
   * the given {@link Throwable cause} indicating the underlying reason for the method handling error.
   *
   * @param cause {@link Throwable} indicating the underlying reason for the method handling error.
   * @see java.lang.Throwable
   */
  public UnhandledMethodInvocationException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of {@link UnhandledMethodInvocationException} initialized with
   * the given {@link String message} to describe the method handling error along with
   * the {@link Throwable probable cause} of why the method handling error occurred.
   *
   * @param message {@link String} describing the method handling error.
   * @param cause {@link Throwable} indicating the underlying reason for the method handling error.
   * @see java.lang.Throwable
   */
  public UnhandledMethodInvocationException(String message, Throwable cause) {
    super(message, cause);
  }
}
