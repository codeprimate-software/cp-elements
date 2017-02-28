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

import java.lang.reflect.Method;

/**
 * The {@link MethodInvocationException} class is a {@link RuntimeException} that is thrown during
 * a {@link Method} invocation when a checked {@link Exception} or {@link Error} occurs.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @see java.lang.reflect.Method
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class MethodInvocationException extends RuntimeException {

  /**
   * Constructs an uninitialized instance of {@link MethodInvocationException}.
   */
  public MethodInvocationException() {
  }

  /**
   * Constructs an instance of {@link MethodInvocationException} initialized with the given {@link String message}
   * to describe the method invocation error.
   *
   * @param message {@link String} describing the method invocation error.
   * @see java.lang.String
   */
  public MethodInvocationException(String message) {
    super(message);
  }

  /**
   * Constructs an instance of {@link MethodInvocationException} initialized with the given {@link Throwable cause}
   * indicating the underlying reason for the method invocatino error.
   *
   * @param cause {@link Throwable} indicating the underlying reason for the method invocation error.
   * @see java.lang.Throwable
   */
  public MethodInvocationException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of {@link MethodInvocationException} initialized with the given {@link String message}
   * to describe the method invocation error along with the {@link Throwable probable cause} of why
   * the method invocation error occurred.
   *
   * @param message {@link String} describing the method invocation error.
   * @param cause {@link Throwable} indicating the underlying reason for the method invocation error.
   * @see java.lang.String
   * @see java.lang.Throwable
   */
  public MethodInvocationException(String message, Throwable cause) {
    super(message, cause);
  }
}
