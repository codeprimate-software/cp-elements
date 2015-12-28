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

/**
 * The MethodInvocationException class is a RuntimeException that is thrown during a method invocation
 * when a checked Exception or Error occurs.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class MethodInvocationException extends RuntimeException {

  /**
   * Constructs an uninitialized instance of the MethodInvocationException.
   */
  public MethodInvocationException() {
  }

  /**
   * Constructs an instance of the MethodInvocationException initialized with the given message describing
   * the method invocation error.
   *
   * @param message a String describing the nature of the method invocation error.
   */
  public MethodInvocationException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the MethodInvocationException initialized with the given Throwable to indicate
   * the cause of the method invocation error.
   *
   * @param cause the Throwable indicating the cause of the method invocation error.
   * @see java.lang.Throwable
   */
  public MethodInvocationException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the MethodInvocationException initialized with the given message describing
   * the method invocation error along with the Throwable indicating the probable cause of the method invocation error.
   *
   * @param message a String describing the nature of the method invocation error.
   * @param cause the Throwable indicating the cause of the method invocation error.
   * @see java.lang.Throwable
   */
  public MethodInvocationException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
