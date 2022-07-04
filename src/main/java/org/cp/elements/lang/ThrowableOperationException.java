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
package org.cp.elements.lang;

/**
 * {@link RuntimeException} thrown to indicate that an operation capable of throwing a {@link Throwable}
 * threw an {@link Exception} or {@link Error}.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ThrowableOperationException extends RuntimeException {

  /**
   * Constructs a new, initialized instance of {@link ThrowableOperationException}
   * having no {@link String message} or {@link Throwable cause}.
   */
  public ThrowableOperationException() { }

  /**
   * Constructs a new instance of {@link ThrowableOperationException} initialized with the given {@link String message}
   * describing this exception.
   *
   * @param message {@link String} containing a message describing this exception.
   */
  public ThrowableOperationException(String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link ThrowableOperationException} initialized with the given {@link Throwable cause}
   * used as the reason this exception was thrown.
   *
   * @param cause {@link Throwable} used as the reason this exception was thrown.
   */
  public ThrowableOperationException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link ThrowableOperationException} initialized with the given {@link String message}
   * describing this exception along with the given {@link Throwable cause} used as the reason this exception
   * was thrown.
   *
   * @param message {@link String} containing a message describing this exception.
   * @param cause {@link Throwable} used as the reason this exception was thrown.
   */
  public ThrowableOperationException(String message, Throwable cause) {
    super(message, cause);
  }
}
