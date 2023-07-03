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
package org.cp.elements.function;

import java.util.function.Function;

/**
 * {@link FunctionExecutionException} implementation thrown when a Java {@link Function} throws an exception during execution.
 *
 * @author John Blum
 * @see java.util.function.Function
 * @see org.cp.elements.function.FunctionExecutionException
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class FunctionExecutionException extends FunctionException {

  /**
   * Constructs a new {@link FunctionExecutionException} with no {@link String message}
   * and no probable {@link Throwable cause}.
   */
  public FunctionExecutionException() { }

  /**
   * Constructs a new {@link FunctionExecutionException} with the given {@link String message} describing the exception.
   *
   * @param message {@link String} containing a description of the exception.
   */
  public FunctionExecutionException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link FunctionExecutionException} with the given {@link Throwable cause}
   * as the reason this exception was thrown.
   *
   * @param cause {@link Throwable} uses as the underlying reason this exception was thrown.
   */
  public FunctionExecutionException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link FunctionExecutionException} with the given {@link String message} describing the exception
   * along with the given {@link Throwable cause} as the reason this exception was thrown.
   *
   * @param message {@link String} containing a description of the exception.
   * @param cause {@link Throwable} uses as the underlying reason this exception was thrown.
   */
  public FunctionExecutionException(String message, Throwable cause) {
    super(message, cause);
  }
}
