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

package org.cp.elements.lang;

import static org.cp.elements.text.FormatUtils.format;

import java.io.IOException;
import java.util.concurrent.TimeoutException;

/**
 * The {@link CheckedExceptionsFactory} class is an object factory class used to construct and initialize
 * common "checked" {@link Exception} classes.
 *
 * @author John Blum
 * @see java.lang.Exception
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class CheckedExceptionsFactory {

  /**
   * Constructs and initializes a new {@link CloneNotSupportedException} with the given {@link String message}
   * formatted with the given {@link Object[] args}.
   *
   * @param message {@link String} describing the exception.
   * @param args {@link Object[]} of values used to replace format placeholders in the {@link String message}.
   * @return a new {@link CloneNotSupportedException} with the given {@link String message}.
   * @see #newCloneNotSupportedException(Throwable, String, Object...)
   * @see java.lang.CloneNotSupportedException
   */
  public static CloneNotSupportedException newCloneNotSupportedException(String message, Object... args) {
    return newCloneNotSupportedException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link CloneNotSupportedException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] args}.
   *
   * @param cause {@link Throwable} identified as the reason this exception was thrown.
   * @param message {@link String} describing the exception.
   * @param args {@link Object[]} of values used to replace format placeholders in the {@link String message}.
   * @return a new {@link CloneNotSupportedException} with the given {@link Throwable cause} and {@link String message}.
   * @see java.lang.CloneNotSupportedException
   */
  public static CloneNotSupportedException newCloneNotSupportedException(Throwable cause,
      String message, Object... args) {

    return (CloneNotSupportedException) new CloneNotSupportedException(format(message, args)).initCause(cause);
  }

  /**
   * Constructs and initializes a new {@link IOException} with the given {@link String message}
   * formatted with the given {@link Object[] args}.
   *
   * @param message {@link String} describing the exception.
   * @param args {@link Object[]} of values used to replace format placeholders in the {@link String message}.
   * @return a new {@link IOException} with the given {@link String message}.
   * @see #newIOException(Throwable, String, Object...)
   * @see java.io.IOException
   */
  public static IOException newIOException(String message, Object... args) {
    return newIOException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link IOException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] args}.
   *
   * @param cause {@link Throwable} identified as the reason this exception was thrown.
   * @param message {@link String} describing the exception.
   * @param args {@link Object[]} of values used to replace format placeholders in the {@link String message}.
   * @return a new {@link IOException} with the given {@link Throwable cause} and {@link String message}.
   * @see java.io.IOException
   */
  public static IOException newIOException(Throwable cause, String message, Object... args) {
    return new IOException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link TimeoutException} with the given {@link String message}
   * formatted with the given {@link Object[] args}.
   *
   * @param message {@link String} describing the exception.
   * @param args {@link Object[]} of values used to replace format placeholders in the {@link String message}.
   * @return a new {@link TimeoutException} with the given {@link String message}.
   * @see #newTimeoutException(Throwable, String, Object...)
   * @see java.util.concurrent.TimeoutException
   */
  public static TimeoutException newTimeoutException(String message, Object... args) {
    return newTimeoutException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link TimeoutException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] args}.
   *
   * @param cause {@link Throwable} identified as the reason this exception was thrown.
   * @param message {@link String} describing the exception.
   * @param args {@link Object[]} of values used to replace format placeholders in the {@link String message}.
   * @return a new {@link TimeoutException} with the given {@link Throwable cause} and {@link String message}.
   * @see java.util.concurrent.TimeoutException
   */
  public static TimeoutException newTimeoutException(Throwable cause, String message, Object... args) {
    return (TimeoutException) new TimeoutException(format(message, args)).initCause(cause);
  }
}
