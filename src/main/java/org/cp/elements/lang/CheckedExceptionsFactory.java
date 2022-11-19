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

import static org.cp.elements.text.FormatUtils.format;

import java.io.IOException;
import java.util.concurrent.TimeoutException;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link Object} factory used to construct and initialize common, {@literal checked} {@link Exception Exceptions}.
 *
 * @author John Blum
 * @see java.lang.Exception
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class CheckedExceptionsFactory {

  protected static final String UNKNOWN_REASON_MESSAGE = "?";

  protected static final Throwable DEFAULT_CAUSE = null;

  /**
   * Constructs a new instance of {@link CloneNotSupportedException} initialized with the given {@link String message}
   * formatted with the given array of {@link Object arguments}.
   *
   * @param message {@link String} describing the {@link CloneNotSupportedException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link CloneNotSupportedException} initialized with the given {@link String message}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see #newCloneNotSupportedException(Throwable, String, Object...)
   * @see java.lang.CloneNotSupportedException
   */
  public static @NotNull CloneNotSupportedException newCloneNotSupportedException(@NotNull String message,
      Object... args) {

    return newCloneNotSupportedException(null, message, args);
  }

  /**
   * Constructs a new instance of {@link CloneNotSupportedException} initialized with the given {@link Throwable cause}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link CloneNotSupportedException}.
   * @return a new {@link CloneNotSupportedException} initialized with the given {@link Throwable cause}.
   * @see #newCloneNotSupportedException(Throwable, String, Object...)
   * @see java.lang.CloneNotSupportedException
   */
  public static @NotNull CloneNotSupportedException newCloneNotSupportedException(@Nullable Throwable cause) {
    return newCloneNotSupportedException(cause, UNKNOWN_REASON_MESSAGE);
  }

  /**
   * Constructs a new instance of {@link CloneNotSupportedException} initialized with the given {@link Throwable cause}
   * and {@link String message} formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link CloneNotSupportedException}.
   * @param message {@link String} describing the {@link CloneNotSupportedException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link CloneNotSupportedException} initialized with the given {@link String message}
   * and {@link Throwable cause}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see java.lang.CloneNotSupportedException
   */
  public static @NotNull CloneNotSupportedException newCloneNotSupportedException(@Nullable Throwable cause,
      @NotNull String message, Object... args) {

    return (CloneNotSupportedException) new CloneNotSupportedException(format(message, args)).initCause(cause);
  }

  /**
   * Constructs a new instance of {@link IOException} initialized with the given {@link String message}
   * formatted with the given array of {@link Object arguments}.
   *
   * @param message {@link String} describing the {@link IOException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IOException} initialized with the given {@link String message}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see #newIOException(Throwable, String, Object...)
   * @see java.io.IOException
   */
  public static @NotNull IOException newIOException(@NotNull String message, Object... args) {
    return newIOException(null, message, args);
  }

  /**
   * Constructs a new instance of {@link IOException} initialized with the given {@link Throwable cause}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link IOException}.
   * @return a new {@link IOException} initialized with the given {@link Throwable cause}.
   * @see #newIOException(Throwable, String, Object...)
   * @see java.io.IOException
   */
  public static @NotNull IOException newIOException(@Nullable Throwable cause) {
    return newIOException(cause, UNKNOWN_REASON_MESSAGE);
  }

  /**
   * Constructs a new instance of {@link IOException} initialized with the given {@link Throwable cause}
   * and {@link String message} formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link IOException}.
   * @param message {@link String} describing the {@link IOException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IOException} initialized with the given {@link String message} and {@link Throwable cause}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see java.io.IOException
   */
  public static @NotNull IOException newIOException(@Nullable Throwable cause,
      @NotNull String message, Object... args) {

    return new IOException(format(message, args), cause);
  }

  /**
   * Constructs a new instance of {@link TimeoutException} initialized with the given {@link String message}
   * formatted with the given array of {@link Object arguments}.
   *
   * @param message {@link String} describing the {@link TimeoutException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link TimeoutException} initialized with the given {@link String message}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see #newTimeoutException(Throwable, String, Object...)
   * @see java.util.concurrent.TimeoutException
   */
  public static @NotNull TimeoutException newTimeoutException(@NotNull String message, Object... args) {
    return newTimeoutException(null, message, args);
  }

  /**
   * Constructs a new instance of {@link TimeoutException} initialized with the given {@link Throwable cause}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link TimeoutException}.
   * @return a new {@link TimeoutException} initialized with the given {@link Throwable cause}.
   * @see #newTimeoutException(Throwable, String, Object...)
   * @see java.util.concurrent.TimeoutException
   */
  public static @NotNull TimeoutException newTimeoutException(@Nullable Throwable cause) {
    return newTimeoutException(cause, UNKNOWN_REASON_MESSAGE);
  }

  /**
   * Constructs a new instance of {@link TimeoutException} initialized with the given {@link Throwable cause}
   * and {@link String message} formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link TimeoutException}.
   * @param message {@link String} describing the {@link TimeoutException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link TimeoutException} initialized with the given {@link String message}
   * and {@link Throwable cause}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see java.util.concurrent.TimeoutException
   */
  public static @NotNull TimeoutException newTimeoutException(@Nullable Throwable cause,
      @NotNull String message, Object... args) {

    return (TimeoutException) new TimeoutException(format(message, args)).initCause(cause);
  }
}
