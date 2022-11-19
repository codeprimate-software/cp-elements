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

import java.util.NoSuchElementException;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link Object} factory used to construct and initialize different types of {@link RuntimeException RuntimeExceptions}
 * with the added convenience of optional {@link Throwable causes} and message formatting.
 *
 * This {@link Object} factory creates the most common {@link RuntimeException RuntimeExceptions}.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @see org.cp.elements.lang.CheckedExceptionsFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class RuntimeExceptionsFactory extends CheckedExceptionsFactory {

  /**
   * Constructs a new instance of {@link IllegalArgumentException} initialized with the given {@link String message}
   * formatted with the array of {@link Object arguments}.
   *
   * @param message {@link String} describing the {@link IllegalArgumentException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IllegalArgumentException} initialized with the given {@link String message}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see #newIllegalArgumentException(Throwable, String, Object...)
   * @see java.lang.IllegalArgumentException
   */
  public static @NotNull IllegalArgumentException newIllegalArgumentException(@NotNull String message, Object... args) {
    return newIllegalArgumentException(DEFAULT_CAUSE, message, args);
  }

  /**
   * Constructs a new instance of {@link IllegalArgumentException} initialized with the given {@link Throwable cause}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link IllegalArgumentException}.
   * @return a new {@link IllegalArgumentException} initialized with the given {@link Throwable cause}.
   * @see #newIllegalArgumentException(Throwable, String, Object...)
   * @see java.lang.IllegalArgumentException
   */
  public static @NotNull IllegalArgumentException newIllegalArgumentException(@Nullable Throwable cause) {
    return newIllegalArgumentException(cause, UNKNOWN_REASON_MESSAGE);
  }

  /**
   * Constructs a new instance of {@link IllegalArgumentException} initialized with the given {@link Throwable cause}
   * and {@link String message} formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link IllegalArgumentException}.
   * @param message {@link String} describing the {@link IllegalArgumentException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IllegalArgumentException} initialized with the given {@link String message}
   * and {@link Throwable cause}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see java.lang.IllegalArgumentException
   */
  public static @NotNull IllegalArgumentException newIllegalArgumentException(@Nullable Throwable cause,
      @NotNull String message, Object... args) {

    return new IllegalArgumentException(format(message, args), cause);
  }

  /**
   * Constructs a new instance of {@link IllegalStateException} initialized with the given {@link String message}
   * formatted with the given array of {@link Object arguments}.
   *
   * @param message {@link String} describing the {@link IllegalStateException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IllegalStateException} initialized with the given {@link String message}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see #newIllegalStateException(Throwable, String, Object...)
   * @see java.lang.IllegalStateException
   */
  public static @NotNull IllegalStateException newIllegalStateException(@NotNull String message, Object... args) {
    return newIllegalStateException(null, message, args);
  }

  /**
   * Constructs a new instance of {@link IllegalStateException} initialized with the given {@link String message}
   * formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of this {@link IllegalStateException}.
   * @return a new {@link IllegalStateException} initialized with the given {@link Throwable cause}.
   * @see #newIllegalStateException(Throwable, String, Object...)
   * @see java.lang.IllegalStateException
   */
  public static @NotNull IllegalStateException newIllegalStateException(@Nullable Throwable cause) {
    return newIllegalStateException(cause, UNKNOWN_REASON_MESSAGE);
  }

  /**
   * Constructs a new instance of {@link IllegalStateException} initialized with the given {@link Throwable cause}
   * and {@link String message} formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of this {@link IllegalStateException}.
   * @param message {@link String} describing the {@link IllegalStateException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IllegalStateException} initialized with the given {@link String message}
   * and {@link Throwable cause}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see java.lang.IllegalStateException
   */
  public static @NotNull IllegalStateException newIllegalStateException(@Nullable Throwable cause,
      @NotNull String message, Object... args) {

    return new IllegalStateException(format(message, args), cause);
  }

  /**
   * Constructs a new instance of {@link IndexOutOfBoundsException} initialized with the given {@link String message}
   * formatted with the given array of {@link Object arguments}.
   *
   * @param message {@link String} describing the {@link IndexOutOfBoundsException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IndexOutOfBoundsException} initialized with the given {@link String message}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see #newIndexOutOfBoundsException(Throwable, String, Object...)
   * @see java.lang.IndexOutOfBoundsException
   */
  public static @NotNull IndexOutOfBoundsException newIndexOutOfBoundsException(@NotNull String message,
      Object... args) {

    return newIndexOutOfBoundsException(null, message, args);
  }

  /**
   * Constructs a new instance of {@link IndexOutOfBoundsException} initialized with the given {@link String message}
   * formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link IndexOutOfBoundsException}.
   * @return a new {@link IndexOutOfBoundsException} initialized with the given {@link Throwable cause}.
   * @see #newIndexOutOfBoundsException(Throwable, String, Object...)
   * @see java.lang.IndexOutOfBoundsException
   */
  public static @NotNull IndexOutOfBoundsException newIndexOutOfBoundsException(@Nullable Throwable cause) {
    return newIndexOutOfBoundsException(cause, UNKNOWN_REASON_MESSAGE);
  }

  /**
   * Constructs a new instance of {@link IndexOutOfBoundsException} initialized with the given {@link Throwable}
   * and {@link String message} formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link IndexOutOfBoundsException}.
   * @param message {@link String} describing the {@link IndexOutOfBoundsException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IndexOutOfBoundsException} initialized with the given {@link String message}
   * and {@link Throwable cause}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see java.lang.IndexOutOfBoundsException
   */
  public static @NotNull IndexOutOfBoundsException newIndexOutOfBoundsException(@Nullable Throwable cause,
      @NotNull String message, Object... args) {

    return (IndexOutOfBoundsException) new IndexOutOfBoundsException(format(message, args)).initCause(cause);
  }

  /**
   * Constructs a new instance of {@link NoSuchElementException} initialized with the given {@link String message}
   * formatted with the given array of {@link Object arguments}.
   *
   * @param message {@link String} describing the {@link NoSuchElementException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link NoSuchElementException} initialized with the given {@link String message}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see #newNoSuchElementException(Throwable, String, Object...)
   * @see java.util.NoSuchElementException
   */
  public static @NotNull NoSuchElementException newNoSuchElementException(@NotNull String message, Object... args) {
    return newNoSuchElementException(null, message, args);
  }

  /**
   * Constructs a new instance of {@link NoSuchElementException} initialized with the given {@link String message}
   * formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of this {@link NoSuchElementException}.
   * @return a new {@link NoSuchElementException} initialized with the given {@link Throwable cause}.
   * @see #newNoSuchElementException(Throwable, String, Object...)
   * @see java.util.NoSuchElementException
   */
  public static @NotNull NoSuchElementException newNoSuchElementException(@Nullable Throwable cause) {
    return newNoSuchElementException(cause, UNKNOWN_REASON_MESSAGE);
  }

  /**
   * Constructs a new instance of {@link NoSuchElementException} initialized with the given {@link Throwable cause}
   * and {@link String message} formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of this {@link NoSuchElementException}.
   * @param message {@link String} describing the {@link NoSuchElementException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link NoSuchElementException} initialized with the given {@link String message}
   * and {@link Throwable cause}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see java.util.NoSuchElementException
   */
  public static @NotNull NoSuchElementException newNoSuchElementException(@Nullable Throwable cause,
      @NotNull String message, Object... args) {

    return (NoSuchElementException) new NoSuchElementException(format(message, args)).initCause(cause);
  }

  /**
   * Constructs a new instance of {@link NullPointerException} initialized with the given {@link String message}
   * formatted with the given array of {@link Object arguments}.
   *
   * @param message {@link String} describing the {@link NullPointerException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link NullPointerException} initialized with the given {@link String message}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see #newNullPointerException(Throwable, String, Object...)
   * @see java.lang.NullPointerException
   */
  public static @NotNull NullPointerException newNullPointerException(@NotNull String message, Object... args) {
    return newNullPointerException(null, message, args);
  }

  /**
   * Constructs a new instance of {@link NullPointerException} initialized with the given {@link Throwable cause}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link NullPointerException}.
   * @return a new {@link NullPointerException} initialized with the given {@link Throwable cause}.
   * @see #newNullPointerException(Throwable, String, Object...)
   * @see java.lang.NullPointerException
   */
  public static @NotNull NullPointerException newNullPointerException(@Nullable Throwable cause) {
    return newNullPointerException(cause, UNKNOWN_REASON_MESSAGE);
  }

  /**
   * Constructs a new instance of {@link NullPointerException} initialized with the given {@link Throwable cause}
   * and {@link String message} formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link NullPointerException}.
   * @param message {@link String} describing the {@link NullPointerException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link NullPointerException} initialized with the given {@link String message}
   * and {@link Throwable cause}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see java.lang.NullPointerException
   */
  public static @NotNull NullPointerException newNullPointerException(@Nullable Throwable cause,
      @NotNull String message, Object... args) {

    return (NullPointerException) new NullPointerException(format(message, args)).initCause(cause);
  }

  /**
   * Constructs a new instance of {@link RuntimeException} initialized with the given {@link String message}
   * formatted with the given array of {@link Object arguments}.
   *
   * @param message {@link String} describing the {@link RuntimeException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link RuntimeException} initialized with the given {@link String message}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see #newRuntimeException(Throwable, String, Object...)
   * @see java.lang.RuntimeException
   */
  public static @NotNull RuntimeException newRuntimeException(@NotNull String message, Object... args) {
    return newRuntimeException(null, message, args);
  }

  /**
   * Constructs a new instance of {@link RuntimeException} initialized with the given {@link Throwable cause}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link RuntimeException}.
   * @return a new {@link RuntimeException} initialized with the given {@link Throwable cause}.
   * @see #newRuntimeException(Throwable, String, Object...)
   * @see java.lang.RuntimeException
   */
  public static @NotNull RuntimeException newRuntimeException(@Nullable Throwable cause) {
    return newRuntimeException(cause, UNKNOWN_REASON_MESSAGE);
  }

  /**
   * Constructs a new instance of {@link RuntimeException} initialized with the given {@link Throwable cause}
   * and {@link String message} formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link RuntimeException}.
   * @param message {@link String} describing the {@link RuntimeException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link RuntimeException} initialized with the given {@link String message}
   * and {@link Throwable cause}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see java.lang.RuntimeException
   */
  public static @NotNull RuntimeException newRuntimeException(@Nullable Throwable cause,
      @NotNull String message, Object... args) {

    return new RuntimeException(format(message, args), cause);
  }

  /**
   * Constructs a new instance of {@link TypeNotPresentException} initialized with the given {@link String message}
   * formatted with the given array of {@link Object arguments}.
   *
   * @param message {@link String} describing the {@link TypeNotPresentException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link TypeNotPresentException} initialized with the given {@link String message}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see #newTypeNotPresentException(Throwable, String, Object...)
   * @see java.lang.TypeNotPresentException
   */
  public static @NotNull TypeNotPresentException newTypeNotPresentException(@NotNull String message, Object... args) {
    return newTypeNotPresentException(null, message, args);
  }

  /**
   * Constructs a new instance of {@link TypeNotPresentException} initialized with the given {@link Throwable cause}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link TypeNotPresentException}.
   * @return a new {@link TypeNotPresentException} initialized with the given {@link Throwable cause}.
   * @see #newTypeNotPresentException(Throwable, String, Object...)
   * @see java.lang.TypeNotPresentException
   */
  public static @NotNull TypeNotPresentException newTypeNotPresentException(@Nullable Throwable cause) {
    return newTypeNotPresentException(cause, UNKNOWN_REASON_MESSAGE);
  }

  /**
   * Constructs a new instance of {@link TypeNotPresentException} initialized with the given {@link Throwable cause}
   * and {@link String message} formatted with the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link TypeNotPresentException}.
   * @param message {@link String} describing the {@link TypeNotPresentException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link TypeNotPresentException} initialized with the given {@link String message}
   * and {@link Throwable cause}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see java.lang.TypeNotPresentException
   */
  public static @NotNull TypeNotPresentException newTypeNotPresentException(@Nullable Throwable cause,
      @NotNull String message, Object... args) {

    return new TypeNotPresentException(format(message, args), cause);
  }

  /**
   * Constructs a new instance of {@link UnsupportedOperationException} initialized with
   * the given {@link String message} formatted with the given array of {@link Object arguments}.
   *
   * @param message {@link String} describing the {@link UnsupportedOperationException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link UnsupportedOperationException} initialized with the given {@link String message}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see #newUnsupportedOperationException(Throwable, String, Object...)
   * @see java.lang.UnsupportedOperationException
   */
  public static @NotNull UnsupportedOperationException newUnsupportedOperationException(@NotNull String message,
      Object... args) {

    return newUnsupportedOperationException(null, message, args);
  }

  /**
   * Constructs a new instance of {@link UnsupportedOperationException} initialized with
   * the given {@link Throwable cause}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link UnsupportedOperationException}.
   * @return a new {@link UnsupportedOperationException} initialized with the given {@link Throwable cause}.
   * @see #newUnsupportedOperationException(Throwable, String, Object...)
   * @see java.lang.UnsupportedOperationException
   */
  public static @NotNull UnsupportedOperationException newUnsupportedOperationException(@Nullable Throwable cause) {
    return newUnsupportedOperationException(cause, UNKNOWN_REASON_MESSAGE);
  }

  /**
   * Constructs a new instance of {@link UnsupportedOperationException} initialized with
   * the given {@link Throwable cause} and {@link String message} formatted with
   * the given array of {@link Object arguments}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of the {@link UnsupportedOperationException}.
   * @param message {@link String} describing the {@link UnsupportedOperationException}; must not be {@literal null}.
   * @param args array of {@link Object arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link UnsupportedOperationException} initialized with the given {@link String message}
   * and {@link Throwable cause}.
   * @throws NullPointerException if the {@link String message} is {@literal null}.
   * @see java.lang.UnsupportedOperationException
   */
  public static @NotNull UnsupportedOperationException newUnsupportedOperationException(@Nullable Throwable cause,
      @NotNull String message, Object... args) {

    return new UnsupportedOperationException(format(message, args), cause);
  }
}
