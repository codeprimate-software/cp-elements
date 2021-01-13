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

/**
 * The {@link RuntimeExceptionsFactory} class is an object factory used to construct and initialize different types of
 * {@link RuntimeException RuntimeExceptions} with the added convenience of optional {@link Throwable causes}
 * and message formatting.
 *
 * This object factory creates the most common {@link RuntimeException RuntimeExceptions}.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @see org.cp.elements.lang.CheckedExceptionsFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class RuntimeExceptionsFactory extends CheckedExceptionsFactory {

  /**
   * Constructs and initializes a new {@link IllegalArgumentException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link IllegalArgumentException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IllegalArgumentException} with the given {@link String message}.
   * @see #newIllegalArgumentException(Throwable, String, Object...)
   * @see java.lang.IllegalArgumentException
   */
  public static IllegalArgumentException newIllegalArgumentException(String message, Object... args) {
    return newIllegalArgumentException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link IllegalArgumentException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link IllegalArgumentException} was thrown.
   * @param message {@link String} describing the {@link IllegalArgumentException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IllegalArgumentException} with the given {@link Throwable cause} and {@link String message}.
   * @see java.lang.IllegalArgumentException
   */
  public static IllegalArgumentException newIllegalArgumentException(Throwable cause, String message, Object... args) {
    return new IllegalArgumentException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link IllegalStateException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link IllegalStateException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IllegalStateException} with the given {@link String message}.
   * @see #newIllegalStateException(Throwable, String, Object...)
   * @see java.lang.IllegalStateException
   */
  public static IllegalStateException newIllegalStateException(String message, Object... args) {
    return newIllegalStateException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link IllegalStateException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link IllegalStateException} was thrown.
   * @param message {@link String} describing the {@link IllegalStateException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IllegalStateException} with the given {@link Throwable cause} and {@link String message}.
   * @see java.lang.IllegalStateException
   */
  public static IllegalStateException newIllegalStateException(Throwable cause, String message, Object... args) {
    return new IllegalStateException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link IndexOutOfBoundsException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link IndexOutOfBoundsException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IndexOutOfBoundsException} with the given {@link String message}.
   * @see #newIndexOutOfBoundsException(Throwable, String, Object...)
   * @see java.lang.IndexOutOfBoundsException
   */
  public static IndexOutOfBoundsException newIndexOutOfBoundsException(String message, Object... args) {
    return newIndexOutOfBoundsException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link IndexOutOfBoundsException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link IndexOutOfBoundsException} was thrown.
   * @param message {@link String} describing the {@link IndexOutOfBoundsException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link IndexOutOfBoundsException} with the given {@link Throwable cause} and {@link String message}.
   * @see java.lang.IndexOutOfBoundsException
   */
  public static IndexOutOfBoundsException newIndexOutOfBoundsException(Throwable cause,
      String message, Object... args) {

    return (IndexOutOfBoundsException) new IndexOutOfBoundsException(format(message, args)).initCause(cause);
  }

  /**
   * Constructs and initializes a new {@link NoSuchElementException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link NoSuchElementException}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link NoSuchElementException} with the given {@link String message}.
   * @see #newNoSuchElementException(Throwable, String, Object...)
   * @see java.util.NoSuchElementException
   */
  public static NoSuchElementException newNoSuchElementException(String message, Object... args) {
    return newNoSuchElementException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link NoSuchElementException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link NoSuchElementException} was thrown.
   * @param message {@link String} describing the {@link NoSuchElementException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link NoSuchElementException} with the given {@link Throwable cause} and {@link String message}.
   * @see java.util.NoSuchElementException
   */
  public static NoSuchElementException newNoSuchElementException(Throwable cause, String message, Object... args) {
    return (NoSuchElementException) new NoSuchElementException(format(message, args)).initCause(cause);
  }

  /**
   * Constructs and initializes a new {@link NullPointerException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link NullPointerException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link NullPointerException} with the given {@link String message}.
   * @see #newNullPointerException(Throwable, String, Object...)
   * @see java.lang.NullPointerException
   */
  public static NullPointerException newNullPointerException(String message, Object... args) {
    return newNullPointerException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link NullPointerException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link NullPointerException} was thrown.
   * @param message {@link String} describing the {@link NullPointerException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link NullPointerException} with the given {@link Throwable cause} and {@link String message}.
   * @see java.lang.NullPointerException
   */
  public static NullPointerException newNullPointerException(Throwable cause, String message, Object... args) {
    return (NullPointerException) new NullPointerException(format(message, args)).initCause(cause);
  }

  /**
   * Constructs and initializes a new {@link RuntimeException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link RuntimeException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link RuntimeException} with the given {@link String message}.
   * @see #newRuntimeException(Throwable, String, Object...)
   * @see java.lang.RuntimeException
   */
  public static RuntimeException newRuntimeException(String message, Object... args) {
    return newRuntimeException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link RuntimeException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link RuntimeException} was thrown.
   * @param message {@link String} describing the {@link RuntimeException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link RuntimeException} with the given {@link Throwable cause} and {@link String message}.
   * @see java.lang.RuntimeException
   */
  public static RuntimeException newRuntimeException(Throwable cause, String message, Object... args) {
    return new RuntimeException(format(message, args), cause);
  }

  /*
  public static RuntimeException newRuntimeException(Throwable cause, String message, Object... args) {
    return Optional.ofNullable(cause).map((because) -> new RuntimeException(format(message, args), because))
      .orElseGet(() -> new RuntimeException(format(message, args)));
  }
  */

  /**
   * Constructs and initializes a new {@link TypeNotPresentException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link TypeNotPresentException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link TypeNotPresentException} with the given {@link String message}.
   * @see #newTypeNotPresentException(Throwable, String, Object...)
   * @see java.lang.TypeNotPresentException
   */
  public static TypeNotPresentException newTypeNotPresentException(String message, Object... args) {
    return newTypeNotPresentException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link TypeNotPresentException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link TypeNotPresentException} was thrown.
   * @param message {@link String} describing the {@link TypeNotPresentException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link TypeNotPresentException} with the given {@link Throwable cause} and {@link String message}.
   * @see java.lang.TypeNotPresentException
   */
  public static TypeNotPresentException newTypeNotPresentException(Throwable cause, String message, Object... args) {
    return new TypeNotPresentException(format(message, args), cause);
  }

  /**
   * Constructs and initializes a new {@link UnsupportedOperationException} with the given {@link String message}
   * formatted with the given {@link Object[] arguments}.
   *
   * @param message {@link String} describing the {@link UnsupportedOperationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link UnsupportedOperationException} with the given {@link String message}.
   * @see #newUnsupportedOperationException(String, Object...)
   * @see java.lang.UnsupportedOperationException
   */
  public static UnsupportedOperationException newUnsupportedOperationException(String message, Object... args) {
    return newUnsupportedOperationException(null, message, args);
  }

  /**
   * Constructs and initializes a new {@link UnsupportedOperationException} with the given {@link Throwable cause}
   * and {@link String message} formatted with the given {@link Object[] arguments}.
   *
   * @param cause {@link Throwable} identified as the reason this {@link UnsupportedOperationException} was thrown.
   * @param message {@link String} describing the {@link UnsupportedOperationException exception}.
   * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
   * @return a new {@link UnsupportedOperationException} with the given {@link Throwable cause}
   * and {@link String message}.
   * @see java.lang.UnsupportedOperationException
   */
  public static UnsupportedOperationException newUnsupportedOperationException(Throwable cause,
      String message, Object... args) {

    return new UnsupportedOperationException(format(message, args), cause);
  }
}
