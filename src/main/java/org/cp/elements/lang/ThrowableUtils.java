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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;

import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

 /**
 * {@link ThrowableUtils} provides methods for working with {@link Throwable} objects
  * ({@link Error Errors} and {@link Exception Exceptions}).
 *
 * @author John J. Blum
 * @see java.lang.Error
 * @see java.lang.Throwable
 * @since 1.0.0
 */
public abstract class ThrowableUtils {

  /**
   * Null-safe method returning the cause of the given {@link Throwable} object.  If the {@link Throwable} object
   * reference is {@literal null} then this method returns {@literal null}.
   *
   * @param throwable {@link Throwable} object who's cause is returned.
   * @return the cause of the given {@link Throwable} object.
   * @see java.lang.Throwable#getCause()
   */
  @NullSafe
  public static @Nullable Throwable getCause(@Nullable Throwable throwable) {
    return throwable != null ? throwable.getCause() : null;
  }

  /**
   * Returns the underlying cause of the given {@link Throwable} object if the {@link Throwable} object
   * is an instance of {@link InvocationTargetException}.  An {@link InvocationTargetException} is typically thrown
   * when a method on some object is invoked reflectively.
   *
   * @param throwable {@link Throwable} object to evaluate as a {@link InvocationTargetException}.
   * @return a {@link Throwable} object indicating the underlying cause of {@code throwable} if {@code throwable} is an instance
   * of {@link InvocationTargetException} otherwise return {@code throwable}.
   * @see java.lang.reflect.InvocationTargetException
   * @see java.lang.Throwable#getCause()
   */
  @NullSafe
  public static @Nullable Throwable getCauseOfInvocationTargetException(@Nullable Throwable throwable) {
    return throwable instanceof InvocationTargetException ? throwable.getCause() : throwable;
  }

  /**
   * Null-safe method returning the message of the given {@link Throwable} object.  If the {@link Throwable} object
   * reference is {@literal null} then this method returns {@literal null}.
   *
   * @param throwable {@link Throwable} object who's message is returned.
   * @return the message of the given {@link Throwable} object.
   * @see java.lang.Throwable#getMessage()
   */
  @NullSafe
  public static @Nullable String getMessage(@Nullable Throwable throwable) {
    return throwable != null ? throwable.getMessage() : null;
  }

  /**
   * Determines the original, root cause of the given {@link Throwable} object.
   *
   * @param throwable {@link Throwable} object who's root cause will be determined.
   * @return a {@link Throwable} object indicating the root cause of the given {@link Throwable} object,
   * or {@literal null} if the {@link Throwable} object reference is {@literal null}.
   * @see java.lang.Throwable#getCause()
   */
  @NullSafe
  @SuppressWarnings("all")
  public static @Nullable Throwable getRootCause(@Nullable Throwable throwable) {

    Throwable throwableCause = throwable;

    while (getCause(throwableCause) != null) {
      throwableCause = throwableCause.getCause();
    }

    return throwableCause;
  }

  /**
   * Returns the Stack Trace of the given {@link Throwable} object as a {@link String}.  Returns {@literal null}
   * if the given {@link Throwable} object reference is {@literal null}.
   *
   * @param throwable {@link Throwable} object who's Stack Trace is returned.
   * @return the Stack Trace of the given {@link Throwable} object as a {@link String}.
   * @see java.lang.Throwable#printStackTrace(java.io.PrintWriter)
   */
  @NullSafe
  public static @Nullable String getStackTrace(@Nullable Throwable throwable) {

    String stackTraceValue = null;

    if (throwable != null) {
      StringWriter writer = new StringWriter();
      throwable.printStackTrace(new PrintWriter(writer));
      stackTraceValue = writer.toString();
    }

    return stackTraceValue;
  }
}
