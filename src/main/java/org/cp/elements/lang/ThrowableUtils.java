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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;

/**
 * The ThrowableUtils class provides methods for working with Throwable objects (Errors and Exceptions).
 * 
 * @author John J. Blum
 * @see java.lang.Error
 * @see java.lang.Throwable
 * @since 1.0.0
 */
public abstract class ThrowableUtils {

  /**
   * Gets the underlying cause of the Throwable object t if t is an instance of InvocationTargetException.
   * InvocationTargetExceptions are typically thrown when a method on some object is invoked reflectively.
   * 
   * @param t the Throwable object being evaluated in order to determine the underlying cause.
   * @return a Throwable object indicating the underlying cause of t if t is an instance of InvocationTargetException,
   * otherwise return the Throwable object t.
   * @see java.lang.Throwable#getCause()
   * @see java.lang.reflect.InvocationTargetException
   */
  public static Throwable getCauseOfInvocationTargetException(final Throwable t) {
    return (t instanceof InvocationTargetException ? t.getCause() : t);
  }

  /**
   * Gets the message of the Throwable object t.  If the Throwable object reference is null, then this method
   * returns null.
   * 
   * @param t the Throwable object from which to extract the message.
   * @return the message of the Throwable object.
   * @see java.lang.Throwable#getMessage()
   */
  public static String getMessage(final Throwable t) {
    return (t == null ? null : t.getMessage());
  }

  /**
   * Determines the original cause of the Throwable object t.
   * 
   * @param t the Throwable object who's root cause is determined.
   * @return a Throwable object indicating the root cause of the Throwable object t, or null if the Throwable object
   * reference is null.
   * @see java.lang.Throwable#getCause()
   */
  public static Throwable getRootCause(Throwable t) {
    if (t != null) {
      while (t.getCause() != null) {
        t = t.getCause();
      }
    }

    return t;
  }

  /**
   * Gets the stack trace of Throwable object t as a String value.  If t is null, then a null String is returned.
   * 
   * @param t the Throwable object who's stack trace will be captured and returned.
   * @return a String value of the stack trace for the Throwable object t.
   * @see java.lang.Throwable#printStackTrace(java.io.PrintWriter)
   */
  public static String getStackTrace(final Throwable t) {
    String stackTraceValue = null;

    if (t != null) {
      StringWriter writer = new StringWriter();
      t.printStackTrace(new PrintWriter(writer));
      stackTraceValue = writer.toString();
    }

    return stackTraceValue;
  }

}
