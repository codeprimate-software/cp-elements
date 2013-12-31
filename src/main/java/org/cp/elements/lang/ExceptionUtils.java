 /*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;

/**
 * The ExceptionUtils class provides methods for working with Throwable objects (Errors and Exceptions).
 * <p/>
 * @author John J. Blum
 * @see java.lang.Error
 * @see java.lang.Throwable
 * @since 1.0.0
 */
public abstract class ExceptionUtils {

  /**
   * Gets the underlying cause of the Throwable object t if t is an instance of InvocationTargetException.
   * InvocationTargetExceptions are typically thrown when a method on some object is invoked reflectively.
   * <p/>
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
   * <p/>
   * @param t the Throwable object from which to extract the message.
   * @return the message of the Throwable object.
   * @see java.lang.Throwable#getMessage()
   */
  public static String getMessage(final Throwable t) {
    return (t == null ? null : t.getMessage());
  }

  /**
   * Determines the original cause of the Throwable object t.
   * <p/>
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
   * <p/>
   * @param t the Throwable object who's stack trace will be captured and returned.
   * @return a String value of the stack trace for the Throwable object t.
   * @see java.lang.Throwable#printStackTrace(java.io.PrintWriter)
   */
  public static String getStackTrace(final Throwable t) {
    String stackTraceValue = null;

    if (t != null) {
      final StringWriter writer = new StringWriter();
      t.printStackTrace(new PrintWriter(writer));
      stackTraceValue = writer.toString();
    }

    return stackTraceValue;
  }

}
