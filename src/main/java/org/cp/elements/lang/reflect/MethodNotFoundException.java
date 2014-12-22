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

package org.cp.elements.lang.reflect;

/**
 * The MethodNotFoundException class is a RuntimeException indicating that a specified method was not found on
 * a particular class type.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class MethodNotFoundException extends RuntimeException {

  /**
   * Constructs an uninitialized instance of the MethodNotFoundException.
   */
  public MethodNotFoundException() {
  }

  /**
   * Constructs an instance of the MethodNotFoundException initialized with the given message describing
   * the method not found error.
   *
   * @param message a String describing the nature of the method not found error.
   */
  public MethodNotFoundException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the MethodNotFoundException initialized with the given Throwable to indicate
   * the cause of the method not found error.
   *
   * @param cause the Throwable indicating the cause of the method not found error.
   * @see java.lang.Throwable
   */
  public MethodNotFoundException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the MethodNotFoundException initialized with the given message describing
   * the method not found error along with the Throwable indicating the probable cause of the method not found error.
   *
   * @param message a String describing the nature of the method not found error.
   * @param cause the Throwable indicating the cause of the method not found error.
   * @see java.lang.Throwable
   */
  public MethodNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
