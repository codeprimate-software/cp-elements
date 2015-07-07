/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

/**
 * CloneException is a RuntimeException type indicating a clone, or copy operation failure.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CloneException extends RuntimeException {

  /**
   * Constructs an uninitialized instance of the CloneException.
   */
  public CloneException() {
  }

  /**
   * Constructor to create an instance of the CloneException with a given message describing the clone error.
   *
   * @param message a String value describing the nature of the clone error.
   */
  public CloneException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the CloneException with the given Throwable to indicate the cause
   * of the clone error.
   *
   * @param cause the Throwable indicating the cause of the clone error.
   * @see java.lang.Throwable
   */
  public CloneException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the CloneException with a message describing the clone error
   * and a Throwable to indicate the probable cause of the clone error.
   *
   * @param message a String value describing the nature of the clone error.
   * @param cause the Throwable indicating the cause of the clone error.
   * @see java.lang.Throwable
   */
  public CloneException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
