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
 * The InterruptedException class is a RuntimeException indicating that the current Threads execution
 * has been interrupted.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @see java.lang.Thread#interrupt()
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class InterruptedException extends RuntimeException {

  /**
   * Default constructor creating an uninitialized instance of InterruptedException.
   */
  public InterruptedException() {
  }

  /**
   * Constructs an instance of InterruptedException with the given message describing the interruption.
   * 
   * @param message a String value describing the nature of the interruption.
   */
  public InterruptedException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of InterruptedException with the given Throwable indicating the cause
   * of the interruption.
   * 
   * @param cause the Throwable indicated as the cause of the interruption.
   * @see java.lang.Throwable
   */
  public InterruptedException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of InterruptedException with a message describing the interruption and a Throwable
   * indicating the probable cause of the interruption.
   * 
   * @param message a String value describing the nature of the interruption.
   * @param cause the Throwable indicated as the cause of the interruption.
   * @see java.lang.Throwable
   */
  public InterruptedException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
