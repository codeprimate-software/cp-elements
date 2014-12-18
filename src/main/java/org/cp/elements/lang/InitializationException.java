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

/**
 * The InitializationException class is a RuntimeException indicating an error during initialization 
 * of a runtime application class or component.
 * 
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class InitializationException extends RuntimeException {

  /**
   * Constructs an uninitialized instance of the InitializationException.
   */
  public InitializationException() {
  }

  /**
   * Constructs an instance of the InitializationException initialized with the given message describing
   * the initialization error.
   *
   * @param message a String describing the nature of the initialization error.
   */
  public InitializationException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the InitializationException initialized with the given Throwable to indicate
   * the cause of the initialization error.
   *
   * @param cause the Throwable indicating the cause of the initialization error.
   * @see java.lang.Throwable
   */
  public InitializationException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the InitializationException initialized with the given message describing
   * the initialization error along with the Throwable indicating the probable cause of the initialization error.
   *
   * @param message a String describing the nature of the initialization error.
   * @param cause the Throwable indicating the cause of the initialization error.
   * @see java.lang.Throwable
   */
  public InitializationException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
