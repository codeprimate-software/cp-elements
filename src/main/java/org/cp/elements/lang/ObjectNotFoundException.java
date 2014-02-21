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
 * The ObjectNotFoundException class is a RuntimeException indicating that a object could not be found.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Object
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ObjectNotFoundException extends RuntimeException {

  /**
   * Default constructor creating an instance of the ObjectNotFoundException.
   */
  public ObjectNotFoundException() {
  }

  /**
   * Constructor to create an instance of the ObjectNotFoundException with a given message to describe the object
   * not found error.
   * <p/>
   * @param message a String value describing the nature of the object not found error.
   */
  public ObjectNotFoundException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the ObjectNotFoundException with the given Throwable to indicate the cause
   * of the object not found error.
   * <p/>
   * @param cause the Throwable indicating the cause of the object not found error.
   * @see java.lang.Throwable
   */
  public ObjectNotFoundException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the ObjectNotFoundException with a message to describe the object not found
   * error and a Throwable to indicate the probable cause of the object not found error.
   * <p/>
   * @param message a String value describing the nature of the object not found error.
   * @param cause the Throwable indicated as the cause of the object not found error.
   * @see java.lang.Throwable
   */
  public ObjectNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
