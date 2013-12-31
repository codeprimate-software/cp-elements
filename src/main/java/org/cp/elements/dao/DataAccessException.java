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

package org.cp.elements.dao;

/**
 * The DataAccessException class is a RuntimeException indicating that a data access operation was unsuccessful.
 * <p/>
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DataAccessException extends RuntimeException {

  /**
   * Default constructor creating an instance of the DataAccessException.
   */
  public DataAccessException() {
  }

  /**
   * Constructor to create an instance of the DataAccessException with a given message to describe the
   * data access error.
   * <p/>
   * @param message a String value describing the nature of the data access error.
   */
  public DataAccessException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the DataAccessException with the given Throwable indicating the cause
   * of the data access error.
   * <p/>
   * @param cause a Throwable indicated as the cause of this data access error.
   */
  public DataAccessException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the DataAccessException with both a message to describe the
   * data access error along with a Throwable indicating the probably cause of the data access error.
   * <p/>
   * @param message a String value describing the nature of the data access error.
   * @param cause a Throwable indicated as the cause of this data access error.
   */
  public DataAccessException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
