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
 * The EqualityException class is an IllegalArgumentException indicating that two objects are not equal.
 * 
 * @author John J. Blum
 * @see java.lang.IllegalArgumentException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class EqualityException extends IllegalArgumentException {

  /**
   * Default constructor creating an instance of the EqualityException.
   */
  public EqualityException() {
  }

  /**
   * Constructor to create an instance of the EqualityException with the given message to describe the equality
   * comparison error.
   * 
   * @param message a String value describing the nature of the equality comparison error.
   */
  public EqualityException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the EqualityException with the given Throwable to indicate the cause
   * of the equality comparison error.
   * 
   * @param cause the Throwable indicated as the cause of this equality comparison error.
   */
  public EqualityException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the EqualityException with a message to describe the equality comparison error
   * and a Throwable to indicate the probable cause of the equality comparison error.
   * 
   * @param message a String value describing the nature of the equality comparison error.
   * @param cause the Throwable indicated as the cause of this equality comparison error.
   */
  public EqualityException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
