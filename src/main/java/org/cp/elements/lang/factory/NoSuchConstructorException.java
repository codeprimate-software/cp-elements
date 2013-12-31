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

package org.cp.elements.lang.factory;

/**
 * The NoSuchConstructorException is a RuntimeException indicating that a constructor with the specified signature
 * could not be found in the specified class.  This class is the unchecked, runtime equivalent of the checked
 * NoSuchMethodException with emphasis on constructors!
 * <p/>
 * @author John J. Blum
 * @see java.lang.NoSuchMethodException
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class NoSuchConstructorException extends RuntimeException {

  /**
   * Default constructor creating an instance of the NoSuchConstructorException.
   */
  public NoSuchConstructorException() {
  }

  /**
   * Constructor to create an instance of the NoSuchConstructorException with a given message to describe the missing
   * constructor.
   * <p/>
   * @param message a String value describing the nature of the missing constructor.
   */
  public NoSuchConstructorException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the NoSuchConstructorException with the given Throwable to indicate the cause
   * of the missing constructor.
   * <p/>
   * @param cause the Throwable indicating the cause of the missing constructor.
   */
  public NoSuchConstructorException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the NoSuchConstructorException with a message to describe the missing
   * constructor and a Throwable to indicate the probable cause of the missing constructor.
   * <p/>
   * @param message a String value describing the nature of the missing constructor.
   * @param cause the Throwable indicated as the cause of the missing constructor.
   */
  public NoSuchConstructorException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
