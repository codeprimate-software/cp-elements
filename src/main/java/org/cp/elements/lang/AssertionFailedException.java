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
 * The AssertionFailedException is a RuntimeException indicating an assertion failure using the Assert class.
 * </p>
 * @author John J. Blum
 * @see java.lang.AssertionError
 * @see java.lang.RuntimeException
 * @see org.cp.elements.lang.AssertionFailedException
 * @since 1.0.0
 * @version 1.0.0
 */
@SuppressWarnings("unused")
public class AssertionFailedException extends RuntimeException {

  /**
   * Default constructor creating an instance of the AssertionFailedException.
   */
  public AssertionFailedException() {
  }

  /**
   * Constructor to create an instance of the AssertionFailedException with the given message to describe the
   * assertion failure.
   * <p/>
   * @param message a String value describing the nature of the assertion failure.
   */
  public AssertionFailedException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the AssertionFailedException with the given Throwable to indicated the cause
   * of the assertion failure.
   * <p/>
   * @param cause the Throwable indicated as the cause of this assertion failure.
   */
  public AssertionFailedException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the AssertionFailedException with a message to describe the assertion failure
   * and a Throwable to indicate the probable cause of the assertion failure.
   * <p/>
   * @param message a String value describing the nature of the assertion failure.
   * @param cause the Throwable indicated as the cause of this assertion failure.
   */
  public AssertionFailedException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
