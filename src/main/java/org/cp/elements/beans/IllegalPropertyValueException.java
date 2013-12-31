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

package org.cp.elements.beans;

/**
 * The IllegalPropertyValueException class signifies that the value being assigned to the property is not valid.
 * <p/>
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class IllegalPropertyValueException extends RuntimeException {

  /**
   * Creates an instance of the IllegalPropertyValueException class.
   */
  public IllegalPropertyValueException() {
  }

  /**
   * Creates an instance of the IllegalPropertyValueException class initialized with a description of the problem.
   * <p/>
   * @param message a String describing the nature of the problem and reason this Exception was thrown.
   */
  public IllegalPropertyValueException(final String message) {
    super(message);
  }

  /**
   * Creates an instance of the IllegalPropertyValueException class initialized with the specified Throwable, which is
   * also the reason, or underlying cause for why this Exception was thrown.
   * <p/>
   * @param cause a Throwable indicating the reason this IllegalPropertyValueException was thrown.
   */
  public IllegalPropertyValueException(final Throwable cause) {
    super(cause);
  }

  /**
   * Creates an instance of the IllegalPropertyValueException class initialized with a message describing the exceptional
   * condition and a reason, or underlying cause for why this Exception was thrown.
   * <p/>
   * @param message a String describing the nature of the problem and reason this Exception was thrown.
   * @param cause a Throwable indicating the reason this IllegalPropertyValueException was thrown.
   */
  public IllegalPropertyValueException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
