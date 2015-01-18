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
 * The RequiredPropertiesNotSetException class is a subclass of RequiredPropertyNotSetException indicating that a bean
 * property annotated with @Required was not properly set.
 *
 * @author John J. Blum
 * @see org.cp.elements.beans.IllegalPropertyValueException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class RequiredPropertyNotSetException extends IllegalPropertyValueException {

  /**
   * Constructs an uninitialized instance of the RequiredPropertyNotSetException class.
   */
  public RequiredPropertyNotSetException() {
  }

  /**
   * Constructs an instance of the RequiredPropertyNotSetException initialized with a message describing the problem.
   *
   * @param message a String describing the nature of the problem and reason this Exception was thrown.
   */
  public RequiredPropertyNotSetException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the RequiredPropertyNotSetException initialized with the specified Throwable
   * indicating the reason, or underlying cause for why this Exception was thrown.
   *
   * @param cause a Throwable indicating the reason this RequiredPropertyNotSetException was thrown.
   */
  public RequiredPropertyNotSetException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the RequiredPropertyNotSetException initialized with a message describing the exceptional
   * condition along with a Throwable indicating the reason, or underlying cause for why this Exception was thrown.
   *
   * @param message a String describing the nature of the problem and reason this Exception was thrown.
   * @param cause a Throwable indicating the reason this RequiredPropertyNotSetException was thrown.
   */
  public RequiredPropertyNotSetException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
