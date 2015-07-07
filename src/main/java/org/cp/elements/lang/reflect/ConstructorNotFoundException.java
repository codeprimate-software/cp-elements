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

package org.cp.elements.lang.reflect;

/**
 * ConstructorNotFoundException is a RuntimeException type indicating that a constructor with the specified signature
 * could not be found on a given class type.
 * 
 * @author John J. Blum
 * @see org.cp.elements.lang.reflect.MethodNotFoundException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConstructorNotFoundException extends MethodNotFoundException {

  /**
   * Constructs an uninitialized instance of the ConstructorNotFoundException.
   */
  public ConstructorNotFoundException() {
  }

  /**
   * Constructs an instance of the ConstructorNotFoundException initialized with the given message describing
   * the constructor not found error.
   *
   * @param message a String describing the nature of the constructor not found error.
   */
  public ConstructorNotFoundException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the ConstructorNotFoundException initialized with the given Throwable to indicate
   * the cause of the constructor not found error.
   *
   * @param cause the Throwable indicating the cause of the constructor not found error.
   * @see java.lang.Throwable
   */
  public ConstructorNotFoundException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the ConstructorNotFoundException initialized with the given message describing
   * the constructor not found error along with the Throwable indicating the probable cause
   * of the constructor not found error.
   *
   * @param message a String describing the nature of the constructor not found error.
   * @param cause the Throwable indicating the cause of the constructor not found error.
   * @see java.lang.Throwable
   */
  public ConstructorNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
