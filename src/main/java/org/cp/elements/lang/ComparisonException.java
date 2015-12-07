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
 * The ComparisonException class is an IllegalArgumentException that indicates two Comparable objects failed
 * a relational comparison.
 *
 * @author John J. Blum
 * @see java.lang.IllegalArgumentException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComparisonException extends IllegalArgumentException {

  /**
   * Default constructor creating an uninitialized instance of ComparisonException.
   */
  public ComparisonException() {
  }

  /**
   * Constructs an instance of ComparisonException with the given message describing the relational comparison error.
   * 
   * @param message a String value describing the nature of the relational comparison error.
   */
  public ComparisonException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of ComparisonException with the given Throwable indicating the cause
   * of the relational comparison error.
   * 
   * @param cause the Throwable indicated as the cause of this relational comparison error.
   */
  public ComparisonException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of ComparisonException with a message describing the relational comparison error
   * and a Throwable indicating the probable cause of the relational comparison error.
   * 
   * @param message a String value describing the nature of the relational comparison error.
   * @param cause the Throwable indicated as the cause of this relational comparison error.
   */
  public ComparisonException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
