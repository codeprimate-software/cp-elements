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

package org.cp.elements.text;

/**
 * The FormatException class is a RuntimeException that indicates a problem with formatting textual data.
 * <p/>
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class FormatException extends RuntimeException {

  /**
   * Default constructor to create an uninitialized instance of the FormatException class.
   */
  public FormatException() {
  }

  /**
   * Constructs an instance of the FormatException class with a message describing the formatting error.
   * <p/>
   * @param message a String describing the parsing error.
   */
  public FormatException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the FormatException class along with the underlying cause of the formatting error.
   * <p/>
   * @param cause a Throwable indicating the cause of the underlying formatting error.
   */
  public FormatException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the FormatException class with a message describing the formatting error along with
   * the underlying cause of the formatting error.
   * <p/>
   * @param message a String describing the formatting error.
   * @param cause a Throwable indicating the cause of the underlying formatting error.
   */
  public FormatException(final String message, Throwable cause) {
    super(message, cause);
  }

}
