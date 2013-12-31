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

package org.cp.elements.util.convert;

/**
 * The ConversionException class is a RuntimeException indicating that a type conversion or conversion process failed.
 * <p/>
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConversionException extends RuntimeException {

  /**
   * Default constructor to create an uninitialized instance of the ConversionException class.
   */
  public ConversionException() {
  }

  /**
   * Constructs an instance of the ConversionException class with a message describing the conversion failure.
   * <p/>
   * @param message a String describing the conversion failure.
   */
  public ConversionException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the ConversionException class with the underlying cause of the conversion failure.
   * <p/>
   * @param cause a Throwable indicating the underlying cause of the conversion failure.
   */
  public ConversionException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the ConversionException class with a message describing the conversion failure along
   * with the underlying cause of the conversion failure.
   * <p/>
   * @param message a String describing the conversion failure.
   * @param cause a Throwable indicating the underlying cause of the conversion failure.
   */
  public ConversionException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
