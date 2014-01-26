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

package org.cp.elements.util.search;

/**
 * The SearchException class is a RuntimeException that is thrown when a search error occurs.
 * <p/>
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SearchException extends RuntimeException {

  /**
   * Default constructor to create an uninitialized instance of the SearchException class.
   */
  public SearchException() {
  }

  /**
   * Constructs an instance of the SearchException class with a message describing the searching failure.
   * <p/>
   * @param message a String describing the searching failure.
   */
  public SearchException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the SearchException class with the underlying cause of the searching failure.
   * <p/>
   * @param cause a Throwable indicating the underlying cause of the searching failure.
   */
  public SearchException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the SearchException class with a message describing the soring failure along
   * with the underlying cause of the searching failure.
   * <p/>
   * @param message a String describing the searching failure.
   * @param cause a Throwable indicating the underlying cause of the searching failure.
   */
  public SearchException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
