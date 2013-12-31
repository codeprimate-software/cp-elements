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

package org.cp.elements.service;

/**
 * The ServiceInvocationException class is a RuntimeException indicating an error in a Service call.
 * <p/>
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ServiceInvocationException extends RuntimeException {

  /**
   * Default constructor creating an instance of the ServiceInvocationException.
   */
  public ServiceInvocationException() {
  }

  /**
   * Constructor to create an instance of the ServiceInvocationException with a given message to describe the
   * service call error.
   * <p/>
   * @param message a String value describing the nature of the service call error.
   */
  public ServiceInvocationException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the ServiceInvocationException with the given Throwable indicating the cause
   * of the service call error.
   * <p/>
   * @param cause a Throwable indicated as the cause of this service call error.
   */
  public ServiceInvocationException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the ServiceInvocationException with both a message to describe the
   * service call error along with a Throwable indicating the probably cause of the service call error.
   * <p/>
   * @param message a String value describing the nature of the service call error.
   * @param cause a Throwable indicated as the cause of this service call error.
   */
  public ServiceInvocationException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
