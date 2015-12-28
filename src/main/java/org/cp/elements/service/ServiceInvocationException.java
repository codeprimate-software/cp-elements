/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.cp.elements.service;

/**
 * The ServiceInvocationException class is a RuntimeException indicating an error in a Service call.
 *
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
   *
   * @param message a String value describing the nature of the service call error.
   */
  public ServiceInvocationException(final String message) {
    super(message);
  }

  /**
   * Constructor to create an instance of the ServiceInvocationException with the given Throwable indicating the cause
   * of the service call error.
   *
   * @param cause a Throwable indicated as the cause of this service call error.
   */
  public ServiceInvocationException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructor to create an instance of the ServiceInvocationException with both a message to describe the
   * service call error along with a Throwable indicating the probably cause of the service call error.
   *
   * @param message a String value describing the nature of the service call error.
   * @param cause a Throwable indicated as the cause of this service call error.
   */
  public ServiceInvocationException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
