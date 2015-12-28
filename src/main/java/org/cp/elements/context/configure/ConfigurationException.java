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

package org.cp.elements.context.configure;

/**
 * The ConfigurationException class is a RuntimeException denoting a configuration problem.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConfigurationException extends RuntimeException {

  /**
   * Default constructor to create an uninitialized instance of the ConfigurationException class.
   */
  public ConfigurationException() {
  }

  /**
   * Constructs an instance of the the ConfigurationException class with a message describing the configuration problem.
   *
   * @param message a String describing the configuration problem.
   */
  public ConfigurationException(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the ConfigurationException class along with the cause of the configuration problem.
   *
   * @param cause a Throwable indicating the underlying cause of the configuration problem.
   */
  public ConfigurationException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the ConfigurationException class with a message describing the configuration problem
   * along with the underlying cause of the configuration problem.
   *
   * @param message a String describing the configuration problem.
   * @param cause a Throwable indicating the underlying cause of the configuration problem.
   */
  public ConfigurationException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
