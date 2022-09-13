/*
 * Copyright 2011-Present Author or Authors.
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
 * Java {@link RuntimeException} used to indicate a configuration problem in the Java application (program).
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConfigurationException extends RuntimeException {

  /**
   * Constructs a new instance of {@link ConfigurationException}
   * with no {@link String message} and no {@link Throwable cause}.
   */
  public ConfigurationException() { }

  /**
   * Constructs a new instance of {@link ConfigurationException} initialized with the given {@link String message}
   * used to description this {@link RuntimeException}.
   *
   * @param message {@link String} containing a {@literal message} to description this {@link RuntimeException}.
   */
  public ConfigurationException(final String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link ConfigurationException} initialized with the given {@link Throwable}
   * used as the cause or reason this {@link RuntimeException} was thrown.
   *
   * @param cause {@link Throwable} used as the reason this {@link RuntimeException} was thrown.
   */
  public ConfigurationException(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link ConfigurationException} initialized with the given {@link String message}
   * used to description this {@link RuntimeException} along with the given {@link Throwable} used as the cause
   * or reason this {@link RuntimeException} was thrown.
   *
   * @param message {@link String} containing a {@literal message} to description this {@link RuntimeException}.
   * @param cause {@link Throwable} used as the reason this {@link RuntimeException} was thrown.
   */
  public ConfigurationException(final String message, final Throwable cause) {
    super(message, cause);
  }
}
