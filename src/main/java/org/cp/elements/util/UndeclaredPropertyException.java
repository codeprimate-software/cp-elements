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
package org.cp.elements.util;

/**
 * Java {@link RuntimeException} used to indicate that a Java property was not declared
 * in the Java application (program) configuration.
 * <p>
 * This {@link RuntimeException} is meant to imply the same meaning as a {@literal PropertyNotFoundException}
 * or {@literal NoSuchPropertyException}. Except, the {@link org.cp.elements.beans.PropertyNotFoundException}
 * class has been reserved for Elements JavaBeans support. This {@link RuntimeException} is reserved for
 * Java {@link java.util.Properties} and {@link System#getProperties() System properties}.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @see org.cp.elements.beans.PropertyNotFoundException
 * @see org.cp.elements.util.UndefinedPropertyException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class UndeclaredPropertyException extends RuntimeException {

  /**
   * Constructs a new instance of {@link UndeclaredPropertyException}
   * with no {@link String message} and no {@link Throwable cause}.
   */
  public UndeclaredPropertyException() { }

  /**
   * Constructs a new instance of {@link UndeclaredPropertyException} initialized with the given {@link String message}
   * used to description this {@link RuntimeException}.
   *
   * @param message {@link String} containing a {@literal message} to description this {@link RuntimeException}.
   */
  public UndeclaredPropertyException(String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link UndeclaredPropertyException} initialized with the given {@link Throwable}
   * used as the cause or reason this {@link RuntimeException} was thrown.
   *
   * @param cause {@link Throwable} used as the reason this {@link RuntimeException} was thrown.
   */
  public UndeclaredPropertyException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link UndeclaredPropertyException} initialized with the given {@link String message}
   * used to description this {@link RuntimeException} along with the given {@link Throwable} used as the cause
   * or reason this {@link RuntimeException} was thrown.
   *
   * @param message {@link String} containing a {@literal message} to description this {@link RuntimeException}.
   * @param cause {@link Throwable} used as the reason this {@link RuntimeException} was thrown.
   */
  public UndeclaredPropertyException(String message, Throwable cause) {
    super(message, cause);
  }
}
