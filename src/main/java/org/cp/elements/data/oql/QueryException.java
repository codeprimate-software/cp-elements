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
package org.cp.elements.data.oql;

/**
 * Java {@link RuntimeException} classifying all {@link Query} exceptions.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class QueryException extends RuntimeException {

  /**
   * Constructs a new {@link MalformedQueryException} with no {@link String message} and no {@link Throwable cause}.
   */
  public QueryException() { }

  /**
   * Constructs a new {@link javax.naming.MalformedLinkException} initialized with a {@link String message}
   * describing this exception.
   *
   * @param message {@link String} containing a description of this exception.
   * @see java.lang.RuntimeException#RuntimeException(String)
   */
  public QueryException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link MalformedQueryException} initialized with a {@link Throwable}
   * object indicating the cause of this exception.
   *
   * @param cause {@link Throwable} indicating the cause of this exception.
   * @see java.lang.RuntimeException#RuntimeException(Throwable)
   */
  public QueryException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link MalformedQueryException} initialized with a {@link String}
   * describing this exception along with a {@link Throwable} object indicating the cause
   * of this exception.
   *
   * @param message {@link String} containing a description of this exception.
   * @param cause {@link Throwable} indicating the cause of this exception.
   * @see java.lang.RuntimeException#RuntimeException(String, Throwable)
   */
  public QueryException(String message, Throwable cause) {
    super(message, cause);
  }
}
