/*
 * Copyright 2017-Present Author or Authors.
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
 * {@link QueryException} thrown when the {@link Query} is malformed.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.QueryException
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class MalformedQueryException extends QueryException {

  /**
   * Factory method used to construct a new {@link MalformedQueryException} with the given {@link String message}.
   *
   * @param message {@link String} containing a description of this exception.
   * @return a new {@link MalformedQueryException} with the given {@link String message}.
   */
  public static MalformedQueryException withMessage(String message) {
    return new MalformedQueryException(message);
  }

  /**
   * Constructs a new {@link MalformedQueryException} with no {@link String message} and no {@link Throwable cause}.
   */
  public MalformedQueryException() { }

  /**
   * Constructs a new {@link javax.naming.MalformedLinkException} initialized with a {@link String message}
   * describing this exception.
   *
   * @param message {@link String} containing a description of this exception.
   * @see java.lang.RuntimeException#RuntimeException(String)
   */
  public MalformedQueryException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link MalformedQueryException} initialized with a {@link Throwable}
   * object indicating the cause of this exception.
   *
   * @param cause {@link Throwable} indicating the cause of this exception.
   * @see java.lang.RuntimeException#RuntimeException(Throwable)
   */
  public MalformedQueryException(Throwable cause) {
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
  public MalformedQueryException(String message, Throwable cause) {
    super(message, cause);
  }
}
