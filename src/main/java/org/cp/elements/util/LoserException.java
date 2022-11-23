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
 * Java {@link RuntimeException} indicating some component was the {@literal loser} in an election.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class LoserException extends RuntimeException {

  /**
   * Constructs a new instance of an uninitialized {@link LoserException}, having no {@link String message}
   * or {@link Throwable cause}.
   */
  public LoserException() {
  }

  /**
   * Constructs a new instance of {@link LoserException} initialized with the given {@link String message}
   * to {@literal describe} this {@link RuntimeException}.
   *
   * @param message {@link String} containing a {@literal description} for this {@link RuntimeException}.
   */
  public LoserException(String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link LoserException} initialized with the given {@link Throwable}
   * used as the {@literal cause} of this {@link RuntimeException}.
   *
   * @param cause {@link Throwable} used as the cause of this {@link RuntimeException}.
   */
  public LoserException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link LoserException} initialized with the given {@link String message}
   * to {@literal describe} this {@link RuntimeException} and the given {@link Throwable} used as
   * the {@literal cause} of this {@link RuntimeException}.
   *
   * @param message {@link String} containing a {@literal description} for this {@link RuntimeException}.
   * @param cause {@link Throwable} used as the cause of this {@link RuntimeException}.
   */
  public LoserException(String message, Throwable cause) {
    super(message, cause);
  }
}
