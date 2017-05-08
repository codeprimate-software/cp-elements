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

package org.cp.elements.biz.rules;

/**
 * The {@link RuleException} class is a {@link RuntimeException} indicating that an error occurred
 * evaluating a business rule.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class RuleException extends RuntimeException {

  /**
   * Constructs an uninitialized instance of the {@link RuleException} class with not {@link Throwable cause}
   * or {@link String message}.
   */
  public RuleException() {
  }

  /**
   * Constructs an instance of {@link RuleException} initialized with the given {@link String message}
   * to describe the business rule error.
   *
   * @param message {@link String} describing the business rule error.
   */
  public RuleException(String message) {
    super(message);
  }

  /**
   * Constructs an instance of {@link RuleException} initialized with the underlying {@link Throwable cause}
   * of the business rule error.
   *
   * @param cause {@link Throwable} indicating the underlying cause of the business rule error.
   * @see java.lang.Throwable
   */
  public RuleException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of {@link RuleException} initialized with the given {@link String message} to describe
   * the business rule error along with the underlying {@link Throwable cause} of the business rule error.
   *
   * @param message {@link String} describing the business rule error.
   * @param cause {@link Throwable} indicating the underlying cause of the business rule error.
   * @see java.lang.Throwable
   */
  public RuleException(String message, Throwable cause) {
    super(message, cause);
  }
}
