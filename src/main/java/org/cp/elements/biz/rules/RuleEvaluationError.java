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

package org.cp.elements.biz.rules;

/**
 * The RuleEvaluationError class is an Error indicating a problem during normal business rule evaluation, execution
 * and processing.
 *
 * @author John J. Blum
 * @see java.lang.Error
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class RuleEvaluationError extends Error {

  /**
   * Default constructor to create an uninitialized instance of the RuleEvaluationError class.
   */
  public RuleEvaluationError() {
  }

  /**
   * Constructs an instance of the RuleEvaluationError class with a message to describe the error that occurred
   * when evaluating an application business rule.
   *
   * @param message a String describing the error when evaluating the application business rule.
   */
  public RuleEvaluationError(final String message) {
    super(message);
  }

  /**
   * Constructs an instance of the RuleEvaluationError class along with the underlying cause of the error when
   * evaluating an application business rule.
   *
   * @param cause a Throwable indicating the underlying cause of the error when evaluating
   * the application business rule.
   * @see java.lang.Throwable
   */
  public RuleEvaluationError(final Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the RuleEvaluationError class with a message to describe the error that occurred
   * when evaluating an application business rule along with the underlying cause of the error when evaluating
   * the application business rule.
   *
   * @param message a String describing the error when evaluating the application business rule.
   * @param cause a Throwable indicating the underlying cause of the error when evaluating
   * the application business rule.
   * @see java.lang.Throwable
   */
  public RuleEvaluationError(final String message, final Throwable cause) {
    super(message, cause);
  }

}
