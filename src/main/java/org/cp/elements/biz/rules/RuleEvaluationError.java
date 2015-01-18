/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * 
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * 
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * 
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * 
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
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
