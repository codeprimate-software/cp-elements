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

package org.cp.elements.lang;

/**
 * The Condition interface defines a contract for implementing objects used to evaluate a required condition
 * of the application or system.
 *
 * @param <T> the Class type of the arguments used in the evaluation of the condition.
 * @author John Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Condition<T> {

  /**
   * Evaluates the required criteria of this condition to determine whether the condition holds.
   *
   * @param argument the arguments using in the evaluation of this condition.
   * @return a boolean value indicating whether the condition holds.
   */
  boolean evaluate(T argument);

}
