/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.biz.rules;

import org.cp.elements.lang.Identifiable;

/**
 * The Rule interface is an abstraction for modeling a business rule.
 * <p/>
 * @author John J. Blum
 * @param <T> the Class type of objects evaluated by this business rule.
 * @see org.cp.elements.lang.Identifiable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Rule<T, ID extends Comparable<ID>> extends Identifiable<ID> {

  /**
   * Determines the outcome expected when evaluating an object with this business rule.  The object is expected to
   * satisfy or violate the criteria of this business rule when evaluated.
   * <p/>
   * @return the outcome expected when evaluating an object with this business rule.
   * @see #evaluate(Object)
   */
  boolean getExpectedOutcome();

  /**
   * Indicates if this business rule is configured to throw an exception on failure when evaluated.  If the object
   * evaluated by this business rule violates the criteria, then an exception is thrown.
   * <p/>
   * @return a boolean value indicating whether this business rule is configured to throw an exception on failure
   * when evaluated.
   * @see #evaluate(Object)
   */
  boolean isThrowExceptionOnFailure();

  /**
   * Evaluates the specified object against the criteria, or requirements of this business rule.
   * <p/>
   * @param obj the Object evaluated by this business rule.
   * @return a boolean value indicating whether the object evaluated satisfied the criteria of this business rule.
   * @see #isThrowExceptionOnFailure()
   * @see #getExpectedOutcome()
   */
  boolean evaluate(T obj);

}
