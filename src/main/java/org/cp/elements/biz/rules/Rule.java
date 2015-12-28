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

import org.cp.elements.lang.Identifiable;

/**
 * The Rule interface is an abstraction for modeling a business rule.
 *
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
   *
   * @return the outcome expected when evaluating an object with this business rule.
   * @see #evaluate(Object)
   */
  boolean getExpectedOutcome();

  /**
   * Indicates if this business rule is configured to throw an exception on failure when evaluated.  If the object
   * evaluated by this business rule violates the criteria, then an exception is thrown.
   *
   * @return a boolean value indicating whether this business rule is configured to throw an exception on failure
   * when evaluated.
   * @see #evaluate(Object)
   */
  boolean isThrowExceptionOnFailure();

  /**
   * Evaluates the specified object against the criteria, or requirements of this business rule.
   *
   * @param obj the Object evaluated by this business rule.
   * @return a boolean value indicating whether the object evaluated satisfied the criteria of this business rule.
   * @see #isThrowExceptionOnFailure()
   * @see #getExpectedOutcome()
   */
  boolean evaluate(T obj);

}
