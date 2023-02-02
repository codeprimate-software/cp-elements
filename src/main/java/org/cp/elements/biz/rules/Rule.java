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

import org.cp.elements.lang.Identifiable;

/**
 * Abstract Data Type (ADT) for modeling a {@literal business rule}.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of {@link Object objects} evaluated by this {@literal business rule}.
 * @param <ID> {@link Comparable} {@link Class type} of the identifier uniquely identifying this {@link Rule}.
 * @see org.cp.elements.lang.Identifiable
 * @see java.lang.Comparable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Rule<T, ID extends Comparable<ID>> extends Identifiable<ID> {

  /**
   * Evaluates the given {@link Object} against the criteria, or requirements defined by this {@literal business rule}.
   *
   * @param obj {@link Object} evaluated by this business rule.
   * @return a boolean value indicating whether the {@link Object} evaluated satisfied the criteria of
   * this {@literal business rule}; may throw an {@link Exception} if {@link #isThrowExceptionOnFailure()}
   * is {@literal true}.
   * @see #isThrowExceptionOnFailure()
   * @see #getExpectedOutcome()
   */
  boolean evaluate(T obj);

  /**
   * Determines the {@link Boolean outcome expected} when evaluating an {@link Object}
   * with this {@literal business rule}.
   *
   * The {@link Object} is expected to either satisfy or violate the criteria of this {@literal business rule}
   * when evaluated.
   *
   * Returns {@literal true} by default.
   *
   * @return the {@link Boolean expected outcome} when evaluating an {@link Object} with this {@literal business rule}.
   * @see #evaluate(Object)
   */
  default boolean getExpectedOutcome() {
    return true;
  }

  /**
   * Indicates whether this {@literal business rule} is configured to throw an {@link Exception} on failure
   * when the {@link Object} is evaluated.
   *
   * If the {@link Object} evaluated by this {@literal business rule} violates the criteria,
   * then an {@link Exception} is thrown.
   *
   * Returns {@literal false} by default.
   *
   * @return a boolean value indicating whether this {@literal business} rule is configured to throw
   * an {@link Exception} on failure when the {@link Object} is evaluated.
   * @see #evaluate(Object)
   */
  default boolean isThrowExceptionOnFailure() {
    return false;
  }
}
