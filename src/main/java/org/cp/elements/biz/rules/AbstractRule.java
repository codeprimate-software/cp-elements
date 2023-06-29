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

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract base class for encapsulating functionality common to all {@link Rule} implementations.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of {@link Object objects} evaluated by this {@literal business rule}.
 * @param <ID> {@link Comparable} {@link Class type} of the identifier uniquely identifying the {@link Rule}.
 * @see org.cp.elements.biz.rules.Rule
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractRule<T, ID extends Comparable<ID>> implements Rule<T, ID> {

  protected static final boolean DEFAULT_EXPECTED_OUTCOME = true;
  protected static final boolean DEFAULT_THROW_EXCEPTION_ON_FAILURE = false;

  private volatile boolean expectedOutcome = DEFAULT_EXPECTED_OUTCOME;
  private volatile boolean throwExceptionOnFailure = DEFAULT_THROW_EXCEPTION_ON_FAILURE;

  private volatile ID id;

  /**
   * Gets the {@link ID identifier} uniquely identifying this {@link Rule}.
   *
   * @return the {@link ID identifier} for this {@link Rule}.
   * @throws IllegalStateException if the {@link ID identifier} for this {@link Rule} was not properly set.
   * @see org.cp.elements.lang.Identifiable#getId()
   */
  @Override
  public @NotNull ID getId() {
    return ObjectUtils.requireState(this.id,
      "Identifier for Rule [%s] was not properly initialized", getClass().getName());
  }

  /**
   * Sets the {@link ID identifier} uniquely identifying this {@link Rule}.
   *
   * @param id {@link ID identifier} assigned as this {@link Rule Rule's} unique identifier.
   * @throws IllegalArgumentException if the {@link ID identifier} is {@literal null}.
   * @see org.cp.elements.lang.Identifiable#setId(Comparable)
   */
  @Override
  public final void setId(@NotNull ID id) {
    this.id = ObjectUtils.requireObject(id, "Identifier for Rule [%s] is required", getClass().getName());
  }

  /**
   * An unsupported operation for this {@link Rule}.
   *
   * @return boolean indicating whether this Rule is a newly created object.
   * @throws UnsupportedOperationException by default.
   */
  @Override
  public boolean isNew() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Determines the {@link Boolean outcome expected} when evaluating an {@link Object}
   * with this {@literal business rule}.
   * <p>
   * The {@link Object} is expected to either satisfy or violate the criteria of this {@literal business rule}
   * when evaluated.
   * <p>
   * Returns {@literal true} by default.
   *
   * @return the {@link Boolean expected outcome} when evaluating an {@link Object} with this {@literal business rule}.
   * @see #evaluate(Object)
   */
  @Override
  public boolean getExpectedOutcome() {
    return this.expectedOutcome;
  }

  /**
   * Sets the expected outcome (pass or fail) of this business rule when evaluating an Object.
   *
   * @param expectedOutcome a boolean value indicating the expected outcome when evaluating an Object with this
   * business rule.
   * @see #evaluate(Object)
   */
  protected final void setExpectedOutcome(boolean expectedOutcome) {
    this.expectedOutcome = expectedOutcome;
  }

  /**
   * Indicates whether this {@literal business rule} is configured to throw an {@link Exception} on failure
   * when the {@link Object} is evaluated.
   * <p>
   * If the {@link Object} evaluated by this {@literal business rule} violates the criteria,
   * then an {@link Exception} is thrown.
   * <p>
   * Returns {@literal false} by default.
   *
   * @return a boolean value indicating whether this {@literal business} rule is configured to throw
   * an {@link Exception} on failure when the {@link Object} is evaluated.
   * @see #evaluate(Object)
   */
  public boolean isThrowExceptionOnFailure() {
    return this.throwExceptionOnFailure;
  }

  /**
   * Configures this business rule to throw or not throw an Exception on failure when an Object is evaluated.
   *
   * @param throwExceptionOnFailure a boolean value indicating whether this business rule should throw an Exception
   * on failure when evaluated.
   * @see #evaluate(Object)
   */
  protected final void setThrowExceptionOnFailure(boolean throwExceptionOnFailure) {
    this.throwExceptionOnFailure = throwExceptionOnFailure;
  }
}
