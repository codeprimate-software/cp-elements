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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Constants;

/**
 * Abstract base class for encapsulating functionality common to all {@link Rule} implementations.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of {@link Object objects} evaluated by this business rule.
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
   * Gets the identifier for this Rule.
   *
   * @return the identifier for this Rule.
   * @see org.cp.elements.lang.Identifiable#getId()
   * @throws IllegalStateException if the identifier for this Rule was not properly set.
   */
  public ID getId() {
    Assert.state(this.id != null, "The identifier for Rule ({0}) was not properly initialized!", getClass().getName());
    return this.id;
  }

  /**
   * Sets the identifier for this Rule.
   *
   * @param id a value of type T assigned as this object's unique identifier.
   * @see org.cp.elements.lang.Identifiable#setId(Comparable)
   * @throws NullPointerException if the identifier for this Rule is null.
   */
  public final void setId(final ID id) {
    Assert.notNull(id, "The identifier for Rule ({0}) cannot be null!", getClass().getName());
    this.id = id;
  }

  /**
   * Unsupported operation for this Rule.
   *
   * @return boolean indicating whether this Rule is a newly created object.
   * @throws UnsupportedOperationException operation not support by the Rule class.
   */
  public boolean isNew() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Determines the outcome expected when evaluating an object with this business rule.  The object is expected to
   * satisfy or violate the criteria of this business rule when evaluated.
   *
   * @return the outcome expected when evaluating an object with this business rule.
   * @see #evaluate(Object)
   */
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
  protected final void setExpectedOutcome(final boolean expectedOutcome) {
    this.expectedOutcome = expectedOutcome;
  }

  /**
   * Indicates whether this business rule is configured to throw an exception on failure when evaluated.  If the object
   * evaluated by this business rule violates the criteria, then an exception is thrown.
   *
   * @return a boolean value indicating whether this business rule is configured to throw an exception on failure
   * when evaluated.
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
  protected final void setThrowExceptionOnFailure(final boolean throwExceptionOnFailure) {
    this.throwExceptionOnFailure = throwExceptionOnFailure;
  }

}
