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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;

/**
 * The AbstractRule class is an abstract base class for encapsulating functionality common to all Rule implementations.
 * <p/>
 * @author John J. Blum
 * @param <T> the Class type of Objects evaluated by this business rule.
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

  public ID getId() {
    Assert.state(id != null, "The identifier for Rule ({0}) was not properly initialized!");
    return id;
  }

  public final void setId(final ID id) {
    Assert.notNull(id, "The identifier for Rule ({0}) cannot be null!", getClass().getName());
    this.id = id;
  }

  public boolean isNew() {
    throw new UnsupportedOperationException(StringUtils.NOT_IMPLEMENTED);
  }

  /**
   * Indicates if this business rule is configured to throw an exception on failure when evaluated.  If the object
   * evaluated by this business rule violates the criteria, then an exception is thrown.
   * <p/>
   * @return a boolean value indicating whether this business rule is configured to throw an exception on failure
   * when evaluated.
   * @see #evaluate(Object)
   */
  public boolean isThrowExceptionOnFailure() {
    return throwExceptionOnFailure;
  }

  /**
   * Configures this business rule to throw, or not throw an Exception on failure when an Object is evaluated.
   * <p/>
   * @param throwExceptionOnFailure a boolean value indicating whether this business rule should throw an Exception
   * on failure when evaluated.
   * @see #evaluate(Object)
   */
  protected final void setThrowExceptionOnFailure(final boolean throwExceptionOnFailure) {
    this.throwExceptionOnFailure = throwExceptionOnFailure;
  }

  /**
   * Determines the outcome expected when evaluating an object with this business rule.  The object is expected to
   * satisfy or violate the criteria of this business rule when evaluated.
   * <p/>
   * @return the outcome expected when evaluating an object with this business rule.
   * @see #evaluate(Object)
   */
  public boolean getExpectedOutcome() {
    return expectedOutcome;
  }

  /**
   * Sets the expected outcome (pass or fail) of this business rule when evaluating an Object.
   * <p/>
   * @param expectedOutcome a boolean value indicating the expected outcome when evaluating an Object with this
   * business rule.
   * @see #evaluate(Object)
   */
  protected final void setExpectedOutcome(final boolean expectedOutcome) {
    this.expectedOutcome = expectedOutcome;
  }

}
