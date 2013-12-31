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

package org.cp.elements.lang.support;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.LogicalOperator;

/**
 * The ComposableFilter class is a Filter implementation for composing, or combining Filters into compound Filters
 * using logical operators (AND and OR).
 * <p/>
 * @author John J. Blum
 * @param <T> the Class type of Objects evaluated by this Filter.
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.DefaultFilter
 * @see org.cp.elements.lang.support.InverseFilter
 * @since 1.0.0
 */
public class ComposableFilter<T> implements Filter<T> {

  private final Filter<T> leftFilter;
  private final Filter<T> rightFilter;

  private final LogicalOperator op;

  /**
   * Constructs an instance of the ComposableFilter class with a given Filter for the left operand, the LogicalOperator
   * used to combine the left and right Filters into a compound Filter, and the given Filter for the right operand.
   * <p/>
   * @param leftFilter the Filter in the left operand of the binary logical operator.
   * @param op the LogicalOperator used to combine the two Filters constituting the left and right operands in the
   * binary logical operation into a compound Filter.
   * @param rightFilter the Filter in the right operand of the binary logical operator.
   */
  private ComposableFilter(final Filter<T> leftFilter, final LogicalOperator op, final Filter<T> rightFilter) {
    this.leftFilter = leftFilter;
    this.op = op;
    this.rightFilter = rightFilter;
  }

  /**
   * Composes two Filter objects into a compound Filter, where the Filters constitute both the left and right operands
   * in the binary logical AND operator.
   * <p/>
   * @param leftFilter the Filter in the left operand of the binary logical operator.
   * @param rightFilter the Filter in the right operand of the binary logical operator.
   * @param <T> the Class type of the objects being filtered by this Filter.
   * @return a Filter implementation combining both the left and right Filter operands into a compound Filter using
   * the binary logical AND operator.
   * @see #compose(org.cp.elements.lang.Filter, org.cp.elements.lang.LogicalOperator, org.cp.elements.lang.Filter)
   */
  public static <T> Filter<T> and(final Filter<T> leftFilter, final Filter<T> rightFilter) {
    return compose(leftFilter, LogicalOperator.AND, rightFilter);
  }

  /**
   * Composes two Filter objects into a compound Filter, where the Filters constitute both the left and right operands
   * in the specified binary logical operator.
   * <p/>
   * @param leftFilter the Filter in the left operand of the binary logical operator.
   * @param op the LogicalOperator used to combine the two Filters constituting the left and right operands in the
   * binary logical operation into a compound Filter.
   * @param rightFilter the Filter in the right operand of the binary logical operator.
   * @param <T> the Class type of the objects being filtered by this Filter.
   * @return a Filter implementation combining both the left and right Filter operands into a compound Filter using
   * the binary logical operator.
   */
  private static <T> Filter<T> compose(final Filter<T> leftFilter, final LogicalOperator op, final Filter<T> rightFilter) {
    return (leftFilter == null ? rightFilter : (rightFilter == null ? leftFilter
      : new ComposableFilter<T>(leftFilter, op, rightFilter)));
  }

  /**
   * Composes two Filter objects into a compound Filter, where the Filters constitute both the left and right operands
   * in the binary logical OR operator.
   * <p/>
   * @param leftFilter the Filter in the left operand of the binary logical operator.
   * @param rightFilter the Filter in the right operand of the binary logical operator.
   * @param <T> the Class type of the objects being filtered by this Filter.
   * @return a Filter implementation combining both the left and right Filter operands into a compound Filter using
   * the binary logical OR operator.
   * @see #compose(org.cp.elements.lang.Filter, org.cp.elements.lang.LogicalOperator, org.cp.elements.lang.Filter)
   */
  public static <T> Filter<T> or(final Filter<T> leftFilter, final Filter<T> rightFilter) {
    return compose(leftFilter, LogicalOperator.OR, rightFilter);
  }

  /**
   * Gets the Filter in the left operand of the binary logical operator.
   * <p/>
   * @return the Filter in the left operand of the binary logical operator.
   */
  protected Filter<T> getLeftFilter() {
    return leftFilter;
  }

  /**
   * The LogicalOperator used to combine the left and right Filter operands into a compound Filter.
   * <p/>
   * @return a LogicalOperator (such as AND or OR) to combine both the left and right Filter operands into a compound
   * Filter.
   */
  protected LogicalOperator getOp() {
    return op;
  }

  /**
   * Gets the Filter in the right operand of the binary logical operator.
   * <p/>
   * @return the Filter in the right operand of the binary logical operator.
   */
  protected Filter<T> getRightFilter() {
    return rightFilter;
  }

  /**
   * Determines whether the specified object meets the criteria (rules) defined by this Filter.
   * <p/>
   * @param obj the Object being evaluated by this Filter.
   * @return a boolean value indicating whether the specified Object satisfies the criteria (rules) of this Filter.
   */
  public boolean accept(final T obj) {
    return getOp().evaluate(getLeftFilter().accept(obj), getRightFilter().accept(obj));
  }

  /**
   * Gets a String representation of the ComposableFilter.
   * <p/>
   * @return a String value representing the ComposableFilter.
   */
  @Override
  public String toString() {
    return getClass().getName();
  }

}
