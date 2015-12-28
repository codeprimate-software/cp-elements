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

package org.cp.elements.lang.support;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.LogicalOperator;

/**
 * The ComposableFilter class is a Filter implementation for composing, or combining Filters into compound Filters
 * using logical operators (AND and OR).
 *
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
   *
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
   *
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
   *
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
      : new ComposableFilter<>(leftFilter, op, rightFilter)));
  }

  /**
   * Composes two Filter objects into a compound Filter, where the Filters constitute both the left and right operands
   * in the binary logical OR operator.
   *
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
   *
   * @return the Filter in the left operand of the binary logical operator.
   */
  protected Filter<T> getLeftFilter() {
    return leftFilter;
  }

  /**
   * The LogicalOperator used to combine the left and right Filter operands into a compound Filter.
   *
   * @return a LogicalOperator (such as AND or OR) to combine both the left and right Filter operands into a compound
   * Filter.
   */
  protected LogicalOperator getOp() {
    return op;
  }

  /**
   * Gets the Filter in the right operand of the binary logical operator.
   *
   * @return the Filter in the right operand of the binary logical operator.
   */
  protected Filter<T> getRightFilter() {
    return rightFilter;
  }

  /**
   * Determines whether the specified object meets the criteria (rules) defined by this Filter.
   *
   * @param obj the Object being evaluated by this Filter.
   * @return a boolean value indicating whether the specified Object satisfies the criteria (rules) of this Filter.
   */
  public boolean accept(final T obj) {
    return getOp().evaluate(getLeftFilter().accept(obj), getRightFilter().accept(obj));
  }

  /**
   * Gets a String representation of the ComposableFilter.
   *
   * @return a String value representing the ComposableFilter.
   */
  @Override
  public String toString() {
    return getClass().getName();
  }

}
