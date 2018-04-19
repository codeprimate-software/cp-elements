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

import org.cp.elements.lang.Composite;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.LogicalOperator;

/**
 * The {@link ComposableFilter} class is a {@link Filter} implementation for composing and combining individual
 * {@link Filter} objects into compound a {@link Filter} using logical operators (AND, OR and XOR).
 *
 * @author John J. Blum
 * @param <T> {@link Class} type of objects evaluated by this {@link Filter}.
 * @see org.cp.elements.lang.Composite
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.LogicalOperator
 * @see org.cp.elements.lang.support.DefaultFilter
 * @see org.cp.elements.lang.support.InverseFilter
 * @since 1.0.0
 */
public class ComposableFilter<T> implements Composite<Filter<T>>, Filter<T> {

  protected static final ComposableFilter<?> INSTANCE = new ComposableFilter<>();

  /**
   * Factory methods returning a single instance of {@link ComposableFilter} used to compose 2 or more
   * individual {@link Filter} objects into a {@link Composite} {@link Filter} object.
   *
   * @param <T> {@link Class} type of the objects evaluated by this {@link Filter}.
   * @return a single instance of {@link ComposableFilter}.
   * @see org.cp.elements.lang.support.ComposableFilter
   */
  @SuppressWarnings("unchecked")
  public static <T> ComposableFilter<T> builder() {
    return (ComposableFilter<T>) INSTANCE;
  }

  /**
   * Composes two {@link Filter} objects into a compound {@link Filter}, where the {@link Filter Filters} constitute
   * both the left and right operands in the specified binary logical expression.
   *
   * @param <T> {@link Class type} of the object being filtered by this {@link Filter}.
   * @param filterOne {@link Filter} in the left operand of the binary logical expression.
   * @param op {@link LogicalOperator} used to combine the two {@link Filter} objects, which constitute
   * the left and right operands in the binary logical expression.
   * @param filterTwo {@link Filter} in the right operand of the binary logical expression.
   * @return a {@link Filter} implementation combining both the {@link Filter} operands
   * into a compound {@link Filter} using the given binary {@link LogicalOperator}.
   * @see org.cp.elements.lang.LogicalOperator
   * @see org.cp.elements.lang.Filter
   */
  protected static <T> Filter<T> compose(Filter<T> filterOne, LogicalOperator op, Filter<T> filterTwo) {

    return filterOne == null ? filterTwo : (filterTwo == null ? filterOne
      : new ComposableFilter<>(filterOne, op, filterTwo));
  }

  /**
   * Composes two {@link Filter} objects into a compound {@link Filter}, where the {@link Filter Filters} constitute
   * both the left and right operands in the binary logical AND expression.
   *
   * @param <T> {@link Class type} of the object being filtered by this {@link Filter}.
   * @param filterOne {@link Filter} in the left operand of the binary logical expression.
   * @param filterTwo {@link Filter} in the right operand of the binary logical expression.
   * @return a {@link Filter} implementation combining both the {@link Filter} operands
   * into a compound {@link Filter} using the binary logical AND operator.
   * @see #compose(org.cp.elements.lang.Filter, org.cp.elements.lang.LogicalOperator, org.cp.elements.lang.Filter)
   * @see org.cp.elements.lang.LogicalOperator#AND
   * @see org.cp.elements.lang.Filter
   */
  public static <T> Filter<T> and(Filter<T> filterOne, Filter<T> filterTwo) {
    return compose(filterOne, LogicalOperator.AND, filterTwo);
  }

  /**
   * Composes two {@link Filter} objects into a compound {@link Filter}, where the {@link Filter Filters} constitute
   * both the left and right operands in the binary logical OR expression.
   *
   * @param <T> {@link Class type} of the object being filtered by this {@link Filter}.
   * @param filterOne {@link Filter} in the left operand of the binary logical expression.
   * @param filterTwo {@link Filter} in the right operand of the binary logical expression.
   * @return a {@link Filter} implementation combining both {@link Filter} operands
   * into a compound {@link Filter} using the binary logical OR operator.
   * @see #compose(org.cp.elements.lang.Filter, org.cp.elements.lang.LogicalOperator, org.cp.elements.lang.Filter)
   * @see org.cp.elements.lang.LogicalOperator#OR
   * @see org.cp.elements.lang.Filter
   */
  public static <T> Filter<T> or(Filter<T> filterOne, Filter<T> filterTwo) {
    return compose(filterOne, LogicalOperator.OR, filterTwo);
  }

  /**
   * Composes two {@link Filter} objects into a compound {@link Filter}, where the {@link Filter Filters} constitute
   * both the left and right operands in the binary logical XOR expression.
   *
   * @param <T> {@link Class type} of the object being filtered by this {@link Filter}.
   * @param filterOne {@link Filter} in the left operand of the binary logical expression.
   * @param filterTwo {@link Filter} in the right operand of the binary logical expression.
   * @return a {@link Filter} implementation combining both {@link Filter} operands
   * into a compound {@link Filter} using the binary logical XOR operator.
   * @see #compose(org.cp.elements.lang.Filter, org.cp.elements.lang.LogicalOperator, org.cp.elements.lang.Filter)
   * @see org.cp.elements.lang.LogicalOperator#XOR
   * @see org.cp.elements.lang.Filter
   */
  public static <T> Filter<T> xor(Filter<T> filterOne, Filter<T> filterTwo) {
    return compose(filterOne, LogicalOperator.XOR, filterTwo);
  }

  private final Filter<T> filterOne;
  private final Filter<T> filterTwo;

  private final LogicalOperator op;

  /**
   * Default, private constructor used to construct a new, single instance of {@link ComposableFilter}
   * used to compose 2 or more individual {@link Filter} objects into a {@link Composite} {@link Filter}
   * object.
   *
   * @see org.cp.elements.lang.Filter#rejecting()
   * @see org.cp.elements.lang.LogicalOperator#AND
   */
  private ComposableFilter() {

    this.filterOne = Filter.rejecting();
    this.op = LogicalOperator.AND;
    this.filterTwo = Filter.rejecting();
  }

  /**
   * Constructs a new instance of {@link ComposableFilter} with a given {@link Filter} for the left operand,
   * the {@link LogicalOperator} combining the two {@link Filter Filters} into a compound {@link Filter},
   * and the given {@link Filter} for the right operand.
   *
   * @param filterOne {@link Filter} in the left operand of the binary logical operator.
   * @param op {@link LogicalOperator} used to combine the two {@link Filter Filters}
   * constituting the left and right operands in the binary logical operation into
   * a compound {@link Filter}.
   * @param filterTwo {@link Filter} in the right operand of the binary logical operator.
   * @see org.cp.elements.lang.LogicalOperator
   * @see org.cp.elements.lang.Filter
   */
  private ComposableFilter(Filter<T> filterOne, LogicalOperator op, Filter<T> filterTwo) {

    this.filterOne = filterOne;
    this.op = op;
    this.filterTwo = filterTwo;
  }

  /**
   * Compose the given {@link Filter} objects into a {@link Composite} {@link Filter} object.
   *
   * @return a {@link Composite} {@link Filter} object composed of the given {@link Filter} objects
   * as an instance of {@link Filter}.
   * @see #compose(Filter, LogicalOperator, Filter)
   * @see org.cp.elements.lang.LogicalOperator#AND
   * @see org.cp.elements.lang.Filter
   */
  @Override
  public Filter<T> compose(Filter<T> one, Filter<T> two) {
    return compose(one, LogicalOperator.AND, two);
  }

  /**
   * Gets the {@link Filter} in the left operand of the binary logical expression.
   *
   * @return the {@link Filter} in the left operand of the binary logical expression.
   * @see org.cp.elements.lang.Filter
   */
  protected Filter<T> getFilterOne() {
    return this.filterOne;
  }

  /**
   * Returns the {@link LogicalOperator} used to combine the left and right {@link Filter} operands
   * into a compound {@link Filter} expression.
   *
   * @return the {@link LogicalOperator} (such as AND or OR) combining both the left and right {@link Filter} operands
   * into a compound {@link Filter} expression.
   * @see org.cp.elements.lang.LogicalOperator
   */
  protected LogicalOperator getOp() {
    return this.op;
  }

  /**
   * Gets the {@link Filter} in the right operand of the binary logical expression.
   *
   * @return the {@link Filter} in the right operand of the binary logical expression.
   * @see org.cp.elements.lang.Filter
   */
  protected Filter<T> getFilterTwo() {
    return this.filterTwo;
  }

  /**
   * Determines whether the specified object meets the criteria (rules) defined by this {@link Filter}.
   *
   * @param obj {@link Object} evaluated by this {@link Filter}.
   * @return a boolean value indicating whether the specified {@link Object} satisfies the criteria (rules)
   * of this {@link Filter}.
   * @see #getFilterOne()
   * @see #getOp()
   * @see #getFilterTwo()
   */
  @SuppressWarnings("unchecked")
  public boolean accept(T obj) {
    return getOp().evaluate(() -> getFilterOne().accept(obj), () -> getFilterTwo().accept(obj));
  }

  /**
   * Returns a {@link String} representation of this {@link ComposableFilter}.
   *
   * @return a {@link String} value representing this {@link ComposableFilter}.
   */
  @Override
  public String toString() {
    return getClass().getName();
  }
}
