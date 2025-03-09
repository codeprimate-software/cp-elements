/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.data.oql.functions;

import java.math.BigDecimal;
import java.util.function.Function;

import org.cp.elements.data.oql.QueryFunction;
import org.cp.elements.data.support.Iterables;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.ThreadSafe;

/**
 * {@link QueryFunction} used to calculate a {@literal sum}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} on which this function is applied.
 * @see java.lang.Comparable
 * @see java.math.BigDecimal
 * @see org.cp.elements.lang.annotation.ThreadSafe
 * @see org.cp.elements.data.oql.QueryFunction
 * @since 2.0.0
 */
@ThreadSafe
public class Sum<T> implements QueryFunction<T, BigDecimal> {

  /**
   * Factory method used to construct a new {@link Sum} query function used to compute a sum of numerical values.
   *
   * @param <S> {@link Class type} of {@link Object} from which the numerical value is extracted.
   * @param function {@link Function} used to extract the numerical value from the {@link S type}.
   * @return a new {@link Sum} query function.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.Function
   */
  public static <S> Sum<S> of(@NotNull Function<S, ? extends Number> function) {
    return new Sum<>(function);
  }

  private String name;

  private final Function<T, ? extends Number> function;

  /**
   * Factory method used to construct a new {@link Sum} query function used to compute a sum of numerical values.
   *
   * @param function {@link Function} used to extract the numerical value from the {@link T type}.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.Function
   */
  protected Sum(@NotNull Function<T, ? extends Number> function) {
    this.function = ObjectUtils.requireObject(function, "Function is required");
  }

  /**
   * Returns the {@link String name} given to this query function.
   *
   * @return the {@link String name} given to this query function.
   */
  @Override
  public String getName() {
    return StringUtils.defaultIfBlank(this.name, Constants.UNKNOWN);
  }

  /**
   * Computes a sum of all the numbers in the {@link Iterable collection}.
   *
   * @param resultSet {@link Iterable} of {@link T Objects} on which this {@link QueryFunction} is applied.
   * @return {@link BigDecimal} containing the sum of all the numbers in the {@link Iterable collection};
   * returns {@link BigDecimal#ZERO} if the {@link Iterable collection} is {@literal null} or {@literal empty}.
   * @see java.math.BigDecimal
   * @see java.lang.Iterable
   */
  @NullSafe
  @Override
  public BigDecimal apply(Iterable<T> resultSet) {

    BigDecimal sum = BigDecimal.ZERO;

    for (T result : Iterables.nullSafeIterable(resultSet)) {
      Number value = this.function.apply(result);
      sum = sum.add(asBigDecimal(value));
    }

    return sum;
  }

  /**
   * Converts the {@link Number} to a {@link BigDecimal}, return {@literal 0} if the numerical value is {@literal null}.
   *
   * @param value {@link Number} to convert into a {@link BigDecimal}.
   * @return a {@link BigDecimal} having the value of the given {@link Number}.
   * @see java.math.BigDecimal
   */
  @NullSafe
  private BigDecimal asBigDecimal(Number value) {
    return value != null ? BigDecimal.valueOf(value.doubleValue()) : BigDecimal.ZERO;
  }

  /**
   * Builder method used to assign a {@link String name} to this query function.
   *
   * @param <U> {@link Class subtype} of {@link Sum}.
   * @param name {@link String} containing the name given to this query function.
   * @return this query function.
   */
  @SuppressWarnings("unchecked")
  public <U extends Sum<T>> U named(String name) {
    this.name = name;
    return (U) this;
  }
}
