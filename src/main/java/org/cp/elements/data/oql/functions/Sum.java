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

/**
 * {@link QueryFunction} used to calculate a {@literal sum}.
 *
 * @author John Blum
 * @param <S> {@link Class type} of {@link Object} on which this function is applied.
 * @see java.lang.Comparable
 * @see org.cp.elements.data.oql.QueryFunction
 * @since 2.0.0
 */
public class Sum<S> implements QueryFunction<S, BigDecimal> {

  public static <S> Sum<S> of(@NotNull Function<S, ? extends Number> function) {
    return new Sum<>(function);
  }

  private String name;

  private final Function<S, ? extends Number> function;

  protected Sum(@NotNull Function<S, ? extends Number> function) {
    this.function = ObjectUtils.requireObject(function, "Function is required");
  }

  @Override
  public String getName() {
    return StringUtils.defaultIfBlank(this.name, Constants.UNKNOWN);
  }

  @Override
  @SuppressWarnings("unchecked")
  public BigDecimal apply(Iterable<S> resultSet) {

    BigDecimal sum = BigDecimal.ZERO;

    for (S result : Iterables.nullSafeIterable(resultSet)) {
      Number value = this.function.apply(result);
      sum = sum.add(asBigDecimal(value));
    }

    return sum;
  }

  private BigDecimal asBigDecimal(Number value) {
    return value != null ? BigDecimal.valueOf(value.doubleValue()) : BigDecimal.ZERO;
  }

  @SuppressWarnings("unchecked")
  public <U extends Sum<S>> U named(String name) {
    this.name = name;
    return (U) this;
  }
}
