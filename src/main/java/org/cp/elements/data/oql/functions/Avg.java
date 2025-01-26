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
import java.math.RoundingMode;
import java.util.function.Function;

import org.cp.elements.data.oql.QueryFunction;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link QueryFunction} used to calculate an {@literal average} from a set of values.
 *
 * @author John Blum
 * @param <S> {@link Class type} of {@link Object} on which this function is applied.
 * @see org.cp.elements.data.oql.functions.Sum
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class Avg<S> extends Sum<S> {

  public static <S> Avg<S> of(@NotNull Function<S, ? extends Number> function) {
    return new Avg<>(function);
  }

  protected Avg(@NotNull Function<S, ? extends Number> function) {
    super(function);
  }

  @Override
  @SuppressWarnings({ "all", "unchecked" })
  public BigDecimal apply(S... resultSet) {

    BigDecimal sum = super.apply(resultSet);
    BigDecimal divisor = BigDecimal.valueOf(resultSet.length);
    BigDecimal average = sum.divide(divisor, RoundingMode.HALF_UP);

    return average;
  }
}
