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
import org.cp.elements.util.CollectionUtils;

/**
 * {@link QueryFunction} used to calculate an {@literal average} from a set of values.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} on which this function is applied.
 * @see org.cp.elements.data.oql.functions.Sum
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class Avg<T> extends Sum<T> {

  public static <T> Avg<T> of(@NotNull Function<T, ? extends Number> function) {
    return new Avg<>(function);
  }

  protected Avg(@NotNull Function<T, ? extends Number> function) {
    super(function);
  }

  @Override
  @SuppressWarnings({ "all", "unchecked" })
  public BigDecimal apply(Iterable<T> resultSet) {

    BigDecimal sum = super.apply(resultSet);
    BigDecimal divisor = BigDecimal.valueOf(CollectionUtils.count(resultSet));
    BigDecimal average = sum.divide(divisor, RoundingMode.HALF_UP);

    return average;
  }
}
