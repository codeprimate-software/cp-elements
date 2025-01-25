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
 * {@link QueryFunction} used to calculate the {@literal average value} in a set.
 *
 * @author John Blum
 * @see java.lang.Number
 * @see org.cp.elements.data.oql.QueryFunction
 * @since 1.0.0
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
  @SuppressWarnings("all")
  public BigDecimal apply(S... targets) {

    BigDecimal sum = super.apply(targets);
    BigDecimal average = sum.divide(BigDecimal.valueOf(targets.length), RoundingMode.HALF_UP);

    return average;
  }
}
