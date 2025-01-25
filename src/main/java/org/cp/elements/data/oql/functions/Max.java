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

import java.util.Objects;
import java.util.function.Function;

import org.cp.elements.data.oql.QueryFunction;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link QueryFunction} used to calculate the {@literal maximum value} in a set.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @see org.cp.elements.data.oql.QueryFunction
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class Max<S, T extends Comparable<T>> implements QueryFunction<S, T> {

  public static <S, T extends Comparable<T>> Max<S, T> of(@NotNull Function<S, T> function) {
    return new Max<>(function);
  }

  private final Function<S, T> function;

  protected Max(@NotNull Function<S, T> function) {
    this.function = Objects.requireNonNull(function, "Function is required");
  }

  @Override
  @SuppressWarnings("unchecked")
  public T apply(S... targets) {

    T max = null;

    for (S target : targets) {
      T targetValue = this.function.apply(target);
      if (targetValue != null) {
        if (max == null) {
          max = targetValue;
        }
        else if (targetValue.compareTo(max) > 0) {
          max = targetValue;
        }
      }
    }

    return max;
  }
}
