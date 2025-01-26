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

import java.util.function.Function;

import org.cp.elements.data.oql.QueryFunction;
import org.cp.elements.data.support.Iterables;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
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

  private String name;

  private final Function<S, T> function;

  protected Max(@NotNull Function<S, T> function) {
    this.function = ObjectUtils.requireObject(function, "Function is required");
  }

  @Override
  public String getName() {
    return StringUtils.defaultIfBlank(this.name, Constants.UNKNOWN);
  }

  @Override
  @SuppressWarnings("unchecked")
  public T apply(Iterable<S> resultSet) {

    T max = null;

    for (S result : Iterables.nullSafeIterable(resultSet)) {
      T value = this.function.apply(result);
      if (value != null) {
        max = max == null || value.compareTo(max) > 0 ? value : max;
      }
    }

    return max;
  }

  public Max<S, T> named(String name) {
    this.name = name;
    return this;
  }
}
