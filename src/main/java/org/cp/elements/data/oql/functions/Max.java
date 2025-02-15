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
 * @param <T> {@link Class type} of {@link Object} on which this function is applied.
 * @param <V> {@link Comparable} type.
 * @see java.lang.Comparable
 * @see org.cp.elements.data.oql.QueryFunction
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class Max<T, V extends Comparable<V>> implements QueryFunction<T, V> {

  public static <T, V extends Comparable<V>> Max<T, V> of(@NotNull Function<T, V> function) {
    return new Max<>(function);
  }

  private String name;

  private final Function<T, V> function;

  protected Max(@NotNull Function<T, V> function) {
    this.function = ObjectUtils.requireObject(function, "Function is required");
  }

  @Override
  public String getName() {
    return StringUtils.defaultIfBlank(this.name, Constants.UNKNOWN);
  }

  @Override
  public V apply(Iterable<T> resultSet) {

    V max = null;

    for (T result : Iterables.nullSafeIterable(resultSet)) {
      V value = this.function.apply(result);
      if (value != null) {
        max = max == null || value.compareTo(max) > 0 ? value : max;
      }
    }

    return max;
  }

  public Max<T, V> named(String name) {
    this.name = name;
    return this;
  }
}
