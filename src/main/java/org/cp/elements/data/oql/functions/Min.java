/*
 * Copyright 2011-Present Author or Authors.
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
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.ThreadSafe;

/**
 * {@link QueryFunction} used to calculate the {@literal minimum value} in a set.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} on which this function is applied.
 * @param <V> {@link Comparable type} of the value compared in the min operation.
 * @see java.lang.Comparable
 * @see org.cp.elements.data.oql.QueryFunction
 * @see org.cp.elements.lang.annotation.ThreadSafe
 * @since 2.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class Min<T, V extends Comparable<V>> implements QueryFunction<T, V> {

  /**
   * Factory method used to construct a new {@link Min} query function.
   *
   * @param <T> {@link Class type} of {@link Object} on which this {@link Function} is applied.
   * @param <V> {@link Comparable type} of the value compared in the min operation.
   * @param function {@link Function} used to extract the {@link Comparable value} from an {@link Object}
   * of type {@link T} used in the min operation.
   * @return a new {@link Max} query function.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.Function
   * @see java.lang.Comparable
   */
  public static <T, V extends Comparable<V>> Min<T, V> of(@NotNull Function<T, V> function) {
    return new Min<>(function);
  }

  private String name;

  private final Function<T, V> function;

  /**
   * Construct a new {@link Min} query function.
   *
   * @param function {@link Function} used to extract the {@link Comparable value} from an {@link Object}
   * of type {@link T} used in the min operation.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.Function
   * @see java.lang.Comparable
   */
  protected Min(@NotNull Function<T, V> function) {
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
   * Computes the minimum {@link Comparable value} from the collection of {@link Iterable objects}.
   *
   * @param resultSet {@link Iterable} of {@link T Objects} use to compute a minimum value.
   * @return the {@link V minimum value} from the collection of {@link Iterable objects};
   * returns {@literal null} if the {@link Iterable collection} is {@literal null} or {@literal empty}.
   * @see java.lang.Iterable
   */
  @NullSafe
  @Override
  public V apply(Iterable<T> resultSet) {

    V min = null;

    for (T result : Iterables.nullSafeIterable(resultSet)) {
      V value = this.function.apply(result);
      if (value != null) {
        min = min == null || value.compareTo(min) < 0 ? value : min;
      }
    }

    return min;
  }

  /**
   * Builder method used to assign a {@link String name} to this query function.
   *
   * @param name {@link String} containing the name given to this query function.
   * @return this query function.
   */
  public Min<T, V> named(String name) {
    this.name = name;
    return this;
  }
}
