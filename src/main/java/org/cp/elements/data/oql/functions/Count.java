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

import org.cp.elements.data.oql.QueryFunction;
import org.cp.elements.data.support.Iterables;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.ThreadSafe;
import org.cp.elements.util.stream.StreamUtils;

/**
 * {@link QueryFunction} used to {@literal count} the elements in the result set derived from a query.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} on which this {@link Count function} is applied.
 * @see org.cp.elements.lang.annotation.ThreadSafe
 * @see org.cp.elements.data.oql.QueryFunction
 * @since 2.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class Count<T> implements QueryFunction<T, Long> {

  protected static final String DEFAULT_NAME = "Count";

  /**
   * Factory method used to construct a new {@link Count} query function to count all the elements.
   *
   * @param <T> {@link Class type} of the {@link Object} being counted.
   * @return a new {@link Count} query function.
   */
  public static <T> Count<T> all() {
    return new Count<>();
  }

  private String name;

  /**
   * Returns the {@link String name} given to this query function.
   *
   * @return the {@link String name} given to this query function.
   */
  @Override
  public String getName() {
    return StringUtils.defaultIfBlank(this.name, DEFAULT_NAME);
  }

  /**
   * Counts all the elements in the {@link Iterable result set}.
   *
   * @param resultSet {@link Iterable} of {@link T Objects} to count.
   * @return a {@link Long count} of all the elments in the {@link Iterable result set}; returns {@literal 0}
   * if the {@link Iterable result set} is {@literal null} or {@literal empty}.
   * @see java.lang.Iterable
   */
  @NullSafe
  @Override
  public Long apply(Iterable<T> resultSet) {
    return StreamUtils.stream(Iterables.nullSafeIterable(resultSet)).count();
  }

  /**
   * Builder method used to assign a {@link String name} to this query function.
   *
   * @param name {@link String} containing the name given to this query function.
   * @return this query function.
   */
  public Count<T> named(String name) {
    this.name = name;
    return this;
  }
}
