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
package org.cp.elements.data.oql;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.util.MapUtils;

/**
 * Abstract Data Type (ADT) modeling a single row from a result set generated from an OQL {@link Query}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the query result modeled by this ADT.
 * @see org.cp.elements.data.oql.Oql
 * @see org.cp.elements.data.oql.Query
 * @since 2.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface QueryResult<T> {

  /**
   * Factory method returning a {@link QueryResult.Builder} used to construct a new {@link QueryResult}.
   *
   * @param <T> {@link Class type} of {@link Object} contained in the {@link QueryResult}.
   * @return a {@link QueryResult.Builder} used to construct a new {@link QueryResult}.
   * @see QueryResult.Builder
   */
  static <T> QueryResult.Builder<T> builder() {
    return new QueryResult.Builder<>();
  }

  /**
   * Gets the {@link T value} of a {@link String named field} in the {@link QueryResult}.
   *
   * @param <V> {@link Class type} of the {@link String field} value.
   * @param fieldName {@link String} containing the name of a field in the {@link QueryResult}.
   * @return the {@link T value} of a {@link String named field} in the {@link QueryResult}.
   */
  <V> V get(String fieldName);

  default T map(@NotNull Function<QueryResult<T>, T> mapper) {
    Assert.notNull(mapper, "Function is required");
    return mapper.apply(this);
  }

  /**
   * Elements {@link org.cp.elements.lang.Builder} used to construct a new {@link QueryResult}.
   *
   * @param <T> {@link Class type} of {@link Object} encapsulated and modeled by the {@link QueryResult}.
   * @see org.cp.elements.lang.Builder
   */
  class Builder<T> implements org.cp.elements.lang.Builder<QueryResult<T>> {

    private final Map<String, Object> namedValues = new HashMap<>();

    /**
     * Builder method used to construct a new {@link QueryResult} from the given {@link Map}.
     *
     * @param map {@link Map} object used as the source of the {@link QueryResult}.
     * @return a new {@link QueryResult.Builder} based on the given {@link Map}.
     * @see java.util.Map
     */
    @NullSafe
    public Builder<T> withMap(Map<String, Object> map) {
      this.namedValues.putAll(MapUtils.nullSafeMap(map));
      return this;
    }

    /**
     * Gets a {@link V named value} from the {@link QueryResult}.
     *
     * @param <V> {@link Class type} of the {@link String field} value.
     * @param name {@link String} of the {@link V value} in the {@link QueryResult}.
     * @return the {@link V named value} from the {@link QueryResult}.
     */
    @SuppressWarnings("unchecked")
    <V> V getNamedValue(String name) {
      return (V) this.namedValues.get(name);
    }

    @Override
    public QueryResult<T> build() {
      return this::getNamedValue;
    }
  }
}
