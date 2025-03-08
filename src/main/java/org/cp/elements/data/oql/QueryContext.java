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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract Data Type (ADT) encapsulating contextual metadata used to execute the {@link Query}.
 *
 * @author John Blum
 * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
 * @param <T> {@link Class type} of the projected {@link Object elements}.
 * @param query OQL {@link Query} to execute.
 * @param metadata {@link Map} of contextual (e.g. environment) metadata useful when executing the {@link Query}.
 * @see org.cp.elements.data.oql.Query
 * @since 2.0.0
 */
public record QueryContext<S, T>(@NotNull Query<S, T> query, Map<String, Object> metadata) {

  /**
   * Constructs a new {@link QueryContext} initialized with the given {@link Query} and {@link Map} of metadata
   * containing environment-specific information when executing the {@link Query}.
   *
   * @param query {@link Query} that is the subject of execution.
   * @param metadata {@link Map} containing metadata about the context in which the {@link Query} will be executed.
   * @throws IllegalArgumentException if {@link Query} or the {@link Map metadata} are {@literal null}.
   * @see Query
   */
  public QueryContext {
    Assert.notNull(query, "Query is required");
    Assert.notNull(metadata, "Map of metadata is required");
  }

  /**
   * Factory metho used to construct a new {@link QueryContext} with the given {@link Query}
   * with no {@link Map metadata}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param query {@link Query} that is the subject of execution.
   * @return a new {@link QueryContext}.
   * @throws IllegalArgumentException if {@link Query} is {@literal null}.
   * @see Query
   */
  public static <S, T> QueryContext<S, T> from(@NotNull Query<S, T> query) {
    return new QueryContext<>(query, new ConcurrentHashMap<>());
  }

  /**
   * Returns the {@link Iterable collection} to query.
   *
   * @return the {@link Iterable collection} to query.
   * @see java.lang.Iterable
   */
  public Iterable<S> getCollection() {
    return query().collection();
  }

  /**
   * Returns the selected {@link Projection} of the {@link Query}.
   *
   * @return the selected {@link Projection} of the {@link Query}.
   * @see Projection
   */
  public Projection<S, T> getProjection() {
    return query().selection().getProjection();
  }

  /**
   * Gets a {@link S value} from the {@link Map metadata} for th given {@link String key}.
   *
   * @param <S> {@link Class type} if the metadata value.
   * @param key {@link String} containing the key in the metadata referencing the value to get.
   * @return a {@link S value} from the {@link Map metadata} for th given {@link String key}.
   * @see #metadata()
   */
  @SuppressWarnings("unchecked")
  public <S> S get(String key) {
    return (S) metadata().get(key);
  }

  /**
   * Sets the given {@link String key} to the provided {@link Object value} in the {@link Map metadata}.
   *
   * @param key {@link String} containing the name of the key to set.
   * @param value {@link Object} value of the key.
   * @throws IllegalArgumentException if the {@link String key} is {@literal null} or {@literal empty},
   * or the {@link Object value} is {@literal null}.
   */
  public void put(@NotNull String key, @NotNull Object value) {
    Assert.hasText(key, "Key [%s] is required", key);
    Assert.notNull(value, "Value is required");
    metadata().put(key, value);
  }
}
