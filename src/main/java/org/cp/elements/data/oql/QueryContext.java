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
 * @see org.cp.elements.data.oql.Query
 * @since 2.0.0
 */
public record QueryContext<S, T>(@NotNull Query<S, T> query, Map<String, Object> metadata) {

  public QueryContext {
    Assert.notNull(query, "Query is required");
    Assert.notNull(metadata, "Map of metadata is required");
  }

  public static <S, T> QueryContext<S, T> from(@NotNull Query<S, T> query) {
    return new QueryContext<>(query, new ConcurrentHashMap<>());
  }

  public Projection<S, T> getProjection() {
    return query().selection().getProjection();
  }

  @SuppressWarnings("unchecked")
  public <S> S get(String key) {
    return (S) metadata().get(key);
  }

  public void put(@NotNull String key, @NotNull Object value) {
    Assert.hasText(key, "Key [%s] is required", key);
    Assert.notNull(value, "Value is required");
    metadata().put(key, value);
  }
}
