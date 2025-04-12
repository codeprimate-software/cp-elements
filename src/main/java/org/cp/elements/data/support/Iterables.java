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
package org.cp.elements.data.support;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.data.struct.tabular.Row;
import org.cp.elements.data.struct.tabular.Table;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.Alias;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;

/**
 * Factory for instantiating {@link Iterable Iterables} from different sources.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @since 2.0.0
 */
public abstract class Iterables {

  /**
   * Returns an empty {@link Iterable}.
   *
   * @param <T> {@link Class type} of {@link Object elements} in the {@link Iterable}.
   * @return an empty {@link Iterable}.
   * @see java.lang.Iterable
   */
  @Alias(forMember = "CollectionUtils.emptyIterable")
  public static <T> Iterable<T> empty() {
    return CollectionUtils.emptyIterable();
  }

  /**
   * Returns an unmodifiable {@link Iterable} over the {@link Object elements} in the array.
   *
   * @param <T> {@link Class type} of {@link Object elements} stored in the array.
   * @param array array of objects used to construct an {@link Iterable}; required.
   * @return an {@link Iterable} over the {@link Object elements} in the array.
   * @throws IllegalArgumentException if array is {@literal null}.
   * @see java.lang.Iterable
   */
  @SafeVarargs
  @Alias(forMember = "ArrayUtils.asIterable")
  public static <T> Iterable<T> from(T... array) {
    Assert.notNull(array, "Array is required");
    return CollectionUtils.unmodifiableIterable(ArrayUtils.asIterable(array));
  }

  /**
   * Returns an unmodifiable {@link Iterable} over the {@link Cache.Entry#getValue() values}
   * stored in the {@link Cache}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored in the {@link Cache}.
   * @param cache {@link Cache} used to construct a new {@link Iterable}
   * iterating over the {@link Cache.Entry cache entries}; required.
   * @return an {@link Iterable} over the {@link Cache.Entry#getValue()} values} stored in the {@link Cache}.
   * @throws IllegalArgumentException if {@link Cache} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache
   * @see java.lang.Iterable
   */
  public static <T> Iterable<T> from(@NotNull Cache<?, T> cache) {

    Assert.notNull(cache, "Cache is required");

    List<T> cacheValues = cache.stream()
      .map(Cache.Entry::getValue)
      .toList();

    return CollectionUtils.unmodifiableIterable(cacheValues);
  }

  /**
   * Returns an unmodifiable {@link Iterable} over the {@link Map#values() values} stored in the {@link Map}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored in the {@link Map}.
   * @param map {@link Map} used to construct a new {@link Iterable}
   * iterating over the {@link Map.Entry map entries}; required.
   * @return an {@link Iterable} over the {@link Map#values() values} stored in the {@link Map}.
   * @throws IllegalArgumentException if {@link Map} is {@literal null}.
   * @see java.lang.Iterable
   * @see java.util.Map
   */
  public static <T> Iterable<T> from(@NotNull Map<?, T> map) {
    Assert.notNull(map, "Map is required");
    Collection<T> mapValues = map.values();
    return CollectionUtils.unmodifiableIterable(mapValues);
  }

  /**
   * Returns an unmodifiable {@link Iterable} over the {@link Row Rows} stored in the {@link Table}.
   *
   * @param table {@link Table} used to construct a new {@link Iterable}
   * iterating over the {@link Table} {@link Row Rows}; required.
   * @return an {@link Iterable} over the {@link Row Rows} stored in the {@link Table}.
   * @throws IllegalArgumentException if {@link Table} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Table
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.lang.Iterable
   */
  public static Iterable<Row> from(@NotNull Table table) {
    Assert.notNull(table, "Table is required");
    return CollectionUtils.unmodifiableIterable(table);
  }

  /**
   * Returns the given {@link Iterable} if not {@literal null} or returns an empty {@link Iterable}.
   *
   * @param <T> {@link Class type} of {@link Object elements} emitted by the given {@link Iterable}.
   * @param iterable {@link Iterable} to evaluate.
   * @return the given {@link Iterable} if not {@literal null}, otherwise return an empty {@link Iterable}.
   * @see java.lang.Iterable
   * @see #empty()
   */
  @Alias(forMember = "CollectionUtils.nullSafeIterable")
  public static <T> Iterable<T> nullSafeIterable(Iterable<T> iterable) {
    return CollectionUtils.nullSafeIterable(iterable);
  }
}
