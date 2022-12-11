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
package org.cp.elements.data.caching.provider;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;
import java.util.function.Predicate;

import org.cp.elements.data.caching.AbstractCache;
import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.concurrent.ThreadSafe;
import org.cp.elements.util.MapUtils;

/**
 * An Elements caching provider implementation of the {@link Cache} interface backed by a Java {@link ConcurrentMap}.
 *
 * @author John Blum
 * @param <KEY> {@link Class type} of the {@link Cache} key.
 * @param <VALUE> {@link Class type} of the {@link Cache} value.
 * @see java.lang.Comparable
 * @see java.util.Map
 * @see java.util.concurrent.ConcurrentMap
 * @see org.cp.elements.data.caching.AbstractCache
 * @see org.cp.elements.data.caching.Cache
 * @since 1.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class ConcurrentMapCache<KEY extends Comparable<KEY>, VALUE> extends AbstractCache<KEY, VALUE> {

  private final ConcurrentMap<KEY, VALUE> map = newConcurrentMap();

  /**
   * Constructs a new instance of {@link ConcurrentMap}.
   *
   * Constructs a new {@link ConcurrentHashMap} by default.
   *
   * @return a new {@link ConcurrentMap}.
   * @see java.util.concurrent.ConcurrentHashMap
   * @see java.util.concurrent.ConcurrentMap
   */
  @NotNull ConcurrentMap<KEY, VALUE> newConcurrentMap() {
    return new ConcurrentHashMap<>();
  }

  /**
   * Get a reference to the configured {@link ConcurrentMap} used to back this {@link Cache}.
   *
   * @return a reference to the configured {@link ConcurrentMap} used to back this {@link Cache}.
   * @see java.util.concurrent.ConcurrentMap
   */
  protected @NotNull ConcurrentMap<KEY, VALUE> getConcurrentMap() {
    return this.map;
  }

  /**
   * Returns {@literal null} since a {@link ConcurrentMap} is already Thread-safe with atomicity guarantees
   * and appropriately coordinates concurrent operations.
   *
   * @return {@literal null} by default.
   */
  @Override
  public final Object getLock() {
    return null;
  }

  /**
   * Determines whether this {@link Cache} contains any {@link Cache.Entry entries}.
   *
   * @return a boolean value indicating whether this {@link Cache} contains any {@link Cache.Entry entries}.
   * @see java.util.concurrent.ConcurrentMap#isEmpty()
   * @see #getConcurrentMap()
   * @see #size()
   */
  @NullSafe
  @Override
  public boolean isEmpty() {
    return getConcurrentMap().isEmpty();
  }

  /**
   * Clears the entire contents of (all {@link Cache.Entry entries} from) this {@link Cache}.
   *
   * @see java.util.concurrent.ConcurrentMap#clear()
   * @see #getConcurrentMap()
   */
  @NullSafe
  @Override
  public void clear() {
    getConcurrentMap().clear();
  }

  /**
   * Determines whether this {@link Cache} contains an {@link Cache.Entry} mapped to the given {@link KEY key}.
   *
   * @param key {@link KEY key} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains an {@link Cache.Entry} mapped to
   * the given {@link KEY key}.
   * @see java.util.concurrent.ConcurrentMap#containsKey(Object)
   * @see #getConcurrentMap()
   */
  @NullSafe
  @Override
  public boolean contains(@Nullable KEY key) {
    return key != null && getConcurrentMap().containsKey(key);
  }

  /**
   * Removes the {@link Cache.Entry} mapped to the given {@link KEY key} in this {@link Cache}.
   *
   * @param key {@link KEY key} identifying the {@link Cache.Entry} to remove (evict) from this {@link Cache}.
   * @see java.util.concurrent.ConcurrentMap#remove(Object)
   * @see #getConcurrentMap()
   */
  @NullSafe
  @Override
  public void evict(@Nullable KEY key) {

    if (key != null) {
      getConcurrentMap().remove(key);
    }
  }

  /**
   * Caches all {@link Map.Entry entries} from given {@link Map} in this {@link Cache}.
   *
   * @param map {@link Map} containing the {@link Map.Entry entries} to cache.
   * @see java.util.concurrent.ConcurrentMap#putAll(Map)
   * @see #getConcurrentMap()
   * @see java.util.Map
   */
  @NullSafe
  @Override
  public void from(@Nullable Map<KEY, VALUE> map) {

    if (MapUtils.isNotEmpty(map)) {

      Predicate<Map.Entry<KEY, VALUE>> noNullEntries = Objects::nonNull;
      Predicate<Map.Entry<KEY, VALUE>> noNullKeys = entry -> Objects.nonNull(entry.getKey());
      Predicate<Map.Entry<KEY, VALUE>> noNullValues = entry -> Objects.nonNull(entry.getValue());

      Map<KEY, VALUE> filteredMap = MapUtils.filter(map, noNullEntries.and(noNullKeys).and(noNullValues));

      getConcurrentMap().putAll(filteredMap);
    }
  }

  /**
   * Gets the {@link VALUE value} stored in this {@link Cache} mapped to the given {@link KEY}.
   *
   * Returns {@literal null} if the {@link VALUE value} mapped to the given {@link KEY key} is {@literal null},
   * or this {@link Cache} does not contain an {@link Cache.Entry} mapped to the given {@link KEY key}.
   *
   * @param key {@link KEY key} mapped to the {@link VALUE value} returned.
   * @return the {@link VALUE value} mapped to the given {@link KEY key}, or return {@literal null}
   * if an {@link Cache.Entry entry} with the given {@link KEY key} does not exist,
   * or a {@link VALUE value} for the given {@link KEY key} is {@literal null}.
   * @see java.util.concurrent.ConcurrentMap#get(Object)
   * @see #put(Comparable, Object)
   * @see #getConcurrentMap()
   */
  @NullSafe
  @Override
  public @Nullable VALUE get(@NotNull KEY key) {
    return key != null ? getConcurrentMap().get(key) : null;
  }

  /**
   * Returns all {@link KEY keys} in this {@link Cache}.
   *
   * @return a {@link Set} containing all the {@link KEY keys} from this {@link Cache}.
   * Returns an {@link Set#isEmpty() empty Set} if there are no {@link Cache.Entry entries}
   * in this {@link Cache}.
   * @see java.util.concurrent.ConcurrentMap#keySet()
   * @see #getConcurrentMap()
   * @see java.util.Set
   */
  @NullSafe
  @Override
  public Set<KEY> keys() {
    return Collections.unmodifiableMap(getConcurrentMap()).keySet();
  }

  /**
   * Puts the {@link VALUE value} in this {@link Cache} mapped to the given {@link KEY key}.
   *
   * @param key {@link KEY} mapped to the {@link VALUE value}; must not be {@literal null}.
   * @param value {@link VALUE} put in this {@link Cache} mapped to the {@link KEY key}.
   * @throws IllegalArgumentException if the {@link KEY key} is {@literal null}.
   * @see java.util.concurrent.ConcurrentMap#put(Object, Object)
   * @see #getConcurrentMap()
   * @see #get(Comparable)
   */
  @Override
  public void put(@NotNull KEY key, @NotNull VALUE value) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(value, "Value is required");

    getConcurrentMap().put(key, value);
  }

  /**
   * Puts the {@link KEY key} mapped to the {@link VALUE value} in this {@link Cache}
   * only if a {@link Cache.Entry} with the given {@link KEY key} does not already exist.
   *
   * @param key {@link KEY} used to map the {@link VALUE value}; must not be {@literal null}.
   * @param value {@link VALUE} to put in this {@link Cache} mapped to the given {@link KEY key}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws IllegalArgumentException if the {@link KEY key} is {@literal null}.
   * @see java.util.concurrent.ConcurrentMap#putIfAbsent(Object, Object)
   * @see #putIfPresent(Comparable, Object)
   * @see #getConcurrentMap()
   */
  @Override
  public @Nullable VALUE putIfAbsent(@NotNull KEY key, @NotNull VALUE value) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(value, "Value is required");

    return getConcurrentMap().putIfAbsent(key, value);
  }

  /**
   * Puts the {@link KEY key} mapped to the {@link VALUE new value} in this {@link Cache}
   * only if an {@link Cache.Entry} with the given {@link KEY key} already exists in this {@link Cache}.
   *
   * @param key {@link KEY key} mapped to the {@link VALUE new value} in this {@link Cache};
   * must not be {@literal null}.
   * @param newValue {@link VALUE new value} replacing the {@link VALUE existing value}
   * in this {@link Cache} mapped to the given {@link KEY key}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws IllegalArgumentException if the {@link KEY key} is {@literal null}.
   * @see java.util.concurrent.ConcurrentMap#computeIfPresent(Object, BiFunction)
   * @see #putIfAbsent(Comparable, Object)
   * @see #getConcurrentMap()
   */
  @Override
  public @Nullable VALUE putIfPresent(@NotNull KEY key, @NotNull VALUE newValue) {

    Assert.notNull(newValue, "Value is required");

    if (key != null) {

      AtomicReference<VALUE> oldValueReference = new AtomicReference<>(null);

      return newValue.equals(getConcurrentMap().computeIfPresent(key, (theKey, oldValue) -> {
        oldValueReference.set(oldValue);
        return newValue;
      })) ? oldValueReference.get() : null;
    }

    return null;
  }

  /**
   * Determines the number of {@link Cache.Entry entries} contained in this {@link Cache}.
   *
   * @return an {@link Integer} value with the number of {@link Cache.Entry entries} contained in this {@link Cache}.
   * @see java.util.concurrent.ConcurrentMap#size()
   * @see #getConcurrentMap()
   * @see #isEmpty()
   */
  @NullSafe
  @Override
  public long size() {
    return getConcurrentMap().size();
  }

  /**
   * Returns this {@link Cache} as an instance of {@link Map}.
   *
   * @return a {@link Map} containing all the {@link Cache.Entry entries} in this {@link Cache}.
   * @see #getConcurrentMap()
   * @see java.util.Map
   */
  @NullSafe
  @Override
  public @NotNull Map<KEY, VALUE> toMap() {
    return Collections.unmodifiableMap(getConcurrentMap());
  }
}
