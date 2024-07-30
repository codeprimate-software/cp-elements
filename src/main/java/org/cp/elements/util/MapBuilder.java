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
package org.cp.elements.util;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.BiFunction;
import java.util.function.Function;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Builder;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Implementation of the {@literal Builder Software Design Pattern} used to build a {@link Map} implementation
 * containing a mapping of keys and values.
 *
 * @author John Blum
 * @param <KEY> {@link Class type} of the {@link Map} key.
 * @param <VALUE> {@link Class type} of the {@link Map} value.
 * @see java.util.HashMap
 * @see java.util.Map
 * @see java.util.SortedMap
 * @see java.util.concurrent.ConcurrentMap
 * @see org.cp.elements.lang.Builder
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class MapBuilder<KEY, VALUE> implements Builder<Map<KEY, VALUE>> {

  /**
   * Factory method used to construct a new {@link MapBuilder} initialized with a {@link ConcurrentMap} implementation.
   *
   * @param <KEY> {@link Class type} of the {@link Map} key.
   * @param <VALUE> {@link Class type} of the {@link Map} value.
   * @return a new {@link MapBuilder} initialized with a {@link ConcurrentMap}.
   * @see java.util.concurrent.ConcurrentMap
   * @see #MapBuilder(Map)
   */
  public static @NotNull <KEY, VALUE> MapBuilder<KEY, VALUE> newConcurrentMap() {
    return new MapBuilder<>(new ConcurrentHashMap<>());
  }

  /**
   * Factory method used to construct a new {@link MapBuilder} initialized with a {@link HashMap} implementation.
   *
   * @param <KEY> {@link Class type} of the {@link Map} key.
   * @param <VALUE> {@link Class type} of the {@link Map} value.
   * @return a new {@link MapBuilder} initialized with a {@link HashMap}.
   * @see java.util.HashMap
   * @see #MapBuilder(Map)
   */
  public static @NotNull <KEY, VALUE> MapBuilder<KEY, VALUE> newHashMap() {
    return new MapBuilder<>(new HashMap<>());
  }

  /**
   * Factory method used to construct a new {@link MapBuilder} initialized with a {@link SortedMap} implementation.
   *
   * @param <KEY> {@link Class type} of the {@link Map} key.
   * @param <VALUE> {@link Class type} of the {@link Map} value.
   * @return a new {@link MapBuilder} initialized with a {@link SortedMap}.
   * @see java.util.SortedMap
   * @see #MapBuilder(Map)
   */
  public static @NotNull <KEY, VALUE> MapBuilder<KEY, VALUE> newSortedMap() {
    return new MapBuilder<>(new TreeMap<>());
  }

  private final Map<KEY, VALUE> map;

  /**
   * Constructs a new {@link MapBuilder} initialized with the given {@link Map}.
   *
   * @param map {@link Map} to build, populated with {@link Map#put(Object, Object) put} operations;
   * must not be {@literal null}.
   * @throws IllegalArgumentException if {@link Map} is {@literal null}.
   * @see java.util.Map
   */
  protected MapBuilder(@NotNull Map<KEY, VALUE> map) {
    this.map = ObjectUtils.requireObject(map, "Map is required");
  }

  /**
   * Returns a reference to the {@link Map} being built.
   *
   * @return a reference to the {@link Map} being built.
   * @see java.util.Map
   */
  protected @NotNull Map<KEY, VALUE> getMap() {
    return this.map;
  }

  /**
   * Puts the given {@link KEY key} and {@link VALUE value} into the {@link Map} being built
   * by this {@link MapBuilder builder}.
   *
   * @param key {@link KEY key} to put.
   * @param value {@link VALUE value} to put mapped to the given {@link KEY key}.
   * @return this {@link MapBuilder}.
   * @see java.util.Map#put(Object, Object)
   */
  public @NotNull MapBuilder<KEY, VALUE> put(KEY key, VALUE value) {
    getMap().put(key, value);
    return this;
  }

  /**
   * Puts all the {@link KEY keys} and {@link VALUE values} from the given {@link Map} into the {@link Map} being built
   * by this {@link MapBuilder builder}.
   *
   * @param map {@link Map} containing the {@link KEY keys} and {@link VALUE values} to put into this {@link Map}.
   * @return this {@link MapBuilder}.
   * @see java.util.Map#putAll(Map)
   */
  public @NotNull MapBuilder<KEY, VALUE> putAll(Map<KEY, VALUE> map) {
    getMap().putAll(map);
    return this;
  }

  /**
   * Puts the given {@link KEY key} mapped to the given {@link VALUE value} into the {@link Map} being built
   * by this {@link MapBuilder} iff the {@link KEY key} is not already mapped to a value.
   *
   * @param key {@link KEY key} to put.
   * @param value {@link VALUE value} to put mapped to the given {@link KEY key}.
   * @return this {@link MapBuilder}.
   * @see java.util.Map#putIfAbsent(Object, Object)
   */
  public @NotNull MapBuilder<KEY, VALUE> putIfAbsent(KEY key, VALUE value) {
    getMap().putIfAbsent(key, value);
    return this;
  }

  /**
   * Makes the {@link Map} unmodifiable.
   *
   * @return a new {@link MapBuilder} with an unmodifiable {@link Map} from the existing {@link #getMap() Map}.
   * @see java.util.Collections#unmodifiableMap(Map)
   */
  public @NotNull MapBuilder<KEY, VALUE> makeUnmodifiable() {
    return new MapBuilder<>(Collections.unmodifiableMap(getMap()));
  }

  /**
   * Computes a new {@link VALUE value} for an existing {@link Map.Entry}.
   *
   * @param key {@link KEY} ot remap.
   * @param function {@link Function} used to compute a new value.
   * @return this {@link MapBuilder}.
   */
  public @NotNull MapBuilder<KEY, VALUE> putIfPresent(KEY key, BiFunction<KEY, VALUE, ? extends VALUE> function) {
    getMap().computeIfPresent(key, function);
    return this;
  }

  /**
   * Makes the underlying {@link Map} a {@literal Singleton}.
   *
   * @return a new {@link MapBuilder} from the existing {@link Map} as a {@literal Singleton}.
   * @throws IllegalArgumentException if the {@link Map} has no entires or more than 1 entry.
   */
  public @NotNull MapBuilder<KEY, VALUE> singleton() {

    KEY key = getMap().keySet().stream()
      .filter(it -> map.size() == 1)
      .findFirst()
      .orElseThrow(() -> newIllegalStateException("Expected Map of size 1; but was [%s]", getMap().size()));

    return new MapBuilder<>(Collections.singletonMap(key, getMap().get(key)));
  }

  /**
   * Synchronizes {@link Map} operations during build.
   *
   * @return a new {@link MapBuilder} with a synchronized {@link Map} from the existing {@link #getMap() Map}.
   * @throws IllegalStateException if {@link Map} is a {@link ConcurrentMap}.
   * @see java.util.Collections#synchronizedMap(Map)
   */
  public @NotNull MapBuilder<KEY, VALUE> synchronize() {

    Assert.state(!(getMap() instanceof ConcurrentMap<?, ?>), () ->
      "Map implementation is already a ConcurrentMap [%s]".formatted(getMap().getClass().getName()));

    return new MapBuilder<>(Collections.synchronizedMap(getMap()));
  }

  /**
   * Builds the {@link Map}.
   *
   * @return the built {@link Map}.
   * @see java.util.Map
   * @see #getMap()
   */
  @SuppressWarnings("unchecked")
  public @NotNull Map<KEY, VALUE> build() {
    return getMap();
  }
}
