/*
 * Copyright 2016 Author or Authors.
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

import java.util.HashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.cp.elements.lang.Assert;

/**
 * The {@link MapBuilder} class is an implementation of the Builder Software Design Pattern and is used to build
 * a {@link Map} implementation with a mapping of keys and values.
 *
 * @author John Blum
 * @param <KEY> {@link Class type} of the {@link Map} key.
 * @param <VALUE> {@link Class type} of the {@link Map} value.
 * @see java.util.HashMap
 * @see java.util.Map
 * @see java.util.SortedMap
 * @see java.util.concurrent.ConcurrentMap
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class MapBuilder<KEY, VALUE> {

  private final Map<KEY, VALUE> map;

  /**
   * Factory method used to construct a new instance of {@link MapBuilder} initialized with
   * a {@link ConcurrentMap} implementation.
   *
   * @param <KEY> {@link Class type} of the {@link Map} key.
   * @param <VALUE> {@link Class type} of the {@link Map} value.
   * @return a new instance of {@link MapBuilder} initialized with a {@link ConcurrentMap}.
   * @see java.util.concurrent.ConcurrentMap
   * @see #MapBuilder(Map)
   */
  public static <KEY, VALUE> MapBuilder<KEY, VALUE> newConcurrentMap() {
    return new MapBuilder<>(new ConcurrentHashMap<>());
  }

  /**
   * Factory method used to construct a new instance of {@link MapBuilder} initialized with
   * a {@link HashMap} implementation.
   *
   * @param <KEY> {@link Class type} of the {@link Map} key.
   * @param <VALUE> {@link Class type} of the {@link Map} value.
   * @return a new instance of {@link MapBuilder} initialized with a {@link HashMap}.
   * @see java.util.HashMap
   * @see #MapBuilder(Map)
   */
  public static <KEY, VALUE> MapBuilder<KEY, VALUE> newHashMap() {
    return new MapBuilder<>(new HashMap<>());
  }

  /**
   * Factory method used to construct a new instance of {@link MapBuilder} initialized with
   * a {@link SortedMap} implementation.
   *
   * @param <KEY> {@link Class type} of the {@link Map} key.
   * @param <VALUE> {@link Class type} of the {@link Map} value.
   * @return a new instance of {@link MapBuilder} initialized with a {@link SortedMap}.
   * @see java.util.SortedMap
   * @see #MapBuilder(Map)
   */
  public static <KEY, VALUE> MapBuilder<KEY, VALUE> newSortedMap() {
    return new MapBuilder<>(new TreeMap<>());
  }

  /**
   * Constructs a new instance of {@link MapBuilder} initialized with the given {@link Map}.
   *
   * @param map {@link Map} to build (populate with {@link Map#put(Object, Object) put} operations;
   * must not be {@literal null}.
   * @throws IllegalArgumentException if {@link Map} is {@literal null}.
   * @see java.util.Map
   */
  protected MapBuilder(Map<KEY, VALUE> map) {
    Assert.notNull(map, "Map is required");
    this.map = map;
  }

  /**
   * Returns a reference to the {@link Map} being built.
   *
   * @return a reference to the {@link Map} being built.
   * @see java.util.Map
   */
  protected Map<KEY, VALUE> getMap() {
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
  public MapBuilder<KEY, VALUE> put(KEY key, VALUE value) {
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
  public MapBuilder<KEY, VALUE> putAll(Map<KEY, VALUE> map) {
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
  public MapBuilder<KEY, VALUE> putIfAbsent(KEY key, VALUE value) {
    getMap().putIfAbsent(key, value);
    return this;
  }

  /**
   * Builds the {@link Map}.
   *
   * @param <T> {@link Class sub-type} of the built {@link Map}.
   * @return the built {@link Map}.
   * @see java.util.Map
   * @see #getMap()
   */
  @SuppressWarnings("unchecked")
  public <T extends Map<KEY, VALUE>> T build() {
    return (T) getMap();
  }
}
