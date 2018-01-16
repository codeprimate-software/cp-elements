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

package org.cp.elements.data.caching.support;

import static org.cp.elements.util.MapUtils.nullSafeMap;

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import org.cp.elements.data.caching.AbstractCache;
import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * The {@link MapToCacheAdapter} class is a {@link Cache} implementation backed by a {@link Map}.
 *
 * This class adapts the {@link Map} interface into an instance of {@link Cache}.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @see java.util.Map
 * @see org.cp.elements.data.caching.AbstractCache
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class MapToCacheAdapter<KEY extends Comparable<KEY>, VALUE> extends AbstractCache<KEY, VALUE> {

  private final Map<KEY, VALUE> map;

  /**
   * Factory method used to construct an instance of the {@link MapToCacheAdapter} initialized with
   * the given {@link Map}.
   *
   * @param <KEY> {@link Class type} of the keys used by this {@link Cache}.
   * @param <VALUE> {@link Class type} of the values stored by this {@link Cache}.
   * @param map {@link Map} adapted as an instance of a {@link Cache} and used to back
   * the {@link MapToCacheAdapter} instance.
   * @return a new instance of the {@link MapToCacheAdapter} initialized and backed by the given {@link Map}.
   * @throws IllegalArgumentException if the {@link Map} is {@literal null}.
   * @see java.util.Map
   * @see #MapToCacheAdapter(Map)
   */
  public static <KEY extends Comparable<KEY>, VALUE> MapToCacheAdapter<KEY, VALUE> of(Map<KEY, VALUE> map) {
    return new MapToCacheAdapter<>(map);
  }

  /**
   * Constructs a new instance of {@link MapToCacheAdapter} initialized with the given {@link Map}.
   *
   * @param map {@link Map} used to back this {@link Cache}.
   * @throws IllegalArgumentException if the {@link Map} is {@literal null}.
   * @see java.util.Map
   */
  public MapToCacheAdapter(Map<KEY, VALUE> map) {

    Assert.notNull(map, "Map is required");

    this.map = map;
  }

  /**
   * Returns a reference to the {@link Map} backing this {@link Cache}.
   *
   * The provided {@link Map} is effectively adapter into an instance of {@link Cache}.
   *
   * @return a reference to the {@link Map} backing this {@link Cache}.
   * @see java.util.Map
   */
  protected Map<KEY, VALUE> getMap() {
    return this.map;
  }

  /**
   * Determines whether this {@link Cache} contains any entries.
   *
   * @return a boolean value indicating whether this {@link Cache} contains any entries.
   * @see #size()
   */
  @Override
  public boolean isEmpty() {
    return getMap().isEmpty();
  }

  /**
   * Clears the entire contents (all entries) of this {@link Cache}.
   *
   * @see #evictAll(Iterable)
   * @see #keys()
   */
  @Override
  public void clear() {
    getMap().clear();
  }

  /**
   * Determines whether this {@link Cache} contains an entry mapped with the given {@link KEY key}.
   *
   * @param key {@link KEY key} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains an entry
   * mapped with the given {@link KEY key}.
   */
  @Override
  public boolean contains(KEY key) {
    return (key != null && getMap().containsKey(key));
  }

  /**
   * Removes the entry mapped to the given {@link KEY key} from this {@link Cache}.
   *
   * @param key {@link KEY key} identifying the entry to remove from this {@link Cache}.
   */
  @Override
  public void evict(KEY key) {

    if (key != null) {
      getMap().remove(key);
    }
  }

  /**
   * Caches all entries from given {@link Map} in this {@link Cache}.
   *
   * @param map {@link Map} containing the entries to cache.
   * @see #put(Comparable, Object)
   * @see java.util.Map
   */
  @Override
  public void from(Map<KEY, VALUE> map) {
    getMap().putAll(nullSafeMap(map));
  }

  /**
   * Gets the {@link VALUE value} mapped to the given {@link KEY} in this {@link Cache}.
   *
   * Returns {@literal null} if the {@link VALUE value} for the given {@link KEY key} is {@literal null},
   * or this {@link Cache} does not contain an entry with the given {@link KEY key}.
   *
   * @param key {@link KEY key} used to lookup the desired {@link VALUE value}.
   * @return the {@link VALUE value} mapped to the given {@link KEY key}.
   * @see #put(Comparable, Object)
   */
  @Override
  public VALUE get(KEY key) {
    return key != null ? getMap().get(key) : null;
  }

  /**
   * Returns an {@link Iterator} iterating over the value in this {@link Cache}.
   *
   * @return an {@link Iterator} iterating over the value in this {@link Cache}.
   * @see java.util.Iterator
   */
  @Override
  public Iterator<VALUE> iterator() {
    return toMap().values().iterator();
  }

  /**
   * Returns all the {@link KEY keys} in this {@link Cache}.
   *
   * @return a {@link Set} containing all of the {@link KEY keys} in this {@link Cache}.
   * @see java.util.Set
   */
  @Override
  public Set<KEY> keys() {
    return toMap().keySet();
  }

  /**
   * Puts the given {@link VALUE value} mapped to the given {@link KEY key) into this {@link Cache}.
   *
   * @param key {@link KEY} used to map the {@link VALUE value}; must not be {@literal null}.
   * @param value {@link VALUE} put into this {@link Cache} mapped to the given {@link KEY key}.
   * @throws IllegalArgumentException if {@link KEY key} is {@literal null}.
   * @see #get(Object)
   */
  @Override
  public void put(KEY key, VALUE value) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(value, "Value is required");

    getMap().put(key, value);
  }

  /**
   * Puts the given {@link KEY key} and {@link VALUE value} in this {@link Cache}
   * iff an entry with given {@link KEY key} does not already exists.
   *
   * @param key {@link KEY} used to map the {@link VALUE value} if not already present; must not be {@literal null}.
   * @param value {@link VALUE} to put into this {@link Cache} mapped to the given {@link KEY key}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws IllegalArgumentException if {@link KEY key} is {@literal null}.
   * @see #contains(Comparable)
   * @see #put(Comparable, Object)
   * @see #putIfPresent(Comparable, Object)
   */
  @Override
  public VALUE putIfAbsent(KEY key, VALUE value) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(value, "Value is required");

    return getMap().putIfAbsent(key, value);
  }

  /**
   * Puts the {@link VALUE value} in this {@link Cache} mapped to the given {@link KEY key} iff an entry
   * with the given {@link KEY key} already exists in this {@link Cache}.
   *
   * @param key {@link KEY key} used to map the {@link VALUE new value} in this {@link Cache}.
   * @param newValue {@link VALUE new value} replacing the existing value mapped to the given {@link KEY key}
   * in this {@link Cache}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws IllegalArgumentException if {@link KEY key} is {@literal null}.
   * @see #contains(Comparable)
   * @see #put(Comparable, Object)
   * @see #putIfAbsent(Comparable, Object)
   */
  @Override
  public VALUE putIfPresent(KEY key, VALUE newValue) {

    Assert.notNull(newValue, "Value is required");

    AtomicReference<VALUE> oldValueRef = new AtomicReference<>(null);

    return newValue.equals(this.map.computeIfPresent(key, (theKey, oldValue) -> {
      oldValueRef.set(oldValue);
      return newValue;
    })) ? oldValueRef.get() : null;
  }

  /**
   * Determines the number of entries contained in this {@link Cache}.
   *
   * @return an integer value with the number of entries contained in this {@link Cache}.
   * @see #isEmpty()
   */
  @Override
  public int size() {
    return getMap().size();
  }

  /**
   * Returns this {@link Cache} as instance of {@link Map}.
   *
   * @return a {@link Map} containing all the entries in this {@link Cache}.
   * @see java.util.Map
   */
  @Override
  @NullSafe
  public Map<KEY, VALUE> toMap() {
    return Collections.unmodifiableMap(nullSafeMap(getMap()));
  }
}
