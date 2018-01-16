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

package org.cp.elements.data.caching.provider;

import static org.cp.elements.util.MapUtils.nullSafeMap;

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicReference;

import org.cp.elements.data.caching.AbstractCache;
import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * The {@link ConcurrentMapCache} class is an implementation of {@link Cache} backed by a {@link ConcurrentMap}.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @see java.util.Map
 * @see java.util.concurrent.ConcurrentMap
 * @see java.util.concurrent.ConcurrentMap
 * @see org.cp.elements.data.caching.AbstractCache
 * @see org.cp.elements.data.caching.Cache
 * @see AbstractCache
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConcurrentMapCache<KEY extends Comparable<KEY>, VALUE> extends AbstractCache<KEY, VALUE> {

  private final ConcurrentMap<KEY, VALUE> map = new ConcurrentHashMap<>();

  /**
   * Determines whether this {@link Cache} contains any entries.
   *
   * @return a boolean value indicating whether this {@link Cache} contains any entries.
   * @see #size()
   */
  @NullSafe
  @Override
  public boolean isEmpty() {
    return this.map.isEmpty();
  }

  /**
   * Clears the entire contents (all entries) of this {@link Cache}.
   *
   * @see #evictAll(Iterable)
   * @see #keys()
   */
  @NullSafe
  @Override
  public void clear() {
    this.map.clear();
  }

  /**
   * Determines whether this {@link Cache} contains an entry mapped with the given {@link KEY key}.
   *
   * @param key {@link KEY key} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains an entry
   * mapped with the given {@link KEY key}.
   */
  @NullSafe
  @Override
  public boolean contains(KEY key) {
    return (key != null && this.map.containsKey(key));
  }

  /**
   * Removes the entry mapped to the given {@link KEY key} from this {@link Cache}.
   *
   * @param key {@link KEY key} identifying the entry to remove from this {@link Cache}.
   */
  @NullSafe
  @Override
  public void evict(KEY key) {

    if (key != null) {
      this.map.remove(key);
    }
  }

  /**
   * Caches all entries from given {@link Map} in this {@link Cache}.
   *
   * @param map {@link Map} containing the entries to cache.
   * @see #put(Comparable, Object)
   * @see java.util.Map
   */
  @NullSafe
  @Override
  public void from(Map<KEY, VALUE> map) {
    this.map.putAll(nullSafeMap(map));
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
    return key != null ? this.map.get(key) : null;
  }

  /**
   * Returns an {@link Iterator} iterating over the value in this {@link Cache}.
   *
   * @return an {@link Iterator} iterating over the value in this {@link Cache}.
   * @see java.util.Iterator
   */
  @NullSafe
  @Override
  public Iterator<VALUE> iterator() {
    return Collections.unmodifiableMap(this.map).values().iterator();
  }

  /**
   * Returns all the {@link KEY keys} in this {@link Cache}.
   *
   * @return a {@link Set} containing all of the {@link KEY keys} in this {@link Cache}.
   * @see java.util.Set
   */
  @NullSafe
  @Override
  public Set<KEY> keys() {
    return Collections.unmodifiableMap(this.map).keySet();
  }

  /**
   * Puts the given {@link VALUE value} mapped to the given {@link KEY key) into this {@link Cache}.
   *
   * @param key {@link KEY} used to map the {@link VALUE value}; must not be {@literal null}.
   * @param value {@link VALUE} put into this {@link Cache} mapped to the given {@link KEY key}.
   * @throws IllegalArgumentException if the {@link KEY key} or {@link VALUE value} is {@literal null}.
   * @see #get(Object)
   */
  @Override
  public void put(KEY key, VALUE value) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(value, "Value is required");

    this.map.put(key, value);
  }

  /**
   * Puts the given {@link KEY key} and {@link VALUE value} in this {@link Cache}
   * iff an entry with given {@link KEY key} does not already exists.
   *
   * @param key {@link KEY} used to map the {@link VALUE value} if not already present; must not be {@literal null}.
   * @param value {@link VALUE} to put into this {@link Cache} mapped to the given {@link KEY key}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws IllegalArgumentException if the {@link KEY key} or {@link VALUE value} is {@literal null}.
   * @see #contains(Comparable)
   * @see #put(Comparable, Object)
   * @see #putIfPresent(Comparable, Object)
   */
  @Override
  public VALUE putIfAbsent(KEY key, VALUE value) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(value, "Value is required");

    return this.map.putIfAbsent(key, value);
  }

  /**
   * Puts the {@link VALUE value} in this {@link Cache} mapped to the given {@link KEY key} iff an entry
   * with the given {@link KEY key} already exists in this {@link Cache}.
   *
   * @param key {@link KEY key} used to map the {@link VALUE new value} in this {@link Cache}.
   * @param newValue {@link VALUE new value} replacing the existing value mapped to the given {@link KEY key}
   * in this {@link Cache}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws IllegalArgumentException if the {@link VALUE value} is {@literal null}.
   * @see #contains(Comparable)
   * @see #put(Comparable, Object)
   * @see #putIfAbsent(Comparable, Object)
   */
  @Override
  public VALUE putIfPresent(KEY key, VALUE newValue) {

    Assert.notNull(newValue, "Value is required");

    if (key != null) {

      AtomicReference<VALUE> oldValueRef = new AtomicReference<>(null);

      return newValue.equals(this.map.computeIfPresent(key, (theKey, oldValue) -> {
        oldValueRef.set(oldValue);
        return newValue;
      })) ? oldValueRef.get() : null;
    }

    return null;
  }

  /**
   * Determines the number of entries contained in this {@link Cache}.
   *
   * @return an integer value with the number of entries contained in this {@link Cache}.
   * @see #isEmpty()
   */
  @NullSafe
  @Override
  public int size() {
    return this.map.size();
  }

  /**
   * Returns this {@link Cache} as instance of {@link Map}.
   *
   * @return a {@link Map} containing all the entries in this {@link Cache}.
   * @see java.util.Map
   */
  @NullSafe
  @Override
  public Map<KEY, VALUE> toMap() {
    return Collections.unmodifiableMap(this.map);
  }
}
