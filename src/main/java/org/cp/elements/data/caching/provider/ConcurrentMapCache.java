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

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Supplier;

import org.cp.elements.data.caching.Cache;

/**
 * The {@link ConcurrentMapCache} class is an implementation of {@link Cache} backed by a {@link ConcurrentMap}.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @see java.util.concurrent.ConcurrentMap
 * @see org.cp.elements.data.caching.Cache
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConcurrentMapCache<KEY extends Comparable<KEY>, VALUE> implements Cache<KEY, VALUE> {

  private final ConcurrentMap<KEY, VALUE> map = new ConcurrentHashMap<>();

  @Override
  public boolean isEmpty() {
    return this.map.isEmpty();
  }

  @Override
  public boolean contains(KEY key) {
    return this.map.containsKey(key);
  }

  @Override
  public void from(Map<KEY, VALUE> map) {
    this.map.putAll(map);
  }

  @Override
  public VALUE get(KEY key) {
    return this.map.get(key);
  }

  @Override
  public Iterator<VALUE> iterator() {
    return Collections.unmodifiableCollection(this.map.values()).iterator();
  }

  @Override
  public Set<KEY> keys() {
    return Collections.unmodifiableSet(this.map.keySet());
  }

  @Override
  public VALUE lookAsideCache(KEY key, Supplier<VALUE> cacheLoader) {
    return this.map.computeIfAbsent(key, theKey -> cacheLoader.get());
  }

  @Override
  public void put(KEY key, VALUE value) {
    this.map.put(key, value);
  }

  @Override
  public void putIfAbsent(KEY key, VALUE value) {
    this.map.putIfAbsent(key, value);
  }

  @Override
  public void putIfPresent(KEY key, VALUE newValue) {
    this.map.computeIfPresent(key, (theKey, theValue) -> newValue);
  }

  @Override
  public void remove(KEY key) {
    this.map.remove(key);
  }

  @Override
  public int size() {
    return this.map.size();
  }

  @Override
  public Map<KEY, VALUE> toMap() {
    return Collections.unmodifiableMap(this.map);
  }
}
