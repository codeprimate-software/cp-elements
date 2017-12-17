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

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.Assert;

/**
 * The {@link MapToCacheAdapter} class is a {@link Cache} implementation backed by a {@link Map}.
 *
 * This class adapts the {@link Map} interface into an instance of {@link Cache}.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @see java.util.Map
 * @see org.cp.elements.data.caching.Cache
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class MapToCacheAdapter<KEY extends Comparable<KEY>, VALUE> implements Cache<KEY, VALUE> {

  private final Map<KEY, VALUE> map;

  public MapToCacheAdapter(Map<KEY, VALUE> map) {

    Assert.notNull(map, "Map is required");

    this.map = map;
  }

  protected Map<KEY, VALUE> getMap() {
    return this.map;
  }

  @Override
  public boolean contains(KEY key) {
    return getMap().containsKey(key);
  }

  @Override
  public VALUE get(KEY key) {
    return getMap().get(key);
  }

  @Override
  public Set<KEY> keys() {
    return Collections.unmodifiableMap(getMap()).keySet();
  }

  @Override
  public void put(KEY key, VALUE value) {
    getMap().put(key, value);
  }

  @Override
  public void remove(KEY key) {
    getMap().remove(key);
  }

  @Override
  public int size() {
    return getMap().size();
  }

  @Override
  public Iterator<VALUE> iterator() {
    return Collections.unmodifiableMap(getMap()).values().iterator();
  }
}
