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

import static org.cp.elements.util.MapUtils.newMapEntry;

import java.util.AbstractMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.Assert;

/**
 * The {@link CacheToMapAdapter} class is a {@link Map} implementation backed by a {@link Cache}.
 *
 * This class adapts the {@link Cache} interface into an instance of a {@link Map}.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @see java.util.AbstractMap
 * @see org.cp.elements.data.caching.Cache
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CacheToMapAdapter<KEY extends Comparable<KEY>, VALUE> extends AbstractMap<KEY, VALUE> {

  private final Cache<KEY, VALUE> cache;

  public CacheToMapAdapter(Cache<KEY, VALUE> cache) {

    Assert.notNull(cache, "Cache is required");

    this.cache = cache;
  }

  protected Cache<KEY, VALUE> getCache() {
    return this.cache;
  }

  @Override
  public Set<Entry<KEY, VALUE>> entrySet() {

    Cache<KEY, VALUE> delegate = getCache();

    return delegate.keys().stream()
      .map(key -> newMapEntry(key, delegate.get(key)))
      .collect(Collectors.toSet());
  }

  @Override
  public VALUE put(KEY key, VALUE value) {

    Cache<KEY, VALUE> delegate = getCache();

    VALUE oldValue = delegate.contains(key) ? delegate.get(key) : null;

    delegate.put(key, value);

    return oldValue;
  }
}
