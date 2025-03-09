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
package org.cp.elements.data.caching.support;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;

import java.util.Map;
import java.util.function.Function;

import org.cp.elements.data.caching.AbstractCache;
import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract base class implementing the {@link Cache} interface and supporting only immutable {@link Cache} operations,
 * effectively making this implementation a Read-only {@link Cache}.
 *
 * @author John Blum
 * @param <KEY> {@link Class type} of the {@link Cache} key.
 * @param <VALUE> {@link Class type} of the {@link Cache} value.
 * @see org.cp.elements.data.caching.AbstractCache
 * @see java.lang.Comparable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ReadOnlyCache<KEY extends Comparable<KEY>, VALUE> extends AbstractCache<KEY, VALUE> {

  @Override
  public final void from(Map<KEY, VALUE> map) {
    throw newUnsupportedOperationException("From Map is not supported");
  }

  @NullSafe
  @Override
  public @Nullable VALUE get(@NotNull KEY key) {
    return null;
  }

  @Override
  public final VALUE getAndEvict(KEY key) {
    throw newUnsupportedOperationException("Get and Evict is not supported");
  }

  @Override
  public final VALUE getAndEvict(KEY key, VALUE expectedValue) {
    throw newUnsupportedOperationException("Get and Evict is not supported");
  }

  @Override
  public final VALUE getAndPut(KEY key, VALUE newValue) {
    throw newUnsupportedOperationException("Get and Put is not supported");
  }

  @Override
  public final VALUE getAndReplace(KEY key, VALUE newValue) {
    throw newUnsupportedOperationException("Get and Replace is not supported");
  }

  @Override
  public final VALUE getAndReplace(KEY key, VALUE expectedValue, VALUE newValue) {
    throw newUnsupportedOperationException("Get and Replace is not supported");
  }

  @Override
  public @Nullable Cache.Entry<KEY, VALUE> getEntry(KEY key) {

    Cache.Entry<KEY, VALUE> existingCacheEntry = doGetEntry(key);

    Function<Cache.Entry<KEY, VALUE>, Cache.Entry<KEY, VALUE>> readOnlyCacheEntryFunction = cacheEntry ->
      new Cache.Entry<>() {

        @Override
        public @NotNull KEY getKey() {
          return cacheEntry.getKey();
        }

        @Override
        public @NotNull Cache<KEY, VALUE> getSource() {
          return cacheEntry.getSource();
        }

        @Override
        public @Nullable VALUE getValue() {
          return cacheEntry.getValue();
        }

        @Override
        public void setValue(VALUE value) {
          throw newUnsupportedOperationException("Setting the value of Cache.Entry(%s) is not supported", getKey());
        }

        @Override
        public @NotNull Cache.Entry<KEY, VALUE> materialize() {
          return cacheEntry.materialize();
        }
      };

    return existingCacheEntry != null ? readOnlyCacheEntryFunction.apply(existingCacheEntry) : null;
  }

  /**
   * Gets the {@link Cache.Entry} for the given {@link KEY key}.
   *
   * @param key {@link KEY key} referencing the {@link Cache.Entry} to get.
   * @return the {@link Cache.Entry} for the given {@link KEY key}.
   * @see Cache.Entry
   */
  @Nullable
  Cache.Entry<KEY, VALUE> doGetEntry(KEY key) {
    return super.getEntry(key);
  }

  @Override
  public final void put(KEY key, VALUE value) {
    throw newUnsupportedOperationException("Put is not supported");
  }

  @Override
  public final void put(Cache.Entry<KEY, VALUE> cacheEntry) {
    throw newUnsupportedOperationException("Put is not supported");
  }

  @Override
  public final void put(Identifiable<KEY> entity) {
    throw newUnsupportedOperationException("Put is not supported");
  }

  @Override
  public final VALUE putIfAbsent(KEY key, VALUE value) {
    throw newUnsupportedOperationException("Put is not supported");
  }

  @Override
  public final VALUE putIfAbsent(Identifiable<KEY> entity) {
    throw newUnsupportedOperationException("Put is not supported");
  }

  @Override
  public final VALUE putIfPresent(KEY key, VALUE newValue) {
    throw newUnsupportedOperationException("Put is not supported");
  }

  @Override
  public final VALUE putIfPresent(Identifiable<KEY> newEntity) {
    throw newUnsupportedOperationException("Put is not supported");
  }
}
