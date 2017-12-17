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

import java.util.Optional;
import java.util.function.Supplier;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.Assert;

/**
 * The CachingTemplate class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CachingTemplate<KEY extends Comparable<KEY>, VALUE> {

  private final Cache<KEY, VALUE> cache;

  private Object lock;

  /**
   * Constructs an instance of the {@link CachingTemplate} initialized with the given {@link Cache}.
   *
   * @param cache {@link Cache} used by this template in all {@link Cache} data access operations.
   * @throws IllegalArgumentException if {@link Cache} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache
   * @see #CachingTemplate(Cache, Object)
   */
  public CachingTemplate(Cache<KEY, VALUE> cache) {
    this(cache, null);
  }

  /**
   * Constructs a new instance of {@link CachingTemplate} initialized with the given {@link Cache} used in all
   * caching operations performed by this template along with a lock to synchronize the cache data access operations.
   *
   * @param cache {@link Cache} used by this template in all {@link Cache} data access operations.
   * @param lock {@link Object} used by this template to synchronized/coordinate
   * all {@link Cache} data access operations.
   * @throws IllegalArgumentException if {@link Cache} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache
   */
  public CachingTemplate(Cache<KEY, VALUE> cache, Object lock) {

    Assert.notNull(cache, "Cache is required");

    this.cache = cache;
    this.lock = lock != null ? lock : this;
  }

  /**
   * Returns a reference to the {@link Cache} used by this template.
   *
   * @return a reference to the {@link Cache} used by this template.
   * @see org.cp.elements.data.caching.Cache
   */
  protected Cache<KEY, VALUE> getCache() {
    return this.cache;
  }

  /**
   * Returns the {@link Object lock} used by this template when performing cache operations.
   *
   * @return the {@link Object lock} used by this template when performing cache operations.
   */
  protected Object getLock() {
    return this.lock;
  }

  /**
   * Sets the {@link Object lock} used by this template to synchronize {@link Cache} data access operations.
   *
   * @param lock {@link Object} used to synchronize/coordinate {@link Cache} data access operations.
   * @return this {@link CachingTemplate}.
   */
  public CachingTemplate withLock(Object lock) {
    this.lock = lock != null ? lock : this;
    return this;
  }

  /**
   * Implementation of the look-aside cache pattern.
   *
   * This caching data access operation first attempts to locate an entry in the {@link Cache}
   * with the given {@link KEY key}, returning the {@link VALUE value} of the entry if present.
   *
   * If an entry with the given {@link KEY key} is not present in the {@link Cache} then the supplied
   * {@link Supplier cacheable operation} is invoked to compute or load a {@link VALUE value}
   * and put into the {@link Cache} as an entry mapped by the given {@link KEY key}; this operation
   * completes by returning the {@link VALUE result} of the {@link Supplier cacheable operation}.
   *
   * @param <T> {@link Class type} of the return {@link VALUE value}.
   * @param key {@link KEY key} used to identify the {@link Cache} entry containing the {@link VALUE} to lookup.
   * @param cacheableOperation {@link Supplier} used to compute or load a {@link VALUE value}
   * for given {@link KEY key} if the cacheable data access operation initially results in a cache miss.
   * @return the cached {@link VALUE value} for the given {@link KEY key} in the {@link Cache} if present,
   * or returns the {@link VALUE value} supplied by invoking the {@link Supplier cacheable operation}.
   * @throws IllegalArgumentException if either the {@link KEY key} or the {@link Supplier} are {@literal null}.
   * @see java.util.function.Supplier
   * @see #getCache()
   * @see #getLock()
   */
  @SuppressWarnings("unchecked")
  public <T extends VALUE> T withCaching(KEY key, Supplier<VALUE> cacheableOperation) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(cacheableOperation, "Supplier is required");

    synchronized (getLock()) {

      Cache<KEY, VALUE> cache = getCache();

      return (T) Optional.ofNullable(cache.get(key)).orElseGet(() -> {

        VALUE value = cacheableOperation.get();

        getCache().put(key, value);

        return value;
      });
    }
  }

  /**
   * This caching data access operation invokes the supplied {@link Supplier cacheable operation} and then caches
   * the {@link VALUE result} before returning the computed {@link VALUE}.
   *
   * The {@link VALUE result} of the {@link Supplier cacheable operation} is only cached if the {@link VALUE result}
   * is not {@literal null}.
   *
   * @param <T> {@link Class type} of the return {@link VALUE value}.
   * @param key {@link KEY key} mapping the {@link VALUE value} returned by the {@link Supplier cacheable operation}
   * as an entry stored in the {@link Cache}.
   * @param cacheableOperation {@link Supplier} used to compute or load a {@link VALUE value}
   * mapped to the given {@link KEY key} and put as an entry in the {@link Cache}.
   * @return the {@link VALUE result} of the {@link Supplier cacheable operation}.
   * @throws IllegalArgumentException if either the {@link KEY key} or the {@link Supplier} are {@literal null}.
   * @see java.util.function.Supplier
   * @see #getCache()
   * @see #getLock()
   */
  @SuppressWarnings("unchecked")
  public <T extends VALUE> T withCachePut(KEY key, Supplier<VALUE> cacheableOperation) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(cacheableOperation, "Supplier is required");

    return (T) Optional.ofNullable(cacheableOperation.get())
      .map(value -> {

        synchronized (getLock()) {
          getCache().put(key, value);
        }

        return value;
      })
      .orElse(null);
  }

  /**
   * This caching data access operation invokes the supplied {@link Supplier cacheable operation} and then evicts
   * the entry in the {@link Cache} identified with the given {@link KEY key} if present, but only if
   * the {@link Supplier cacheable operation} completes successfully.
   *
   * @param <T> {@link Class type} of the return {@link VALUE value}.
   * @param key {@link KEY key} identifying the entry in the {@link Cache} to evict.
   * @param cacheableOperation {@link Supplier} used to compute or load a {@link VALUE value}.
   * @return the {@link VALUE result} of the {@link Supplier cacheable operation}.
   * @throws IllegalArgumentException if the {@link Supplier} is {@literal null}.
   * @see java.util.function.Supplier
   * @see #getCache()
   * @see #getLock()
   */
  @SuppressWarnings("unchecked")
  public <T extends VALUE> T withCacheEvict(KEY key, Supplier<VALUE> cacheableOperation) {

    Assert.notNull(cacheableOperation, "Supplier is required");

    T result = (T) cacheableOperation.get();

    synchronized (getLock()) {
      getCache().remove(key);
    }

    return result;
  }
}
