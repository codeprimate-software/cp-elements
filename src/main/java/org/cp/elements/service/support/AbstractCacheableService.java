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
package org.cp.elements.service.support;

import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.data.caching.provider.ConcurrentMapCache;
import org.cp.elements.data.caching.support.CachingTemplate;

/**
 * Abstract base class extended by application service classes in order to keep track of the cacheable state
 * of the service's operations.
 *
 * @author John Blum
 * @param <KEY> {@link Class type} of the keys used by the {@link Cache} served by this service.
 * @param <VALUE> {@link Class type} of the values stored by the {@link Cache} served by this service.
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractCacheableService<KEY extends Comparable<KEY>, VALUE> {

  private final AtomicBoolean cacheMiss = new AtomicBoolean(false);

  private final Cache<KEY, VALUE> cache = newCache();

  /**
   * Returns an {@link Optional} reference to the configured {@link Cache}.
   *
   * @return an {@link Optional} reference to the configured {@link Cache}.
   * @see org.cp.elements.data.caching.Cache
   * @see java.util.Optional
   */
  protected Optional<Cache<KEY, VALUE>> getCache() {
    return Optional.ofNullable(this.cache);
  }

  /**
   * Constructs a new instance of the {@link Cache} interface.
   *
   * The {@link Cache} is used to provide caching services to this application service component.
   *
   * @return a new instance of the {@link Cache} interface.
   * @see org.cp.elements.data.caching.Cache
   */
  protected Cache<KEY, VALUE> newCache() {
    return new ConcurrentMapCache<>();
  }

  /**
   * Determines whether this application service component has been configured with caching enabled.
   *
   * The default implementation returns {@literal true} if a {@link Cache} instance is present.
   *
   * @return a boolean value indicating whether this application service component
   * has been configured with caching enabled.
   * @see #getCache()
   */
  protected boolean isCachingEnabled() {
    return getCache().isPresent();
  }

  /**
   * Determines whether the cacheable service operation resulted in a cache hit.
   *
   * @return a boolean value indicating whether the cacehable service operation
   * resulted in a cache hit.
   * @see #isCacheMiss()
   */
  public boolean isCacheHit() {
    return !isCacheMiss();
  }

  /**
   * Determines whether the cacheable service operation resulted in a cache miss.
   *
   * @return a boolean value indicating whether the cacehable service operation
   * resulted in a cache miss.
   * @see #isCacheHit()
   */
  public boolean isCacheMiss() {
    return this.cacheMiss.getAndSet(false);
  }

  /**
   * Sets the state of the cacheable service as a cache miss for the current operation.
   *
   * @return a boolean value indicating whether the current cacheable service operation
   * was the first cache miss in the transaction.
   */
  protected boolean setCacheMiss() {
    return this.cacheMiss.compareAndSet(false, true);
  }

  /**
   * Enables an application service method to optionally apply and use caching to carry out its function.
   *
   * @param key {@link KEY key} used to look up an existing {@link VALUE value} in the {@link Cache}.
   * @param cacheLoader {@link Supplier} used to compute/execute the application serivce method function
   * if the {@link Cache} does not contain an already computed {@link VALUE value} or caching is not enabled.
   * @return the cached or computed {@link VALUE value}.
   * @see org.cp.elements.data.caching.support.CachingTemplate#withCaching(Comparable, Supplier)
   * @see org.cp.elements.data.caching.Cache
   * @see java.util.function.Supplier
   * @see #isCachingEnabled()
   * @see #getCache()
   */
  protected VALUE withCaching(KEY key, Supplier<VALUE> cacheLoader) {

    return getCache()
      .map(CachingTemplate::with)
      .<VALUE>map(template -> template.withCaching(key, () -> {

        setCacheMiss();

        return cacheLoader.get();
      }))
      .orElseGet(cacheLoader);
  }
}
