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

import java.util.Optional;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Supplier;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.Assert;

/**
 * The {@link CachingTemplate} class is an implementation of the Template Method Software Design Pattern
 * wrapping a {@link Cache} to provide additional capabilities such as look-aside caching, cache puts
 * on successful service method completion and cache eviction when a service method performs an update.
 *
 * This template additionally performs locking for both read and write data access operations
 * on the wrapped {@link Cache} to coordinate concurrent {@link Cache} operations in a multi-thread
 * environment.
 *
 * @author John Blum
 * @see java.util.concurrent.locks.Lock
 * @see java.util.concurrent.locks.ReadWriteLock
 * @see org.cp.elements.data.caching.Cache
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CachingTemplate<KEY extends Comparable<KEY>, VALUE> {

  private final Cache<KEY, VALUE> cache;

  private ReadWriteLock lock;

  /**
   * Factory method used to construct a new instance of the {@link CachingTemplate}
   * initialized with the given {@link Cache}.
   *
   * @param <KEY> {@link Class type} of the {@link Cache} entry key.
   * @param <VALUE> {@link Class type} of the {@link Cache} entry value.
   * @param cache {@link Cache} used to initialize the new {@link CachingTemplate}.
   * @return a new {@link CachingTemplate} initialized with the given {@link Cache}.
   * @throws IllegalArgumentException if the {@link Cache} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache
   * @see #CachingTemplate(Cache)
   */
  public static <KEY extends Comparable<KEY>, VALUE> CachingTemplate<KEY, VALUE> with(Cache<KEY, VALUE> cache) {
    return new CachingTemplate<>(cache);
  }

  /**
   * Constructs an instance of the {@link CachingTemplate} initialized with the given {@link Cache}.
   *
   * @param cache {@link Cache} used by this template in all {@link Cache} data access operations.
   * @throws IllegalArgumentException if {@link Cache} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache
   * @see #CachingTemplate(Cache, ReadWriteLock)
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
  public CachingTemplate(Cache<KEY, VALUE> cache, ReadWriteLock lock) {

    Assert.notNull(cache, "Cache is required");

    this.cache = cache;
    this.lock = lock != null ? lock : newReadWriteLock();
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
   * Returns the {@link ReadWriteLock lock} used by this template when performing {@link Cache} operations.
   *
   * @return the {@link ReadWriteLock lock} used by this template when performing {@link Cache} operations.
   * @see java.util.concurrent.locks.ReadWriteLock
   */
  protected ReadWriteLock getLock() {
    return this.lock;
  }

  /**
   * Constructs a new instance of the {@link ReadWriteLock}.
   *
   * @return a new instance of the {@link ReadWriteLock}.
   * @see java.util.concurrent.locks.ReadWriteLock
   */
  protected ReadWriteLock newReadWriteLock() {
    return new ReentrantReadWriteLock();
  }

  /**
   * Sets the {@link ReadWriteLock lock} used by this template to synchronize {@link Cache}
   * read and write data access operations.
   *
   * @param lock {@link ReadWriteLock} used to synchronize/coordinate {@link Cache}
   * read and write data access operations.
   * @return this {@link CachingTemplate}.
   * @see java.util.concurrent.locks.ReadWriteLock
   * @see #newReadWriteLock()
   */
  public CachingTemplate using(ReadWriteLock lock) {
    this.lock = lock != null ? lock : newReadWriteLock();
    return this;
  }

  /**
   * Clears the entire contents of the {@link Cache}.
   *
   * The {@link Cache} operation acquires a write lock.
   *
   * @param lock {@link ReadWriteLock} used to coordinate the {@link Cache} clear operation
   * with possibly other concurrent {@link Cache} operations.
   * @see java.util.concurrent.locks.ReadWriteLock#writeLock()
   * @see #getCache()
   */
  protected void clear(ReadWriteLock lock) {

    Lock writeLock = lock.writeLock();

    try {
      writeLock.lock();
      getCache().clear();
    }
    finally {
      writeLock.unlock();
    }
  }

  /**
   * Evicts the entry identified by the given {@link KEY key} from the {@link Cache}.
   *
   * This {@link Cache} operation acquires a write lock.
   *
   * @param lock {@link ReadWriteLock} used to coordinate the {@link Cache} eviction (remove) operation
   * with possibly other concurrent {@link Cache} operations.
   * @param key {@link KEY key} identifying the {@link Cache} entry to evict.
   * @see java.util.concurrent.locks.ReadWriteLock#writeLock()
   * @see #getCache()
   */
  protected void evict(ReadWriteLock lock, KEY key) {

    Lock writeLock = lock.writeLock();

    try {
      writeLock.lock();
      getCache().evict(key);
    }
    finally {
      writeLock.unlock();
    }
  }

  /**
   * Reads the value of the {@link Cache} entry identified by the given {@link KEY key}.
   *
   * This {@link Cache} operation acquires a read lock.
   *
   * @param lock {@link ReadWriteLock} used to coordinate the {@link Cache} read (get) operation
   * with possibly other concurrent {@link Cache} operations.
   * @param key {@link KEY key} identifying the {@link Cache} entry with the {@link VALUE value} to read.
   * @return the {@link VALUE} from the {@link Cache} entry identified by the given {@link KEY key},
   * or {@literal null} if the {@link Cache} entry with the given {@link KEY key} is not present.
   * @see java.util.concurrent.locks.ReadWriteLock#readLock()
   * @see #getCache()
   */
  protected VALUE read(ReadWriteLock lock, KEY key) {

    Lock readLock = lock.readLock();

    try {
      readLock.lock();

      return getCache().get(key);
    }
    finally {
      readLock.unlock();
    }
  }

  /**
   * Writes the given {@link KEY key} and {@link VALUE value} to a new entry stored in the {@link Cache}.
   *
   * This {@link Cache} operation acquires a write lock.
   *
   * @param lock {@link ReadWriteLock} used to coordinate the {@link Cache} write (put) operation
   * with possibly other concurrent {@link Cache} operations.
   * @param key {@link KEY key} used to map the {@link VALUE value} in a new entry stored in the {@link Cache}.
   * @param value {#link VALUE value} to write to the new entry stored in the {@link Cache}.
   * @return the given {@link VALUE}.
   * @see java.util.concurrent.locks.ReadWriteLock#writeLock()
   * @see #getCache()
   */
  protected VALUE write(ReadWriteLock lock, KEY key, VALUE value) {

    Lock writeLock = lock.writeLock();

    try {
      writeLock.lock();
      getCache().put(key, value);

      return value;
    }
    finally {
      writeLock.unlock();
    }
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

    ReadWriteLock lock = getLock();

    return (T) Optional.ofNullable(read(lock, key)).orElseGet(() ->
      Optional.ofNullable(cacheableOperation.get())
        .map(value -> write(lock, key, value))
        .orElse(null));
  }

  /**
   * This caching data access operation invokes the supplied {@link Supplier cacheable operation} and then clears
   * the contents of the entire {@link Cache}, but only if the {@link Supplier cacheable operation} completes
   * successfully.
   *
   * @param <T> {@link Class type} of the return {@link VALUE value}.
   * @param cacheableOperation {@link Supplier} used to compute or load a {@link VALUE value}.
   * @return the {@link VALUE result} of the {@link Supplier cacheable operation}.
   * @throws IllegalArgumentException if the {@link Supplier} is {@literal null}.
   * @see java.util.function.Supplier
   * @see #getCache()
   * @see #getLock()
   */
  @SuppressWarnings("unchecked")
  public <T extends VALUE> T withCacheClear(Supplier<VALUE> cacheableOperation) {

    Assert.notNull(cacheableOperation, "Supplier is required");

    VALUE returnValue = cacheableOperation.get();

    clear(getLock());

    return (T) returnValue;
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

    VALUE result = cacheableOperation.get();

    evict(getLock(), key);

    return (T) result;
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
      .map(value -> write(getLock(), key, value))
      .orElse(null);
  }
}
