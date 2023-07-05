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
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * The {@link CachingTemplate} class is an implementation of the {@literal Template Method Software Design Pattern}
 * wrapping and delegating to a {@link Cache} in order to provide additional capabilities such as
 * {@literal Look-Aside Caching} ({@literal Cache-Aside Caching}), cache puts on successful service method completion
 * and cache eviction when a service method performs an update.
 * <p>
 * This Template additionally performs locking for both read and write data access operations when using the wrapped
 * {@link Cache} in order to coordinate concurrent {@link Cache} operations in a multi-Thread environment.
 *
 * @author John Blum
 * @param <KEY> {@link Class type} of the {@link Cache} key.
 * @param <VALUE> {@link Class type} of the {@link Cache} value.
 * @see java.util.concurrent.locks.Lock
 * @see java.util.concurrent.locks.ReadWriteLock
 * @see java.util.concurrent.locks.ReentrantReadWriteLock
 * @see org.cp.elements.data.caching.Cache
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CachingTemplate<KEY extends Comparable<KEY>, VALUE> {

  /**
   * Factory method used to construct a new {@link CachingTemplate} initialized with the given, required {@link Cache}.
   *
   * @param <KEY> {@link Class type} of the {@link Cache} key.
   * @param <VALUE> {@link Class type} of the {@link Cache} value.
   * @param cache {@link Cache} used to initialize the new {@link CachingTemplate};
   * must not be {@literal null}.
   * @return a new {@link CachingTemplate} initialized with the given, required {@link Cache}.
   * @throws IllegalArgumentException if the {@link Cache} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache
   * @see #CachingTemplate(Cache)
   */
  public static @NotNull <KEY extends Comparable<KEY>, VALUE> CachingTemplate<KEY, VALUE> with(
      @NotNull Cache<KEY, VALUE> cache) {

    return new CachingTemplate<>(cache);
  }

  private final Cache<KEY, VALUE> cache;

  private ReadWriteLock lock;

  /**
   * Constructs a new {@link CachingTemplate} initialized with the given, required {@link Cache}.
   *
   * @param cache {@link Cache} used by this template for all {@link Cache} data access operations;
   * must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link Cache} is {@literal null}.
   * @see #CachingTemplate(Cache, ReadWriteLock)
   * @see org.cp.elements.data.caching.Cache
   */
  public CachingTemplate(@NotNull Cache<KEY, VALUE> cache) {
    this(cache, null);
  }

  /**
   * Constructs a new {@link CachingTemplate} initialized with the given, required {@link Cache}
   * used in all caching operations performed by this template along with a {@link Lock} to synchronize on
   * the {@link Cache} data access operations when performed concurrently in a multi-Threaded environment.
   *
   * @param cache {@link Cache} used by this template for all {@link Cache} data access operations;
   * must not be {@literal null}.
   * @param lock {@link ReadWriteLock} used by this template to synchronize all {@link Cache} data access operations.
   * @throws IllegalArgumentException if the {@link Cache} is {@literal null}.
   * @see #resolveReadWriteLock(ReadWriteLock)
   * @see org.cp.elements.data.caching.Cache
   * @see #newReadWriteLock()
   */
  public CachingTemplate(@NotNull Cache<KEY, VALUE> cache, @Nullable ReadWriteLock lock) {

    this.cache = ObjectUtils.requireObject(cache, "Cache is required");
    this.lock = resolveReadWriteLock(lock);
  }

  private @NotNull ReadWriteLock resolveReadWriteLock(@Nullable ReadWriteLock lock) {
    return lock != null ? lock : newReadWriteLock();
  }

  /**
   * Returns a reference to the configured {@link Cache} used by this template.
   *
   * @return a reference to the configured {@link Cache} used by this template.
   * @see org.cp.elements.data.caching.Cache
   */
  protected @NotNull Cache<KEY, VALUE> getCache() {
    return this.cache;
  }

  /**
   * Returns the {@link ReadWriteLock} used by this template to synchronize all {@link Cache} data access operations
   * when performed concurrently in a multi-Threaded environment.
   *
   * @return the {@link ReadWriteLock lock} used by this template to synchronize all {@link Cache} data access
   * operations when performed concurrently in a multi-Threaded environment.
   * @see java.util.concurrent.locks.ReadWriteLock
   */
  protected @NotNull ReadWriteLock getLock() {
    return this.lock;
  }

  /**
   * Constructs a new {@link ReadWriteLock}.
   * <p>
   * The {@link Lock} is reentrant.
   *
   * @return a new {@link ReadWriteLock}.
   * @see java.util.concurrent.locks.ReentrantReadWriteLock
   * @see java.util.concurrent.locks.ReadWriteLock
   */
  protected @NotNull ReadWriteLock newReadWriteLock() {
    return new ReentrantReadWriteLock();
  }

  /**
   * Builder method used to set the {@link ReadWriteLock} used by this template to synchronize {@link Cache}
   * read and write data access operations.
   *
   * @param lock {@link ReadWriteLock} used to synchronize {@link Cache} read and write data access operations.
   * @return this {@link CachingTemplate}.
   * @see java.util.concurrent.locks.ReadWriteLock
   * @see #resolveReadWriteLock(ReadWriteLock)
   * @see #newReadWriteLock()
   */
  public @NotNull CachingTemplate<KEY, VALUE> using(@Nullable ReadWriteLock lock) {
    this.lock = resolveReadWriteLock(lock);
    return this;
  }

  private <T> T doWithReadLock(@NotNull ReadWriteLock lock, @NotNull Supplier<T> cacheOperation) {

    Lock readLock = lock.readLock();

    try {
      readLock.lock();
      return cacheOperation.get();
    }
    finally {
      readLock.unlock();
    }
  }

  // @see doWithWriteLock(:ReadWriteLock, :Supplier<?>)
  private void doWithWriteLock(@NotNull ReadWriteLock lock, @NotNull Runnable cacheOperation) {

    doWithWriteLock(lock, () -> {
      cacheOperation.run();
      return null;
    });
  }

  private <T> T doWithWriteLock(@NotNull ReadWriteLock lock, @NotNull Supplier<T> cacheOperation) {

    Lock writeLock = lock.writeLock();

    try {
      writeLock.lock();
      return cacheOperation.get();
    }
    finally {
      writeLock.unlock();
    }
  }

  /**
   * Clears the entire contents of the {@link Cache}.
   * <p>
   * The {@link Cache} operation acquires a {@literal write lock}.
   *
   * @param lock {@link ReadWriteLock} used to synchronize the {@link Cache} clear operation.
   * @see java.util.concurrent.locks.ReadWriteLock#writeLock()
   * @see #doWithWriteLock(ReadWriteLock, Runnable)
   * @see #getCache()
   */
  protected void clear(@NotNull ReadWriteLock lock) {
    doWithWriteLock(lock, getCache()::clear);
  }

  /**
   * Evicts the {@link Cache.Entry} mapped to the given {@link KEY key} from the {@link Cache}.
   * <p>
   * This {@link Cache} operation acquires a {@literal write lock}.
   *
   * @param lock {@link ReadWriteLock} used to synchronize the {@link Cache} eviction (remove) operation.
   * @param key {@link KEY key} identifying the {@link Cache.Entry} to evict (remove).
   * @see java.util.concurrent.locks.ReadWriteLock#writeLock()
   * @see #doWithWriteLock(ReadWriteLock, Runnable)
   * @see #getCache()
   */
  protected void evict(@NotNull ReadWriteLock lock, @NotNull KEY key) {
    doWithWriteLock(lock, () -> getCache().evict(key));
  }

  /**
   * Reads the {@link VALUE value} stored in this {@link Cache} mapped to the given {@link KEY}.
   * <p>
   * This {@link Cache} operation acquires a {@literal read lock}.
   *
   * @param lock {@link ReadWriteLock} used to synchronize the {@link Cache} read (get) operation.
   * @param key {@link KEY key} identifying the {@link Cache.Entry} containing the {@link VALUE value} to read.
   * @return the {@link VALUE} stored in the {@link Cache} mapped to the given {@link KEY key},
   * or {@literal null} if no {@link Cache.Entry} having the given {@link KEY key} is present.
   * @see java.util.concurrent.locks.ReadWriteLock#readLock()
   * @see #doWithReadLock(ReadWriteLock, Supplier)
   * @see #getCache()
   */
  protected @Nullable VALUE read(@NotNull ReadWriteLock lock, @NotNull KEY key) {
    return doWithReadLock(lock, () -> getCache().get(key));
  }

  /**
   * Writes (puts) the given {@link VALUE value} to the {@link Cache} mapped to the given {@link KEY key}.
   * <p>
   * This {@link Cache} operation acquires a {@literal write lock}.
   *
   * @param lock {@link ReadWriteLock} used to synchronize the {@link Cache} write (put) operation.
   * @param key {@link KEY key} mapped to the {@link VALUE value} stored in the {@link Cache}.
   * @param value {@link VALUE value} to store in the {@link Cache}.
   * @return the given {@link VALUE}.
   * @see java.util.concurrent.locks.ReadWriteLock#writeLock()
   * @see #doWithWriteLock(ReadWriteLock, Supplier)
   * @see #getCache()
   */
  protected @Nullable VALUE write(@NotNull ReadWriteLock lock, @NotNull KEY key, @Nullable VALUE value) {

    return doWithWriteLock(lock, () -> {
      getCache().put(key, value);
      return value;
    });
  }

  /**
   * Implementation of the {@literal Cache-Aside} Cache Pattern, also known as {@literal Look-Aside} Cache Pattern.
   * <p>
   * This caching data access operation first attempts to locate an {@link Cache.Entry} in the {@link Cache}
   * with the given {@link KEY key}, returning the {@link VALUE value} of the {@link Cache.Entry} if present.
   * <p>
   * If an {@link Cache.Entry} with the given {@link KEY key} is not present in the {@link Cache},
   * then the given {@link Supplier cacheable operation} is invoked to compute or load a {@link VALUE value}.
   * The {@link VALUE value} is then put into the {@link Cache} as a {@link Cache.Entry} mapped to the given
   * {@link KEY key}; this operation completes by returning the {@link VALUE result} of
   * the {@link Supplier cacheable operation}.
   *
   * @param <T> {@link Class type} of the {@link VALUE return value}.
   * @param key {@link KEY key} used to identify the {@link Cache.Entry} containing the {@link VALUE} to lookup.
   * @param cacheableOperation {@link Supplier} used to compute or load the {@link VALUE value}
   * for given {@link KEY key} if the {@link Cache} lookup results in a {@literal cache miss};
   * must not be {@literal null}.
   * @return the {@link VALUE cached value} for the given {@link KEY key} if present in the {@link Cache}.
   * Returns the {@link VALUE value} supplied by invoking the {@link Supplier cacheable operation} otherwise.
   * @throws IllegalArgumentException if either the {@link KEY key} or the {@link Supplier} are {@literal null}.
   * @see #write(ReadWriteLock, Comparable, Object)
   * @see #read(ReadWriteLock, Comparable)
   * @see java.util.function.Supplier
   * @see #getLock()
   */
  @SuppressWarnings("unchecked")
  public @Nullable <T extends VALUE> T withCaching(@NotNull KEY key, @NotNull Supplier<VALUE> cacheableOperation) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(cacheableOperation, "Supplier is required");

    ReadWriteLock lock = getLock();

    return (T) Optional.ofNullable(read(lock, key)).orElseGet(() ->
      Optional.ofNullable(cacheableOperation.get())
        .map(value -> write(lock, key, value))
        .orElse(null));
  }

  /**
   * This caching data access operation invokes the given, required {@link Supplier cacheable operation}
   * and then {@link Cache#clear() clears} the contents of the entire {@link Cache}, but only if
   * the {@link Supplier cacheable operation} completes successfully.
   *
   * @param <T> {@link Class type} of the {@link VALUE return value}.
   * @param cacheableOperation {@link Supplier} used to compute or load a {@link VALUE value};
   * must not be {@literal null}.
   * @return the {@link VALUE result} of the {@link Supplier cacheable operation}.
   * @throws IllegalArgumentException if the {@link Supplier} is {@literal null}.
   * @see java.util.function.Supplier
   * @see #clear(ReadWriteLock)
   * @see #getLock()
   */
  @SuppressWarnings("unchecked")
  public @Nullable <T extends VALUE> T withCacheClear(@NotNull Supplier<VALUE> cacheableOperation) {

    Assert.notNull(cacheableOperation, "Supplier is required");

    VALUE returnValue = cacheableOperation.get();

    clear(getLock());

    return (T) returnValue;
  }

  /**
   * This caching data access operation invokes the given, required {@link Supplier cacheable operation}
   * and then {@link Cache#evict(Comparable) evicts} the {@link Cache.Entry} from the {@link Cache}
   * mapped to the given {@link KEY key} if present, but only if the {@link Supplier cacheable operation}
   * completes successfully.
   *
   * @param <T> {@link Class type} of the {@link VALUE return value}.
   * @param key {@link KEY key} identifying the {@link Cache.Entry} in the {@link Cache} to evict.
   * @param cacheableOperation {@link Supplier} used to compute or load a {@link VALUE value};
   * must not be {@literal null}.
   * @return the {@link VALUE result} of the {@link Supplier cacheable operation}.
   * @throws IllegalArgumentException if the {@link Supplier} is {@literal null}.
   * @see #evict(ReadWriteLock, Comparable)
   * @see java.util.function.Supplier
   * @see #getLock()
   */
  @SuppressWarnings("unchecked")
  public @Nullable <T extends VALUE> T withCacheEvict(@NotNull KEY key, @NotNull Supplier<VALUE> cacheableOperation) {

    Assert.notNull(cacheableOperation, "Supplier is required");

    VALUE result = cacheableOperation.get();

    evict(getLock(), key);

    return (T) result;
  }

  /**
   * This caching data access operation invokes the given, required {@link Supplier cacheable operation}
   * and then caches the {@link VALUE result} before returning the computed {@link VALUE}.
   * <p>
   * The {@link VALUE result} of the {@link Supplier cacheable operation} is only cached if the {@link VALUE result}
   * is not {@literal null} and the {@link Supplier cacheable operation} completes successfully.
   *
   * @param <T> {@link Class type} of the {@link VALUE return value}.
   * @param key {@link KEY key} mapping the {@link VALUE value} returned by the {@link Supplier cacheable operation}
   * as a {@link Cache.Entry} stored in the {@link Cache}; must not be {@literal null}.
   * @param cacheableOperation {@link Supplier} used to compute or load a {@link VALUE value}
   * mapped to the given {@link KEY key} and put as an {@link Cache.Entry} in the {@link Cache};
   * must not be {@literal null}.
   * @return the {@link VALUE result} of the {@link Supplier cacheable operation}.
   * @throws IllegalArgumentException if either the {@link KEY key} or the {@link Supplier} are {@literal null}.
   * @see #write(ReadWriteLock, Comparable, Object)
   * @see java.util.function.Supplier
   * @see #getLock()
   */
  @SuppressWarnings("unchecked")
  public @Nullable <T extends VALUE> T withCachePut(@NotNull KEY key, @NotNull Supplier<VALUE> cacheableOperation) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(cacheableOperation, "Supplier is required");

    return (T) Optional.ofNullable(cacheableOperation.get())
      .map(value -> write(getLock(), key, value))
      .orElse(null);
  }
}
