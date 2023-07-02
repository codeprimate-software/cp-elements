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
package org.cp.elements.data.caching;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;

import java.util.Collections;
import java.util.Iterator;
import java.util.Set;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.CollectionUtils;

/**
 * Abstract base class supporting the implementation of different types of {@link Cache Caches}.
 *
 * @param <KEY> {@link Class type} of the {@link Cache} key.
 * @param <VALUE> {@link Class type} of the {@link Cache} value.
 * @author John Blum
 * @see org.cp.elements.data.caching.Cache
 * @see java.lang.Comparable
 * @since 1.0.0
 */
public abstract class AbstractCache<KEY extends Comparable<KEY>, VALUE> implements Cache<KEY, VALUE> {

  private volatile Object lock;

  private volatile String name;

  /**
   * Gets the configured {@link Object lock} used to run operations on this {@link Cache} atomically / synchronously.
   * <p>
   * If no {@link Object lock} has been configured, that is, the {@link Object lock} is {@literal null},
   * then the {@link Cache} operations will not be atomic.
   * <p>
   * No {@link Object lock} is configured by default.
   *
   * @return the configured {@link Object lock}; may be {@literal null}.
   */
  @Override
  public @Nullable Object getLock() {
    return this.lock;
  }

  /**
   * Configures the {@link Object lock} to synchronize the operation on this {@link Cache} so they execute atomically.
   *
   * @param lock {@link Object} to use as a {@literal lock} to synchronize operations on this {@link Cache}.
   * @see #getLock()
   */
  public void setLock(@Nullable Object lock) {
    this.lock = lock;
  }

  /**
   * Clears the entire contents of (all entries from) this {@link Cache}.
   * <p>
   * This {@link Cache} operation is not supported by default since it can be a very costly operation,
   * especially in a distributed context. Caching providers and implementors should therefore provide
   * a custom and efficient implementation to clear the contents and {@link Cache.Entry entries} from
   * this {@link Cache}, which is likely to be data store dependent.
   *
   * @throws UnsupportedOperationException by default.
   */
  @Override
  public void clear() {
    throw new UnsupportedOperationException("Clear is not supported");
  }

  /**
   * Removes the {@link Cache.Entry entry} mapped to the given {@link KEY key} in this {@link Cache}.
   *
   * @param key {@link KEY key} identifying the entry to remove (evict) from this {@link Cache}.
   * @throws UnsupportedOperationException by default.
   */
  @Override
  public void evict(KEY key) {
    throw newUnsupportedOperationException("Eviction is not supported");
  }

  /**
   * Gets the {@link VALUE value} stored in this {@link Cache} mapped to the given {@link KEY}.
   * <p>
   * Returns {@literal null} if the {@link VALUE value} mapped to the given {@link KEY key} is {@literal null},
   * or this {@link Cache} does not contain an entry mapped to the given {@link KEY key}.
   *
   * @param key {@link KEY key} mapped to the {@link VALUE value} returned.
   * @return the {@link VALUE value} mapped to the given {@link KEY key}, or return {@literal null}
   * if an {@link Cache.Entry entry} with the given {@link KEY key} does not exist,
   * or a {@link VALUE value} for the given {@link KEY key} is {@literal null}.
   * @throws UnsupportedOperationException by default.
   * @see #put(Comparable, Object)
   */
  @Override
  public VALUE get(KEY key) {
    throw newUnsupportedOperationException("Get is not supported");
  }

  /**
   * Returns the {@link String name} of this {@link Cache}.
   * <p>
   * The configured {@link String name} may be {@literal null}.
   *
   * @return the {@link String name} of this {@link Cache}; may be {@literal null}.
   * @see #named(String)
   */
  @Override
  public @Nullable String getName() {
    return this.name;
  }

  /**
   * Returns an {@link Iterator} iterating over the {@link Cache.Entry entries} in this {@link Cache}.
   * <p>
   * When extending from {@link AbstractCache}, caching providers must be careful to either override
   * the {@link #keys()} or this {@literal iterator} method, since {@literal iterator()} calls {@link #keys()}
   * and {@link #keys()} calls {@literal iterator()}, which will end up in infinite recursion.
   *
   * @return an {@link Iterator} iterating over the {@link Cache.Entry entries} in this {@link Cache}.
   * @see org.cp.elements.data.caching.AbstractCache.AttachedCacheEntry
   * @see org.cp.elements.data.caching.Cache.Entry
   * @see java.util.Iterator
   * @see #keys()
   */
  @Override
  public Iterator<Cache.Entry<KEY, VALUE>> iterator() {

    return new Iterator<>() {

      private final Iterator<KEY> keys = CollectionUtils.nullSafeSet(keys()).iterator();

      @Override
      public boolean hasNext() {
        return this.keys.hasNext();
      }

      @Override
      public Entry<KEY, VALUE> next() {
        return AttachedCacheEntry.from(AbstractCache.this, this.keys.next());
      }
    };
  }

  /**
   * Returns all {@link KEY keys} in this {@link Cache}.
   * <p>
   * Returns an {@link Collections#emptySet()} by default.
   *
   * @return a {@link Set} containing all the {@link KEY keys} from this {@link Cache}.
   * Returns an {@link Set#isEmpty() empty Set} if there are no {@link Cache.Entry entries}
   * in this {@link Cache}.
   * @see java.util.Set
   * @see #iterator()
   */
  @Override
  public Set<KEY> keys() {
    return Collections.emptySet();
  }

  /**
   * Puts the {@link VALUE value} in this {@link Cache} mapped to the given {@link KEY key}.
   *
   * @param key {@link KEY} mapped to the {@link VALUE value}; must not be {@literal null}.
   * @param value {@link VALUE} put in this {@link Cache} mapped to the {@link KEY key}.
   * @throws IllegalArgumentException if the {@link KEY key} is {@literal null}.
   * @throws UnsupportedOperationException by default.
   * @see #get(Comparable)
   */
  @Override
  public void put(KEY key, VALUE value) {
    throw newUnsupportedOperationException("Put is not supported");
  }

  /**
   * Builder method to set (configure) the {@link String name} of this {@link Cache}.
   *
   * @param <T> {@link Class type} of this {@link Cache}.
   * @param name {@link String} containing the {@literal name} to assign to this {@link Cache}.
   * @return this {@link Cache}.
   */
  @SuppressWarnings("unchecked")
  public @NotNull <T extends AbstractCache<KEY, VALUE>> T named(@Nullable String name) {
    this.name = name;
    return (T) this;
  }

  /**
   * Abstract base class encapsulating functionality common to all {@link Cache.Entry} implementations.
   *
   * @param <KEY> {@link Class type} of the {@link Cache.Entry} key.
   * @param <VALUE> {@link Class type} of the {@link Cache.Entry} value.
   * @see org.cp.elements.data.caching.Cache.Entry
   * @see java.lang.Comparable
   */
  protected abstract static class AbstractEntry<KEY extends Comparable<KEY>, VALUE> implements Cache.Entry<KEY, VALUE> {

    private final Cache<KEY, VALUE> cache;

    private final KEY key;

    /**
     * Constructs a new {@link AbstractEntry} initialized with the given, required {@link Cache}
     * used as the {@literal source} of this {@link Cache.Entry} along with the given, required {@link KEY key}.
     *
     * @param cache {@link Cache} used as the {@literal source} of this {@link Cache.Entry};
     * must not be {@literal null}.
     * @param key {@link KEY key} in the {@link Cache.Entry} mapping; must not be {@literal null}.
     * @throws IllegalArgumentException if the {@link Cache} or the {@link KEY key} is {@literal null}.
     * @see org.cp.elements.data.caching.Cache
     */
    protected AbstractEntry(@NotNull Cache<KEY, VALUE> cache, @NotNull KEY key) {

      this.cache = ObjectUtils.requireObject(cache, "Cache used as the source of this Entry is required");
      this.key = ObjectUtils.requireObject(key, "Key is required");
    }

    @Override
    public @NotNull Cache<KEY, VALUE> getSource() {
      return this.cache;
    }

    @Override
    public @NotNull KEY getKey() {
      return this.key;
    }
  }

  /**
   * Abstract Data Type (ADT) modeling an {@literal attached} {@link Cache.Entry}, which is backed by
   * the underlying {@link Cache} reflecting changes to the {@link Cache} in realtime.
   *
   * @param <KEY> {@link Class type} of the {@link Cache.Entry} key.
   * @param <VALUE> {@link Class type} of the {@link Cache.Entry} value.
   * @see org.cp.elements.data.caching.AbstractCache.AbstractEntry
   * @see java.lang.Comparable
   */
  @SuppressWarnings("unused")
  public static class AttachedCacheEntry<KEY extends Comparable<KEY>, VALUE> extends AbstractEntry<KEY, VALUE> {

    /**
     * Factory method used to construct a new {@link AttachedCacheEntry} initialized with the given,
     * required {@link Cache} used as the {@literal source} of this {@link Cache.Entry} along with the given,
     * required {@link KEY key}.
     *
     * @param <KEY> {@link Class type} of the {@link Cache.Entry} key.
     * @param <VALUE> {@link Class type} of the {@link Cache.Entry} value.
     * @param cache {@link Cache} used as the {@literal source} of this {@link Cache.Entry};
     * must not be {@literal null}.
     * @param key {@link KEY key} in the {@link Cache.Entry} mapping; must not be {@literal null}.
     * @return a new {@link AttachedCacheEntry}.
     * @throws IllegalArgumentException if the {@link Cache} or the {@link KEY key} is {@literal null}.
     * @see org.cp.elements.data.caching.Cache
     * @see java.lang.Comparable
     */
    public static @NotNull <KEY extends Comparable<KEY>, VALUE> AttachedCacheEntry<KEY, VALUE> from(
        @NotNull Cache<KEY, VALUE> cache, @NotNull KEY key) {

      return new AttachedCacheEntry<>(cache, key);
    }

    /**
     * Constructs a new {@link AttachedCacheEntry} initialized with the given, required {@link Cache}
     * used as the {@literal source} of this {@link Cache.Entry} along with the given, required {@link KEY key}.
     *
     * @param cache {@link Cache} used as the {@literal source} of this {@link Cache.Entry};
     * must not be {@literal null}.
     * @param key {@link KEY key} in the {@link Cache.Entry} mapping; must not be {@literal null}.
     * @throws IllegalArgumentException if the {@link Cache} or the {@link KEY key} is {@literal null}.
     * @see org.cp.elements.data.caching.Cache
     */
    protected AttachedCacheEntry(@NotNull Cache<KEY, VALUE> cache, @NotNull KEY key) {
      super(cache, key);
    }
  }

  /**
   * Abstract Data Type (ADT) modeling a simple, {@literal detached} {@link Cache.Entry} that is not associated with
   * or backed by any {@link Cache}.
   *
   * @param <KEY> {@link Class type} of the {@link Cache.Entry} key.
   * @param <VALUE> {@link Class type} of the {@link Cache.Entry} value.
   * @see org.cp.elements.data.caching.Cache.Entry
   * @see java.lang.Comparable
   */
  @SuppressWarnings("unused")
  public static class SimpleCacheEntry<KEY extends Comparable<KEY>, VALUE> implements Cache.Entry<KEY, VALUE> {

    /**
     * Factory method used to construct a new {@link SimpleCacheEntry} initialized with the given,
     * required {@link KEY key} and {@link VALUE value}.
     *
     * @param <KEY> {@link Class type} of the {@link Cache.Entry} key.
     * @param <VALUE> {@link Class type} of the {@link Cache.Entry} value.
     * @param key {@link KEY key} in this {@link Cache.Entry} mapping; must not be {@literal null}.
     * @param value {@link VALUE} mapped to the {@link KEY key} in this {@link Cache.Entry} mapping.
     * @return a new {@link SimpleCacheEntry}.
     * @throws IllegalArgumentException if the {@link KEY key} is {@literal null}.
     * @see java.lang.Comparable
     */
    public static @NotNull <KEY extends Comparable<KEY>, VALUE> SimpleCacheEntry<KEY, VALUE> of(
        @NotNull KEY key, @Nullable VALUE value) {

      return new SimpleCacheEntry<>(key, value);
    }

    private final KEY key;
    private final VALUE value;

    /**
     * Constructs a new {@link SimpleCacheEntry} initialized with the given, required {@link KEY key}
     * and {@link VALUE value}.
     *
     * @param key {@link KEY key} in this {@link Cache.Entry} mapping; must not be {@literal null}.
     * @param value {@link VALUE} mapped to the {@link KEY key} in this {@link Cache.Entry} mapping.
     * @throws IllegalArgumentException if the {@link KEY key} is {@literal null}.
     */
    public SimpleCacheEntry(@NotNull KEY key, @Nullable VALUE value) {

      this.key = ObjectUtils.requireObject(key, "Key is required");
      this.value = value;
    }

    @Override
    public @NotNull KEY getKey() {
      return this.key;
    }

    @Override
    public @Nullable Cache<KEY, VALUE> getSource() {
      return null;
    }

    @Override
    public @Nullable VALUE getValue() {
      return this.value;
    }

    @Override
    public void setValue(VALUE value) {
      throw newUnsupportedOperationException("Value of Cache.Entry(%s) cannot be set", getKey());
    }

    @Override
    public @NotNull Entry<KEY, VALUE> materialize() {
      return this;
    }
  }
}
