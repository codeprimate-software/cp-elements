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

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

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

  /**
   * Factory method used to construct a new instance of {@link CacheToMapAdapter} initialized with
   * the given {@link Cache} used to back the {@link Map}.
   *
   * @param <KEY> {@link Class type} of keys used by the {@link Map}.
   * @param <VALUE> {@link Class type} of the value stored by the {@link Map}.
   * @param cache to {@link Cache} adapt as a {@link Map}, backing the {@link Map} instance.
   * @return a new {@link CacheToMapAdapter} initialized with the given {@link Cache}.
   * @throws IllegalArgumentException if the {@link Cache} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache
   * @see #CacheToMapAdapter(Cache)
   */
  public static <KEY extends Comparable<KEY>, VALUE> CacheToMapAdapter<KEY, VALUE> of(Cache<KEY, VALUE> cache) {
    return new CacheToMapAdapter<>(cache);
  }

  /**
   * Constructs a new instance of {@link CacheToMapAdapter} initialized with the given {@link Cache}.
   *
   * @param cache the {@link Cache} to adapt as a {@link Map}.
   * @throws IllegalArgumentException if the {@link Cache} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache
   */
  public CacheToMapAdapter(Cache<KEY, VALUE> cache) {

    Assert.notNull(cache, "Cache is required");

    this.cache = cache;
  }

  /**
   * Returns a reference to the {@link Cache} backing this {@link Map}.
   *
   * @return a reference to the {@link Cache} backing this {@link Map}.
   * @see org.cp.elements.data.caching.Cache
   */
  protected Cache<KEY, VALUE> getCache() {
    return this.cache;
  }

  @Override
  public void clear() {
    getCache().clear();
  }

  @Override
  public Set<Entry<KEY, VALUE>> entrySet() {

    Cache<KEY, VALUE> cache = getCache();

    return new AbstractSet<Entry<KEY, VALUE>>() {

      @Override
      public Iterator<Entry<KEY, VALUE>> iterator() {

        return new Iterator<Entry<KEY, VALUE>>() {

          private boolean nextCalled = false;

          private KEY currentKey = null;

          private final Iterator<KEY> keysIterator = cache.keys().iterator();

          @Override
          public boolean hasNext() {
            return this.keysIterator.hasNext();
          }

          @Override
          public Entry<KEY, VALUE> next() {

            this.currentKey = this.keysIterator.next();
            this.nextCalled = true;

            return CacheEntry.of(this.currentKey, cache);
          }

          @Override
          public void remove() {

            Assert.state(this.currentKey != null && this.nextCalled,
              "Next must be called before calling remove");

            this.nextCalled = false;
            cache.evict(this.currentKey);
          }
        };
      }

      @Override
      public int size() {
        return cache.size();
      }
    };
  }

  @Override
  public VALUE put(KEY key, VALUE value) {

    Cache<KEY, VALUE> cache = getCache();

    VALUE oldValue = cache.get(key);

    cache.put(key, value);

    return oldValue;
  }

  /**
   * The {@link CacheEntry} class is an implementation of {@link Map.Entry} encapsulating the {@link KEY key}
   * and {@link VALUE value} for a single entry in the {@link Cache}.
   *
   * @param <KEY> {@link Comparable} {@link Class type} of the {@link Cache} key.
   * @param <VALUE> {@link Class type} of the {@link Cache} value.
   * @see java.lang.Comparable
   * @see java.util.Map.Entry
   */
  protected static class CacheEntry<KEY extends Comparable<KEY>, VALUE> implements Entry<KEY, VALUE> {

    private final KEY key;

    private final Cache<KEY, VALUE> cache;

    /**
     * Factory method used to construct a new instance of the {@link CacheEntry} class initialized with
     * the given {@link KEY key} identifying the target entry in the given {@link Cache}.
     *
     * @param <KEY> {@link Comparable} {@link Class type} of the {@link Cache} key.
     * @param <VALUE> {@link Class type} of the {@link Cache} value.
     * @param key {@link KEY} identifying the entry in the {@link Cache} that this {@link CacheEntry} represents.
     * @param cache reference to the underlying {@link Cache} backing this {@link CacheEntry}.
     * @return a new instance of {@link CacheEntry} initialized with the given {@link KEY key} and {@link Cache}
     * backing this {@link Entry}.
     * @throws IllegalArgumentException if either the {@link KEY key} or the {@link Cache} are {@literal null}.
     * @throws IllegalStateException if the {@link Cache} does not {@link Cache#contains(Comparable) contain}
     * the {@link KEY key}.
     * @see org.cp.elements.data.caching.Cache
     * @see java.lang.Comparable
     */
    protected static <KEY extends Comparable<KEY>, VALUE> CacheEntry<KEY, VALUE> of(KEY key, Cache<KEY, VALUE> cache) {
      return new CacheEntry<>(key, cache);
    }

    /**
     * Constructs a new instance of {@link CacheEntry} initialized with the given {@link KEY key}
     * identifying the target entry in the given {@link Cache}.
     *
     * @param key {@link KEY} identifying the entry in the {@link Cache} that this {@link CacheEntry} represents.
     * @param cache reference to the underlying {@link Cache} backing this {@link CacheEntry}.
     * @throws IllegalArgumentException if either the {@link KEY key} or the {@link Cache} are {@literal null}.
     * @throws IllegalStateException if the {@link Cache} does not {@link Cache#contains(Comparable) contain}
     * the {@link KEY key}.
     * @see org.cp.elements.data.caching.Cache
     */
    protected CacheEntry(KEY key, Cache<KEY, VALUE> cache) {

      Assert.notNull(key, "Key is required");
      Assert.notNull(cache, "Cache is required");
      Assert.state(cache.contains(key), "Key is not contained in the ");

      this.key = key;
      this.cache = cache;
    }

    /**
     * Returns a reference to the {@link Cache} backing this {@link Entry}.
     *
     * @return a reference to the {@link Cache} backing this {@link Entry}.
     * @see org.cp.elements.data.caching.Cache
     */
    protected Cache<KEY, VALUE> getCache() {
      return this.cache;
    }

    /**
     * Returns the {@link KEY key} identifying this {@link Entry} in the {@link Cache}.
     *
     * @return the {@link KEY key} identifying this {@link Entry} in the {@link Cache}.
     */
    @Override
    public KEY getKey() {
      return this.key;
    }

    /**
     * Gets the {@link VALUE value} of this {@link Entry} in the {@link Cache}.
     *
     * @return the {@link VALUE value} of this {@link Entry} in the {@link Cache}.
     * @see org.cp.elements.data.caching.Cache#get(Comparable)
     * @see #getCache()
     * @see #getKey()
     */
    @Override
    public VALUE getValue() {
      return getCache().get(getKey());
    }

    /**
     * Sets the {@link VALUE value} of this {@link Entry} in the {@link Cache} to the given {@link VALUE value}.
     *
     * @param value {@link VALUE value} to set this {@link Entry} in the {@link Cache} to.
     * @return the previous {@link VALUE value} of this {@link Entry} in the {@link Cache}.
     * @see org.cp.elements.data.caching.Cache#put(Comparable, Object)
     * @see #getCache()
     * @see #getKey()
     */
    @Override
    public VALUE setValue(VALUE value) {

      Cache<KEY, VALUE> cache = getCache();

      KEY key = getKey();
      VALUE oldValue = cache.get(key);

      getCache().put(key, value);

      return value;
    }
  }
}
