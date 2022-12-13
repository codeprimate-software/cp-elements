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

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Sourced;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.text.FormatUtils;
import org.cp.elements.util.CollectionUtils;

/**
 * The {@link CacheToMapAdapter} class is a {@link Map} implementation backed by a {@link Cache}.
 *
 * This class adapts the {@link Cache} interface as an instance of a {@link Map}.
 *
 * @author John Blum
 * @param <KEY> {@link Class type} of the {@link Map} key.
 * @param <VALUE> {@link Class type} of the {@link Map} value.
 * @see java.lang.Comparable
 * @see java.util.AbstractMap
 * @see java.util.Map
 * @see org.cp.elements.data.caching.Cache
 * @see <a href="https://en.wikipedia.org/wiki/Adapter_pattern">Adapter Software Design Pattern</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CacheToMapAdapter<KEY extends Comparable<KEY>, VALUE> extends AbstractMap<KEY, VALUE> {

  /**
   * Factory method used to construct a new instance of {@link CacheToMapAdapter} initialized with
   * the given, required {@link Cache} adapted and used to back the returned {@link Map} implementation.
   *
   * @param <KEY> {@link Class type} of keys used by the {@link Map}.
   * @param <VALUE> {@link Class type} of the values stored by the {@link Map}.
   * @param cache {@link Cache} to adapt as a {@link Map}; must not be {@literal null}.
   * @return a new {@link CacheToMapAdapter} initialized with the given {@link Cache}.
   * @throws IllegalArgumentException if {@link Cache} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache
   * @see #CacheToMapAdapter(Cache)
   */
  public static @NotNull <KEY extends Comparable<KEY>, VALUE> CacheToMapAdapter<KEY, VALUE> of(
      @NotNull Cache<KEY, VALUE> cache) {

    return new CacheToMapAdapter<>(cache);
  }

  private final Cache<KEY, VALUE> cache;

  /**
   * Constructs a new instance of {@link CacheToMapAdapter} initialized with the given, required {@link Cache}
   * adapted and used to back this {@link Map} implementation.
   *
   * @param cache {@link Cache} to adapt as a {@link Map}; must not be {@literal null}.
   * @throws IllegalArgumentException if {@link Cache} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache
   */
  public CacheToMapAdapter(@NotNull Cache<KEY, VALUE> cache) {
    this.cache = ObjectUtils.requireObject(cache, "Cache is required");
  }

  /**
   * Returns a reference to the configured {@link Cache} backing this {@link Map}.
   *
   * @return a reference to the configured {@link Cache} backing this {@link Map}.
   * @see org.cp.elements.data.caching.Cache
   */
  protected @NotNull Cache<KEY, VALUE> getCache() {
    return this.cache;
  }

  @Override
  public void clear() {
    getCache().clear();
  }

  @NullSafe
  @Override
  @SuppressWarnings("unchecked")
  public boolean containsKey(@Nullable Object key) {
    return key != null && getCache().contains((KEY) key);
  }

  @Override
  public Set<Map.Entry<KEY, VALUE>> entrySet() {

    return new AbstractSet<Map.Entry<KEY, VALUE>>() {

      @Override
      public Iterator<Map.Entry<KEY, VALUE>> iterator() {

        return new Iterator<Map.Entry<KEY, VALUE>>() {

          private final Iterator<KEY> keys = CollectionUtils.nullSafeSet(keySet()).iterator();

          @Override
          public boolean hasNext() {
            return this.keys.hasNext();
          }

          @Override
          public @NotNull Map.Entry<KEY, VALUE> next() {

            try {
              return CacheMapEntry.of(getCache(), this.keys.next());
            }
            catch (NoSuchElementException ignore) {
              throw new NoSuchElementException("No more cache entries");
            }
          }
        };
      }

      @Override
      public int size() {
        return Long.valueOf(getCache().size()).intValue();
      }
    };
  }

  @Override
  @SuppressWarnings("unchecked")
  public @Nullable VALUE get(Object key) {
    return getCache().get((KEY) key);
  }

  @Override
  public boolean isEmpty() {
    return getCache().isEmpty();
  }

  @Override
  public Set<KEY> keySet() {
    return getCache().keys();
  }

  @Override
  public @Nullable VALUE put(KEY key, VALUE value) {
    return getCache().getAndPut(key, value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public void putAll(Map<? extends KEY, ? extends VALUE> map) {
    getCache().from((Map<KEY, VALUE>) map);
  }

  @Override
  public @Nullable VALUE putIfAbsent(KEY key, VALUE value) {
    return getCache().putIfAbsent(key, value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public VALUE remove(Object key) {
    return getCache().getAndEvict((KEY) key);
  }

  @Override
  public int size() {
    return Long.valueOf(getCache().size()).intValue();
  }

  /**
   * Implementation of the {@link Map.Entry} interface encapsulating a single {@link KEY key} and {@link VALUE value}
   * in a given {@link Cache.Entry} from the {@link Cache}.
   *
   * @param <KEY> {@link Comparable} {@link Class type} of the {@link Cache} key.
   * @param <VALUE> {@link Class type} of the {@link Cache} value.
   * @see org.cp.elements.data.caching.Cache
   * @see org.cp.elements.lang.Sourced
   * @see java.lang.Comparable
   * @see java.util.Map.Entry
   */
  protected static class CacheMapEntry<KEY extends Comparable<KEY>, VALUE>
      implements Map.Entry<KEY, VALUE>, Sourced<Cache<KEY, VALUE>>, Comparable<Map.Entry<KEY, VALUE>> {

    /**
     * Factory method used to construct a new instance of {@link CacheMapEntry} initialized with
     * the given {@link KEY key} identifying the target {@link Cache.Entry} in the given {@link Cache}.
     *
     * @param <KEY> {@link Comparable} {@link Class type} of the {@link Cache} key.
     * @param <VALUE> {@link Class type} of the {@link Cache} value.
     * @param cache reference to the {@link Cache} backing this {@link CacheMapEntry}; must not be {@literal null}.
     * @param key {@link KEY} identifying the {@link Cache.Entry} in the {@link Cache} that this {@link CacheMapEntry}
     * will represent; most not be {@literal null}.
     * @return a new {@link CacheMapEntry} initialized with the given, required {@link KEY key} and {@link Cache}.
     * @throws IllegalArgumentException if either the {@link KEY key} or the {@link Cache} is {@literal null}.
     * @throws IllegalStateException if the {@link Cache} does not {@link Cache#contains(Comparable) contain}
     * a {@link Cache.Entry} with the give, required {@link KEY key}.
     * @see org.cp.elements.data.caching.Cache
     * @see #CacheMapEntry(Cache, Comparable)
     * @see java.lang.Comparable
     */
    protected static @NotNull <KEY extends Comparable<KEY>, VALUE> CacheMapEntry<KEY, VALUE> of(
        @NotNull Cache<KEY, VALUE> cache, @NotNull KEY key) {

      return new CacheMapEntry<>(cache, key);
    }

    private final KEY key;

    private final Cache<KEY, VALUE> cache;

    /**
     * Constructs a new instance of {@link CacheMapEntry} initialized with the given, required {@link KEY key}
     * identifying the target {@link Cache.Entry} in the given, required {@link Cache}.
     *
     * @param cache reference to the {@link Cache} backing this {@link CacheMapEntry}; must not be {@literal null}.
     * @param key {@link KEY} identifying the {@link Cache.Entry} in the {@link Cache} that this {@link CacheMapEntry}
     * will represent; most not be {@literal null}.
     * @throws IllegalArgumentException if either the {@link KEY key} or the {@link Cache} is {@literal null}.
     * @throws IllegalStateException if the {@link Cache} does not {@link Cache#contains(Comparable) contain}
     * a {@link Cache.Entry} with the give, required {@link KEY key}.
     * @see org.cp.elements.data.caching.Cache
     * @see java.lang.Comparable
     */
    protected CacheMapEntry(@NotNull Cache<KEY, VALUE> cache, @NotNull KEY key) {

      Assert.notNull(cache, "Cache is required");
      Assert.notNull(key, "Key is required");

      Assert.state(cache.contains(key), () ->
        FormatUtils.format("Key [%1$s] is not contained in Cache [%2$s]", key, cache.getName()));

      this.key = key;
      this.cache = cache;
    }

    /**
     * Returns a reference to the {@link Cache} backing this {@link CacheMapEntry}.
     *
     * @return a reference to the {@link Cache} backing this {@link CacheMapEntry}.
     * @see org.cp.elements.data.caching.Cache
     */
    protected @NotNull Cache<KEY, VALUE> getCache() {
      return this.cache;
    }

    /**
     * Returns the {@link KEY key} identifying this {@link CacheMapEntry} in the {@link Cache}.
     *
     * @return the {@link KEY key} identifying this {@link CacheMapEntry} in the {@link Cache}.
     * @see java.lang.Comparable
     */
    @Override
    public @NotNull KEY getKey() {
      return this.key;
    }

    @Override
    public @NotNull Cache<KEY, VALUE> getSource() {
      return getCache();
    }

    /**
     * Gets the {@link VALUE value} of this {@link CacheMapEntry} in the {@link Cache}.
     *
     * @return the {@link VALUE value} of this {@link CacheMapEntry} in the {@link Cache}.
     * @see org.cp.elements.data.caching.Cache#get(Comparable)
     * @see #getCache()
     * @see #getKey()
     */
    @Override
    public @Nullable VALUE getValue() {
      return getCache().get(getKey());
    }

    /**
     * Sets the {@link #getValue() value} of this {@link CacheMapEntry} in the {@link Cache}
     * to the given {@link VALUE value}.
     *
     * @param value {@link VALUE new value} for this {@link CacheMapEntry}.
     * @return the {@link VALUE previous value} of this {@link CacheMapEntry}.
     * @see org.cp.elements.data.caching.Cache#put(Comparable, Object)
     * @see #getCache()
     * @see #getKey()
     */
    @Override
    public VALUE setValue(VALUE value) {
      return getCache().getAndPut(getKey(), value);
    }

    @Override
    public int compareTo(@NotNull Map.Entry<KEY, VALUE> mapEntry) {
      return this.getKey().compareTo(mapEntry.getKey());
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof Map.Entry)) {
        return false;
      }

      Map.Entry<?, ?> that = (Map.Entry<?, ?>) obj;

      return this.getKey().equals(that.getKey());
    }

    @Override
    public int hashCode() {
      return ObjectUtils.hashCodeOf(getKey());
    }

    @Override
    public String toString() {
      return String.format("%1$s = %2$s", getKey(), getValue());
    }
  }
}
