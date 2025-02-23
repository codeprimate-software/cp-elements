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

import static org.cp.elements.lang.ElementsExceptionsFactory.newCacheNotFoundException;
import static org.cp.elements.lang.LangExtensions.given;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Sourced;
import org.cp.elements.lang.annotation.Alias;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.concurrent.ThreadUtils;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.MapUtils;
import org.cp.elements.util.stream.StreamUtils;
import org.cp.elements.util.stream.Streamable;

/**
 * Abstract Data Type (ADT) modeling a cache data structure, mapping {@link KEY keys} to {@link VALUE values}
 * in-memory for quick access.
 * <p>
 * Caches are used in cases when, given identical input, the data access operation returns the same output. Caching
 * is ideal for fast access to infrequently changing data, or relatively static data compared to transactional data
 * that is frequently changing. Although caches can be used to store and process transactional data as well, caches
 * excel when the number of reads exceeds the number of writes.
 * <p>
 * Caching providers and implementors of this {@link Cache} interface, must minimally provide implementations of
 * the following {@link Cache} operations:
 *
 * <ul>
 *   <li>{@link #evict(Comparable)}</li>
 *   <li>{@link #get(Comparable)}</li>
 *   <li>{@link #put(Comparable, Object)}</li>
 * </ul>
 *
 * If the {@link Cache} implementation should support atomic, synchronous operations using locking, then caching
 * providers should additionally override the {@link #getLock()} method and return a {@literal non-null} {@link Object}
 * to be used as the {@literal lock}.
 *
 * @author John Blum
 * @param <KEY> {@link Comparable} {@link Class type} of the {@literal keys} used for mapping by this {@link Cache}.
 * @param <VALUE> {@link Class type} of {@link VALUE values} stored in this {@link Cache}.
 * @see java.lang.Comparable
 * @see java.lang.Iterable
 * @see java.util.Map
 * @see org.cp.elements.data.caching.AbstractCache
 * @see org.cp.elements.data.caching.Cache.Entry
 * @see org.cp.elements.data.caching.provider.ConcurrentMapCache
 * @see org.cp.elements.data.caching.support.CachingTemplate
 * @see org.cp.elements.lang.Nameable
 * @see org.cp.elements.util.stream.Streamable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Cache<KEY extends Comparable<KEY>, VALUE>
    extends Iterable<Cache.Entry<KEY, VALUE>>, Nameable<String>, Streamable<Cache.Entry<KEY, VALUE>> {

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
  default @Nullable Object getLock() {
    return null;
  }

  /**
   * Determines whether this {@link Cache} contains any {@link Cache.Entry entries}.
   *
   * @return a boolean value indicating whether this {@link Cache} contains any {@link Cache.Entry entries}.
   * @see #isNotEmpty()
   * @see #size()
   */
  @NullSafe
  default boolean isEmpty() {
    return ThreadUtils.runAtomically(getLock(), () -> size() == 0L);
  }

  /**
   * Determines whether this {@link Cache} contains any {@link Cache.Entry entries}.
   * <p/>
   * Alias for {@link #isEmpty()}.
   *
   * @return a boolean value indicating whether this {@link Cache} contains any {@link Cache.Entry entries}.
   * @see #isEmpty()
   */
  @Alias(forMember = "isEmpty")
  default boolean isNotEmpty() {
    return !isEmpty();
  }

  /**
   * Clears the entire contents of this {@link Cache}.
   *
   * @see #evictAll(Iterable)
   * @see #keys()
   */
  @NullSafe
  default void clear() {
    ThreadUtils.runAtomically(getLock(), () -> evictAll(keys()));
  }

  /**
   * Determines whether this {@link Cache} contains an {@link Cache.Entry} mapped to the given {@link KEY key}.
   *
   * @param key {@link KEY key} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains an {@link Cache.Entry} mapped to
   * the given {@link KEY key}.
   * @see #iterator()
   */
  default boolean contains(KEY key) {
    return key != null && stream().anyMatch(cacheEntry -> ObjectUtils.equals(cacheEntry.getKey(), key));
  }

  /**
   * Determines whether this {@link Cache} contains {@link Cache.Entry entries} for each {@link KEY key}
   * in the given array.
   *
   * @param keys array of {@link KEY keys} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains {@link Cache.Entry entries}
   * for each {@link KEY key} in the given array.
   * @see #containsAll(Iterable)
   * @see #contains(Comparable)
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  default boolean containsAll(KEY... keys) {
    return ArrayUtils.isNotEmpty(keys)
      && ThreadUtils.runAtomically(getLock(), () -> Arrays.stream(keys).allMatch(this::contains));
  }

  /**
   * Determines whether this {@link Cache} contains {@link Cache.Entry entries} for each {@link KEY key}
   * in the {@link Iterable} object.
   *
   * @param keys {@link Iterable} of {@link KEY keys} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains {@link Cache.Entry entries}
   * for each {@link KEY key} in the {@link Iterable} object.
   * @see #containsAll(Comparable[])
   * @see #contains(Comparable)
   * @see java.lang.Iterable
   */
  @NullSafe
  default boolean containsAll(Iterable<KEY> keys) {
    return CollectionUtils.isNotEmpty(keys)
      && ThreadUtils.runAtomically(getLock(), () -> StreamUtils.stream(keys).allMatch(this::contains));
  }

  /**
   * Determines whether this {@link Cache} contains at least 1 {@link Cache.Entry} mapped to a {@link KEY key}
   * from the given array.
   *
   * @param keys array of {@link KEY keys} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains at least 1 {@link Cache.Entry}
   * mapped to a {@link KEY key} from the given array.
   * @see #containsAny(Iterable)
   * @see #contains(Comparable)
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  default boolean containsAny(KEY... keys) {
    return ArrayUtils.isNotEmpty(keys)
      && ThreadUtils.runAtomically(getLock(), () -> Arrays.stream(keys).anyMatch(this::contains));
  }

  /**
   * Determines whether this {@link Cache} contains at least 1 {@link Cache.Entry} mapped to a {@link KEY key}
   * from the given {@link Iterable}.
   *
   * @param keys {@link Iterable} of {@link KEY keys} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains at least 1 {@link Cache.Entry}
   * mapped to a {@link KEY key} from the given {@link Iterable}.
   * @see #containsAny(Comparable[])
   * @see #contains(Comparable)
   * @see java.lang.Iterable
   */
  @NullSafe
  default boolean containsAny(Iterable<KEY> keys) {
    return CollectionUtils.isNotEmpty(keys)
      && ThreadUtils.runAtomically(getLock(), () -> StreamUtils.stream(keys).anyMatch(this::contains));
  }

  /**
   * Removes the {@link Cache.Entry} mapped to the given {@link KEY key} in this {@link Cache}.
   *
   * @param key {@link KEY key} identifying the {@link Cache.Entry} to remove (evict) from this {@link Cache}.
   */
  void evict(KEY key);

  /**
   * Removes all {@link Cache.Entry entries} mapped to {@link KEY keys} in the given array from this {@link Cache}.
   *
   * @param keys array of {@link KEY keys} identifying {@link Cache.Entry entries} to remove (evict)
   * from this {@link Cache}.
   * @see #evictAll(Iterable)
   * @see #evict(Comparable)
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  default void evictAll(KEY... keys) {

    if (ArrayUtils.isNotEmpty(keys)) {
      ThreadUtils.runAtomically(getLock(), () -> Arrays.stream(keys)
        .filter(Objects::nonNull)
        .forEach(this::evict));
    }
  }

  /**
   * Removes all {@link Cache.Entry entries} mapped to {@link KEY keys} in the given {@link Iterable}
   * from this {@link Cache}.
   *
   * @param keys {@link Iterable} of {@link KEY keys} identifying {@link Cache.Entry entries} to remove (evict)
   * from this {@link Cache}.
   * @see #evictAll(Comparable[])
   * @see #evict(Comparable)
   * @see java.lang.Iterable
   */
  @NullSafe
  default void evictAll(Iterable<KEY> keys) {

    if (CollectionUtils.isNotEmpty(keys)) {
      ThreadUtils.runAtomically(getLock(), () -> StreamUtils.stream(keys)
        .filter(Objects::nonNull)
        .forEach(this::evict));
    }
  }

  /**
   * Caches all {@link Map.Entry entries} from given {@link Map} in this {@link Cache}.
   *
   * @param map {@link Map} containing the {@link Map.Entry entries} to cache.
   * @see #put(Comparable, Object)
   * @see java.util.Map
   */
  default void from(@Nullable Map<KEY, VALUE> map) {

    if (MapUtils.isNotEmpty(map)) {
      ThreadUtils.runAtomically(getLock(), () -> map.entrySet().stream()
          .filter(mapEntry -> given(mapEntry).thenGiven(Map.Entry::getKey).expectThat(Objects::nonNull).result())
          .map(Cache.Entry::from)
          .forEach(this::put));
    }
  }

  /**
   * Gets the {@link VALUE value} stored in this {@link Cache} mapped to the given {@link KEY}.
   * <p>
   * Returns {@literal null} if the {@link VALUE value} mapped to the given {@link KEY key} is {@literal null},
   * or this {@link Cache} does not contain an {@link Cache.Entry} mapped to the given {@link KEY key}.
   * <p>
   * Even though the {@link Cache} interface provides a default implementation, cache providers are still encouraged
   * to override this method and provide a more efficient, custom implementation that is conducive to the underlying
   * data store as recommended in the {@link Cache} interface Javadoc.
   *
   * @param key {@link KEY key} mapped to the {@link VALUE value} returned.
   * @return the {@link VALUE value} mapped to the given {@link KEY key}, or return {@literal null}
   * if an {@link Cache.Entry entry} with the given {@link KEY key} does not exist,
   * or a {@link VALUE value} for the given {@link KEY key} is {@literal null}.
   * @see #put(Comparable, Object)
   * @see #stream()
   */
  default @Nullable VALUE get(@NotNull KEY key) {

    return key == null ? null
      : stream()
        .filter(cacheEntry -> ObjectUtils.equals(cacheEntry.getKey(), key))
        .findFirst()
        .map(Cache.Entry::getValue)
        .orElse(null);
  }

  /**
   * Gets all {@link VALUE values} stored in this {@link Cache} mapped to {@link KEY keys} in the given array.
   * <p>
   * Returns an empty {@link List} if the array of {@link KEY keys} is {@literal null} or {@literal empty}.
   * <p>
   * Given an array of {@link KEY keys}: {@literal [ keyOne, keyTwo, ..., keyN ]} this method will return
   * a {@link List} of {@link VALUE values}: {@literal [ valueOne, valueTwo, ..., valueN ]} even if the values
   * are {@literal null}. The {@link List} of {@link VALUE values} matches 1 for 1 for each {@link KEY key}
   * from the array in the order that the {@link KEY keys} are stored in the array.
   *
   * @param keys array of {@link KEY keys} mapped to the {@link VALUE values} returned.
   * @return a {@link List} of {@link VALUE values} for all {@link KEY keys} in the given array.
   * @see #getAll(Iterable)
   * @see #get(Comparable)
   * @see java.util.List
   */
  @SuppressWarnings("unchecked")
  default List<VALUE> getAll(KEY... keys) {

    if (ArrayUtils.isNotEmpty(keys)) {
      return ThreadUtils.runAtomically(getLock(), () -> Arrays.stream(keys)
        .map(this::get)
        .toList());
    }

    return Collections.emptyList();
  }

  /**
   * Gets all {@link VALUE values} stored in this {@link Cache} mapped to {@link KEY keys}
   * in the given {@link Iterable}.
   * <p>
   * Returns an empty {@link List} if the {@link Iterable} of {@link KEY keys} is {@literal null} or {@literal empty}.
   * <p>
   * Given an {@link Iterable} of {@link KEY keys}: {@literal [ keyOne, keyTwo, ..., keyN ]} this method will return
   * a {@link List} of {@link VALUE values}: {@literal [ valueOne, valueTwo, ..., valueN ]} even if the values
   * are {@literal null}. The {@link List} of {@link VALUE values} matches 1 for 1 for each {@link KEY key}
   * from the {@link Iterable} in the order that the {@link KEY keys} are returned from the {@link Iterable}.
   *
   * @param keys array of {@link KEY keys} mapped to the {@link VALUE values} returned.
   * @return a {@link List} of {@link VALUE values} for all {@link KEY keys} in the given {@link Iterable}.
   * @see #getAll(Comparable[])
   * @see #get(Comparable)
   * @see java.lang.Iterable
   * @see java.util.List
   */
  default List<VALUE> getAll(Iterable<KEY> keys) {

    if (CollectionUtils.isNotEmpty(keys)) {
      return ThreadUtils.runAtomically(getLock(), () -> StreamUtils.stream(keys)
        .map(this::get)
        .toList());
    }

    return Collections.emptyList();
  }

  /**
   * Gets the {@link VALUE value} stored in this {@link Cache} mapped to the given {@link KEY key}
   * then removes the mapping if the mapping existed in the first place.
   *
   * @param key {@link KEY key} in which the mapping will be evicted; must not be {@literal null}.
   * @return the {@link VALUE value} currently mapped to the given {@link KEY key}
   * or return {@literal null} if the given {@link KEY key} was not mapped to a {@link VALUE value}.
   * @see #getAndEvict(Comparable, Object)
   * @see #get(Comparable)
   * @see #evict(Comparable)
   */
  default VALUE getAndEvict(KEY key) {

    return ThreadUtils.runAtomically(getLock(), () -> {
      VALUE existingValue = get(key);
      evict(key);
      return existingValue;
    });
  }

  /**
   * Gets the {@link VALUE value} stored in this {@link Cache} mapped to the given {@link KEY key}
   * then removes the mapping if the {@link VALUE existing value} is equal to
   * the given {@link VALUE expected value}.
   *
   * @param key {@link KEY key} in which the mapping will be evicted; must not be {@literal null}.
   * @param expectedValue {@link VALUE value} expected to match the {@link VALUE existing value}
   * for the given {@link KEY key}.
   * @return the {@link VALUE existing value} currently mapped to the given {@link KEY key}
   * or return {@literal null} if the given {@link KEY key} was not mapped to a {@link VALUE value}.
   * @see #getAndEvict(Comparable)
   * @see #get(Comparable)
   * @see #evict(Comparable)
   */
  default VALUE getAndEvict(KEY key, VALUE expectedValue) {

    return ThreadUtils.runAtomically(getLock(), () -> {

      VALUE existingValue = get(key);

      if (ObjectUtils.equals(existingValue, expectedValue)) {
        evict(key);
      }

      return existingValue;
    });
  }

  /**
   * Gets the {@link VALUE existing value} stored in this {@link Cache} mapped to the given {@link KEY key}
   * then maps the {@link VALUE new value} to the given {@link KEY key}.
   * <p>
   * The {@link VALUE new value} will be {@link #put(Comparable, Object)} (stored) in this {@link Cache}
   * regardless if the given {@link KEY key} was mapped to a {@link VALUE} or not.
   *
   * @param key {@link KEY key} in which the mapping will be changed; must not be {@literal null}.
   * @param newValue {@link VALUE new value} to map to the given {@link KEY key}.
   * @return the {@link VALUE existing value} mapped to the given {@link KEY key}
   * or return {@literal null} if the given {@link KEY key} was not mapped to a {@link VALUE value}.
   * @see #get(Comparable)
   * @see #put(Comparable, Object)
   */
  default VALUE getAndPut(KEY key, VALUE newValue) {

    return ThreadUtils.runAtomically(getLock(), () -> {
      VALUE existingValue = get(key);
      put(key, newValue);
      return existingValue;
    });
  }

  /**
   * Gets the {@link VALUE existing value} stored in this {@link Cache} mapped to the given {@link KEY key}
   * then maps the {@link VALUE new value} to the given {@link KEY key} only if the given {@link KEY key}
   * was currently mapped to a {@link VALUE value} in the first place.
   * <p>
   * The {@link VALUE new value} will be {@link #put(Comparable, Object)} (stored) in this {@link Cache}
   * if and only if (iff) the given {@link KEY key} was mapped to an {@link VALUE existing value}.
   *
   * @param key {@link KEY key} in which the mapping will be changed; must not be {@literal null}.
   * @param newValue {@link VALUE new value} to map to the given {@link KEY key}.
   * @return the {@link VALUE existing value} mapped to the given {@link KEY key}
   * or return {@literal null} if the given {@link KEY key} was not mapped to a {@link VALUE value}.
   * @see #getAndReplace(Comparable, Object, Object)
   * @see #contains(Comparable)
   * @see #get(Comparable)
   * @see #put(Comparable, Object)
   */
  default VALUE getAndReplace(KEY key, VALUE newValue) {

    return ThreadUtils.runAtomically(getLock(), () -> {

      if (contains(key)) {
        VALUE existingValue = get(key);
        put(key, newValue);
        return existingValue;
      }

      return null;
    });
  }

  /**
   * Gets the {@link VALUE existing value} stored in this {@link Cache} mapped to the given {@link KEY key}
   * then maps the {@link VALUE new value} to the given {@link KEY key} only if the {@link VALUE existing value}
   * is equal to the {@link VALUE expected value}.
   * <p>
   * The {@link VALUE new value} will be {@link #put(Comparable, Object)} (stored) in this {@link Cache}
   * if and only if (iff) the given {@link KEY key} was mapped to the {@link VALUE expected value}.
   *
   * @param key {@link KEY key} in which the mapping will be changed; must not be {@literal null}.
   * @param expectedValue {@link VALUE value} expected to match the {@link VALUE existing value}
   * mapped to the given {@link KEY key}.
   * @param newValue {@link VALUE new value} to map to the given {@link KEY key}.
   * @return the {@link VALUE existing value} mapped to the given {@link KEY key}
   * or return {@literal null} if the given {@link KEY key} was not mapped to a {@link VALUE value}.
   * @see #getAndReplace(Comparable, Object)
   * @see #contains(Comparable)
   * @see #get(Comparable)
   * @see #put(Comparable, Object)
   */
  default VALUE getAndReplace(KEY key, VALUE expectedValue, VALUE newValue) {

    return ThreadUtils.runAtomically(getLock(), () -> {

      if (contains(key)) {

        VALUE existingValue = get(key);

        if (ObjectUtils.equals(existingValue, expectedValue)) {
          put(key, newValue);
        }

        return existingValue;
      }

      return null;
    });
  }

  /**
   * Gets the {@link Cache.Entry}, key/value mapping from this {@link Cache} for the given {@link KEY}.
   * <p>
   * The {@link Cache.Entry} returned by this method is considered {@literal attached}. Any changes to
   * the key/value mapping in this {@link Cache} are reflected in the {@link Cache.Entry} returned for
   * the given {@link KEY key}.
   *
   * @param key {@link KEY key} for which to return the {@link Cache.Entry} mapping in this {@link Cache}.
   * @return a {@link Cache.Entry} mapping from this {@link Cache} for this given {@link KEY key}
   * or return {@literal null} if this {@link Cache} does not {@link #contains(Comparable) contain} a mapping
   * for the given {@link KEY key}.
   * @see org.cp.elements.data.caching.Cache.Entry
   * @see #contains(Comparable)
   */
  default @Nullable Cache.Entry<KEY, VALUE> getEntry(KEY key) {

    Supplier<Cache.Entry<KEY, VALUE>> cacheEntrySupplier = () -> new Cache.Entry<>() {

      private <T> T assertCacheEntryExists(T returnValue) {
        Assert.state(contains(getKey()), () -> "Cache [%s] no longer contains key [%s]".formatted(getName(), getKey()));
        return returnValue;
      }

      @Override
      public KEY getKey() {
        return key;
      }

      @Override
      public @NotNull Cache<KEY, VALUE> getSource() {
        return assertCacheEntryExists(Cache.this);
      }
    };

    return contains(key) ? cacheEntrySupplier.get() : null;
  }

  /**
   * Returns all {@link KEY keys} in this {@link Cache}.
   *
   * @return a {@link Set} containing all the {@link KEY keys} from this {@link Cache}.
   * Returns an {@link Set#isEmpty() empty Set} if there are no {@link Cache.Entry entries}
   * in this {@link Cache}.
   * @see java.util.Set
   * @see #stream()
   */
  default Set<KEY> keys() {

    return stream()
      .map(Cache.Entry::getKey)
      .collect(Collectors.toSet());
  }

  /**
   * Puts the {@link VALUE value} in this {@link Cache} mapped to the given {@link KEY key}.
   *
   * @param key {@link KEY} mapped to the {@link VALUE value}; must not be {@literal null}.
   * @param value {@link VALUE} put in this {@link Cache} mapped to the {@link KEY key}.
   * @throws IllegalArgumentException if the {@link KEY key} is {@literal null}.
   * @see #get(Comparable)
   */
  void put(KEY key, VALUE value);

  /**
   * Puts the given, required {@link Cache.Entry} in this {@link Cache}.
   *
   * @param cacheEntry {@link Cache.Entry} to stare in this {@link Cache};
   * must not be {@literal null}.
   * @throws IllegalArgumentException if {@link Cache.Entry} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache.Entry
   */
  default void put(@NotNull Cache.Entry<KEY, VALUE> cacheEntry) {

    Assert.notNull(cacheEntry, "The Cache.Entry to put in this cache is required");

    ThreadUtils.runAtomically(getLock(), () -> put(cacheEntry.getKey(), cacheEntry.getValue()));
  }

  /**
   * Puts the {@link Identifiable object} in this {@link Cache} by mapping the {@link Identifiable#getId() ID}
   * of the {@link Identifiable object} to the {@link Identifiable object} itself.
   * <p>
   * The {@link Identifiable} object must be an instance of {@link VALUE}.
   *
   * @param entity {@link Identifiable object} to {@literal put} in this {@link Cache}; must not be {@literal null}.
   * @throws ClassCastException if the {@link Identifiable object} is not an instance of {@link VALUE}.
   * @throws IllegalArgumentException if the {@link Identifiable object} or its {@link Identifiable#getId() ID}
   * is {@literal null}.
   * @see org.cp.elements.lang.Identifiable
   * @see #put(Comparable, Object)
   */
  @SuppressWarnings("unchecked")
  default void put(@NotNull Identifiable<KEY> entity) {

    Assert.notNull(entity, "Entity to cache is required");
    Assert.notNull(entity.getId(), "ID of entity to cache is required");

    ThreadUtils.runAtomically(getLock(), () -> put(entity.getId(), (VALUE) entity));
  }

  /**
   * Puts all {@link Identifiable objects} in this {@link Cache} by mapping each {@link Identifiable object's}
   * {@link Identifiable#getId() ID} to the {@link Identifiable object} itself.
   * <p>
   * All {@link Identifiable objects} must be an instance of {@link VALUE}.
   * <p>
   * WARNING: This putAll(..) operation is not atomic.
   * <p>
   * For example, if any {@link Identifiable object} in the array is {@literal null}, then it will cause
   * an {@link IllegalArgumentException} to be thrown. However, any {@link Identifiable object} that came before
   * the {@literal null} value will still be persisted to this {@link Cache}.
   *
   * @param entities array of {@link Identifiable objects} to put in this {@link Cache}.
   * @throws ClassCastException if any {@link Identifiable object} is not an instance of {@link VALUE}.
   * @throws IllegalArgumentException if any {@link Identifiable object} or its {@link Identifiable#getId() ID}
   * is {@literal null}.
   * @see org.cp.elements.lang.Identifiable
   * @see #putAll(Iterable)
   * @see #put(Identifiable)
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  default void putAll(Identifiable<KEY>... entities) {

    if (ArrayUtils.isNotEmpty(entities)) {
      ThreadUtils.runAtomically(getLock(), () -> Arrays.stream(entities)
        .filter(Objects::nonNull)
        .forEach(this::put));
    }
  }

  /**
   * Puts all {@link Identifiable objects} in this {@link Cache} by mapping each {@link Identifiable object's}
   * {@link Identifiable#getId() ID} to the {@link Identifiable object} itself.
   * <p>
   * All {@link Identifiable objects} must be an instance of {@link VALUE}.
   * <p>
   * WARNING: This putAll(..) operation is not atomic.
   * <p>
   * For example, if any {@link Identifiable object} in the {@link Iterable} is {@literal null}, then it will cause
   * an {@link IllegalArgumentException} to be thrown. However, any {@link Identifiable object} that came before
   * the {@literal null} value will still be persisted to this {@link Cache}.
   *
   * @param entities {@link Iterable} of {@link Identifiable objects} to put in this {@link Cache}.
   * @throws ClassCastException if any {@link Identifiable object} is not an instance of {@link VALUE}.
   * @throws IllegalArgumentException if any {@link Identifiable object} or its {@link Identifiable#getId() ID}
   * is {@literal null}.
   * @see org.cp.elements.lang.Identifiable
   * @see #putAll(Identifiable[])
   * @see #put(Identifiable)
   * @see java.lang.Iterable
   */
  @NullSafe
  default void putAll(Iterable<Identifiable<KEY>> entities) {

    if (CollectionUtils.isNotEmpty(entities)) {
      ThreadUtils.runAtomically(getLock(), () -> StreamUtils.stream(entities)
        .filter(Objects::nonNull)
        .forEach(this::put));
    }
  }

  /**
   * Puts the {@link KEY key} mapped to the {@link VALUE value} in this {@link Cache}
   * only if a {@link Cache.Entry} with the given {@link KEY key} does not already exist.
   *
   * @param key {@link KEY} used to map the {@link VALUE value}; must not be {@literal null}.
   * @param value {@link VALUE} to put in this {@link Cache} mapped to the given {@link KEY key}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws IllegalArgumentException if {@link KEY key} is {@literal null}.
   * @see #putIfPresent(Comparable, Object)
   * @see #contains(Comparable)
   * @see #get(Comparable)
   * @see #put(Comparable, Object)
   */
  default VALUE putIfAbsent(KEY key, VALUE value) {

    Assert.notNull(key, "Key is required");

    return ThreadUtils.runAtomically(getLock(), () -> {
      if (!contains(key)) {
        put(key, value);
        return null;
      }
      else {
        return get(key);
      }
    });
  }

  /**
   * Puts the {@link KEY} and {@link VALUE} from the given {@link Cache.Entry} in this {@link Cache} if absent.
   *
   * @param cacheEntry {@link Cache.Entry} containing the {@link KEY} and {@link VALUE} to put in this {@link Cache}.
   * @return the existing {@link VALUE} mapped to the {@link Cache.Entry} {@link KEY} in this {@link Cache}.
   * @throws IllegalArgumentException if {@link Cache.Entry} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache.Entry
   * @see #putIfAbsent(Comparable, Object)
   */
  default VALUE putIfAbsent(@NotNull Cache.Entry<KEY, VALUE>  cacheEntry) {

    Assert.notNull(cacheEntry, "Cache.Entry to put when absent is required");

    return ThreadUtils.runAtomically(getLock(), () -> putIfAbsent(cacheEntry.getKey(), cacheEntry.getValue()));
  }

  /**
   * Puts the {@link Identifiable object} in this {@link Cache} mapped to its {@link Identifiable#getId() ID}
   * iff the {@link Identifiable object} is not {@literal null} and the {@link Identifiable object}
   * is not already present in this {@link Cache}.
   *
   * @param entity {@link Identifiable object} to put in this {@link Cache}; must not be {@literal null}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws ClassCastException if the {@link Identifiable object} is not an instance of {@link VALUE}.
   * @throws IllegalArgumentException if the {@link Identifiable object} or its {@link Identifiable#getId() ID}
   * is {@literal null}.
   * @see org.cp.elements.lang.Identifiable
   * @see #putIfAbsent(Comparable, Object)
   */
  @SuppressWarnings("unchecked")
  default VALUE putIfAbsent(@NotNull Identifiable<KEY> entity) {

    Assert.notNull(entity, "Entity to cache is required");

    KEY entityId = entity.getId();

    Assert.notNull(entityId, "ID of the entity to cache is required");

    return putIfAbsent(entityId, (VALUE) entity);
  }

  /**
   * Puts the {@link KEY key} mapped to the {@link VALUE new value} in this {@link Cache}
   * only if an {@link Cache.Entry} with the given {@link KEY key} already exists in this {@link Cache}.
   *
   * @param key {@link KEY key} mapped to the {@link VALUE new value} in this {@link Cache};
   * must not be {@literal null}.
   * @param newValue {@link VALUE new value} replacing the {@link VALUE existing value}
   * in this {@link Cache} mapped to the given {@link KEY key}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws IllegalArgumentException if the {@link KEY key} is {@literal null}.
   * @see #putIfAbsent(Comparable, Object)
   * @see #contains(Comparable)
   * @see #get(Comparable)
   * @see #put(Comparable, Object)
   */
  default VALUE putIfPresent(KEY key, VALUE newValue) {

    Assert.notNull(key, "Key is required");

    return ThreadUtils.runAtomically(getLock(), () -> {

      if (contains(key)) {
        VALUE existingValue = get(key);
        put(key, newValue);
        return existingValue;
      }

      return null;
    });
  }

  /**
   * Puts the {@link KEY} and {@link VALUE} from the given {@link Cache.Entry} in this {@link Cache} if present.
   *
   * @param cacheEntry {@link Cache.Entry} containing the {@link KEY} and {@link VALUE} to put in this {@link Cache}.
   * @return the existing {@link VALUE} mapped to the {@link Cache.Entry} {@link KEY} in this {@link Cache}.
   * @throws IllegalArgumentException if {@link Cache.Entry} is {@literal null}.
   * @see org.cp.elements.data.caching.Cache.Entry
   * @see #putIfPresent(Comparable, Object)
   */
  default VALUE putIfPresent(Cache.Entry<KEY, VALUE> cacheEntry) {

    Assert.notNull(cacheEntry, "Cache.Entry to put if present is required");

    return ThreadUtils.runAtomically(getLock(), () -> putIfPresent(cacheEntry.getKey(), cacheEntry.getValue()));
  }

  /**
   * Puts the {@link Identifiable object} in this {@link Cache} mapped to its {@link Identifiable#getId() ID}
   * iff if the {@link Identifiable object} is already present in this {@link Cache}.
   *
   * @param newEntity {@link Identifiable object} replacing the {@link Identifiable existing object}
   * with the same {@link Identifiable#getId() ID}; must not be {@literal null}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws ClassCastException if the {@link Identifiable} object is not an instance of {@link VALUE}.
   * @throws IllegalArgumentException if the {@link Identifiable object} is {@literal null}.
   * @see #putIfPresent(Comparable, Object)
   * @see org.cp.elements.lang.Identifiable
   */
  @SuppressWarnings("unchecked")
  default VALUE putIfPresent(Identifiable<KEY> newEntity) {

    Assert.notNull(newEntity, "Entity to cache is required");

    KEY entityId = newEntity.getId();

    return putIfPresent(entityId, (VALUE) newEntity);
  }

  /**
   * Determines the number of {@link Cache.Entry entries} contained in this {@link Cache}.
   *
   * @return an {@link Integer} value with the number of {@link Cache.Entry entries} contained in this {@link Cache}.
   * @see #iterator()
   * @see #isEmpty()
   */
  default long size() {
    return stream().count();
  }

  /**
   * {@link Stream} the {@link Cache.Entry Entries} in this {@link Cache}.
   *
   * @return {@link Stream} of the {@link Cache.Entry Entries} in this {@link Cache}.
   * @see org.cp.elements.data.caching.Cache.Entry
   * @see java.util.stream.Stream
   */
  default Stream<Cache.Entry<KEY, VALUE>> stream() {
    return StreamUtils.stream(this);
  }

  /**
   * Returns this {@link Cache} as an instance of {@link Map}.
   *
   * @return a {@link Map} containing all the {@link Cache.Entry entries} in this {@link Cache}.
   * @see java.util.Map
   * @see #keys()
   */
  @NullSafe
  default @NotNull Map<KEY, VALUE> toMap() {
    return ThreadUtils.runAtomically(getLock(), () ->
      CollectionUtils.nullSafeSet(keys()).stream().collect(Collectors.toMap(key -> key, this::get)));
  }

  /**
   * Abstract Data Type (ADT) modeling an {@literal entry} in the {@link Cache}
   * mapping a {@link KEY key} to a {@link VALUE value}.
   *
   * @param <KEY> {@link Comparable} {@link Class type} of the {@literal keys} used for mapping by this {@link Cache}.
   * @param <VALUE> {@link Class type} of {@link VALUE values} stored in this {@link Cache}.
   * @see java.lang.FunctionalInterface
   * @see org.cp.elements.lang.Sourced
   * @see java.lang.Comparable
   */
  @FunctionalInterface
  interface Entry<KEY extends Comparable<KEY>, VALUE>
      extends Comparable<Cache.Entry<KEY, VALUE>>, Sourced<Cache<KEY, VALUE>> {

    /**
     * Factory method used to copy a given, required {@link Cache.Entry} as a new instance.
     * <p>
     * The resulting {@link Cache.Entry} will be {@link #materialize() materialized} and therefore,
     * will effectively be independent of the {@link Cache.Entry} on which the new instance is based.
     * The {@link #setValue(Object)} method is not supported.
     *
     * @param <KEY> {@link Class type} of the {@link Cache.Entry#getKey() key}.
     * @param <VALUE> {@link Class type} of the {@link Cache.Entry#getValue()} value}.
     * @param cacheEntry {@link Cache.Entry} to copy; must not be {@literal null}.
     * @return a new {@link Cache.Entry} copy.
     * @throws IllegalArgumentException if the {@link Cache.Entry} to copy is {@literal null}.
     */
    static @NotNull <KEY extends Comparable<KEY>, VALUE> Cache.Entry<KEY, VALUE> copy(
        @NotNull Cache.Entry<KEY, VALUE> cacheEntry) {

      Assert.notNull(cacheEntry, "Cache.Entry to copy is required");

      return new Entry<>() {

        private final Cache<KEY, VALUE> source = cacheEntry.getSource();

        private final KEY cacheEntryKey = cacheEntry.getKey();
        private final VALUE cacheEntryValue = cacheEntry.getValue();

        @Override
        public @NotNull KEY getKey() {
          return this.cacheEntryKey;
        }

        @Override
        public @NotNull Cache<KEY, VALUE> getSource() {
          return this.source;
        }

        @Override
        public @Nullable VALUE getValue() {
          return this.cacheEntryValue;
        }

        @Override
        public void setValue(VALUE value) {
          throw newUnsupportedOperationException("Value for Cache.Entry(%s) copy cannot be set", getKey());
        }

        @Override
        public @NotNull Entry<KEY, VALUE> materialize() {
          return this;
        }
      };
    }

    /**
     * Factory method used to convert the given, required {@link Map.Entry} into a {@link Cache.Entry}.
     *
     * @param <KEY> {@link Class type} of the {@link Map.Entry} key.
     * @param <VALUE> {@link Class type} of the {@link Map.Entry} value.
     * @param mapEntry {@link Map.Entry} to copy and convert into a {@link Cache.Entry};
     * must not be {@literal null}.
     * @return a new {@link Cache.Entry}.
     * @throws IllegalArgumentException if the {@link Map.Entry} is {@literal null}.
     * @see java.util.Map.Entry
     */
    static @NotNull <KEY extends Comparable<KEY>, VALUE> Cache.Entry<KEY, VALUE> from(
        @NotNull Map.Entry<KEY, VALUE> mapEntry) {

      Assert.notNull(mapEntry, "Map.Entry to convert is required");

      return new Entry<>() {

        private final KEY mapEntryKey = mapEntry.getKey();
        private final VALUE mapEntryValue = mapEntry.getValue();

        @Override
        public KEY getKey() {
          return this.mapEntryKey;
        }

        @Override
        public @Nullable Cache<KEY, VALUE> getSource() {
          return null;
        }

        @Override
        public VALUE getValue() {
          return this.mapEntryValue;
        }

        @Override
        public void setValue(VALUE value) {
          throw newUnsupportedOperationException("Value for Cache.Entry(%s) sourced from Map.Entry cannot be set",
            getKey());
        }

        @Override
        public Entry<KEY, VALUE> materialize() {
          return this;
        }
      };
    }

    /**
     * Gets the {@link KEY key} from this {@link Cache.Entry}.
     *
     * @return the {@link KEY key} from this {@link Cache.Entry}.
     */
    KEY getKey();

    /**
     * Gets the {@link Cache source} of this {@link Entry}.
     *
     * @return the {@link Cache source} of this {@link Entry}.
     * @throws CacheNotFoundException by default.
     */
    @Override
    default @NotNull Cache<KEY, VALUE> getSource() {
      throw newCacheNotFoundException("Cache cannot be determined");
    }

    /**
     * Gets the {@link VALUE value} from this {@link Cache.Entry}.
     * <p>
     * Returns {@literal null} by default.
     *
     * @return the {@link VALUE value} from this {@link Cache.Entry}.
     * @see org.cp.elements.data.caching.Cache#get(Comparable)
     * @see #getSource()
     * @see #getKey()
     */
    default @Nullable VALUE getValue() {
      return getSource().get(getKey());
    }

    /**
     * Gets the current {@link VALUE value} of this {@link Cache.Entry} or returns the given {@link VALUE default value}
     * if the {@link VALUE} of this {@link Cache.Entry} is {@literal null}.
     *
     * @param defaultValue {@link VALUE default value} to return if this {@link Cache.Entry entry's}
     * {@link #getValue() value} is {@literal null}.
     * @return the {@link VALUE value} of this {@link Cache.Entry} or the given {@link VALUE default value}
     * if the {@link VALUE} of this {@link Cache.Entry} is {@literal null}.
     * @see #getValue()
     */
    @SuppressWarnings("unchecked")
    default @Nullable VALUE getValue(@Nullable VALUE defaultValue) {
      return ObjectUtils.returnFirstNonNullValue(getValue(), defaultValue);
    }

    /**
     * Returns the {@link #getValue() value} of this {@link Cache.Entry} wrapped in an {@link Optional}.
     *
     * @return the {@link #getValue() value} of this {@link Cache.Entry} wrapped in an {@link Optional}.
     * @see java.util.Optional
     * @see #getValue()
     */
    default Optional<VALUE> getOptionalValue() {
      return Optional.ofNullable(getValue());
    }

    /**
     * Sets the {@link VALUE value} of this {@link Cache.Entry} and key/value mapping in the {@link Cache}.
     *
     * @param value new {@link VALUE} to set the {@link Cache.Entry} and key/value mapping to in the {@link Cache}.
     * @see org.cp.elements.data.caching.Cache#put(Comparable, Object)
     * @see #getSource()
     * @see #getKey()
     */
    default void setValue(@Nullable VALUE value) {
      getSource().put(getKey(), value);
    }

    /**
     * Compares this {@link Cache.Entry} to the given, required {@link Cache.Entry} to determine sort order.
     *
     * @param cacheEntry {@link Cache.Entry} to compare with this {@link Cache.Entry}; must not be {@literal null}.
     * @return an {@link Integer} value reflecting the order of this {@link Cache.Entry} to the given,
     * required {@link Cache.Entry}.
     * @throws NullPointerException if the given {@link Cache.Entry} is {@literal null}.
     * @see java.lang.Comparable#compareTo(Object)
     */
    @Override
    default int compareTo(@NotNull Cache.Entry<KEY, VALUE> cacheEntry) {
      return getKey().compareTo(cacheEntry.getKey());
    }

    /**
     * Materializes this {@link Cache.Entry} into a static, unchanging {@link KEY key} / {@link VALUE value} pair.
     *
     * @return this {@link Cache.Entry} as a static, unchanging {@link KEY key} / {@link VALUE value} pair.
     * @see org.cp.elements.data.caching.Cache.Entry#copy(Entry)
     */
    default @NotNull Cache.Entry<KEY, VALUE> materialize() {
      return copy(this);
    }
  }
}
