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

import static org.cp.elements.util.ArrayUtils.nullSafeArray;
import static org.cp.elements.util.CollectionUtils.nullSafeIterable;
import static org.cp.elements.util.MapUtils.nullSafeMap;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;

/**
 * The {@link Cache} interface is an Abstract Data Type (ADT) defining a cache data structure,
 * mapping keys to values in-memory for quick access.
 *
 * Caches are used in use cases when, given identical input, the data access operation returns
 * the same output.
 *
 * @author John Blum
 * @param <KEY> {@link Class type} of the keys used by this {@link Cache}.
 * @param <VALUE> {@link Class type} of the values stored by this {@link Cache}.
 * @see java.lang.Comparable
 * @see java.lang.Iterable
 * @see java.util.Map
 * @see org.cp.elements.data.caching.provider.ConcurrentMapCache
 * @see org.cp.elements.data.caching.AbstractCache
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.Nameable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Cache<KEY extends Comparable<KEY>, VALUE> extends Iterable<VALUE>, Nameable<String> {

  /**
   * Determines whether this {@link Cache} contains any entries.
   *
   * @return a boolean value indicating whether this {@link Cache} contains any entries.
   * @see #size()
   */
  @NullSafe
  default boolean isEmpty() {
    return size() == 0;
  }

  /**
   * Clears the entire contents (all entries) of this {@link Cache}.
   *
   * @see #evictAll(Iterable)
   * @see #keys()
   */
  @NullSafe
  default void clear() {
    evictAll(keys());
  }

  /**
   * Determines whether this {@link Cache} contains an entry mapped with the given {@link KEY key}.
   *
   * @param key {@link KEY key} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains an entry
   * mapped with the given {@link KEY key}.
   */
  boolean contains(KEY key);

  /**
   * Determines whether this {@link Cache} contains entries for each and every {@link KEY key}.
   *
   * @param keys {@link KEY keys} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains entries for each and every
   * given {@link KEY key}.
   * @see #containsAll(Iterable)
   * @see #contains(Comparable)
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  default boolean containsAll(KEY... keys) {
    return ArrayUtils.isNotEmpty(keys) && Arrays.stream(nullSafeArray(keys)).allMatch(this::contains);
  }

  /**
   * Determines whether this {@link Cache} contains entries for each and every {@link KEY key}.
   *
   * @param keys {@link KEY keys} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains entries for each and every
   * given {@link KEY key}.
   * @see #containsAll(Comparable[])
   * @see #contains(Comparable)
   * @see java.lang.Iterable
   */
  @NullSafe
  default boolean containsAll(Iterable<KEY> keys) {
    return CollectionUtils.isNotEmpty(keys)
      && StreamSupport.stream(nullSafeIterable(keys).spliterator(), false).allMatch(this::contains);
  }

  /**
   * Determines whether this {@link Cache} contains at least 1 entry mapped with any of the given {@link KEY keys}.
   *
   * @param keys {@link KEY keys} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains at least 1 entry
   * mapped with any of the given {@link KEY keys}.
   * @see #containsAny(Iterable)
   * @see #contains(Comparable)
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  default boolean containsAny(KEY... keys) {
    return Arrays.stream(nullSafeArray(keys)).anyMatch(this::contains);
  }

  /**
   * Determines whether this {@link Cache} contains at least 1 entry mapped with any of the given {@link KEY keys}.
   *
   * @param keys {@link KEY keys} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains at least 1 entry
   * mapped with any of the given {@link KEY keys}.
   * @see #containsAny(Iterable)
   * @see #contains(Comparable)
   * @see java.lang.Iterable
   */
  @NullSafe
  default boolean containsAny(Iterable<KEY> keys) {
    return StreamSupport.stream(nullSafeIterable(keys).spliterator(), false).anyMatch(this::contains);
  }

  /**
   * Removes the entry mapped to the given {@link KEY key} from this {@link Cache}.
   *
   * @param key {@link KEY key} identifying the entry to remove from this {@link Cache}.
   */
  void evict(KEY key);

  /**
   * Removes all the entries mapped to the given {@link KEY keys} from this {@link Cache}.
   *
   * @param keys array of {@link KEY keys} identifying entries to remove from this {@link Cache}.
   * @see #evict(Comparable)
   * @see #evictAll(Comparable[])
   */
  @SuppressWarnings("unchecked")
  default void evictAll(KEY... keys) {
    Arrays.stream(nullSafeArray(keys)).forEach(this::evict);
  }

  /**
   * Removes all the entries mapped to the given {@link KEY keys} from this {@link Cache}.
   *
   * @param keys {@link Iterable} of {@link KEY keys} identifying entries to remove from this {@link Cache}.
   * @see #evict(Comparable)
   * @see #evictAll(Comparable[])
   * @see java.lang.Iterable
   */
  default void evictAll(Iterable<KEY> keys) {
    StreamSupport.stream(nullSafeIterable(keys).spliterator(), false).forEach(this::evict);
  }

  /**
   * Caches all entries from given {@link Map} in this {@link Cache}.
   *
   * @param map {@link Map} containing the entries to cache.
   * @see #put(Comparable, Object)
   * @see java.util.Map
   */
  default void from(Map<KEY, VALUE> map) {
    nullSafeMap(map).forEach(this::put);
  }

  /**
   * Gets the {@link VALUE value} mapped to the given {@link KEY} in this {@link Cache}.
   *
   * Returns {@literal null} if the {@link VALUE value} for the given {@link KEY key} is {@literal null},
   * or this {@link Cache} does not contain an entry with the given {@link KEY key}.
   *
   * @param key {@link KEY key} used to lookup the desired {@link VALUE value}.
   * @return the {@link VALUE value} mapped to the given {@link KEY key}.
   * @see #put(Comparable, Object)
   */
  VALUE get(KEY key);

  /**
   * Gets all the {@link VALUE values} in this {@link Cache} mapped to the given {@link KEY keys}.
   *
   * Returns an empty {@link List} if the array of {@link KEY keys} is {@literal null} or {@literal empty}.
   *
   * Given an array of {@link KEY keys}: {@literal keyOne}, {@literal keyTwo}, ..., {@literal keyN}, this method
   * will return a {@link List} of {@link VALUE values}: {@literal valueOne}, {@literal valueTwo}, ...,
   * {@literal valueN} even if the values are {@literal null}.  The {@link List} of {@link VALUE values}
   * matches 1 for 1 for each key in the array in the order that the {@link KEY keys} are given.
   *
   * @param keys {@link KEY keys} used to lookup the desired {@link VALUE values}.
   * @return a {@link List} of {@link VALUE values} for all the given {@link KEY keys}
   * in the order given by the {@link KEY keys}.
   * @see #get(Comparable)
   * @see java.util.List
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  default List<VALUE> getAll(KEY... keys) {
    return Arrays.stream(nullSafeArray(keys)).map(this::get).collect(Collectors.toList());
  }

  /**
   * Gets all the {@link VALUE values} in this {@link Cache} mapped to the given {@link KEY keys}.
   *
   * Returns an empty {@link List} if the {@link Iterable} of {@link KEY keys} is {@literal null} or {@literal empty}.
   *
   * Given an {@link Iterable} of {@link KEY keys}: {@literal keyOne}, {@literal keyTwo}, ..., {@literal keyN},
   * this method will return a {@link List} of {@link VALUE values}: {@literal valueOne}, {@literal valueTwo}, ...,
   * {@literal valueN} even if the values are {@literal null}.  The {@link List} of {@link VALUE values}
   * matches 1 for 1 for each key in the {@link Iterable} in the order that the {@link KEY keys} are given.
   *
   * @param keys {@link KEY keys} used to lookup the desired {@link VALUE values}.
   * @return a {@link List} of {@link VALUE values} for all the given {@link KEY keys}
   * in the order given by the {@link KEY keys}.
   * @see #get(Comparable)
   * @see java.lang.Iterable
   * @see java.util.List
   */
  @NullSafe
  default List<VALUE> getAll(Iterable<KEY> keys) {
    return StreamSupport.stream(nullSafeIterable(keys).spliterator(), false).map(this::get)
      .collect(Collectors.toList());
  }

  /**
   * Returns all the {@link KEY keys} in this {@link Cache}.
   *
   * @return a {@link Set} containing all of the {@link KEY keys} in this {@link Cache}.
   * @see java.util.Set
   */
  @NullSafe
  Set<KEY> keys();

  /**
   * Puts the given {@link VALUE value} mapped to the given {@link KEY key} into this {@link Cache}.
   *
   * @param key {@link KEY} used to map the {@link VALUE value}; must not be {@literal null}.
   * @param value {@link VALUE} put into this {@link Cache} mapped to the given {@link KEY key}.
   * @throws IllegalArgumentException if {@link KEY key} is {@literal null}.
   * @see #get(Comparable)
   */
  void put(KEY key, VALUE value);

  /**
   * Puts the given {@link Identifiable} object into this {@link Cache} by mapping the {@link Identifiable#getId() ID}
   * of the {@link Identifiable} object to the {@link Identifiable} object itself.
   *
   * The {@link Identifiable} object must be an instance of {@link VALUE}.
   *
   * @param entity {@link Identifiable} object to put into this {@link Cache}.
   * @throws ClassCastException if the {@link Identifiable} object is not an instance of {@link VALUE}.
   * @throws IllegalArgumentException if the {@link Identifiable} object or its {@link Identifiable#getId() ID}
   * is {@literal null}.
   * @see org.cp.elements.lang.Identifiable
   * @see #put(Comparable, Object)
   */
  @SuppressWarnings("unchecked")
  default void put(Identifiable<KEY> entity) {

    Assert.notNull(entity, "Entity is required");
    Assert.notNull(entity.getId(), "Entity ID is required");

    put(entity.getId(), (VALUE) entity);
  }

  /**
   * Puts all the {@link Identifiable} objects into this {@link Cache} by mapping each {@link Identifiable} object's
   * {@link Identifiable#getId() ID} to the {@link Identifiable} object itself.
   *
   * All {@link Identifiable} objects must be an instance of {@link VALUE}.
   *
   * Warning: this putAll(..) operation is not atomic.
   *
   * For example, if any {@link Identifiable} object in the array is {@literal null}, then it will cause
   * an {@link IllegalArgumentException} to be thrown.  However, any {@link Identifiable} object that came before
   * the {@literal null} {@link Identifiable} object will still be persisted to this {@link Cache}.
   *
   * @param entities array of {@link Identifiable} objects to put into this {@link Cache}.
   * @throws ClassCastException if any {@link Identifiable} object is not an instance of {@link VALUE}.
   * @throws IllegalArgumentException if any {@link Identifiable} object or their {@link Identifiable#getId() IDs}
   * are {@literal null}.
   * @see org.cp.elements.lang.Identifiable
   * @see #put(Comparable, Object)
   */
  @SuppressWarnings("unchecked")
  default void putAll(Identifiable<KEY>... entities) {
    Arrays.stream(nullSafeArray(entities, Identifiable.class)).forEach(this::put);
  }

  /**
   * Puts all the {@link Identifiable} objects into this {@link Cache} by mapping each {@link Identifiable} object's
   * {@link Identifiable#getId() ID} to the {@link Identifiable} object itself.
   *
   * All {@link Identifiable} objects must be an instance of {@link VALUE}.
   *
   * Warning: this putAll(..) operation is not atomic.
   *
   * For example, if any {@link Identifiable} object  in the iteration is {@literal null}, then it will cause
   * an {@link IllegalArgumentException} to be thrown.  However, any {@link Identifiable} object that came before
   * the {@literal null} {@link Identifiable} object will still be persisted to this {@link Cache}.
   *
   * @param entities {@link Iterable} of {@link Identifiable} objects to put into this {@link Cache}.
   * @throws ClassCastException if any {@link Identifiable} object is not an instance of {@link VALUE}.
   * @throws IllegalArgumentException if any {@link Identifiable} object or their {@link Identifiable#getId() IDs}
   * are {@literal null}.
   * @see org.cp.elements.lang.Identifiable
   * @see #put(Comparable, Object)
   * @see java.lang.Iterable
   */
  default void putAll(Iterable<Identifiable<KEY>> entities) {
    StreamSupport.stream(nullSafeIterable(entities).spliterator(), false).forEach(this::put);
  }

  /**
   * Puts the given {@link KEY key} and {@link VALUE value} in this {@link Cache}
   * iff an entry with given {@link KEY key} does not already exists.
   *
   * @param key {@link KEY} used to map the {@link VALUE value} if not already present; must not be {@literal null}.
   * @param value {@link VALUE} to put into this {@link Cache} mapped to the given {@link KEY key}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws IllegalArgumentException if {@link KEY key} is {@literal null}.
   * @see #contains(Comparable)
   * @see #put(Comparable, Object)
   * @see #putIfPresent(Comparable, Object)
   */
  default VALUE putIfAbsent(KEY key, VALUE value) {

    Assert.notNull(key, "Key is required");

    if (!contains(key)) {
      put(key, value);
      return null;
    }
    else {
      return get(key);
    }
  }

  /**
   * Puts the given {@link Identifiable} object in this {@link Cache} iff the {@link Identifiable} object
   * is not {@literal null} and the {@link Identifiable} object identified by its {@link Identifiable#getId() ID}
   * is not already present in this {@link Cache}.
   *
   * @param entity {@link Identifiable} object to put into this {@link Cache} if not already present.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws ClassCastException if the {@link Identifiable} object is not an instance of {@link VALUE}.
   * @see org.cp.elements.lang.Identifiable
   * @see #contains(Comparable)
   * @see #put(Comparable, Object)
   * @see #putIfPresent(Identifiable)
   */
  @SuppressWarnings("unchecked")
  default VALUE putIfAbsent(Identifiable<KEY> entity) {

    Assert.notNull(entity, "Entity is required");

    KEY entityId = entity.getId();

    Assert.notNull(entityId, "Entity ID is required");

    if (!contains(entityId)) {
      put(entityId, (VALUE) entity);
      return null;
    }
    else {
      return get(entityId);
    }
  }

  /**
   * Puts the {@link VALUE value} in this {@link Cache} mapped to the given {@link KEY key} iff an entry
   * with the given {@link KEY key} already exists in this {@link Cache}.
   *
   * @param key {@link KEY key} used to map the {@link VALUE new value} in this {@link Cache}.
   * @param newValue {@link VALUE new value} replacing the existing value mapped to the given {@link KEY key}
   * in this {@link Cache}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws IllegalArgumentException if {@link KEY key} is {@literal null}.
   * @see #contains(Comparable)
   * @see #put(Comparable, Object)
   * @see #putIfAbsent(Comparable, Object)
   */
  default VALUE putIfPresent(KEY key, VALUE newValue) {

    Assert.notNull(key, "Key is required");

    if (contains(key)) {
      VALUE existingValue = get(key);
      put(key, newValue);
      return existingValue;
    }

    return null;
  }

  /**
   * Puts the given {@link Identifiable} object in this {@link Cache} iff if the {@link Identifiable} object
   * is already present.
   *
   * @param newEntity {@link Identifiable} object replacing the existing object with the same {@link Identifiable#getId() ID}.
   * @return the existing {@link VALUE value} if present, otherwise return {@literal null}.
   * @throws ClassCastException if the {@link Identifiable} object is not an instance of {@link VALUE}.
   * @see org.cp.elements.lang.Identifiable
   * @see #contains(Comparable)
   * @see #put(Comparable, Object)
   * @see #putIfAbsent(Identifiable)
   */
  @SuppressWarnings("unchecked")
  default VALUE putIfPresent(Identifiable<KEY> newEntity) {

    Assert.notNull(newEntity, "Entity is required");

    KEY entityId = newEntity.getId();

    if (contains(entityId)) {
      VALUE existingValue = get(entityId);
      put(entityId, (VALUE) newEntity);
      return existingValue;
    }

    return null;
  }

  /**
   * Determines the number of entries contained in this {@link Cache}.
   *
   * @return an integer value with the number of entries contained in this {@link Cache}.
   * @see #isEmpty()
   */
  int size();

  /**
   * Returns this {@link Cache} as instance of {@link Map}.
   *
   * @return a {@link Map} containing all the entries in this {@link Cache}.
   * @see java.util.Map
   */
  default Map<KEY, VALUE> toMap() {
    return keys().stream().collect(Collectors.toMap(key -> key, this::get));
  }
}
