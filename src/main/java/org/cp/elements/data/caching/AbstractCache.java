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

import java.util.Iterator;
import java.util.Set;

/**
 * {@link AbstractCache} is an abstract base class supporting the implementation of different {@link Cache Caches}.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @see org.cp.elements.data.caching.Cache
 * @since 1.0.0
 */
public abstract class AbstractCache<KEY extends Comparable<KEY>, VALUE> implements Cache<KEY, VALUE> {

  private String name;

  /**
   * Clears the entire contents (all entries) of this {@link Cache}.
   *
   * @see #evictAll(Iterable)
   * @see #keys()
   */
  @Override
  public void clear() {
    throw new UnsupportedOperationException("Clear is not supported");
  }

  /**
   * Determines whether this {@link Cache} contains an entry mapped with the given {@link KEY key}.
   *
   * @param key {@link KEY key} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains an entry
   * mapped with the given {@link KEY key}.
   */
  @Override
  public boolean contains(KEY key) {
    throw newUnsupportedOperationException("Contains is not supported");
  }

  /**
   * Removes the entry mapped to the given {@link KEY key} from this {@link Cache}.
   *
   * @param key {@link KEY key} identifying the entry to remove from this {@link Cache}.
   */
  @Override
  public void evict(KEY key) {
    throw newUnsupportedOperationException("Eviction is not supported");
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
  @Override
  public VALUE get(KEY key) {
    throw newUnsupportedOperationException("Get is not supported");
  }

  /**
   * Returns the {@link String name} of this {@link Cache}.
   *
   * The {@link String name} may be {@literal null}.
   *
   * @return the {@link String name} of this {@link Cache}; may be {@literal null}.
   */
  @Override
  public String getName() {
    return this.name;
  }

  /**
   * Returns an {@link Iterator} iterating over the value in this {@link Cache}.
   *
   * @return an {@link Iterator} iterating over the value in this {@link Cache}.
   * @see java.util.Iterator
   */
  @Override
  public Iterator<VALUE> iterator() {
    throw newUnsupportedOperationException("Iterating all values is not supported");
  }

  /**
   * Returns all the {@link KEY keys} in this {@link Cache}.
   *
   * @return a {@link Set} containing all of the {@link KEY keys} in this {@link Cache}.
   * @see java.util.Set
   */
  @Override
  public Set<KEY> keys() {
    throw newUnsupportedOperationException("Returning all keys is not supported");
  }

  /**
   * Puts the given {@link VALUE value} mapped to the given {@link KEY key} into this {@link Cache}.
   *
   * @param key {@link KEY} used to map the {@link VALUE value}; must not be {@literal null}.
   * @param value {@link VALUE} put into this {@link Cache} mapped to the given {@link KEY key}.
   * @throws IllegalArgumentException if {@link KEY key} is {@literal null}.
   * @see #get(Comparable)
   */
  @Override
  public void put(KEY key, VALUE value) {
    throw newUnsupportedOperationException("Put is not supported");
  }

  /**
   * Determines the number of entries contained in this {@link Cache}.
   *
   * @return an integer value with the number of entries contained in this {@link Cache}.
   * @see #isEmpty()
   */
  @Override
  public int size() {
    throw newUnsupportedOperationException("Size is not supported");
  }

  /**
   * Builder method to set the {@link String name} of this {@link Cache}.
   *
   * @param <T> {@link Class type} of this {@link Cache}.
   * @param name {@link String} containing the name for this {@link Cache}.
   * @return this {@link Cache}.
   */
  @SuppressWarnings("unchecked")
  public <T extends AbstractCache<KEY, VALUE>> T named(String name) {
    this.name = name;
    return (T) this;
  }
}
