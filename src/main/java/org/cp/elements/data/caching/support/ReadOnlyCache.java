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

import java.util.Collections;
import java.util.Iterator;
import java.util.Set;

import org.cp.elements.data.caching.AbstractCache;
import org.cp.elements.data.caching.Cache;

/**
 * {@link ReadOnlyCache} is an abstract class implementing {@link Cache} supporting only
 * immutable {@link Cache} operations, effectively making this implementation a Read-only {@link Cache}.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @see org.cp.elements.data.caching.AbstractCache
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ReadOnlyCache<KEY extends Comparable<KEY>, VALUE> extends AbstractCache<KEY, VALUE> {

  /**
   * Determines whether this {@link Cache} contains an entry mapped with the given {@link KEY key}.
   *
   * @param key {@link KEY key} to evaluate.
   * @return a boolean value indicating whether this {@link Cache} contains an entry
   * mapped with the given {@link KEY key}.
   */
  @Override
  public boolean contains(KEY key) {
    return false;
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
    return null;
  }

  /**
   * Returns an {@link Iterator} iterating over the value in this {@link Cache}.
   *
   * @return an {@link Iterator} iterating over the value in this {@link Cache}.
   * @see java.util.Iterator
   */
  @Override
  public Iterator<VALUE> iterator() {
    return Collections.emptyIterator();
  }

  /**
   * Returns all the {@link KEY keys} in this {@link Cache}.
   *
   * @return a {@link Set} containing all of the {@link KEY keys} in this {@link Cache}.
   * @see java.util.Set
   */
  @Override
  public Set<KEY> keys() {
    return Collections.emptySet();
  }

  /**
   * Determines the number of entries contained in this {@link Cache}.
   *
   * @return an integer value with the number of entries contained in this {@link Cache}.
   * @see #isEmpty()
   */
  @Override
  public int size() {
    return 0;
  }
}
