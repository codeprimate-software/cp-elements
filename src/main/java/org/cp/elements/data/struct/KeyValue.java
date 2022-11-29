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
package org.cp.elements.data.struct;

import java.util.Map;
import java.util.Optional;

import org.cp.elements.util.MapUtils;

/**
 * The {@link KeyValue} interface defines a contract for a data structure modeling both a {@link Object key}
 * and a {@link Object value} mapped to the required {@link Object key}.
 *
 * @author John Blum
 * @param <KEY> {@link Class type} of the key.
 * @param <VALUE> {@link Class type} of the value.
 * @see java.util.Optional
 * @see java.util.Map
 * @since 1.0.0
 */
@FunctionalInterface
public interface KeyValue<KEY, VALUE> {

  /**
   * Determines whether the {@link #getKey() key} is mapped to a {@link #getValue() value}.
   *
   * @return a boolean value indicating whether the {@link #getKey() key} has a {@link #getValue() value}.
   * @see java.util.Optional#isPresent()
   * @see #getValue()
   */
  default boolean isSet() {
    return getValue().isPresent();
  }

  /**
   * Return the {@link KEY key} in this key/value mapping.
   *
   * @return the {@link KEY key}.
   */
  KEY getKey();

  /**
   * Returns a null-safe {@link Optional} {@link VALUE value} in this key/value mapping.
   *
   * Defaults to {@link Optional#empty()}.
   *
   * @return an {@link Optional} {@link VALUE value}.
   * @see java.util.Optional
   * @see #getValue(Object)
   */
  default Optional<VALUE> getValue() {
    return Optional.empty();
  }

  /**
   * Return a materialized {@link Object value} in this key/value mapping.
   *
   * If the {@link VALUE value} is {@literal null}, then this method will return the type compatible
   * {@link VALUE defaultValue}.
   *
   * @param defaultValue {@link VALUE default value} to return if the {@link VALUE value} is {@literal null}.
   * @return the {@link VALUE value} in this key/value mapping or {@link VALUE defaultValue} if {@link VALUE value}
   * is {@literal null}.
   * @see java.util.Optional#orElse(Object)
   * @see #getValue()
   */
  default VALUE getValue(VALUE defaultValue) {
    return getValue().orElse(defaultValue);
  }

  /**
   * Returns this {@link KeyValue} object as an immutable instance of {@link Map.Entry}.
   *
   * @return this {@link KeyValue} object as an immutable instance of {@link Map.Entry}.
   * @see org.cp.elements.util.MapUtils#newMapEntry(Object, Object)
   * @see java.util.Map.Entry
   * @see #getValue(Object)
   * @see #getKey()
   */
  default Map.Entry<KEY, VALUE> asMapEntry() {
    return MapUtils.newMapEntry(getKey(), getValue(null));
  }
}
