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

import static org.cp.elements.util.MapUtils.newMapEntry;

import java.util.Map;
import java.util.Optional;

/**
 * The {@link KeyValue} interface defines a contract for a data structure modeling both a key and a value
 * mapped to the key.
 *
 * @author John Blum
 * @see java.util.Map
 * @since 1.0.0
 */
public interface KeyValue<KEY, VALUE> {

  /**
   * Determines whether the {@link #getKey() key} has a value.
   *
   * @return a boolean value indicating whether the {@link #getKey() key} has a value.
   * @see #getValue(Object)
   */
  boolean isSet();

  /**
   * Return the key in the key/value mapping.
   *
   * @return the key.
   */
  KEY getKey();

  /**
   * Return the value as a null-safe {@link Optional} value in the key/value mapping.
   *
   * @return the optional value.
   * @see java.util.Optional
   * @see #getValue(Object)
   */
  Optional<VALUE> getValue();

  /**
   * Return the value in the key/value mapping.
   *
   * If the value is {@literal null}, then return the {@code defaulValue}.
   *
   * @param defaultValue default value to return if the value is {@literal null}.
   * @return the value of the key/value mapping or {@code defaultValue} if value is {@literal null}.
   * @see #getValue()
   */
  VALUE getValue(VALUE defaultValue);

  /**
   * Returns this {@link SimpleKeyValue} object as an immutable instance of {@link Map.Entry}.
   *
   * @return this {@link SimpleKeyValue} object as an immutable instance of {@link Map.Entry}.
   * @see org.cp.elements.util.MapUtils#newMapEntry(Object, Object)
   * @see java.util.Map.Entry
   */
  default Map.Entry<KEY, VALUE> asMapEntry() {
    return newMapEntry(getKey(), getValue().orElse(null));
  }
}
