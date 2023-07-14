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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.Immutable;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.annotation.ThreadSafe;

/**
 * A {@link ThreadSafe Thread-safe} {@link KeyValue} data structure implementation modeling both a {@link Object key}
 * and a {@link Object value}.
 *
 * @author John Blum
 * @param <KEY> {@link Class type} of the key.
 * @param <VALUE> {@link Class type} of the value.
 * @see java.util.Map
 * @see java.util.Map.Entry
 * @see org.cp.elements.data.struct.KeyValue
 * @see org.cp.elements.lang.annotation.Immutable
 * @see org.cp.elements.lang.annotation.ThreadSafe
 * @since 1.0.0
 */
@Immutable
@ThreadSafe
@SuppressWarnings("unused")
public class SimpleKeyValue<KEY, VALUE> implements KeyValue<KEY, VALUE> {

  protected static final String KEY_VALUE_TO_STRING = "%1$s = %2$s";

  /**
   * Factory method used to construct a new {@link SimpleKeyValue} initialized from the given,
   * required {@link Map.Entry}.
   *
   * @param <KEY> {@link Class type} of the key.
   * @param <VALUE> {@link Class type} of the value.
   * @param mapEntry {@link Map.Entry} used to construct and initialize a new instance of {@link SimpleKeyValue}.
   * @return a new {@link SimpleKeyValue} initialized from the given, required {@link Map.Entry}.
   * @throws IllegalArgumentException if the {@link Map.Entry} is {@literal null}.
   * @see #newKeyValue(Object, Object)
   * @see java.util.Map.Entry
   */
  public static @NotNull <KEY, VALUE> SimpleKeyValue<KEY, VALUE> from(@NotNull Map.Entry<KEY, VALUE> mapEntry) {

    Assert.notNull(mapEntry, "Map.Entry is required");

    return newKeyValue(mapEntry.getKey(), mapEntry.getValue());
  }

  /**
   * Factory method used to construct a new {@link SimpleKeyValue} initialized with the given, required {@link KEY key}
   * and a {@literal null} value.
   *
   * @param <KEY> {@link Class type} of the key.
   * @param <VALUE> {@link Class type} of the value.
   * @param key {@link KEY} in the key/value mapping; must not be {@literal null}.
   * @return a new {@link SimpleKeyValue} initialized with the given, required {@link KEY key}
   * and a {@literal null} value.
   * @throws IllegalArgumentException if the {@link KEY key} is {@literal null}.
   * @see #SimpleKeyValue(Object)
   */
  public static @NotNull <KEY, VALUE> SimpleKeyValue<KEY, VALUE> newKeyValue(@NotNull KEY key) {
    return new SimpleKeyValue<>(key);
  }

  /**
   * Factory method used to construct a new {@link SimpleKeyValue} initialized with the given, required {@link KEY key}
   * and {@link VALUE value}.
   *
   * @param <KEY> {@link Class type} of the key.
   * @param <VALUE> {@link Class type} of the value.
   * @param key {@link KEY key} in the key/value mapping; must not be {@literal null}.
   * @param value {@link VALUE value} in the key/value mapping; may be {@literal null}.
   * @return a new {@link SimpleKeyValue} initialized with the given, required {@link KEY key} and {@link VALUE value}.
   * @throws IllegalArgumentException if the {@link KEY key} is {@literal null}.
   * @see #SimpleKeyValue(Object, Object)
   */
  public static @NotNull <KEY, VALUE> SimpleKeyValue<KEY, VALUE> newKeyValue(@NotNull KEY key, @Nullable VALUE value) {
    return new SimpleKeyValue<>(key, value);
  }

  private final KEY key;

  private final VALUE value;

  /**
   * Constructs a new {@link SimpleKeyValue} initialized with the given, required {@link KEY key}
   * and a {@literal null} value.
   *
   * @param key {@link KEY key} in the key/value mapping; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link KEY key} is {@literal null}.
   * @see #SimpleKeyValue(Object, Object)
   */
  public SimpleKeyValue(@NotNull KEY key) {
    this(key, null);
  }

  /**
   * Constructs a new {@link SimpleKeyValue} initialized with the given, required {@link KEY key}
   * and {@link VALUE value}.
   *
   * @param key {@link KEY key} in the key/value mapping; must not be {@literal null}.
   * @param value {@link VALUE value} in the key/value mapping; may be {@literal null}.
   * @throws IllegalArgumentException if the {@link KEY key} is {@literal null}.
   */
  public SimpleKeyValue(@NotNull KEY key, @Nullable VALUE value) {

    this.key = ObjectUtils.requireObject(key, "Key is required");
    this.value = value;
  }

  /**
   * Returns the {@link KEY key} in this key/value mapping.
   *
   * @return the {@link KEY key}.
   */
  @Override
  public @NotNull KEY getKey() {
    return this.key;
  }

  /**
   * Returns the {@link VALUE value} as a null-safe {@link Optional} value in this key/value mapping.
   *
   * @return an {@link Optional value}.
   * @see java.util.Optional
   * @see #getValue(Object)
   */
  @Override
  public Optional<VALUE> getValue() {
    return Optional.ofNullable(this.value);
  }

  /**
   * Determines whether this {@link SimpleKeyValue} is equal to the given {@link Object}.
   *
   * @param obj {@link Object} to compare for equality with this {@link SimpleKeyValue}.
   * @return a boolean value indicating whether this {@link SimpleKeyValue} is equal to the given {@link Object}.
   * @see java.lang.Object#equals(Object)
   */
  @Override
  public boolean equals(Object obj) {

    if (this == obj) {
      return true;
    }

    if (!(obj instanceof SimpleKeyValue<?, ?> that)) {
      return false;
    }

    if (!getClass().equals(that.getClass())) {
      return false;
    }

    return this.getKey().equals(that.getKey())
      && ObjectUtils.equals(this.getValue(), that.getValue());
  }

  /**
   * Computes the hash code for this {@link SimpleKeyValue}.
   *
   * @return an {@link Integer value} containing the computed hash code of this {@link SimpleKeyValue}.
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    return ObjectUtils.hashCodeOf(getKey(), getValue(null));
  }

  /**
   * Returns a {@link String} representation (view) of this {@link SimpleKeyValue}.
   *
   * @return a {@link String} describing this {@link SimpleKeyValue}.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return String.format(KEY_VALUE_TO_STRING, getKey(), getValue(null));
  }
}
