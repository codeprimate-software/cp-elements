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
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.concurrent.ThreadSafe;

/**
 * The {@link SimpleKeyValue} class is a {@link ThreadSafe Thread-safe} data structure
 * modeling both a key and a value mapped to the key.
 *
 * @author John Blum
 * @see java.util.Map
 * @see java.util.Map.Entry
 * @see org.cp.elements.data.struct.KeyValue
 * @since 1.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class SimpleKeyValue<KEY, VALUE> implements KeyValue<KEY, VALUE> {

  protected static final String KEY_VALUE_TO_STRING = "%1$s = %2$s";

  /**
   * Factory method used to construct a new instance of {@link SimpleKeyValue} initialized from the given,
   * required {@link Map.Entry}.
   *
   * @param <KEY> {@link Class type} of the key.
   * @param <VALUE> {@link Class type} of the value.
   * @param mapEntry {@link Map.Entry} used to construct and initialize an instance of {@link SimpleKeyValue}.
   * @return a new {@link SimpleKeyValue} initialized from the given, required {@link Map.Entry}.
   * @see #newKeyValue(Object, Object)
   * @see java.util.Map.Entry
   */
  public static @NotNull <KEY, VALUE> SimpleKeyValue<KEY, VALUE> from(@NotNull Map.Entry<KEY, VALUE> mapEntry) {

    Assert.notNull(mapEntry, "Map.Entry is required");

    return newKeyValue(mapEntry.getKey(), mapEntry.getValue());
  }

  /**
   * Factory method used to construct a new instance of {@link SimpleKeyValue} initialized with
   * the given, required {@code key} and a {@literal null} value.
   *
   * @param <KEY> {@link Class type} of the key.
   * @param <VALUE> {@link Class type} of the value.
   * @param key key in the key/value mapping; must not be {@literal null}.
   * @return a new  {@link SimpleKeyValue} initialized with the given, required {@code key}
   * and a {@literal null} value.
   * @throws IllegalArgumentException if {@code key} is {@literal null}.
   * @see #SimpleKeyValue(Object)
   */
  public static @NotNull <KEY, VALUE> SimpleKeyValue<KEY, VALUE> newKeyValue(@NotNull KEY key) {
    return new SimpleKeyValue<>(key);
  }

  /**
   * Factory method used to construct a new instance of {@link SimpleKeyValue} initialized with
   * the given, required {@code key} and {@code value}.
   *
   * @param <KEY> {@link Class type} of the key.
   * @param <VALUE> {@link Class type} of the value.
   * @param key key in the key/value mapping; must not be {@literal null}.
   * @param value value in the key/value mapping; may be {@literal null}.
   * @return a new {@link SimpleKeyValue} initialized with the given, required {@code key} and {@code value}.
   * @throws IllegalArgumentException if {@code key} is {@literal null}.
   * @see #SimpleKeyValue(Object, Object)
   */
  public static @NotNull <KEY, VALUE> SimpleKeyValue<KEY, VALUE> newKeyValue(@NotNull KEY key, @Nullable VALUE value) {
    return new SimpleKeyValue<>(key, value);
  }

  private final KEY key;

  private final VALUE value;

  /**
   * Constructs a new instance of {@link SimpleKeyValue} initialized with the given, required {@code key}
   * and a {@literal null} value.
   *
   * @param key key in the key/value mapping; must not be {@literal null}.
   * @throws IllegalArgumentException if {@code key} is {@literal null}.
   * @see #SimpleKeyValue(Object, Object)
   */
  public SimpleKeyValue(@NotNull KEY key) {
    this(key, null);
  }

  /**
   * Constructs a new instance of {@link SimpleKeyValue} initialized with the given, required {@code key}
   * and {@code value}.
   *
   * @param key key in the key/value mapping; must not be {@literal null}.
   * @param value value in the key/value mapping; may be {@literal null}.
   * @throws IllegalArgumentException if {@code key} is {@literal null}.
   */
  public SimpleKeyValue(@NotNull KEY key, @Nullable VALUE value) {

    this.key = ObjectUtils.requireObject(key, "Key is required");
    this.value = value;
  }

  /**
   * Determines whether the {@link #getKey() key} is mapped to a value.
   *
   * @return a boolean value indicating whether the {@link #getKey() key} has a value.
   * @see #getValue(Object)
   */
  @Override
  public boolean isSet() {
    return getValue(null) != null;
  }

  /**
   * Return the key in the key/value mapping.
   *
   * @return the key.
   */
  @Override
  public @NotNull KEY getKey() {
    return this.key;
  }

  /**
   * Return the value as a null-safe {@link Optional} value in the key/value mapping.
   *
   * @return an {@link Optional} value.
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

    if (!(obj instanceof SimpleKeyValue)) {
      return false;
    }

    if (!getClass().equals(obj.getClass())) {
      return false;
    }

    SimpleKeyValue<?, ?> that = (SimpleKeyValue<?, ?>) obj;

    return this.getKey().equals(that.getKey())
      && ObjectUtils.equals(this.getValue(), that.getValue());
  }

  /**
   * Computes the hash code for this {@link SimpleKeyValue}.
   *
   * @return an integer value containing the computed hash code of this {@link SimpleKeyValue}.
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
