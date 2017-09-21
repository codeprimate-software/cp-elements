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

package org.cp.elements.data.struct;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.util.MapUtils.newMapEntry;

import java.util.Map;
import java.util.Optional;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.concurrent.ThreadSafe;

/**
 * The {@link KeyValue} class is a Thread-safe data structure modeling both a key and a value mapped to the key.
 *
 * @author John Blum
 * @see java.util.Map.Entry
 * @since 1.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class KeyValue<K, V> {

  /**
   * Factory method used to construct a new instance of {@link KeyValue} initialized with the given {@code key}
   * and no value.
   *
   * @param <K> {@link Class} type of the key.
   * @param <V> {@link Class} type of the value.
   * @param key key in the key/value mapping. Must not be {@literal null}.
   * @return a new instance of {@link KeyValue} initialized with the given {@code key} with no value.
   * @throws IllegalArgumentException if {@code key} is {@literal null}.
   * @see #KeyValue(Object)
   */
  public static <K, V> KeyValue<K, V> newKeyValue(K key) {
    return new KeyValue<>(key);
  }

  /**
   * Factory method used to construct a new instance of {@link KeyValue} initialized with the given {@code key}
   * and {@code value}.
   *
   * @param <K> {@link Class} type of the key.
   * @param <V> {@link Class} type of the value.
   * @param key key in the key/value mapping. Must not be {@literal null}.
   * @param value value in the key/value mapping. May be {@literal null}.
   * @return a new instance of {@link KeyValue} initialized with the given {@code key} with no value.
   * @throws IllegalArgumentException if {@code key} is {@literal null}.
   * @see #KeyValue(Object, Object)
   */
  public static <K, V> KeyValue<K, V> newKeyValue(K key, V value) {
    return new KeyValue<>(key, value);
  }

  /**
   * Factory method to construct a new instance of {@link KeyValue} initialized from the given {@link Map.Entry}.
   *
   * @param <K> {@link Class} type of the key.
   * @param <V> {@link Class} type of the value.
   * @param mapEntry {@link Map.Entry} used to construct and initialize an instance of {@link KeyValue}.
   * @return a new instance of {@link KeyValue} initialized from the the given {@link Map.Entry}.
   * @see java.util.Map.Entry
   */
  public static <K, V> KeyValue<K, V> from(Map.Entry<K, V> mapEntry) {

    return Optional.ofNullable(mapEntry)
      .map(it -> newKeyValue(it.getKey(), it.getValue()))
      .orElseThrow(() -> newIllegalArgumentException("Map.Entry is required"));
  }

  private final K key;

  private final V value;

  /**
   * Constructs a new instance of {@link KeyValue} initialized with the given {@code key} and a {@literal null} value.
   *
   * @param key key in the key/value mapping. Must not be {@literal null}.
   * @throws IllegalArgumentException if {@code key} is {@literal null}.
   * @see #KeyValue(Object, Object)
   */
  public KeyValue(K key) {
    this(key, null);
  }

  /**
   * Constructs a new instance of {@link KeyValue} initialized with the given {@code key} and {@code value}.
   *
   * @param key key in the key/value mapping. Must not be {@literal null}.
   * @param value value in the key/value mapping. May be {@literal null}.
   * @throws IllegalArgumentException if {@code key} is {@literal null}.
   */
  public KeyValue(K key, V value) {

    Assert.notNull(key, "Key is required");

    this.key = key;
    this.value = value;
  }

  /**
   * Determines whether the {@link #getKey() key} has a value.
   *
   * @return a boolean value indicating whether the {@link #getKey() key} has a value.
   * @see #getValue(Object)
   */
  public boolean isSet() {
    return (getValue(null) != null);
  }

  /**
   * Return the key in the key/value mapping.
   *
   * @return the key.
   */
  public K getKey() {
    return this.key;
  }

  /**
   * Return the value as a null-safe {@link Optional} value in the key/value mapping.
   *
   * @return the optional value.
   * @see java.util.Optional
   * @see #getValue(Object)
   */
  public Optional<V> getValue() {
    return Optional.ofNullable(this.value);
  }

  /**
   * Return the value in the key/value mapping.
   *
   * If the value is {@literal null}, then return the {@code defaulValue}.
   *
   * @param defaultValue default value to return if the value is {@literal null}.
   * @return the value of the key/value mapping or {@code defaultValue} if value is {@literal null}.
   * @see #getValue()
   */
  public V getValue(V defaultValue) {
    return getValue().orElse(defaultValue);
  }

  /**
   * Returns this {@link KeyValue} object as an immutable instance of {@link Map.Entry}.
   *
   * @return this {@link KeyValue} object as an immutable instance of {@link Map.Entry}.
   * @see org.cp.elements.util.MapUtils#newMapEntry(Object, Object)
   * @see java.util.Map.Entry
   */
  public Map.Entry<K, V> asMapEntry() {
    return newMapEntry(getKey(), getValue().orElse(null));
  }

  /**
   * Determines whether this {@link KeyValue} is equal to the given {@link Object}.
   *
   * @param obj {@link Object} to compare for equality with this {@link KeyValue}.
   * @return a boolean value indicating whether this {@link KeyValue} is equal to the given {@link Object}.
   * @see java.lang.Object#equals(Object)
   */
  @Override
  public boolean equals(Object obj) {

    if (this == obj) {
      return true;
    }

    if (!(obj instanceof KeyValue)) {
      return false;
    }

    if (!getClass().equals(obj.getClass())) {
      return false;
    }

    KeyValue<?, ?> that = (KeyValue<?, ?>) obj;

    return this.getKey().equals(that.getKey())
      && ObjectUtils.equals(this.getValue(), that.getValue());
  }

  /**
   * Computes the hash code for this {@link KeyValue}.
   *
   * @return an integer value containing the computed hash code of this {@link KeyValue}.
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {

    int hashValue = 17;

    hashValue = 37 * hashValue + ObjectUtils.hashCode(this.getKey());
    hashValue = 37 * hashValue + ObjectUtils.hashCode(this.getValue());

    return hashValue;
  }

  /**
   * Returns a {@link String} representation (view) of this {@link KeyValue}.
   *
   * @return a {@link String} describing this {@link KeyValue}.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return String.format("%1$s = %2$s", getKey(), getValue(null));
  }
}
