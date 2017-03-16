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

import java.util.Optional;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.concurrent.ThreadSafe;

/**
 * The {@link KeyValue} class is a Thread-safe data structure modeling both a key and a value mapped to the key.
 *
 * @author John Blum
 * @since 1.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class KeyValue<K, V> {

  /**
   * Factory method used to construct a new instance of {@link KeyValue} initialized with the given {@code key}
   * with no value.
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
    Assert.notNull(key, "Key must not be null");

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
   * @inheritDoc
   */
  @Override
  public boolean equals(Object obj) {
    if (obj == this) {
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
   * @inheritDoc
   */
  @Override
  public int hashCode() {
    int hashValue = 17;
    hashValue = 37 * hashValue + ObjectUtils.hashCode(this.getKey());
    hashValue = 37 * hashValue + ObjectUtils.hashCode(this.getValue());
    return hashValue;
  }

  /**
   * @inheritDoc
   */
  @Override
  public String toString() {
    return String.format("%1$s = %2$s", getKey(), getValue(null));
  }
}
