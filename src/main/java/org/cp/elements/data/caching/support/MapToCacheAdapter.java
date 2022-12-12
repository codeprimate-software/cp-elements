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

import static org.cp.elements.lang.LangExtensions.given;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import org.cp.elements.data.caching.AbstractCache;
import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.MapUtils;

/**
 * The {@link MapToCacheAdapter} class is a {@link Cache} implementation backed by a {@link Map}.
 *
 * This class adapts the {@link Map} interface into an instance of {@link Cache}.
 *
 * @author John Blum
 * @param <KEY> {@link Class type} of the {@link Cache} key.
 * @param <VALUE> {@link Class type} of the {@link Cache} value.
 * @see java.lang.Comparable
 * @see java.util.Map
 * @see org.cp.elements.data.caching.AbstractCache
 * @see <a href="https://en.wikipedia.org/wiki/Adapter_pattern">Adapter Software Design Pattern</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class MapToCacheAdapter<KEY extends Comparable<KEY>, VALUE> extends AbstractCache<KEY, VALUE> {

  /**
   * Factory method used to construct a new instance of {@link MapToCacheAdapter} initialized with
   * the given, required {@link Map}.
   *
   * @param <KEY> {@link Class type} of the keys used by this {@link Cache}.
   * @param <VALUE> {@link Class type} of the values stored by this {@link Cache}.
   * @param map {@link Map} adapted as an instance of {@link Cache} and used to back the {@link Cache} instance;
   * must not be {@literal null}.
   * @return a new {@link MapToCacheAdapter} initialized and backed by the given, required {@link Map}.
   * @throws IllegalArgumentException if {@link Map} is {@literal null}.
   * @see #MapToCacheAdapter(Map)
   * @see java.util.Map
   */
  public static @NotNull <KEY extends Comparable<KEY>, VALUE> MapToCacheAdapter<KEY, VALUE> of(
      @NotNull Map<KEY, VALUE> map) {

    return new MapToCacheAdapter<>(map);
  }

  private final Map<KEY, VALUE> map;

  /**
   * Constructs a new instance of {@link MapToCacheAdapter} initialized with the given, required {@link Map}.
   *
   * @param map {@link Map} used to back this {@link Cache}; must not be {@literal null}.
   * @throws IllegalArgumentException if {@link Map} is {@literal null}.
   * @see java.util.Map
   */
  public MapToCacheAdapter(@NotNull Map<KEY, VALUE> map) {
    this.map = ObjectUtils.requireObject(map, "Map is required");
  }

  /**
   * Returns a reference to the configured {@link Map} used to back this {@link Cache}.
   *
   * @return a reference to the configured {@link Map} used to back this {@link Cache}.
   * @see java.util.Map
   */
  protected @NotNull Map<KEY, VALUE> getMap() {
    return this.map;
  }

  @NullSafe
  @Override
  public boolean isEmpty() {
    return getMap().isEmpty();
  }

  @NullSafe
  @Override
  public void clear() {
    getMap().clear();
  }

  @NullSafe
  @Override
  public boolean contains(KEY key) {
    return key != null && getMap().containsKey(key);
  }

  @NullSafe
  @Override
  public void evict(KEY key) {

    if (key != null) {
      getMap().remove(key);
    }
  }

  @NullSafe
  @Override
  public void from(@Nullable Map<KEY, VALUE> map) {

    Map<KEY, VALUE> filteredMap = MapUtils.nullSafeMap(map).entrySet().stream()
      .filter(mapEntry -> given(mapEntry).thenGiven(Map.Entry::getKey).expectThat(Objects::nonNull).result())
      .filter(mapEntry -> given(mapEntry).thenGiven(Map.Entry::getValue).expectThat(Objects::nonNull).result())
      .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

    getMap().putAll(filteredMap);
  }

  @Override
  public @Nullable VALUE get(@Nullable KEY key) {
    return key != null ? getMap().get(key) : null;
  }

  @NullSafe
  @Override
  public Set<KEY> keys() {
    return toMap().keySet();
  }

  @Override
  public void put(@NotNull KEY key, @NotNull VALUE value) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(value, "Value is required");

    getMap().put(key, value);
  }

  @Override
  public @Nullable VALUE putIfAbsent(@NotNull KEY key, @NotNull VALUE value) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(value, "Value is required");

    return getMap().putIfAbsent(key, value);
  }

  @Override
  public @Nullable VALUE putIfPresent(@NotNull KEY key, @NotNull VALUE newValue) {

    Assert.notNull(key, "Key is required");
    Assert.notNull(newValue, "Value is required");

    AtomicReference<VALUE> oldValueReference = new AtomicReference<>(null);

    return newValue.equals(this.map.computeIfPresent(key, (theKey, oldValue) -> {
      oldValueReference.set(oldValue);
      return newValue;
    })) ? oldValueReference.get() : null;
  }

  @NullSafe
  @Override
  public long size() {
    return getMap().size();
  }

  @NullSafe
  @Override
  public @NotNull Map<KEY, VALUE> toMap() {
    return Collections.unmodifiableMap(MapUtils.nullSafeMap(getMap()));
  }
}
