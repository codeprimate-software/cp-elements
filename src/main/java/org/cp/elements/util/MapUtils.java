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
package org.cp.elements.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.TreeMap;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Transformer;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An abstract utility {@link Class} providing methods and functionality for working with the Java Collections Framework
 * and specifically the {@link Map} class.
 *
 * @author John J. Blum
 * @see java.util.Collections
 * @see java.util.Map
 * @see java.util.Map.Entry
 * @see java.util.function.Predicate
 * @see java.util.stream.Collectors
 * @see org.cp.elements.lang.Constants
 * @see org.cp.elements.lang.FilteringTransformer
 * @see org.cp.elements.lang.Transformer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class MapUtils {

  /**
   * Determines the number of {@link Map.Entry entries} in the {@link Map}.
   *
   * This method is null-safe and will return {@literal 0} if the {@link Map} is {@literal null} or {@literal empty}.
   *
   * @param <K> {@link Class} type of the key.
   * @param <V> {@link Class} type of the value.
   * @param map {@link Map} to evaluate.
   * @return the size, or number of elements, in the {@link Map}, returning {@literal 0} if the {@link Map}
   * is {@literal null} or {@literal empty}.
   * @see java.util.Map
   */
  @NullSafe
  public static <K, V> int count(@Nullable Map<K, V> map) {
    return map != null ? map.size() : 0;
  }

  /**
   * Counts the number of {@link Map.Entry entries} in the {@link Map} accepted by the {@link Predicate}.
   *
   * @param <K> {@link Class type} of the key.
   * @param <V> {@link Class type} of the value.
   * @param map {@link Map} to evaluate.
   * @param predicate {@link Predicate} used to determine the number of {@link Map.Entry entries} in the {@link Map}
   * accepted by the {@link Predicate}; must not be {@literal null}.
   * @return an integer value indicating the number of {@link Map.Entry entries} in the {@link Map}
   * accepted by the {@link Predicate}.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @see java.util.function.Predicate
   * @see java.util.Map
   */
  public static <K, V> long count(@Nullable Map<K, V> map, @NotNull Predicate<Entry<K, V>> predicate) {

    Assert.notNull(predicate, "Predicate is required");

    return nullSafeMap(map).entrySet().stream()
      .filter(predicate)
      .count();
  }

  /**
   * Returns a filtered {@link Map} containing only the {@link Map.Entry Key/Value entries} from the given {@link Map}
   * that are accepted by the {@link Predicate}.
   *
   * @param <K> {@link Class type} of the key.
   * @param <V> {@link Class type} of the value.
   * @param map {@link Map} to filter; must not be {@literal null}.
   * @param predicate {@link Predicate} used to filter the {@link Map}; must not be {@literal null}.
   * @return a filtered {@link Map} containing only the {@link Map.Entry Key/Value entries} from the given {@link Map}
   * accepted by the {@link Predicate}.
   * @throws IllegalArgumentException if either the {@link Map} or {@link Predicate} are {@literal null}.
   * @see java.util.function.Predicate
   * @see java.util.Map
   */
  public static @NotNull <K, V> Map<K, V> filter(@NotNull Map<K, V> map, @NotNull Predicate<Map.Entry<K, V>> predicate) {

    Assert.notNull(map, "Map is required");
    Assert.notNull(predicate, "Predicate is required");

    return map.entrySet().stream()
      .filter(predicate)
      .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
  }

  /**
   * Filters and transform the {@link Map.Entry entries} in the given {@link Map}
   * using the provided {@link FilteringTransformer}.
   *
   * @param <K> {@link Class type} of the key.
   * @param <V> {@link Class type} of the value.
   * @param map {@link Map} to filter and transform; must not be {@literal null}.
   * @param filteringTransformer {@link FilteringTransformer} used to filter and transform the given {@link Map};
   * must not be {@literal null}.
   * @return a filtered, transformed {@link Map} of {@link Map.Entry entries} from the given {@link Map}.
   * @throws IllegalArgumentException if the {@link Map} or {@link FilteringTransformer} are {@literal null}.
   * @see org.cp.elements.lang.FilteringTransformer
   * @see #transform(Map, Transformer)
   * @see #filter(Map, Predicate)
   * @see java.util.Map
   */
  public static @NotNull <K, V> Map<K, V> filterAndTransform(@NotNull Map<K, V> map,
      @NotNull FilteringTransformer<Map.Entry<K, V>> filteringTransformer) {

    Assert.notNull(map, "Map is required");
    Assert.notNull(filteringTransformer, "FilteringTransformer is required");

    return map.entrySet().stream()
      .filter(filteringTransformer::accept)
      .map(filteringTransformer::transform)
      .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
  }

  /**
   * Finds all {@link Map.Entry Key/Value entries} from the given {@link Map} accepted by the {@link Predicate}.
   *
   * @param <K> {@link Class type} of the key.
   * @param <V> {@link Class type} of the value.
   * @param map {@link Map} to search; must not be {@literal null}.
   * @param predicate {@link Predicate} used to find matching {@link Map.Entry Key/Value entries} from the {@link Map};
   * must not be {@literal null}.
   * @return a new {@link Map} containing {@link Map.Entry Key/Value entries} from the given {@link Map}
   * accepted by the {@link Predicate}.
   * @throws IllegalArgumentException if either the {@link Map} or {@link Predicate} are {@literal null}.
   * @see java.util.function.Predicate
   * @see #filter(Map, Predicate)
   * @see java.util.Map
   */
  public static @NotNull <K, V> Map<K, V> findAll(@NotNull Map<K, V> map, @NotNull Predicate<Map.Entry<K, V>> predicate) {
    return filter(map, predicate);
  }

  /**
   * Converts the given associative array into a {@link Map}.
   *
   * @param associativeArray associate array to convert into a {@link Map}.
   * @return a {@link Map} populated with the Key/Value pairs in the associate array.
   * @see java.util.Map
   */
  @NullSafe
  public static @NotNull Map<String, String> fromAssociativeArray(@Nullable String[] associativeArray) {

    Map<String, String> map = new HashMap<>(ArrayUtils.nullSafeLength(associativeArray));

    int index = 0;

    for (String element : ArrayUtils.nullSafeArray(associativeArray, String.class)) {

      Assert.hasText(element, "Entry [%s] at index [%d] must be specified", element, index);

      String[] entry = element.split("=");

      Assert.isTrue(entry.length == 2,
        "Entry [%s] at index [%d] must have both a key and a value", element, index);

      String key = StringUtils.trim(entry[0]);
      String value = StringUtils.trim(entry[1]);

      map.put(key, value);

      index++;
    }

    return map;
  }

  /**
   * Determines whether the given {@link Map} is empty.
   *
   * A {@link Map} is considered empty if it contains no entries or is {@literal null}.
   *
   * @param map {@link Map} to evaluate.
   * @return a boolean value indicating whether the {@link Map} is empty.
   * @see java.util.Map#isEmpty()
   * @see #isNotEmpty(Map)
   */
  @NullSafe
  public static boolean isEmpty(@Nullable Map<?, ?> map) {
    return map == null || map.isEmpty();
  }

  /**
   * Determines whether the given {@link Map} is not empty.
   *
   * A {@link Map} is considered non-empty if it is not {@literal null} and contains at least 1 entry.
   *
   * @param map {@link Map} to evaluate.
   * @return a boolean value indicating whether the {@link Map} is not empty.
   * @see java.util.Map#isEmpty()
   * @see #isEmpty(Map)
   */
  @NullSafe
  public static boolean isNotEmpty(@Nullable Map<?, ?> map) {
    return !isEmpty(map);
  }

  /**
   * Determines whether the given {@link Map} has the expected size.
   *
   * @param map {@link Map} to evaluate.
   * @param size {@link Integer#TYPE} indicating the expected size of the {@link Map}.
   * @return a boolean value indicating whether the {@link Map} size equals the expected size.
   * @see #isSizeOne(Map)
   * @see java.util.Map
   */
  @NullSafe
  public static boolean isSize(@Nullable Map<?, ?> map, int size) {
    return size(map) == size;
  }

  /**
   * Null-safe operation to determine whether the size of the given {@link Map} is {@literal 1}.
   *
   * @param map {@link Map} to evaluate.
   * @return a boolean value indicating whether the size of the given {@link Map} is {@literal 1}.
   * @see #isSize(Map, int)
   * @see java.util.Map
   */
  @NullSafe
  public static boolean isSizeOne(@Nullable Map<?, ?> map) {
    return size(map) == 1;
  }

  /**
   * Constructs a new instance of {@link Map.Entry} initialized with the given {@link Object key}
   * and {@link Object value}.
   *
   * @param <K> {@link Class} type of the key.
   * @param <V> {@link Class} type of the value.
   * @param key key of the new {@link Map.Entry}.
   * @param value value of the new {@link Map.Entry}.
   * @return a new {@link Map.Entry} initialized with the {@link Object key} and {@link Object value}.
   * @see java.util.Map.Entry
   */
  public static @NotNull <K, V> Map.Entry<K, V> newMapEntry(K key, V value) {

    return new Map.Entry<K, V>() {

      @Override
      public K getKey() {
        return key;
      }

      @Override
      public V getValue() {
        return value;
      }

      @Override
      public V setValue(V value) {
        throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
      }
    };
  }

  /**
   * Determines whether the {@link Map} is not {@literal null} and contains no {@literal null}
   * {@link Map.Entry Map Entries}.
   *
   * The {@link Map} contains no {@literal null} {@link Map.Entry Map Entries} is there does not exist a mapping
   * where either the key or value, or both, are {@literal null}.
   *
   * @param map {@link Map} to evaluate.
   * @return a boolean value indicating whether the {@link Map} is not {@literal null}
   * and contains no {@literal null} {@link Map.Entry Map Entries}.
   * @see java.util.Map
   */
  @NullSafe
  public static boolean noNullEntries(@Nullable Map<?, ?> map) {

    return !(map == null || map.entrySet().stream()
      .anyMatch(mapEntry -> Objects.isNull(mapEntry.getKey()) || Objects.isNull(mapEntry.getValue())));
  }

  /**
   * Returns the given {@link Map} if not {@literal null} or returns an empty {@link Map}.
   *
   * @param <K> {@link Class} type of the key.
   * @param <V> {@link Class} type of the value.
   * @param map {@link Map} to evaluate.
   * @return the given {@link Map} if not {@literal null} or return an empty {@link Map} otherwise.
   * @see java.util.Collections#emptyMap()
   * @see java.util.Map
   */
  @NullSafe
  public static @NotNull <K, V> Map<K, V> nullSafeMap(@Nullable Map<K, V> map) {
    return map != null ? map : Collections.emptyMap();
  }

  /**
   * Determines the size, or number of {@link Map.Entry entries}, in the {@link Map}, returning {@literal 0}
   * if the {@link Map} is {@literal null} or {@literal empty}.
   *
   * @param map {@link Map} to evaluate.
   * @return an integer value indicating the size or number of {@link Map.Entry entries} in the {@link Map}.
   * @see java.util.Map#size()
   * @see #count(Map)
   */
  @NullSafe
  public static int size(@Nullable Map<?, ?> map) {
    return count(map);
  }

  /**
   * Converts the given {@link Map} into an associative array.
   *
   * The associative array takes the form of [key1=value1, key2=value2, ..., keyN=valueN].
   *
   * @param map {@link Map} to convert into an associative array.
   * @return an associative {@link String} array containing the keys and values
   * from the given {@link Map}.
   * @see #nullSafeMap(Map)
   * @see #size(Map)
   * @see java.util.Map
   */
  @NullSafe
  public static String[] toAssociativeArray(@Nullable Map<?, ?> map) {

    List<String> list = new ArrayList<>(size(map));

    for (Map.Entry<?, ?> entry : nullSafeMap(map).entrySet()) {
      list.add(String.format("%1$s=%2$s", entry.getKey(), entry.getValue()));
    }

    return list.toArray(new String[0]);
  }

  /**
   * Prints the given {@link Map} as a {@link String}.
   *
   * @param map {@link Map} to print as a {@link String}.
   * @return a {@link String} representation of the given {@link Map}.
   * @see java.lang.String
   * @see java.util.Map
   */
  @NullSafe
  public static @NotNull String toString(@Nullable Map<?, ?> map) {

    StringBuilder builder = new StringBuilder("[");

    int count = 0;

    Map<?, ?> resolvedMap = new TreeMap<>(nullSafeMap(map));

    for (Map.Entry<?, ?> entry : resolvedMap.entrySet()) {
      builder.append("\n\t");
      builder.append(entry.getKey());
      builder.append(" = ");
      builder.append(entry.getValue());
      builder.append(++count == resolvedMap.size() ? "\n" : ",");
    }

    builder.append("]");

    return builder.toString();
  }

  /**
   * Transforms the values of the given {@link Map} with the specified {@link Transformer}.
   *
   * @param <K> {@link Class} type of the key.
   * @param <V> {@link Class} type of the value.
   * @param map {@link Map} to transform; must not be {@literal null}.
   * @param transformer {@link Transformer} used to transform the {@link Map}'s values; must not be {@literal null}.
   * @return a new {@link Map} from the given {@link Map} transformed by the {@link Transformer}.
   * @throws IllegalArgumentException if either the {@link Map} or {@link Transformer} are {@literal null}.
   * @see org.cp.elements.lang.Transformer
   * @see java.util.Map
   */
  public static @NotNull <K, V> Map<K, V> transform(@NotNull Map<K, V> map, @NotNull Transformer<V> transformer) {

    Assert.notNull(map, "Map is required");
    Assert.notNull(transformer, "Transformer is required");

    return map.entrySet().stream()
      .collect(Collectors.toMap(Map.Entry::getKey, (entry) -> transformer.transform(entry.getValue())));
  }
}
