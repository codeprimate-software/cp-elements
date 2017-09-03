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

package org.cp.elements.util;

import static org.cp.elements.util.ArrayUtils.nullSafeArray;
import static org.cp.elements.util.ArrayUtils.nullSafeLength;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Transformer;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * The MapUtils class provides utility methods for working with the Java Collections Framework and specifically
 * the {@link Map} class.
 *
 * @author John J. Blum
 * @see java.util.Collections
 * @see java.util.Map
 * @see java.util.stream.Collectors
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class MapUtils {

  /**
   * Determines the number of entries (key-value pairs) in the {@link Map}.  This method is null-safe and will
   * return 0 if the {@link Map} is null or empty.
   *
   * @param <K> Class type of the key.
   * @param <V> Class type of the value.
   * @param map {@link Map} to evaluate.
   * @return the size, or number of elements in the {@link Map}, returning 0 if the {@link Map} is null or empty.
   */
  @NullSafe
  public static <K, V> int count(Map<K, V> map) {
    return (map != null ? map.size() : 0);
  }

  /**
   * Counts the number of entries (key-value pairs) in the {@link Map} accepted by the {@link Filter}.
   *
   * @param <K> Class type of the key.
   * @param <V> Class type of the value.
   * @param map {@link Map} to evaluate.
   * @param filter {@link Filter} used to determine the number of entries in the {@link Map} accepted by
   * the {@link Filter}.
   * @return an integer value indicating the number of entries in the {@link Map} accepted by the {@link Filter}.
   * @throws IllegalArgumentException if {@link Filter} is null.
   * @see org.cp.elements.lang.Filter
   * @see java.util.Map
   */
  public static <K, V> int count(Map<K, V> map, Filter<Map.Entry<K, V>> filter) {
    Assert.notNull(filter, "Filter cannot be null");

    int count = 0;

    for (Map.Entry<K, V> entry : nullSafeMap(map).entrySet()) {
      if (filter.accept(entry)) {
        count++;
      }
    }

    return count;
  }

  /**
   * Returns a filtered {@link Map} containing only the key-value entries from the given {@link Map} that are accepted
   * by the {@link Filter}.
   *
   * @param <K> Class type of the key.
   * @param <V> Class type of the value.
   * @param map {@link Map} to filter.
   * @param filter {@link Filter} used to filter the {@link Map}.
   * @return a filtered {@link Map} containing only the key-value entries from the given {@link Map} }accepted by
   * the {@link Filter}.
   * @throws IllegalArgumentException if either the {@link Map} or {@link Filter} are null.
   * @see org.cp.elements.lang.Filter
   * @see java.util.Map
   */
  public static <K, V> Map<K, V> filter(Map<K, V> map, Filter<Map.Entry<K, V>> filter) {
    Assert.notNull(map, "Map cannot be null");
    Assert.notNull(filter, "Filter cannot be null");

    return map.entrySet().stream().filter(filter::accept).collect(Collectors.toMap(
      Map.Entry::<K>getKey, Map.Entry::<V>getValue));
  }

  /**
   * Filters and transform the entries in the given {@link Map} using the provided {@link FilteringTransformer}.
   *
   * @param <K> Class type of the key.
   * @param <V> Class type of the value.
   * @param map {@link Map} to filter and transform.
   * @param filteringTransformer {@link FilteringTransformer} used to filter and transform the given {@link Map}.
   * @return a filtered, transformed {@link Map} of entries from the given {@link Map}.
   * @throws IllegalArgumentException if the {@link Map} or {@link FilteringTransformer} are null.
   * @see org.cp.elements.lang.FilteringTransformer
   * @see java.util.Map
   * @see #filter(Map, Filter)
   * @see #transform(Map, Transformer)
   */
  public static <K, V> Map<K, V> filterAndTransform(Map<K, V> map,
      FilteringTransformer<Map.Entry<K, V>> filteringTransformer) {

    Assert.notNull(map, "Map cannot be null");
    Assert.notNull(filteringTransformer, "FilteringTransformer cannot be null");

    return map.entrySet().stream().filter(filteringTransformer::accept).map(filteringTransformer::transform).collect(
      Collectors.toMap(Map.Entry::<K>getKey, Map.Entry::<V>getValue));
  }

  /**
   * Finds all key-value entries from the given {@link Map} accepted by the {@link Filter}.
   *
   * @param <K> Class type of the key.
   * @param <V> Class type of the value.
   * @param map {@link Map} to search.
   * @param filter {@link Filter} used to find matching key-value entries from the {@link Map}.
   * @return a new {@link Map} containing key-value entries from the given {@link Map} accepted by the {@link Filter}.
   * @throws IllegalArgumentException if either the {@link Map} or {@link Filter} are null.
   * @see org.cp.elements.lang.Filter
   * @see java.util.Map
   * @see #filter(Map, Filter)
   */
  public static <K, V> Map<K, V> findAll(Map<K, V> map, Filter<Map.Entry<K, V>> filter) {
    return filter(map, filter);
  }

  /**
   * Converts the given associative array into a {@link Map}.
   *
   * @param associativeArray associate array to convert into a {@link Map}.
   * @return a {@link Map} populated with the key/value pairs in the associate array.
   * @see ArrayUtils#nullSafeArray(Object[], Class)
   * @see ArrayUtils#nullSafeLength(Object[])
   */
  @NullSafe
  public static Map<String, String> fromAssociativeArray(String[] associativeArray) {
    Map<String, String> map = new HashMap<>(nullSafeLength(associativeArray));

    int index = 0;

    for (String element : nullSafeArray(associativeArray, String.class)) {
      Assert.hasText(element, "Entry [%s] at index [%d] must be specified", element, index);

      String[] entry = element.split("=");

      Assert.isTrue(entry.length == 2, "Entry [%s] at index [%d] must have both a key and a value", element, index);

      String key = StringUtils.trim(entry[0]);
      String value = StringUtils.trim(entry[1]);

      map.put(key, value);
      index++;
    }

    return map;
  }

  /**
   * Determines whether the given {@link Map} is empty.  A {@link Map} is empty if it contains no entries or is null.
   *
   * @param map {@link Map} to evaluate.
   * @return a boolean value indicating whether the {@link Map} is empty.
   * @see java.util.Map#isEmpty()
   * @see #isNotEmpty(Map)
   */
  @NullSafe
  public static boolean isEmpty(Map map) {
    return (map == null || map.isEmpty());
  }

  /**
   * Determines whether the given {@link Map} is not empty.  A {@link Map} is considered non-empty if it is not null
   * and contains at least 1 entry.
   *
   * @param map {@link Map} to evaluate.
   * @return a boolean value indicating whether the {@link Map} is not empty.
   * @see java.util.Map#isEmpty()
   * @see #isEmpty(Map)
   */
  @NullSafe
  public static boolean isNotEmpty(Map map) {
    return !isEmpty(map);
  }

  /**
   * Returns the given {@link Map} if not null or an empty {@link Map}.
   *
   * @param <K> Class type of the key.
   * @param <V> Class type of the value.
   * @param map the {@link Map} to evaluate.
   * @return the given {@link Map} if not null or an empty {@link Map} otherwise.
   * @see java.util.Collections#emptyMap()
   * @see java.util.Map
   */
  @NullSafe
  public static <K, V> Map<K, V> nullSafeMap(Map<K, V> map) {
    return (map != null ? map : Collections.emptyMap());
  }

  /**
   * Determines the size, or number of entries in the {@link Map}, returning 0 if the {@link Map} is null or empty.
   *
   * @param map {@link Map} to evaluate.
   * @return an integer value indicating the size or number of entries in the {@link Map}.
   * @see java.util.Map#size()
   * @see #count(Map)
   */
  @NullSafe
  public static int size(Map<?, ?> map) {
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
   * @see java.util.Map
   * @see #size(Map)
   * @see #nullSafeMap(Map)
   */
  @NullSafe
  public static String[] toAssociativeArray(Map<?, ?> map) {
    List<String> list = new ArrayList<>(size(map));

    for (Map.Entry<?, ?> entry : nullSafeMap(map).entrySet()) {
      list.add(String.format("%1$s=%2$s", String.valueOf(entry.getKey()), entry.getValue()));
    }

    return list.toArray(new String[list.size()]);
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
  public static String toString(Map<?, ?> map) {
    StringBuilder builder = new StringBuilder("[");
    int count = 0;

    map = new TreeMap<Object, Object>(nullSafeMap(map));

    for (Map.Entry<?, ?> entry : map.entrySet()) {
      builder.append("\n\t");
      builder.append(entry.getKey());
      builder.append(" = ");
      builder.append(entry.getValue());
      builder.append(++count == map.size() ? "\n" : ",");
    }

    builder.append("]");

    return builder.toString();
  }

  /**
   * Transforms the values of the given Map with the specified Transformer.
   *
   * @param <K> Class type of the key.
   * @param <V> Class type of the value.
   * @param map {@link Map} to transform.
   * @param transformer {@link Transformer} used to transform the {@link Map}'s values.
   * @return a new {@link Map} from the given {@link Map} transformed by the {@link Transformer}.
   * @throws IllegalArgumentException if either the {@link Map} or {@link Transformer} are null.
   * @see org.cp.elements.lang.Transformer
   * @see java.util.Map
   */
  public static <K, V> Map<K, V> transform(Map<K, V> map, Transformer<V> transformer) {
    Assert.notNull(map, "Map cannot be null");
    Assert.notNull(transformer, "Transformer cannot be null");

    return map.entrySet().stream().collect(Collectors.toMap(Map.Entry::<K>getKey,
      (entry) -> transformer.transform(entry.getValue())));
  }
}
