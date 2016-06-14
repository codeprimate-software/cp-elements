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

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.Transformer;

/**
 * The MapUtils class provides utility methods for working with the Java Collections Framework and specifically
 * the {@link Map} classes.
 * 
 * @author John J. Blum
 * @see java.util.Collections
 * @see java.util.Iterator
 * @see java.util.Map
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.Transformer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class MapUtils {

  @NullSafe
  public static <K, V> int count(Map<K, V> map) {
    return size(map);
  }

  /**
   * Counts the number of key-value pairs in the Map matching the criteria defined by the Filter.
   * 
   * @param <K> the Class type of the key.
   * @param <V> the Class type of the value.
   * @param map the Map of key-value pairs to search.
   * @param filter the Filter used to tally the number of key-value pairs matching the criteria defined by the Filter.
   * @return an integer value indicating the number of key-value pairs matching the criteria defined by the Filter.
   * @throws NullPointerException if either the Map or Filter are null!
   * @see #find(java.util.Map, org.cp.elements.lang.Filter)
   * @see java.util.Map
   * @see org.cp.elements.lang.Filter
   */
  public static <K, V> int count(Map<K, V> map, Filter<Map.Entry<K, V>> filter) {
    return find(map, filter).size();
  }

  /**
   * Gets Map if not null otherwise returns an empty Map.
   * 
   * @param <K> the Class type of the key.
   * @param <V> the Class type of the value.
   * @param map the Map reference being tested with a null check.
   * @return the Map if not null otherwise return an empty Map.
   * @see java.util.Collections#emptyMap()
   * @see java.util.Map
   */
  public static <K, V> Map<K, V> emptyMap(final Map<K, V> map) {
    return (map != null ? map : Collections.<K, V>emptyMap());
  }

  /**
   * Filters the Map of key-value pairs based on the criteria defined by the Filter.
   * 
   * @param <K> the class type of the key.
   * @param <V> the class type of the value.
   * @param <M> the subclass type of Map to filter.
   * @param map the Map of key-value pairs to filter.
   * @param filter the Filter defining the criteria used to match key-value pairs from the Map.
   * @return a new Map containing only the key-value pairs from the original Map matching the criteria defined
   * by the Filter.
   * @throws NullPointerException if either the Map or Filter are null.
   * @see java.util.Map
   * @see org.cp.elements.lang.Filter
   */
  public static <K, V, M extends Map<K, V>> M filter(final M map, final Filter<Map.Entry<K, V>> filter) {
    Assert.notNull(map, "The Map to filter cannot be null!");
    Assert.notNull(filter, "The Filter used to filter the Map cannot be null!");

    for (Iterator<Map.Entry<K, V>> it = map.entrySet().iterator(); it.hasNext(); ) {
      if (!filter.accept(it.next())) {
        it.remove();
      }
    }

    return map;
  }

  /**
   * Finds key-value pairs from the specified Map matching criteria defined by the Filter.
   * 
   * @param <K> the Class type of the key.
   * @param <V> the Class type of the value.
   * @param map the Map of key-value pairs to search.
   * @param filter the Filter used to find and match key-value pairs from the Map.
   * @return a new Map containing key-value pairs from the original Map that match the criteria defined by the Filter.
   * @throws NullPointerException if either the Map or Filter are null.
   * @see java.util.Map
   * @see org.cp.elements.lang.Filter
   */
  public static <K, V> Map<K, V> find(final Map<K, V> map, final Filter<Map.Entry<K, V>> filter) {
    Assert.notNull(map, "The Map to search and find key-value pairs matching the criteria defined by the Filter cannot be null!");
    Assert.notNull(filter, "The Filter used to find and match key-value pairs from the Map cannot be null!");

    Map<K, V> resultMap = new HashMap<>(map.size());

    for (Map.Entry<K, V> entry : map.entrySet()) {
      if (filter.accept(entry)) {
        resultMap.put(entry.getKey(), entry.getValue());
      }
    }

    return resultMap;
  }

  /**
   * Determines whether the specified Map is empty.  A Map is empty if it contains no key-value pairs
   * or the specified Map object reference is null.
   * 
   * @param map the Map being tested as empty.
   * @return a boolean value indicating whether the specified Map is empty.
   * @see java.util.Map#isEmpty()
   */
  public static boolean isEmpty(final Map map) {
    return (map == null || map.isEmpty());
  }

  /**
   * Determines the size of the specified Map.  If the Map is null or contains no key-value pairs, then the
   * size of the Map is 0, otherwise the size of the Map is determined by it's size() method.
   * 
   * @param map the Map who's size is being determined.
   * @return an integer value specifying the size of the Map.
   * @see java.util.Map#size()
   */
  public static int size(final Map map) {
    return (map == null ? 0 : map.size());
  }

  /**
   * Transforms the values of the given Map with the specified Transformer.
   *
   * @param <K> the Class type of the Map key.
   * @param <V> the Class type of the Map value.
   * @param <M> the Class type of the Map.
   * @param map the Map of key/values to transform.
   * @param transformer the Transformers used to transform the Map's values.
   * @return the value of the Map transformed by the specified Transformer.
   * @throws java.lang.NullPointerException if the Map or Transformer references are null.
   * @see org.cp.elements.lang.Transformer
   * @see java.util.Map
   */
  public static <K, V, M extends Map<K, V>> M transform(final M map, final Transformer<V> transformer) {
    Assert.notNull(map, "The Map of values to transform cannot be null!");
    Assert.notNull(transformer, "The Transformer used to transform the Map's values cannot be null!");

    for (Map.Entry<K, V> mapEntry : map.entrySet()) {
      map.put(mapEntry.getKey(), transformer.transform(mapEntry.getValue()));
    }

    return map;
  }

}
