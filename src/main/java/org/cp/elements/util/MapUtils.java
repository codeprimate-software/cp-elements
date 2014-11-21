/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * 
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * 
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * 
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * 
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;

/**
 * The MapUtils class provides utility methods for working with the Java Collections Framework and specifically
 * the Map classes.
 * 
 * @author John J. Blum
 * @see java.util.Collections
 * @see java.util.Iterator
 * @see java.util.Map
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class MapUtils {

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
  public static <K, V> int count(final Map<K, V> map, final Filter<Map.Entry<K, V>> filter) {
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
   * @param <K> the Class type of the key.
   * @param <V> the Class type of the value.
   * @param map the Map of key-value pairs to filter.
   * @param filter the Filter defining the criteria used to match key-value pairs from the Map.
   * @return a new Map containing only the key-value pairs from the original Map matching the criteria defined
   * by the Filter.
   * @throws NullPointerException if either the Map or Filter are null.
   * @see java.util.Map
   * @see org.cp.elements.lang.Filter
   */
  public static <K, V> Map<K, V> filter(final Map<K, V> map, final Filter<Map.Entry<K, V>> filter) {
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

    Map<K, V> resultMap = new HashMap<K, V>(map.size());

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

}
