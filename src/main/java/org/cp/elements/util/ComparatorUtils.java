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

import java.util.Comparator;

/**
 * The ComparatorUtils class provides common functionality for ordering operations using the Comparable
 * and Comparator interfaces.
 *
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see java.util.Comparator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ComparatorUtils {

  /**
   * Compares two Comparable objects for absolute ordering, ignoring possible null values.  Null values are
   * ordered last.
   *
   * @param obj1 the Comparable object being compared with obj2.
   * @param obj2 the Comparable object being compared with obj1.
   * @param <T> the type of Comparable objects in the comparison.
   * @return a integer value indicating the absolute ordering of the Comparable objects as defined by
   * their natural ordering.
   * @see java.lang.Comparable
   */
  public static <T extends Comparable<T>> int compareIgnoreNull(final T obj1, final T obj2) {
    return (obj1 == null ? 1 : (obj2 == null ? -1 : obj1.compareTo(obj2)));
  }

  /**
   * Inverts the result of the Comparator.  Used to implement a descending sort order.
   *
   * @param comparator the Comparator to invert.
   * @param <T> the type of Comparable objects in the comparison.
   * @return a Comparator wrapper around the specified Comparator inverting the result of the comparison.
   * @see java.util.Comparator
   */
  public static <T> Comparator<T> invert(final Comparator<T> comparator) {
    return (obj1, obj2) -> (-1 * comparator.compare(obj1, obj2));
  }

}
