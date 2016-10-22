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

import org.cp.elements.lang.NullSafe;

/**
 * The {@link ComparatorUtils} class is an abstract utility class providing common functionality
 * for ordering operations using the {@link Comparable} and {@link Comparator} interfaces.
 *
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see java.util.Comparator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ComparatorUtils {

  /**
   * Compares two {@link Comparable} objects for absolute ordering, ignoring possible {@literal null} values.
   * Sorts (orders) {@literal null} values last.
   *
   * @param <T> {@link Class} type of {@link Comparable} objects in the comparison.
   * @param obj1 {@link Comparable} object being compared with {@code obj2}.
   * @param obj2 {@link Comparable} object being compared with {@code obj1}.
   * @return a integer value indicating the absolute ordering of the {@link Comparable} objects
   * as defined by their declared, natural ordering.
   * @see java.lang.Comparable
   */
  @NullSafe
  public static <T extends Comparable<T>> int compareIgnoreNull(T obj1, T obj2) {
    return (obj1 == null ? 1 : (obj2 == null ? -1 : obj1.compareTo(obj2)));
  }

  /**
   * Inverts the result of the {@link Comparator}.  Used to implement a descending sort order.
   *
   * @param <T> {@link Class} type of {@link Comparable} objects in the comparison.
   * @param comparator the {@link Comparator} to invert.
   * @return a {@link Comparator} wrapper around the given {@link Comparator} inverting the result
   * of the comparison.
   * @see java.util.Comparator
   */
  public static <T> Comparator<T> invert(Comparator<T> comparator) {
    return (obj1, obj2) -> (-1 * comparator.compare(obj1, obj2));
  }

  /**
   * Wraps an exiting {@link Comparator} in a null-safe, delegating {@link Comparator} implementation to protect
   * against {@literal null} arguments passed to the {@literal compare} method.
   * Sorts (order) {@literal null} values last.
   *
   * @param <T> Class type of the objects to compare.
   * @param delegate {@link Comparator} delegate.
   * @return a null-safe, delegating {@link Comparator} implementation wrapping the given {@link Comparator}.
   * @see java.util.Comparator
   */
  public static <T> Comparator<T> nullSafeArgumentsComparator(Comparator<T> delegate) {
    return (T obj1, T obj2) -> (obj1 == null ? 1 : (obj2 == null ? -1 : delegate.compare(obj1, obj2)));
  }

  /**
   * Null-safe check on the given {@link Comparator} returning the given {@link Comparator} if not {@literal null}
   * or providing a default {@link Comparator }implementation if the given {@link Comparator} is {@literal null}.
   *
   * @param <T> {@link Comparable} class type of the objects in the comparison.
   * @param comparator {@link Comparator} to evaluate for {@literal null}.
   * @return the given {@link Comparator} if not {@literal null} or a default, provided {@link Comparator}
   * implementation if the given {@link Comparator} is {@literal null}.
   * @see #nullSafeArgumentsComparator(Comparator)
   * @see java.util.Comparator
   */
  @NullSafe
  public static <T extends Comparable<T>> Comparator<T> nullSafeComparator(Comparator<T> comparator) {
    return (comparator != null ? comparator : ComparatorUtils::compareIgnoreNull);
  }
}
