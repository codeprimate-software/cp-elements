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

package org.cp.elements.lang.support;

import java.util.Comparator;

import org.cp.elements.lang.Orderable;

/**
 * The OrderableComparator class is an implementation of the Comparator interface to compare Orderable objects.
 *
 * @author John J. Blum
 * @param <T> a type parameter indicating the class of the Orderable type.
 * @see java.util.Comparator
 * @see org.cp.elements.lang.Orderable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class OrderableComparator<T extends Comparable<T>> implements Comparator<Orderable<T>> {

  /**
   * Compares two Orderable objects to determine their relative order by their order property.
   *
   * @param orderable1 the first Orderable object in the order comparison.
   * @param orderable2 the second Orderable object in the order comparison.
   * @return an integer value indicating one Orderable object's order relative to another Orderable object.
   */
  @Override
  public int compare(final Orderable<T> orderable1, final Orderable<T> orderable2) {
    return orderable1.getOrder().compareTo(orderable2.getOrder());
  }

}
