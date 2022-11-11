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
package org.cp.elements.lang.support;

import java.util.Comparator;

import org.cp.elements.lang.Orderable;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link Comparator} implementation used to compare {@link Orderable objects}.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} parameter indicating the specific {@link Class} of the {@link Orderable} type.
 * @see java.lang.Comparable
 * @see java.util.Comparator
 * @see org.cp.elements.lang.Orderable
 * @since 1.0.0
 */
public class OrderableComparator<T extends Comparable<T>> implements Comparator<Orderable<T>> {

  /**
   * Compares two {@link Orderable objects} to determine their relative sort order
   * by their {@link Orderable#getOrder() order property}.
   *
   * @param orderableOne first {@link Orderable object} in the order comparison; must not be {@literal null}.
   * @param orderableTwo second {@link Orderable object} in the order comparison; must not be {@literal null}.
   * @return an {@link Integer value} indicating an {@link Orderable object's} order relative to
   * another {@link Orderable object}.
   * @see org.cp.elements.lang.Orderable
   */
  @Override
  public int compare(@NotNull Orderable<T> orderableOne, @NotNull Orderable<T> orderableTwo) {
    return orderableOne.getOrder().compareTo(orderableTwo.getOrder());
  }
}
