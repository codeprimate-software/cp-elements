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

import org.cp.elements.lang.Ordered;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link Comparator} implementation for {@link Ordered} objects.
 *
 * @author John J. Blum
 * @see java.util.Comparator
 * @see org.cp.elements.lang.Ordered
 * @since 1.0.0
 */
public class OrderedComparator implements Comparator<Ordered> {

  public static final OrderedComparator INSTANCE = new OrderedComparator();

  /**
   * Compares two {@link Ordered} objects to determine their relative order using an {@link Integer index}.
   *
   * @param orderedOne first {@link Ordered} object in the order comparison; must not be {@literal null}.
   * @param orderedTwo second {@link Ordered} object in the order comparison; must not be {@literal null}.
   * @return an {@link Integer value} indicating an {@link Ordered} object's order relative to
   * another {@link Ordered} object.
   * @see org.cp.elements.lang.Ordered
   */
  @Override
  public int compare(@NotNull Ordered orderedOne, @NotNull Ordered orderedTwo) {
    return Integer.compare(orderedOne.getIndex(), orderedTwo.getIndex());
  }
}
