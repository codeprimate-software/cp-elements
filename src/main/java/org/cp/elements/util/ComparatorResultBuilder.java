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

import static org.cp.elements.util.ComparatorUtils.compareIgnoreNull;

import java.util.Comparator;

import org.cp.elements.lang.NullSafe;

/**
 * The {@link ComparatorResultBuilder} class is a {@link Comparator} implementation that builds a comparison expression
 * accumulating the result.
 *
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see java.util.Comparator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComparatorResultBuilder<T extends Comparable<T>> implements Comparator<T> {

  private int result = 0;

  /**
   * Factory method to construct a new instance of {@link ComparatorResultBuilder}.
   *
   * @param <T> {@link Class} type of the {@link Comparable} objects evaluated in the comparison operation.
   * @return a new instance of {@link ComparatorResultBuilder}.
   * @see org.cp.elements.util.ComparatorResultBuilder
   */
  public static <T extends Comparable<T>> ComparatorResultBuilder<T> create() {
    return new ComparatorResultBuilder<>();
  }

  /**
   * @inheritDoc
   * @see org.cp.elements.util.ComparatorUtils#compareIgnoreNull(Comparable, Comparable)
   */
  @NullSafe
  @Override
  public int compare(T obj1, T obj2) {
    return compareIgnoreNull(obj1, obj2);
  }

  /**
   * Performs the comparison between 2 {@link Comparable} objects.
   *
   * @param obj1 {@link Comparable} object in the comparison operation.
   * @param obj2 {@link Comparable} object in the comparison operation.
   * @return this {@link ComparatorResultBuilder}.
   * @see org.cp.elements.util.ComparatorResultBuilder
   * @see #compare(Comparable, Comparable)
   */
  @NullSafe
  public ComparatorResultBuilder<T> doCompare(T obj1, T obj2) {
    this.result = (this.result != 0 ? this.result : compare(obj1, obj2));
    return this;
  }

  /**
   * Returns the result of the comparison.
   *
   * @return an {@link Integer} value containing the result of the comparison.
   */
  public int getResult() {
    return this.result;
  }
}
