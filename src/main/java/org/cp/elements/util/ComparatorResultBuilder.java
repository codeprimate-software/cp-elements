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

import java.io.Serializable;
import java.util.Comparator;
import java.util.function.Supplier;

import org.cp.elements.lang.Builder;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * A {@link Comparator} implementation that builds a comparison expression, accumulating the {@link Integer result}
 * of the comparison.
 *
 * @author John J. Blum
 * @param <T> {@link Comparable} {@link Class type} of {@link Object objects} compared by this {@link Comparator}.
 * @see java.lang.Comparable
 * @see java.io.Serializable
 * @see java.util.Comparator
 * @see org.cp.elements.lang.Builder
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComparatorResultBuilder<T extends Comparable<T>>
    implements Builder<Integer>, Comparator<T>, Serializable {

  /**
   * Factory method used to construct a new instance of {@link ComparatorResultBuilder}.
   *
   * @param <T> {@link Comparable} {@link Class type} of objects evaluated in the comparison operation.
   * @return a new {@link ComparatorResultBuilder}.
   * @see org.cp.elements.util.ComparatorResultBuilder
   */
  public static @NotNull <T extends Comparable<T>> ComparatorResultBuilder<T> create() {
    return new ComparatorResultBuilder<>();
  }

  private int result;

  /**
   * Builds the {@link Integer result} of the aggregated {@link Comparator} operations.
   *
   * @return an {@link Integer value} containing the result of this {@link Comparator Comparator's} calculations.
   * @see #getResult()
   */
  @Override
  public @NotNull Integer build() {
    return getResult();
  }

  /**
   * Null-safe operation to compare two {@link Comparable} objects.
   *
   * @param obj1 left-hand side {@link Comparable} operand in the comparison expression.
   * @param obj2 right-hand side {@link Comparable} operand in the comparison expression.
   * @return the {@link Integer result} of comparing the {@link Comparable} objects.
   * @see org.cp.elements.util.ComparatorUtils#compareIgnoreNull(Comparable, Comparable)
   */
  @NullSafe
  @Override
  public int compare(@Nullable T obj1, @Nullable T obj2) {
    return ComparatorUtils.compareIgnoreNull(obj1, obj2);
  }

  /**
   * Builder method used to perform the comparison between two {@link Comparable} objects.
   *
   * @param obj1 {@link Comparable} object in the comparison operation.
   * @param obj2 {@link Comparable} object in the comparison operation.
   * @return this {@link ComparatorResultBuilder}.
   * @see org.cp.elements.util.ComparatorResultBuilder
   * @see #compare(Comparable, Comparable)
   */
  @NullSafe
  public @NotNull ComparatorResultBuilder<T> doCompare(@Nullable T obj1, @Nullable T obj2) {
    this.result = resolveResult(this.result, () -> compare(obj1, obj2));
    return this;
  }

  /**
   * Builder method used to invert the {@link Integer} result of the aggregate comparisons.
   *
   * @return this {@link ComparatorResultBuilder}.
   */
  public ComparatorResultBuilder<T> invert() {
    this.result *= -1;
    return this;
  }

  /**
   * Returns the {@link Integer result} of the comparison.
   *
   * @return the {@link Integer result} of the comparison.
   * @see #getResult(Supplier)
   * @see #build()
   */
  public int getResult() {
    return this.result;
  }

  /**
   * Returns the {@link Integer result} of the comparison or computes the {@link Integer result}
   * using the given {@link Supplier} if the current state of {@link #getResult()} is {@literal 0}.
   *
   * @param resultSupplier {@link Supplier} to invoke to compute a {@literal result}
   * if the current {@link #getResult()} is {@literal 0}.
   * @return the {@link Integer result} of the comparison.
   * @see java.util.function.Supplier
   * @see #getResult()
   */
  public int getResult(@NotNull Supplier<Integer> resultSupplier) {
    return resolveResult(getResult(), resultSupplier);
  }

  private @NotNull Supplier<Integer> nullSafeSupplier(@Nullable Supplier<Integer> supplier) {
    return supplier != null ? supplier : () -> this.result;
  }

  private int resolveResult(int result, @NotNull Supplier<Integer> resultSupplier) {
    return result != 0 ? result : nullSafeSupplier(resultSupplier).get();
  }
}
