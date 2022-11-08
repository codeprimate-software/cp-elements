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
package org.cp.elements.io;

import java.io.File;
import java.io.FileFilter;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.RelationalOperator;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * Abstract factory class of Java {@link FileFilter} and Elements {@link Filter} implementations used to
 * evaluate (match) and filter {@link File Files} by {@link File#length() size}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.RelationalOperator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileSizeFilter implements FileFilter, Filter<File> {

  /**
   * Factory method used to construct a new instance of {@link FileSizeFilter} initialized with the given, required
   * {@link RelationalOperator} used during the filtering process to evaluate (match) and filter {@link File Files}
   * by {@link File#length() size}.
   *
   * @param operator [{@link RelationalOperator} used to evaluate a {@link File} by {@link File#length() size};
   * must not be {@literal null}.
   * @return a new {@link FileSizeFilter}.
   * @throws IllegalArgumentException if the {@link RelationalOperator} is {@literal null}.
   * @see org.cp.elements.lang.RelationalOperator
   */
  protected static @NotNull FileSizeFilter create(@NotNull RelationalOperator<Long> operator) {

    Assert.notNull(operator, "RelationalOperator is required");

    return new FileSizeFilter() {

      @NullSafe
      @Override
      public boolean accept(@NotNull File file) {
        return file != null && operator.evaluate(file.length());
      }
    };
  }

  /**
   * Determines whether the given {@link File} is accepted by this {@link FileFilter} based on
   * {@link File} {@link File#length() size}.
   *
   * The {@link File} is accepted if the {@link File File's} {@link File#length() length (byte size)} satisfies
   * the filtering criteria of this {@link FileFilter}.
   *
   * @param file {@link File} to filter.
   * @return a boolean value indicating whether the given {@link File} is accepted by this {@link FileFilter}.
   * @see java.io.FileFilter#accept(File)
   * @see java.io.File
   */
  public abstract boolean accept(File file);

  /**
   * Factory method used to construct a new instance of {@link FileSizeFilter} that filters {@link File Files}
   * by {@link File#length() size} between a minimum and maximum (inclusive) number of bytes.
   *
   * In other words, {@link File#length()} must be greater than {@code minSize} AND less than {@code maxSize}.
   *
   * @param minSize minimum acceptable {@link File} {@link File#length() length (size)} in bytes.
   * @param maxSize maximum acceptable {@link File} {@link File#length() length (size)} in bytes.
   * @return a new {@link FileSizeFilter}.
   * @see org.cp.elements.lang.RelationalOperator#greaterThanEqualToAndLessThanEqualTo(Comparable, Comparable)
   * @see #create(RelationalOperator)
   * @see java.io.File#length()
   */
  public static FileSizeFilter between(long minSize, long maxSize) {
    return create(RelationalOperator.greaterThanEqualToAndLessThanEqualTo(minSize, maxSize));
  }

  /**
   * Factory method used to construct a new instance of {@link FileSizeFilter} that filters {@link File Files}
   * by {@link File#length() size} equal to the given number of bytes.
   *
   * @param size acceptable {@link File} {@link File#length() length (size)} in bytes.
   * @return a new {@link FileSizeFilter}.
   * @see org.cp.elements.lang.RelationalOperator#equalTo(Comparable)
   * @see #create(RelationalOperator)
   * @see java.io.File#length()
   */
  public static FileSizeFilter equalTo(long size) {
    return create(RelationalOperator.equalTo(size));
  }

  /**
   * Factory method used to construct a new instance of {@link FileSizeFilter} that filters {@link File Files}
   * by {@link File#length() size} greater than the given number of bytes.
   *
   * @param size minimum {@link File} {@link File#length() length (size)} in bytes.
   * @return a new {@link FileSizeFilter}.
   * @see org.cp.elements.lang.RelationalOperator#greaterThan(Comparable)
   * @see #create(RelationalOperator)
   * @see java.io.File#length()
   */
  public static FileSizeFilter greaterThan(long size) {
    return create(RelationalOperator.greaterThan(size));
  }

  /**
   * Factory method used to construct a new instance of {@link FileSizeFilter} that filters {@link File Files}
   * by {@link File#length() size} less than the given number of bytes.
   *
   * @param size maximum {@link File} {@link File#length() length (size)} in bytes.
   * @return a new {@link FileSizeFilter}.
   * @see org.cp.elements.lang.RelationalOperator#lessThan(Comparable)
   * @see #create(RelationalOperator)
   * @see java.io.File#length()
   */
  public static FileSizeFilter lessThan(long size) {
    return create(RelationalOperator.lessThan(size));
  }

  /**
   * Factory method used to construct a new instance of {@link FileSizeFilter} that filters {@link File Files}
   * by {@link File#length() size} outside of a minimum and maximum (exclusive) number of bytes.
   *
   * In other words, {@link File#length()} must either be less than {@code lessThanSize}
   * OR greater than {@code greaterThanSize}.
   *
   * @param lessThanSize maximum acceptable {@link File} {@link File#length() length (size)} in bytes.
   * @param greaterThanSize minimum acceptable {@link File} {@link File#length() length (size)} in bytes.
   * @return a new {@link FileSizeFilter}.
   * @see org.cp.elements.lang.RelationalOperator#lessThanOrGreaterThan(Comparable, Comparable)
   * @see #create(RelationalOperator)
   * @see java.io.File#length()
   */
  public static FileSizeFilter outside(long lessThanSize, long greaterThanSize) {
    return create(RelationalOperator.lessThanOrGreaterThan(lessThanSize, greaterThanSize));
  }
}
