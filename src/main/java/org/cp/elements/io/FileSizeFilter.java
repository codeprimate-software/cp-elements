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

package org.cp.elements.io;

import java.io.File;
import java.io.FileFilter;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.RelationalOperator;

/**
 * The FileSizeFilter class is a {@link FileFilter} and {@link Filter} of {@link File}s implementation
 * that filters files based on their byte size.
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
   * Factory method to create an instance of the {@link FileSizeFilter} initialized with the given
   * {@link RelationalOperator} used during the filtering process to evaluate {@link File}s by size.
   *
   * @param operator [{@link RelationalOperator} used to evaluate a {@link File}'s size.
   * @return an instance of {@link FileSizeFilter}.
   * @see org.cp.elements.lang.RelationalOperator#evaluate(Comparable)
   * @see org.cp.elements.io.FileSizeFilter
   * @see java.io.File
   */
  protected static FileSizeFilter create(RelationalOperator<Long> operator) {
    return new FileSizeFilter() {
      @Override @NullSafe
      public boolean accept(File file) {
        return (file != null && operator.evaluate(file.length()));
      }
    };
  }

  /**
   * Determines whether the given {@link File} is accepted by this {@link FileFilter}.  The {@link File} is accepted
   * if the {@link File}'s length (byte size) satisfies the criteria of this {@link FileFilter}.
   *
   * @param file {@link File} to filter.
   * @return a boolean value indicating whether the given {@link File}'s length (byte size) satisfies the criteria
   * of this {@link FileFilter}.
   * @see java.io.FileFilter#accept(java.io.File)
   * @see java.io.File
   */
  public abstract boolean accept(File file);

  /**
   * Factory method to create an instance of the {@link FileSizeFilter}, which filters {@link File}s by length
   * between a minimum and maximum (inclusive) size in bytes.
   *
   * @param minSize minimum acceptable {@link File} length (size) in bytes.
   * @param maxSize maximum acceptable {@link File} length (size) in bytes.
   * @return an instance of {@link FileSizeFilter}.
   * @see org.cp.elements.lang.RelationalOperator#greaterThanEqualToAndLessThanEqualTo(Comparable, Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static FileSizeFilter between(long minSize, long maxSize) {
    return create(RelationalOperator.greaterThanEqualToAndLessThanEqualTo(minSize, maxSize));
  }

  /**
   * Factory method to create an instance of the {@link FileSizeFilter}, which filters {@link File}s by length
   * equal to the given size in bytes.
   *
   * @param size acceptable {@link File} length (size) in bytes.
   * @return an instance of {@link FileSizeFilter}.
   * @see org.cp.elements.lang.RelationalOperator#equalTo(Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static FileSizeFilter equalTo(long size) {
    return create(RelationalOperator.equalTo(size));
  }

  /**
   * Factory method to create an instance of the {@link FileSizeFilter}, which filters {@link File}s by length
   * greater than the given size in bytes.
   *
   * @param size acceptable lower bound {@link File} length (size) in bytes.
   * @return an instance of {@link FileSizeFilter}.
   * @see org.cp.elements.lang.RelationalOperator#greaterThan(Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static FileSizeFilter greaterThan(long size) {
    return create(RelationalOperator.greaterThan(size));
  }

  /**
   * Factory method to create an instance of the {@link FileSizeFilter}, which filters {@link File}s by length
   * less than the given size in bytes.
   *
   * @param size acceptable upper bound {@link File} length (size) in bytes.
   * @return an instance of {@link FileSizeFilter}.
   * @see org.cp.elements.lang.RelationalOperator#lessThan(Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static FileSizeFilter lessThan(long size) {
    return create(RelationalOperator.lessThan(size));
  }

  /**
   * Factory method to create an instance of the {@link FileSizeFilter}, which filters {@link File}s by length
   * outsize a minimum and maximum (inclusive) size in bytes.
   *
   * @param lessorSize lessor acceptable {@link File} length (size) in bytes.
   * @param greaterSize greater acceptable {@link File} length (size) in bytes.
   * @return an instance of {@link FileSizeFilter}.
   * @see org.cp.elements.lang.RelationalOperator#lessThanEqualToOrGreaterThanEqualTo(Comparable, Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static FileSizeFilter outside(long lessorSize, long greaterSize) {
    return create(RelationalOperator.lessThanEqualToOrGreaterThanEqualTo(lessorSize, greaterSize));
  }
}
