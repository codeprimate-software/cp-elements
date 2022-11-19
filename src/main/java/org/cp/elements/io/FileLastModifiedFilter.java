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
import java.util.function.Function;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.RelationalOperator;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * Abstract factory class of Java {@link FileFilter} and Elements {@link Filter} implementations used to
 * evaluate (match) and filter {@link File Files} by {@link File#lastModified() last modified timestamp}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.RelationalOperator
 * @since 1.0.0
 */
public abstract class FileLastModifiedFilter implements FileFilter, Filter<File> {

  /**
   * Factory method used to construct a new instance of {@link FileLastModifiedFilter} initialized with
   * the given, required {@link RelationalOperator} used in the evaluation and filtering of {@link File Files}
   * {@link File#lastModified() last modified timestamp} when {@link #accept(File)} is called.
   *
   * @param operator {@link RelationalOperator} used to evaluate the {@link File}; must not be {@literal null}.
   * @return a new {@link FileLastModifiedFilter} initialized with the given, required {@link RelationalOperator}
   * to evaluate and filter {@link File Files} by {@link File#lastModified() last modified timestamp}.
   * @throws IllegalArgumentException if the {@link RelationalOperator} is {@literal null}.
   * @see org.cp.elements.lang.RelationalOperator
   * @see #create(RelationalOperator, Function)
   * @see java.io.File#lastModified()
   */
  protected static @NotNull FileLastModifiedFilter create(@NotNull RelationalOperator<Long> operator) {
    return create(operator, File::lastModified);
  }

  /**
   * Factory method used to construct a new instance of {@link FileLastModifiedFilter} initialized with the given,
   * required {@link RelationalOperator} used in the evaluation and filtering of {@link File Files}
   * by {@link File#lastModified() last modified timestamp}.
   *
   * By default, the {@link FileLastModifiedFilter} reject {@literal null} {@link File} references.
   *
   * @param <T> {@link Class type} of the {@link File} attribute that is the subject of the comparison.
   * @param operator {@link RelationalOperator} used to evaluate the {@link File}; must not be {@literal null}.
   * @param fileFunction {@link Function} used to extract the {@link File} attribute to compare;
   * must not be {@literal null}.
   * @return a new {@link FileLastModifiedFilter}.
   * @throws IllegalArgumentException if the {@link RelationalOperator} or {@link Function} used to
   * extract the {@link File} attribute is {@literal null}.
   * @see org.cp.elements.lang.RelationalOperator
   * @see java.util.function.Function
   */
  protected static @NotNull <T extends Comparable<T>> FileLastModifiedFilter create(
      @NotNull RelationalOperator<T> operator, @NotNull Function<File, T> fileFunction) {

    Assert.notNull(operator, "RelationalOperator is required");
    Assert.notNull(fileFunction, "Function used to transform the File for relational comparison is required");

    return new FileLastModifiedFilter() {

      @NullSafe
      @Override
      public boolean accept(@NotNull File file) {
        return file != null && operator.evaluate(fileFunction.apply(file));
      }
    };
  }

  /**
   * Determines whether the given {@link File} is accepted by this {@link FileFilter}.
   *
   * The {@link File} is accepted if the {@link File File's} {@link File#lastModified() last modified timestamp}
   * matches the expected date/time criteria of this {@link FileFilter}.
   *
   * @param file {@link File} to evaluate and filter.
   * @return a boolean value indicating whether the given {@link File} is accepted by this {@link FileFilter}
   * based on the {@link File#lastModified() File's last modified timestamp}.
   * @see java.io.FileFilter#accept(File)
   * @see java.io.File
   */
  public abstract boolean accept(File file);

  /**
   * Factory method used to construct a new instance of {@link FileLastModifiedFilter} that evaluates (matches)
   * and filters {@link File Files} by a {@link File#lastModified() last modified timestamp} occurring {@literal after}
   * the given date/time declared in {@link Long milliseconds since the epoch}.
   *
   * @param lastModified {@link Long date/time} declared in milliseconds since the epoch used to
   * filter {@link File Files} by {@link File#lastModified() last modified timestamp} after the given date/time.
   * @return a new {@link FileLastModifiedFilter}.
   * @see org.cp.elements.lang.RelationalOperator#greaterThan(Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static @NotNull FileLastModifiedFilter after(long lastModified) {
    return create(RelationalOperator.greaterThan(lastModified));
  }

  /**
   * Factory method used to construct a new instance of {@link FileLastModifiedFilter} that evaluates (matches)
   * and filters {@link File Files} by a {@link File#lastModified() last modified timestamp} occurring {@literal before}
   * the given date/time declared in {@link Long milliseconds since the epoch}.
   *
   * @param lastModified {@link Long date/time} declared in milliseconds since the epoch used to
   * filter {@link File Files} by {@link File#lastModified() last modified timestamp} before the given date/time.
   * @return a new {@link FileLastModifiedFilter}.
   * @see org.cp.elements.lang.RelationalOperator#lessThan(Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static @NotNull FileLastModifiedFilter before(long lastModified) {
    return create(RelationalOperator.lessThan(lastModified));
  }

  /**
   * Factory method used to construct a new instance of {@link FileLastModifiedFilter} that evaluates (matches)
   * and filters {@link File Files} by {@link File#lastModified() last modified timestamp} occurring {@literal during}
   * (between) the given dates and times declared in {@link Long milliseconds since the epoch}.
   *
   * @param lastModifiedOnAfter {@link Long date/time} declared in milliseconds since the epoch used to
   * filter {@link File Files} by {@link File#lastModified() last modified timestamp} on or after the given date/time.
   * @param lastModifiedOnBefore {@link Long date/time} declared in milliseconds since the epoch used to
   * filter {@link File Files} by {@link File#lastModified() last modified timestamp} on or before the given date/time.
   * @return a new {@link FileLastModifiedFilter}.
   * @see org.cp.elements.lang.RelationalOperator#greaterThanEqualToAndLessThanEqualTo(Comparable, Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static @NotNull FileLastModifiedFilter during(long lastModifiedOnAfter, long lastModifiedOnBefore) {
    return create(RelationalOperator.greaterThanEqualToAndLessThanEqualTo(lastModifiedOnAfter, lastModifiedOnBefore));
  }

  /**
   * Factory method used to construct a new instance of {@link FileLastModifiedFilter} that evaluates (matches)
   * and filters {@link File Files} by a {@link File#lastModified() last modified timestamp} occurring {@literal on}
   * the given date/time declared in {@link Long milliseconds since the epoch}.
   *
   * @param lastModified {@link Long date/time} declared in milliseconds since the epoch used to
   * filter {@link File Files} by {@link File#lastModified() last modified timestamp} on the given date/time.
   * @return a new {@link FileLastModifiedFilter}.
   * @see org.cp.elements.lang.RelationalOperator#equalTo(Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static @NotNull FileLastModifiedFilter on(long lastModified) {
    return create(RelationalOperator.equalTo(lastModified));
  }

  /**
   * Factory method used to construct a new instance of {@link FileLastModifiedFilter} that evaluates (matches)
   * and filters {@link File Files} by {@link File#lastModified() last modified timestamp} occurring {@literal outside}
   * the given dates and times declared in {@link Long milliseconds since the epoch}.
   *
   * @param lastModifiedBefore {@link Long date/time} declared in milliseconds since the epoch used to
   * filter {@link File Files} by {@link File#lastModified() last modified timestamp} before the given date/time.
   * @param lastModifiedAfter {@link Long date/time} declared in milliseconds since the epoch used to
   * filter {@link File Files} by {@link File#lastModified() last modified timestamp} after the given date/time.
   * @return a new {@link FileLastModifiedFilter}.
   * @see org.cp.elements.lang.RelationalOperator#lessThanEqualToOrGreaterThanEqualTo(Comparable, Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static @NotNull FileLastModifiedFilter outside(long lastModifiedBefore, long lastModifiedAfter) {
    return create(RelationalOperator.lessThanOrGreaterThan(lastModifiedBefore, lastModifiedAfter));
  }
}
