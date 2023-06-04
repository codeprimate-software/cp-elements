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

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Java {@link FileFilter} and Elements {@link Filter} implementation that evaluates and filters {@link File Files}
 * based on a predetermined result.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DefaultFileFilter implements FileFilter, Filter<File> {

  public static final DefaultFileFilter DEFAULT_ACCEPT = new DefaultFileFilter(true);
  public static final DefaultFileFilter DEFAULT_REJECT = new DefaultFileFilter(false);

  private final boolean acceptReturnValue;

  /**
   * Factory method used to get reference to a Singleton instance of {@link DefaultFileFilter} that will either
   * accept or reject all {@link File Files} based on the argument to the accept parameter.
   *
   * @param accept boolean value indicating whether all {@link File Files} are accepted or rejected
   * by the {@link FileFilter}.
   * @return a default {@link FileFilter} implementation that will either accept or reject all {@link File Files}
   * based on the boolean argument.
   * @see java.io.FileFilter
   */
  public static @NotNull FileFilter getInstance(boolean accept) {
    return accept ? DEFAULT_ACCEPT : DEFAULT_REJECT;
  }

  /**
   * Constructs a new {@link DefaultFileFilter} initialized with the predetermined {@link Boolean return value}
   * returned by the {@link #accept(File)} method.
   *
   * @param acceptReturnValue predetermined {@link Boolean boolean value} returned by the {@link #accept(File)} method.
   */
  protected DefaultFileFilter(boolean acceptReturnValue) {
    this.acceptReturnValue = acceptReturnValue;
  }

  /**
   * Evaluates the given {@link File} and determines whether it is accepted by this {@link FileFilter}.
   *
   * @param pathname {@link File} to evaluate.
   * @return a boolean value indicating whether the given {@link File} is accepted by this {@link FileFilter}.
   * @see org.cp.elements.lang.Filter#accept(Object)
   * @see java.io.FileFilter#accept(File)
   * @see java.io.File
   */
  @NullSafe
  @Override
  public boolean accept(@Nullable File pathname) {
    return this.acceptReturnValue;
  }
}
