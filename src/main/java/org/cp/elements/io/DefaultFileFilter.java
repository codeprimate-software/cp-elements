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

/**
 * The DefaultFileFilter class is a {@link FileFilter} and {@link Filter} implementation that evaluates and filters
 * {@link File}s based on a predetermined result.
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
   * Constructs an instance of the DefaultFileFilter class initialized with the predetermined return value
   * for the {@link #accept(File)} method.
   *
   * @param acceptReturnValue a boolean value specifying the predetermined result of the {@link #accept(File)} method.
   * @see #accept(File)
   */
  public DefaultFileFilter(final boolean acceptReturnValue) {
    this.acceptReturnValue = acceptReturnValue;
  }

  /**
   * Evaluates whether the given {@link File} is accepted by this {@link FileFilter}.
   *
   * @param pathname the {@link File} to evaluate.
   * @return a boolean value indicating whether the given {@link File} is accepted by this {@link FileFilter}.
   * @see org.cp.elements.lang.Filter#accept(Object)
   * @see java.io.FileFilter#accept(File)
   * @see java.io.File
   */
  @Override
  public boolean accept(final File pathname) {
    return acceptReturnValue;
  }

}
