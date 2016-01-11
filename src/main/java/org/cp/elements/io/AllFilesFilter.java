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
 * The AllFilesFilter class is a {@link FileFilter} and {@link Filter} implementation that accepts
 * all non-null {@link File}s
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AllFilesFilter implements FileFilter, Filter<File> {

  public static final AllFilesFilter INSTANCE = new AllFilesFilter();

  /**
   * Evaluates and determines whether the given {@link File} path satisfies the criteria of this filter.
   *
   * @param path the {@link File} to evaluate.
   * @return a boolean value indicating whether the given file satisfies the criteria of this filter.
   * @see java.io.FileFilter#accept(File)
   */
  @Override
  public boolean accept(final File path) {
    return (path != null);
  }

}
