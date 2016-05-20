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
 * The DirectoriesOnlyFilter class is a {@link FileFilter} as well as {@link Filter} implementation that evaluates
 * and filters {@link File}s to only include file system directories.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DirectoriesOnlyFilter implements FileFilter, Filter<File> {

  public static final DirectoriesOnlyFilter INSTANCE = new DirectoriesOnlyFilter();

  /**
   * Accepts all {@link File}s referring to directories in the file system.
   *
   * @param file the {@link File} to evaluate.
   * @return a boolean value indicating whether the given {@link File} refers to a directory.
   * @see org.cp.elements.io.FileUtils#isDirectory(File)
   * @see org.cp.elements.lang.Filter#accept(Object)
   * @see java.io.FileFilter#accept(File)
   * @see java.io.File
   */
  public boolean accept(File file) {
    return FileUtils.isDirectory(file);
  }
}
