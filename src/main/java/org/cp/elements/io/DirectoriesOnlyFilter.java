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
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Java {@link FileFilter} and Elements {@link Filter} implementation that evaluates and filters {@link File Files}
 * to only include file system {@link File#isDirectory() directories}.
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
   * Accepts all {@link File Files} referring to {@link File#isDirectory() directories} in the file system.
   *
   * @param file {@link File} to evaluate.
   * @return a boolean value indicating whether the given {@link File} refers to a {@link File#isDirectory() directory}.
   * @see org.cp.elements.io.FileUtils#isDirectory(File)
   * @see org.cp.elements.lang.Filter#accept(Object)
   * @see java.io.FileFilter#accept(File)
   * @see java.io.File
   */
  @NullSafe
  public boolean accept(@Nullable File file) {
    return FileUtils.isDirectory(file);
  }
}
