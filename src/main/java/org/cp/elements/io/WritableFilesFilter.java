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
 * The WritableFilesFilter class is a {@link FileFilter} and {@link Filter} implementation that filters {@link File}s
 * by whether they are writable.
 *
 * @author John J. blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class WritableFilesFilter implements FileFilter, Filter<File> {

  public static final WritableFilesFilter WRITABLE_FILES = new WritableFilesFilter(true);
  public static final WritableFilesFilter NON_WRITABLE_FILES = new WritableFilesFilter(false);

  private final boolean writable;

  /**
   * Constructs an instance of the WritableFilesFilter class initialized with the boolean value indicating whether
   * writable {@link File}s are accepted or rejected by this {@link FileFilter}.
   *
   * @param writable a boolean value indicating whether writable {@link File}s are accepted or rejected
   * by this {@link FileFilter}.
   */
  protected WritableFilesFilter(final boolean writable) {
    this.writable = writable;
  }

  /**
   * Determines whether the given {@link File} is accepted by this {@link FileFilter}, which evaluates whether
   * the {@link File} is writable or not.
   *
   * @param pathname the {@link File} to evaluate.
   * @return a boolean value indicating whether writable {@link File}s are accepted or rejected.
   * @see java.io.FileFilter#accept(File)
   * @see java.io.File#canWrite()
   */
  @Override
  public boolean accept(final File pathname) {
    return (pathname.canWrite() == writable);
  }

}
