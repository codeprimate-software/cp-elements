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
 * by whether or not they are {@link File#canRead() readable}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class ReadableFilesFilter implements FileFilter, Filter<File> {

  public static final ReadableFilesFilter READABLE_FILES = new ReadableFilesFilter(true);
  public static final ReadableFilesFilter NON_READABLE_FILES = new ReadableFilesFilter(false);

  private final boolean readable;

  /**
   * Constructs a new {@link ReadableFilesFilter} initialized with the given boolean value
   * used to indicate whether readable {@link File Files} are accepted or rejected by this {@link FileFilter}.
   *
   * @param readable a boolean value indicating whether {@link File#canRead() readable} {@link File Files}
   * are accepted or rejected by this {@link FileFilter}.
   */
  protected ReadableFilesFilter(boolean readable) {
    this.readable = readable;
  }

  /**
   * Determines whether the given {@link File} is accepted by this {@link FileFilter} based on whether the {@link File}
   * is {@link File#canRead() readable}.
   *
   * @param pathname {@link File} to evaluate.
   * @return a boolean value indicating whether {@link File#canRead() readable} {@link File Files}
   * are accepted or rejected by this {@link FileFilter}.
   * @see org.cp.elements.lang.Filter#accept(Object)
   * @see java.io.FileFilter#accept(File)
   * @see java.io.File#canRead()
   */
  @NullSafe
  @Override
  public boolean accept(@Nullable File pathname) {
    return pathname != null && pathname.canRead() == this.readable;
  }
}
