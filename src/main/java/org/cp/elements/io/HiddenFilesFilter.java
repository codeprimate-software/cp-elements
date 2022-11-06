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
 * by whether or not they are {@link File#isHidden() hidden}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class HiddenFilesFilter implements FileFilter, Filter<File> {

  public static final HiddenFilesFilter HIDDEN_FILES = new HiddenFilesFilter(true);
  public static final HiddenFilesFilter NON_HIDDEN_FILES = new HiddenFilesFilter(false);

  private final boolean hidden;

  /**
   * Constructs a new instance of {@link HiddenFilesFilter} initialized with the given boolean value used to
   * indicate whether {@link File#isHidden() hidden} {@link File Files} will be accepted or rejected by
   * this {@link FileFilter}.
   *
   * @param hidden a boolean value indicating whether {@link File#isHidden() hidden} {@link File Files}
   * are accepted or rejected by this {@link FileFilter}.
   */
  protected HiddenFilesFilter(boolean hidden) {
    this.hidden = hidden;
  }

  /**
   * Determines whether the given {@link File} is accepted by this {@link FileFilter} based on whether or not
   * the {@link File} is {@link File#isHidden()}.
   *
   * @param pathname {@link File} to evaluate.
   * @return a boolean value indicating whether {@link File#isHidden() hidden} {@link File Files}
   * are accepted or rejected by this {@link FileFilter}.
   * @see org.cp.elements.lang.Filter#accept(Object)
   * @see java.io.FileFilter#accept(File)
   * @see java.io.File#isHidden()
   * @see java.io.File
   */
  @NullSafe
  @Override
  public boolean accept(@Nullable File pathname) {
    return pathname != null && pathname.isHidden() == this.hidden;
  }
}
