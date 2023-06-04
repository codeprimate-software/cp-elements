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
 * Java {@link FileFilter} and Elements {@link Filter} implementation evaluating and filtering {@link File Files}
 * based on whether they are {@link File#canExecute() executable}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ExecutableFilesFilter implements FileFilter, Filter<File> {

  public static final ExecutableFilesFilter EXECUTABLE_FILES = new ExecutableFilesFilter(true);
  public static final ExecutableFilesFilter NON_EXECUTABLE_FILES = new ExecutableFilesFilter(false);

  /**
   * Factory method used to return a Singleton instance of {@link ExecutableFilesFilter} based on whether it accepts
   * or rejects {@link File#canExecute() executable} {@link File Files}.
   *
   * @param accept boolean value determining whether {@link File#canExecute() executable} {@link File Files}
   * are accepted or rejected by the {@link FileFilter}.
   * @return a Singleton instance of {@link ExecutableFilesFilter}.
   */
  public static @NotNull ExecutableFilesFilter getInstance(boolean accept) {
    return accept ? EXECUTABLE_FILES : NON_EXECUTABLE_FILES;
  }

  private final boolean executable;

  /**
   * Constructs a new {@link ExecutableFilesFilter} initialized with the given boolean value used to
   * indicate whether {@link File#canExecute() executable} {@link File Files} are accepted or rejected by
   * this {@link FileFilter}.
   *
   * @param executable boolean value determining whether {@link File#canExecute() executable} {@link File Files}
   * are accepted or rejected by this {@link FileFilter}.
   */
  protected ExecutableFilesFilter(boolean executable) {
    this.executable = executable;
  }

  /**
   * Null-safe method used to accept or reject the given {@link File} based on whether it
   * is {@link File#canExecute() executable}.
   *
   * @param pathname {@link File} to evaluate.
   * @return a boolean value indicating whether the {@link File#canExecute() executable} {@link File}
   * is accepted or rejected.
   * @see org.cp.elements.lang.Filter#accept(Object)
   * @see java.io.FileFilter#accept(File)
   * @see java.io.File#canExecute()
   * @see java.io.File
   */
  @NullSafe
  @Override
  public boolean accept(@Nullable File pathname) {
    return pathname != null && pathname.canExecute() == this.executable;
  }
}
