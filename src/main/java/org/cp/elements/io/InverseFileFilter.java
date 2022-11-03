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
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Java {@link FileFilter} and Elements {@link Filter} implementation that returns the inverse result
 * of the delegating {@link FileFilter} from which the {@link InverseFileFilter} is composed.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class InverseFileFilter implements FileFilter, Filter<File> {

  /**
   * Factory method used to construct a new instance of {@link InverseFileFilter} initialized with the given,
   * required {@link FileFilter} used as the delegate to invert and filter {@link File Files}.
   *
   * @param fileFilter {@link FileFilter} used as the delegate and filter for {@link File} objects;
   * must not be {@literal null}.
   * @return a new {@link InverseFileFilter}.
   * @throws IllegalArgumentException if the {@link FileFilter} is {@literal null}.
   * @see java.io.FileFilter
   */
  public static @NotNull InverseFileFilter invert(@NotNull FileFilter fileFilter) {
    return new InverseFileFilter(fileFilter);
  }

  private final FileFilter delegate;

  /**
   * Constructs a new instance of {@link InverseFileFilter} initialized with the given, required {@link FileFilter}
   * used as the delegate to invert and filter {@link File Files}.
   *
   * @param delegate {@link FileFilter} used as the delegate and filter for {@link File} objects;
   * must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link FileFilter} is {@literal null}.
   * @see java.io.FileFilter
   */
  public InverseFileFilter(@NotNull FileFilter delegate) {
    this.delegate = ObjectUtils.requireObject(delegate, "FileFilter is required");
  }

  /**
   * Gets a reference to the configured {@link FileFilter} used as the delegate for filtering {@link File Files}.
   *
   * @return a reference to the configured {@link FileFilter} used as the delegate for filtering {@link File Files}.
   * @see java.io.FileFilter
   */
  protected @NotNull FileFilter getDelegate() {
    return this.delegate;
  }

  /**
   * Determines whether the given {@link File} is accepted by this {@link FileFilter}.
   *
   * @param file {@link File} to filter.
   * @return a boolean value indicating whether the given {@link File} is accepted by this {@link FileFilter}.
   * @see #getDelegate()
   * @see java.io.File
   */
  @Override
  public boolean accept(File file) {
    return !getDelegate().accept(file);
  }
}
