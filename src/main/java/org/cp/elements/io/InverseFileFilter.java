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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;

/**
 * The InverseFileFilter class is a {@link FileFilter} and {@link Filter} implementation that returns the inverse
 * result of the delegating {@link FileFilter} from which the {@link InverseFileFilter} is composed.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class InverseFileFilter implements FileFilter, Filter<File> {

  private final FileFilter delegate;

  /**
   * Constructs an instance of the InverseFileFilter class initialized with the required, delegating FileFilter.
   *
   * @param delegate the FileFilter used as the delegate for the actual file filtering operation.
   * @throws java.lang.NullPointerException if the delegating FileFilter reference is null.
   * @see java.io.FileFilter
   */
  public InverseFileFilter(FileFilter delegate) {
    Assert.notNull(delegate, "FileFilter must not be null");
    this.delegate = delegate;
  }

  /**
   * Gets the {@link FileFilter} used as the delegate for the {@link InverseFileFilter}.
   *
   * @return the backing, delegating {@link FileFilter} instance.
   * @see java.io.FileFilter
   */
  protected FileFilter getDelegate() {
    return delegate;
  }

  /**
   * Determines whether the specified {@link File} is accepted by this {@link FileFilter}.
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
