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
 * The InverseFileFilter class is a FileFilter implementation that returns the inverse result
 * of the delegating FileFilter from which the InverseFileFilter is composed.
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
  public InverseFileFilter(final FileFilter delegate) {
    Assert.notNull(delegate, "The delegating FileFilter must not be null!");
    this.delegate = delegate;
  }

  /**
   * Gets the FileFilter used as the delegate to the InverseFilterFilter.
   *
   * @return the backing, delegating FileFilter instance.
   * @see java.io.File
   */
  protected FileFilter getDelegate() {
    return delegate;
  }

  /**
   * Determines whether the specified File matches the criteria of this FileFilter.
   *
   * @param file the File to filter.
   * @return a boolean value indicating whether the specified File matches the criteria of this FileFilter.
   * @see #getDelegate()
   * @see java.io.File
   */
  @Override
  public boolean accept(final File file) {
    return !getDelegate().accept(file);
  }

}
