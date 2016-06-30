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

import static org.cp.elements.util.stream.StreamUtils.stream;

import java.io.File;
import java.io.FileFilter;
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;
import java.util.stream.Collectors;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.ArrayUtils;

/**
 * The FileExtensionFilter class is a {@link FileFilter} and {@link Filter} implementation
 * filtering {@link File}s by extension.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class FileExtensionFilter implements FileFilter, Filter<File>, Iterable<String> {

  private final Set<String> fileExtensions;

  /**
   * Constructs an instance of {@link FileExtensionFilter} initialized with the given array of file extensions
   * used to define filtering criteria used by this {@link FileFilter} to filter files.
   *
   * @param fileExtensions array of file extensions used as the filtering criteria by this {@link FileFilter}
   * to filter files.
   * @see #FileExtensionFilter(Iterable)
   */
  @NullSafe
  public FileExtensionFilter(String... fileExtensions) {
    this(ArrayUtils.iterable(fileExtensions));
  }

  /**
   * Constructs an instance of {@link FileExtensionFilter} initialized with the given {@link Iterable} collection
   * of file extensions used to define filtering criteria used by this {@link FileFilter} to filter files.
   *
   * @param fileExtensions {@link Iterable} collection of file extensions used to define filtering criteria used
   * by this {#link FileFilter} to filter files.
   * @see java.lang.Iterable
   */
  public FileExtensionFilter(Iterable<String> fileExtensions) {
    this.fileExtensions = stream(fileExtensions).filter(StringUtils::hasText).map((fileExtension) ->
      (fileExtension.startsWith(StringUtils.DOT_SEPARATOR) ? fileExtension.substring(1)
        : fileExtension).toLowerCase().trim()
    ).collect(Collectors.toSet());
  }

  /**
   * Returns the {@link Set} of file extensions used by this {@link FileFilter} as filtering criteria to evaluate
   * and filter files.
   *
   * @return the {@link Set} of file extensions used by this {@link FileFilter} as filtering criteria to evaluate
   * and filter files.
   * @see java.util.Set
   */
  public Set<String> getFileExtensions() {
    return Collections.unmodifiableSet(this.fileExtensions);
  }

  /**
   * Determines whether the given {@link File} is accepted by this {@link FileFilter} based on it's extension.
   *
   * @param file {@link File} to evaluate.
   * @return a boolean value indicating whether the given {@link File} is accepted by this {@link FileFilter}.
   * @see org.cp.elements.io.FileUtils#getExtension(java.io.File)
   * @see #getFileExtensions()
   * @see java.io.File
   */
  @Override
  public boolean accept(File file) {
    Set<String> fileExtensions = getFileExtensions();

    return (fileExtensions.isEmpty() || fileExtensions.contains(FileUtils.getExtension(file).toLowerCase().trim()));
  }

  /**
   * Returns an {@link Iterator} over the file extensions used in the filtering criteria of this {@link FileFilter}.
   *
   * @return an {@link Iterator} over the file extensions used in the filtering criteria of this {@link FileFilter}.
   * @see java.lang.Iterable#iterator()
   * @see java.util.Iterator
   */
  @Override
  public Iterator<String> iterator() {
    return getFileExtensions().iterator();
  }
}
