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
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Java {@link FileFilter} and Elements {@link Filter} implementation evaluating and filtering {@link File Files}
 * by {@link String extension}.
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

  protected static final Function<String, String> STRIP_FILE_EXTENSION_SEPARATOR = fileExtension ->
    fileExtension.startsWith(FileUtils.FILE_EXTENSION_SEPARATOR)
      ? fileExtension.substring(1)
      : fileExtension;

  protected static final Function<String, String> TO_LOWER_CASE_AND_TRIM =
    value -> StringUtils.toLowerCase(StringUtils.trim(value));

  protected static final Function<String, String> FILE_EXTENSION_RESOLVER =
    TO_LOWER_CASE_AND_TRIM.andThen(STRIP_FILE_EXTENSION_SEPARATOR);

  private final Set<String> fileExtensions;

  /**
   * Constructs a new {@link FileExtensionFilter} initialized with the given array of {@link File}
   * {@link String extensions} used to define filtering criteria for matching with this {@link FileFilter}.
   *
   * @param fileExtensions array of {@link File} {@link String extensions} used to define filtering criteria
   * for matching with this {@link FileFilter}.
   * @see #FileExtensionFilter(Iterable)
   */
  @NullSafe
  public FileExtensionFilter(@Nullable String... fileExtensions) {
    this(ArrayUtils.asIterable(fileExtensions));
  }

  /**
   * Constructs a new {@link FileExtensionFilter} initialized with the given {@link Iterable} collection of {@link File}
   * {@link String extensions} used to define filtering criteria for matching with this {@link FileFilter}.
   *
   * @param fileExtensions {@link Iterable} collection of {@link File} {@link String extensions} used to
   * define filtering criteria for matching with this {#link FileFilter}.
   * @see java.lang.Iterable
   */
  @NullSafe
  public FileExtensionFilter(@NotNull Iterable<String> fileExtensions) {

    this.fileExtensions = StreamUtils.stream(CollectionUtils.nullSafeIterable(fileExtensions))
      .filter(StringUtils::hasText)
      .map(FILE_EXTENSION_RESOLVER)
      .collect(Collectors.toSet());
  }

  /**
   * Returns the configured {@link Set} of {@link File} {@link String extensions} used by this {@link FileFilter}
   * as filtering criteria to evaluate (match) and filter {@link File Files}.
   *
   * @return the configured {@link Set} of {@link File} {@link String extensions} used by this {@link FileFilter}
   * as filtering criteria to evaluate (match) and filter {@link File Files}.
   * @see java.util.Set
   */
  public Set<String> getFileExtensions() {
    return Collections.unmodifiableSet(this.fileExtensions);
  }

  /**
   * Determines whether the given {@link File} is accepted by this {@link FileFilter} based on {@link File} extension.
   *
   * @param file {@link File} to evaluate.
   * @return a boolean value indicating whether the given {@link File} is accepted by this {@link FileFilter}
   * based on {@link File} extension.
   * @see org.cp.elements.io.FileUtils#getExtension(java.io.File)
   * @see #getFileExtensions()
   * @see java.io.File
   */
  @NullSafe
  @Override
  public boolean accept(@NotNull File file) {

    Set<String> fileExtensions = getFileExtensions();

    return file != null
      && (fileExtensions.isEmpty()
      || fileExtensions.contains(FILE_EXTENSION_RESOLVER.apply(FileUtils.getExtension(file))));
  }

  /**
   * Returns an {@link Iterator} iterating over the configured {@link File} extensions used as filtering criteria
   * and matching by this {@link FileFilter}.
   *
   * @return an {@link Iterator} iterating over the configured {@link File} extensions used as filtering criteria
   * and matching by this {@link FileFilter}.
   * @see java.lang.Iterable#iterator()
   * @see #getFileExtensions()
   * @see java.util.Iterator
   */
  @Override
  public Iterator<String> iterator() {
    return getFileExtensions().iterator();
  }
}
