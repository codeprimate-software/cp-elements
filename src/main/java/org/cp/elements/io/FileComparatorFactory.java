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
import java.util.Comparator;

import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract factory class used to return different {@link File} {@link Comparator} implementations based on
 * different {@link File} properties and attributes.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.util.Comparator
 * @since 1.0.0
 */
public abstract class FileComparatorFactory {

  /**
   * Returns a {@link Comparator} used to compare {@link File Files} by extension.
   *
   * @return a {@link Comparator} used to compare {@link File Files} by extension.
   * @see org.cp.elements.io.FileUtils#getExtension(File)
   * @see java.util.Comparator
   * @see java.io.File
   */
  public static @NotNull Comparator<File> fileExtensionComparator() {
    return Comparator.comparing(FileUtils::getExtension);
  }

  /**
   * Returns a {@link Comparator} used to compare {@link File Files} by
   * {@link File#lastModified() last modified timestamp}.
   *
   * @return a {@link Comparator} used to compare {@link File Files} by
   * {@link File#lastModified() last modified timestamp}.
   * @see java.io.File#lastModified()
   * @see java.util.Comparator
   * @see java.io.File
   */
  public static @NotNull Comparator<File> fileLastModifiedComparator() {
    return Comparator.comparingLong(File::lastModified);
  }

  /**
   * Returns a {@link Comparator} used to compare {@link File Files} by {@link File#getName() name}.
   *
   * @return a {@link Comparator} used to compare {@link File Files} by {@link File#getName() name}.
   * @see org.cp.elements.io.FileUtils#getName(File)
   * @see java.io.File#getName()
   * @see java.util.Comparator
   * @see java.io.File
   */
  public static @NotNull Comparator<File> fileNameComparator() {
    return Comparator.comparing(FileUtils::getName);
  }

  /**
   * Returns a {@link Comparator} used to compare {@link File Files} by
   * {@link File#getAbsolutePath() file system location (absolute path)}.
   *
   * @return a {@link Comparator} used to compare {@link File Files} by
   * {@link File#getAbsolutePath() file system location (absolute path)}.
   * @see org.cp.elements.io.FileUtils#tryGetCanonicalPathElseGetAbsolutePath(File)
   * @see java.io.File#getAbsolutePath()
   * @see java.io.File#getCanonicalPath()
   * @see java.util.Comparator
   * @see java.io.File
   */
  public static @NotNull Comparator<File> filePathComparator() {
    return Comparator.comparing(FileUtils::tryGetCanonicalPathElseGetAbsolutePath);
  }

  /**
   * Returns a {@link Comparator} used to compare {@link File Files} by {@link File#length() size}.
   *
   * @return a {@link Comparator} used to compare {@link File Files} by {@link File#length() size}.
   * @see java.io.File#length()
   * @see java.util.Comparator
   * @see java.io.File
   */
  public static @NotNull Comparator<File> fileSizeComparator() {
    return Comparator.comparingLong(File::length);
  }
}
