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

/**
 * The FileComparatorFactory class is a factory returning different {@link File} {@link Comparator} implementations
 * based on various {@link File} properties and attributes.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.util.Comparator
 * @since 1.0.0
 */
public abstract class FileComparatorFactory {

  /**
   * Returns a {@link Comparator} used to compare two different {@link File}s for order based on their extension.
   *
   * @return a {@link Comparator} used to compare {@link File}s for order by their extension.
   * @see org.cp.elements.io.FileUtils#getExtension(File)
   * @see java.lang.Comparable#compareTo(Object)
   * @see java.util.Comparator
   * @see java.io.File
   */
  public static Comparator<File> fileExtensionComparator() {
    return Comparator.comparing(FileUtils::getExtension);
  }

  /**
   * Returns a {@link Comparator} used to compare two different {@link File}s for order based on their last modified
   * timestamp.
   *
   * @return a {@link Comparator} used to compare {@link File}s for order by their last modified timestamp.
   * @see java.io.File#lastModified()
   * @see java.lang.Comparable#compareTo(Object)
   * @see java.util.Comparator
   */
  public static Comparator<File> fileLastModifiedComparator() {
    return (fileOne, fileTwo) -> Long.valueOf(fileOne.lastModified()).compareTo(fileTwo.lastModified());
  }

  /**
   * Returns a {@link Comparator} used to compare two different {@link File}s for order based on their name.
   *
   * @return a {@link Comparator} used to compare {@link File}s for order by their name.
   * @see java.io.File#getName()
   * @see java.lang.Comparable#compareTo(Object)
   * @see java.util.Comparator
   */
  public static Comparator<File> fileNameComparator() {
    return Comparator.comparing(File::getName);
  }

  /**
   * Returns a {@link Comparator} used to compare two different {@link File}s for order based on their
   * file system location (absolute path).
   *
   * @return a {@link Comparator} used to compare {@link File}s for order by their file system location (absolute path).
   * @see java.io.File#getAbsolutePath()
   * @see java.lang.Comparable#compareTo(Object)
   * @see java.util.Comparator
   */
  public static Comparator<File> filePathComparator() {
    return Comparator.comparing(File::getAbsolutePath);
  }

  /**
   * Returns a {@link Comparator} used to compare two different {@link File}s for order based on their size.
   *
   * @return a {@link Comparator} used to compare {@link File}s for order by their size.
   * @see java.io.File#length()
   * @see java.lang.Comparable#compareTo(Object)
   * @see java.util.Comparator
   */
  public static Comparator<File> fileSizeComparator() {
    return (fileOne, fileTwo) -> Long.valueOf(fileOne.length()).compareTo(fileTwo.length());
  }
}
