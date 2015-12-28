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
import java.math.BigDecimal;
import java.util.Comparator;

/**
 * The FileComparatorFactory class is a factory class returning File Comparator implementations based on various
 * File properties and attributes.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.util.Comparator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileComparatorFactory {

  public static Comparator<File> fileExtensionComparator() {
    return (fileOne, fileTwo) -> FileUtils.getExtension(fileOne).compareTo(FileUtils.getExtension(fileTwo));
  }

  public static Comparator<File> fileLastModifiedComparator() {
    return (fileOne, fileTwo) -> new BigDecimal(fileOne.lastModified() - fileTwo.lastModified()).intValue();
  }

  public static Comparator<File> fileNameComparator() {
    return (fileOne, fileTwo) -> fileOne.getName().compareTo(fileTwo.getName());
  }

  public static Comparator<File> filePathComparator() {
    return (fileOne, fileTwo) -> fileOne.getAbsolutePath().compareTo(fileTwo.getAbsolutePath());
  }

  public static Comparator<File> fileSizeComparator() {
    return (fileOne, fileTwo) -> new BigDecimal(fileOne.length() - fileTwo.length()).intValue();
  }

}
