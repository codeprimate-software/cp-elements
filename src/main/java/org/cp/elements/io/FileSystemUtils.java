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

import static org.cp.elements.lang.LangExtensions.assertThat;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.util.ArrayUtils;

/**
 * The FileSystemUtils class is an abstract utility class for working with the operating system's file system.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.io.FileUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileSystemUtils extends FileUtils {

  public static final File JAVA_HOME = new File(System.getProperty("java.home"));
  public static final File JAVA_EXE = new File(new File(JAVA_HOME, "bin"), "java");

  public static final File TEMPORARY_DIRECTORY = new File(System.getProperty("java.io.tmpdir"));
  public static final File USER_HOME_DIRECTORY = new File(System.getProperty("user.home"));
  public static final File WORKING_DIRECTORY = new File(System.getProperty("user.dir"));

  public static final File[] NO_FILES = new File[0];

  public static final String FILE_SEPARATOR = System.getProperty("file.separator");

  /**
   * Constructs a fully-qualified file system path with the given path elements appended to the given base pathname
   * using the {@link File#separator}.  If the array of path elements is null or empty then base pathname is returned.
   *
   * @param basePathname a {@link String} indicating the base pathname.
   * @param pathElements an array of path elements to append to the base pathname.
   * @return the path elements appended to the base pathname.
   * @throws NullPointerException if the basePathname is null.
   * @see java.io.File#separator
   */
  public static String appendToPath(String basePathname, String... pathElements) {
    Assert.notNull(basePathname, "basePathname cannot be null");

    String fileSeparator = (SystemUtils.isWindows() ? "\\" : FILE_SEPARATOR);

    for (String pathElement : ArrayUtils.nullSafeArray(pathElements, String.class)) {
      if (StringUtils.hasText(pathElement)) {
        basePathname = String.format("%1$s%2$s%3$s", basePathname.trim(), fileSeparator, pathElement.trim());
      }
    }

    return (SystemUtils.isWindows() ? basePathname.replace("\\\\", "\\") : basePathname.replace("//", fileSeparator));
  }

  /**
   * Creates a path with the given path elements delimited with File.separator.
   *
   * @param pathElements an array of Strings constituting elements of the path.
   * @return a fully constructed pathname containing the elements from the given array as path elements separated
   * by File.separator.
   * @see #createPath(String[], String)
   * @see java.io.File#separator
   */
  public static String createPath(String... pathElements) {
    return createPath(pathElements, File.separator);
  }

  /**
   * Creates a path with the given path elements delimited by separator.
   *
   * @param pathElements an array of Strings constituting elements of the path.
   * @param separator a String specifying the path separator.  If the given String is null, then separator defaults to
   * File.separator.
   * @return a fully constructed pathname containing the elements of the path from the given array delimited
   * by the separator.
   * @throws NullPointerException if pathElements is null.
   * @see #createPath(String...)
   * @see java.io.File#separator
   */
  public static String createPath(String[] pathElements, String separator) {
    separator = ObjectUtils.defaultIfNull(separator, File.separator);

    StringBuilder buffer = new StringBuilder();

    for (String pathElement : pathElements) {
      buffer.append(separator).append(pathElement);
    }

    return buffer.toString();
  }

  public static boolean deleteRecursive(final File path) {
    Assert.notNull(path, "The file system path to delete cannot be null");

    boolean success = true;

    if (isDirectory(path)) {
      for (File file : safeListFiles(path)) {
        success &= deleteRecursive(file);
      }
    }

    return (path.delete() && success);
  }

  public static boolean isEmpty(final File directory) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  @NullSafe
  public static boolean isRelativeToWorkingDirectory(final File path) {
    return (isDirectory(path) && tryGetCanonicalPathElseGetAbsolutePath(path).startsWith(
      tryGetCanonicalPathElseGetAbsolutePath(WORKING_DIRECTORY)));
  }

  @NullSafe
  public static File[] listFiles(final File directory, FileFilter fileFilter) {
    Assert.isTrue(isDirectory(directory), String.format("(%1$s) is not a valid directory",
      directory));

    List<File> results = new ArrayList<>();

    fileFilter = (fileFilter != null ? fileFilter : FileOnlyFilter.INSTANCE);

    for (File file : safeListFiles(directory, fileFilter)) {
      if (file.isDirectory()) {
        results.addAll(Arrays.asList(listFiles(file, fileFilter)));
      }
      else {
        results.add(file);
      }
    }

    return results.toArray(new File[results.size()]);
  }

  /* (non-Javadoc) */
  @NullSafe
  private static File[] safeListFiles(final File directory) {
    return safeListFiles(directory, AcceptingAllNonNullFilesFilter.INSTANCE);
  }

  /* (non-Javadoc) */
  @NullSafe
  private static File[] safeListFiles(final File directory, final FileFilter fileFilter) {
    File[] files = (isDirectory(directory) ? directory.listFiles(fileFilter) : null);
    return (files != null ? files : NO_FILES);
  }

  public static int size(final File directory, final boolean recurse) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

}
