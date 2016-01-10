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
import java.io.FileNotFoundException;
import java.io.IOException;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.StringUtils;

/**
 * The FileUtils class encapsulates several utility methods for working with files.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.cp.elements.io.IOUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileUtils extends IOUtils {

  /**
   * Asserts that the given file exists.
   *
   * @param path the {@link File} to assert for existence.
   * @return a reference back to the file.
   * @throws FileNotFoundException if the file does not exist.
   * @see #isExisting(File)
   */
  public static File assertExists(final File path) throws FileNotFoundException {
    if (isExisting(path)) {
      return path;
    }

    throw new FileNotFoundException(String.format("(%1$s) was not found", path));
  }

  /**
   * Returns the extension of the given file.
   *
   * @param file the {@link File} from which to get the extension.
   * @return the file extension of the given file or an empty String if the file does not have an extension.
   * @throws java.lang.NullPointerException if the file reference is null.
   * @see java.io.File#getName()
   */
  public static String getExtension(final File file) {
    Assert.notNull(file, "File cannot be null");
    String filename = file.getName();
    int index = filename.indexOf(StringUtils.DOT_SEPARATOR);
    return (index != -1 ? filename.substring(index + 1) : StringUtils.EMPTY_STRING);
  }

  /**
   * Returns the absolute path of the given file.
   *
   * @param file the {@link File} from which to get the absolute filesystem path.
   * @return a String indicating the absolute filesystem pathname (location) of the given file.
   * @throws java.lang.NullPointerException if the file reference is null.
   * @see #tryGetCanonicalPathElseGetAbsolutePath(java.io.File)
   * @see java.io.File#getParentFile()
   */
  public static String getLocation(final File file) {
    Assert.notNull(file, "File cannot be null");
    File parent = file.getParentFile();
    Assert.notNull(parent, new IllegalArgumentException(String.format(
      "Unable to determine the location of file (%1$s)", file)));
    return tryGetCanonicalPathElseGetAbsolutePath(parent);
  }

  /**
   * Returns the name of the given file without it's extension.
   *
   * @param file the {@link File} from which to get the name.
   * @return a String indicating the name of the file without it's extension.
   * @throws java.lang.NullPointerException if the file reference is null.
   * @see java.io.File#getName()
   */
  public static String getName(final File file) {
    Assert.notNull(file, "File cannot be null");
    String filename = file.getName();
    int index = filename.indexOf(StringUtils.DOT_SEPARATOR);
    return (index != -1 ? filename.substring(0, index) : filename);
  }

  /**
   * Determines whether the given file actually exists and is a directory.
   *
   * @param path the {@link File} to be evaluated as a directory.
   * @return a boolean valued indicating whether the given file actually exists and is a directory.
   * @see java.io.File#isDirectory()
   */
  @NullSafe
  public static boolean isDirectory(final File path) {
    return (path != null && path.isDirectory());
  }

  /**
   * Determines whether the given file actually exists.
   *
   * @param path the {@link File} to evaluate.
   * @return a boolean valued indicating whether the given file actually exists.
   * @see java.io.File#exists()
   */
  @NullSafe
  public static boolean isExisting(final File path) {
    return (path != null && path.exists());
  }

  /**
   * Determines whether the given file actually exists and is a file.
   *
   * @param path the {@link File} to be evaluated as a file.
   * @return a boolean valued indicating whether the given file actually exists and is a file.
   * @see java.io.File#isFile()
   */
  @NullSafe
  public static boolean isFile(final File path) {
    return (path != null && path.isFile());
  }

  /**
   * Attempts to the get the canonical form of the given file, otherwise returns the absolute form of the file.
   *
   * @param file the {@link File} from which the canonical or absolute file will be returned.
   * @return the canonical form of the file unless an IOException occurs then return the absolute form of the file.
   * @throws NullPointerException if the file reference is null.
   * @see java.io.File#getAbsoluteFile()
   * @see java.io.File#getCanonicalFile()
   */
  public static File tryGetCanonicalFileElseGetAbsoluteFile(final File file) {
    try {
      return file.getCanonicalFile();
    }
    catch (IOException ignore) {
      return file.getAbsoluteFile();
    }
  }

  /**
   * Attempts to the get the canonical path of the given file, otherwise returns the absolute path of the file.
   *
   * @param file the {@link File} from which the canonical or absolute path will be returned.
   * @return the canonical path of the file unless an IOException occurs then return the absolute path of the file.
   * @see java.io.File#getAbsolutePath()
   * @see java.io.File#getCanonicalPath()
   */
  public static String tryGetCanonicalPathElseGetAbsolutePath(final File file) {
    try {
      return file.getCanonicalPath();
    }
    catch (IOException ignore) {
      return file.getAbsolutePath();
    }
  }

}
