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

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

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
  @NullSafe
  public static File assertExists(final File path) throws FileNotFoundException {
    if (isExisting(path)) {
      return path;
    }

    throw new FileNotFoundException(String.format("(%1$s) was not found", path));
  }

  /**
   * Creates a file system directory with the given {@link File} path.
   *
   * @param path the given {@link File} indicating the file system path of the directory to create.
   * @return true if the path represented by the {@link File} object is not null, is not an existing file,
   * or the path can be created as a directory if it does not already exist.  Returns true if the directory
   * already exists.
   * @see java.io.File
   */
  @NullSafe
  public static boolean createDirectory(final File path) {
    return (path != null && !path.isFile() && (path.isDirectory() || path.mkdirs()));
  }

  /**
   * Creates a file with the given {@link File} path.
   *
   * @param path the given {@link File} indicating the absolute location and name of the file.
   * @return true if the path represented by the {@link File} object is not null, is not an existing directory,
   * or the path can be created as a file if it does not already exist.  Returns true if the file already exists.
   * @see java.io.File
   */
  @NullSafe
  public static boolean createFile(final File path) {
    try {
      return (path != null && !path.isDirectory() && (path.isFile() || path.createNewFile()));
    }
    catch (IOException ignore) {
      return false;
    }
  }

  /**
   * Deletes the given {@link File}.
   *
   * @param path the {@link File} to delete.
   * @return a boolean value indicating whether the given {@link File} was deleted successfully.
   * Returns false if the {@link File} references is null or does not exist.
   * @see java.io.File#delete()
   * @see #isExisting(File)
   */
  @NullSafe
  public static boolean delete(final File path) {
    return (isExisting(path) && path.delete());
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
    int dotIndex = filename.indexOf(StringUtils.DOT_SEPARATOR);
    return (dotIndex != -1 ? filename.substring(dotIndex + 1) : StringUtils.EMPTY_STRING);
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
    int dotIndex = filename.indexOf(StringUtils.DOT_SEPARATOR);
    return (dotIndex != -1 ? filename.substring(0, dotIndex) : filename);
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
   * Determines whether the given {@link File} has any content.
   *
   * @param path the {@link File} to evaluate.
   * @return a boolean value indicating whether the given {@link File} has any content.
   * @see java.io.File#length()
   * @see #size(File)
   */
  @NullSafe
  public static boolean isEmpty(final File path) {
    return (size(path) == 0l);
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
   * Creates an instance of {@link File} initialized with the given pathname.
   *
   * @param pathname a String indicating the path of the file.
   * @return a new {@link File} initialized with the given pathname.
   * @throws NullPointerException if pathname is null.
   * @see java.io.File#File(String)
   */
  public static File newFile(final String pathname) {
    return new File(pathname);
  }

  /**
   * Reads the contents of the given {@link File} into a {@link String}.
   *
   * @param file the source {@link File} to read the contents from.
   * @return a {@link String} containing the contents of the given {@link File}.
   * @throws IOException if an I/O error occurs while reading the {@link File}.
   * @throws IllegalArgumentException if the {@link File} is not a valid file.
   * @throws IllegalStateException if the file cannot be read.
   * @see java.io.File
   */
  public static String read(final File file) throws IOException {
    Assert.isTrue(isFile(file), "(%1$s) must be a valid file", file);
    Assert.state(file.canRead(), "(%1$s) is unreadable", tryGetCanonicalPathElseGetAbsolutePath(file));

    BufferedReader fileReader = new BufferedReader(new FileReader(file));
    StringBuilder buffer = new StringBuilder();

    try {
      for (String line = fileReader.readLine(); line != null; line = fileReader.readLine()) {
        buffer.append(line);
        buffer.append(StringUtils.LINE_SEPARATOR);
      }

      return buffer.toString().trim();
    }
    finally {
      close(fileReader);
    }
  }

  /**
   * Determines the size in bytes of the given {@link File}.  If the {@link File} is null or does not exist,
   * then 0 is returned.
   *
   * @param path the {@link File} to evaluate.
   * @return a long value indicating the size of the given {@link File} in bytes.  If the {@link File} is null
   * or does not exist, then 0 is returned.
   * @see java.io.File#length()
   */
  @NullSafe
  public static long size(final File path) {
    return (isExisting(path) ? path.length() : 0l);
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

  /**
   * Writes the contents of the given {@link InputStream} out to the given {@link File}.
   *
   * @param in the {@link InputStream} that is used as the source of the {@link File} content.
   * @param file the {@link File} to write the contents of the {@link InputStream} to.
   * @throws IOException if writing the contents of the {@link InputStream} to the {@link File}
   * results in an I/O error.
   * @throws NullPointerException if either the {@link InputStream} or {@link File} reference are null.
   * @throws IllegalStateException if the file exists and is not writable.
   * @see java.io.InputStream
   * @see java.io.File
   * @see #copy(InputStream, OutputStream)
   */
  public static void write(final InputStream in, final File file) throws IOException {
    Assert.notNull(in, "InputStream cannot be null");
    Assert.notNull(file, "File cannot be null");
    Assert.state(!isExisting(file) || file.canWrite(), "(%1$s) is not writable",
      tryGetCanonicalPathElseGetAbsolutePath(file));

    OutputStream out = null;

    try {
      copy(in, out = new BufferedOutputStream(new FileOutputStream(file)));
    }
    finally {
      close(out);
    }
  }

}
