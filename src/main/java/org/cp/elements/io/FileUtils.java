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

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.stream.Collectors;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * Abstract utility {@link Class} encapsulating several utility methods for working with {@link File Files}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.InputStream
 * @see java.io.OutputStream
 * @see org.cp.elements.io.IOUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileUtils extends IOUtils {

  /**
   * Asserts that the given {@link File} exists.
   *
   * @param path {@link File} to assert for existence.
   * @return a reference back to the given {@link File}.
   * @throws java.io.FileNotFoundException if the {@link File} does not exist.
   * @see #isExisting(File)
   * @see java.io.File
   */
  @NullSafe
  public static File assertExists(File path) throws FileNotFoundException {

    if (isExisting(path)) {
      return path;
    }

    throw new FileNotFoundException(String.format("[%s] was not found", path));
  }

  /**
   * Creates a file system directory with the given {@link File} path.
   *
   * @param path the given {@link File} indicating the file system path of the directory to create.
   * @return true if the path represented by the {@link File} object is not null, is not an existing file,
   * or the path can be created as a directory if it does not already exist.  Returns true if the directory
   * already exists.
   * @see java.io.File#mkdirs()
   */
  @NullSafe
  public static boolean createDirectory(File path) {

    return path != null
      && !path.isFile()
      && (path.isDirectory() || path.mkdirs());
  }

  /**
   * Creates a file system file with the given {@link File} path.
   *
   * @param path the given {@link File} indicating the absolute location and name of the file.
   * @return true if the path represented by the {@link File} object is not null, is not an existing directory,
   * or the path can be created as a file if it does not already exist.  Returns true if the file already exists.
   * @see java.io.File#createNewFile()
   */
  @NullSafe
  public static boolean createFile(File path) {

    try {
      return path != null
        && !path.isDirectory()
        && (path.isFile() || path.createNewFile());
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
  public static boolean delete(File path) {
    return isExisting(path) && path.delete();
  }

  /**
   * Returns the {@link String extension} of the given {@link File}.
   *
   * @param file {@link File} from which to get the {@link String extension}.
   * @return the {@link File} {@link String extension} of the given {@link File} or an empty {@link String}
   * if the {@link File} does not have an extension.
   * @throws java.lang.NullPointerException if the {@link File} reference is {@literal null}.
   * @see java.io.File#getName()
   */
  public static String getExtension(File file) {

    Assert.notNull(file, "File cannot be null");

    String filename = file.getName();

    int dotIndex = filename.indexOf(StringUtils.DOT_SEPARATOR);

    return dotIndex != -1 ? filename.substring(dotIndex + 1) : StringUtils.EMPTY_STRING;
  }

  /**
   * Returns the {@link String absolute path} of the given {@link File}.
   *
   * @param file {@link File} from which to get the {@link String absolute filesystem path}.
   * @return a {@link String} containing the absolute filesystem pathname (location) of the given {@link File}.
   * @throws java.lang.NullPointerException if the {@link File} reference is {@literal null}.
   * @see #tryGetCanonicalPathElseGetAbsolutePath(java.io.File)
   * @see java.io.File#getParentFile()
   */
  public static String getLocation(File file) {

    Assert.notNull(file, "File cannot be null");

    File parent = file.getParentFile();

    Assert.notNull(parent, newIllegalArgumentException("Unable to determine the location of file [%1$s]", file));

    return tryGetCanonicalPathElseGetAbsolutePath(parent);
  }

  /**
   * Returns the {@link String name} of the given {@link File} without it's extension.
   *
   * @param file {@link File} from which to get the {@link String name}.
   * @return a {@link String} containing the name of the {@link File} without it's extension.
   * @throws java.lang.NullPointerException if the {@link File} reference is {@literal null}.
   * @see java.io.File#getName()
   */
  public static String getName(File file) {

    Assert.notNull(file, "File cannot be null");

    String filename = file.getName();

    int dotIndex = filename.indexOf(StringUtils.DOT_SEPARATOR);

    return dotIndex != -1 ? filename.substring(0, dotIndex) : filename;
  }

  /**
   * Determines whether the given file actually exists and is a directory.
   *
   * @param path the {@link File} to be evaluated as a directory.
   * @return a boolean valued indicating whether the given file actually exists and is a directory.
   * @see java.io.File#isDirectory()
   */
  @NullSafe
  public static boolean isDirectory(File path) {
    return path != null && path.isDirectory();
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
  public static boolean isEmpty(File path) {
    return size(path) == 0L;
  }

  /**
   * Determines whether the given file actually exists.
   *
   * @param path the {@link File} to evaluate.
   * @return a boolean valued indicating whether the given file actually exists.
   * @see java.io.File#exists()
   */
  @NullSafe
  public static boolean isExisting(File path) {
    return path != null && path.exists();
  }

  /**
   * Determines whether the given file actually exists and is a file.
   *
   * @param path the {@link File} to be evaluated as a file.
   * @return a boolean valued indicating whether the given file actually exists and is a file.
   * @see java.io.File#isFile()
   */
  @NullSafe
  public static boolean isFile(File path) {
    return path != null && path.isFile();
  }

  /**
   * Creates a new instance of {@link File} initialized with the given {@link String pathname}.
   *
   * @param pathname {@link String} containing the path of the {@link File} to create.
   * @return a new {@link File} initialized with the given {@link String pathname}.
   * @throws NullPointerException if {@link String pathname} is {@literal null}.
   * @see java.io.File#File(String)
   */
  public static File newFile(String pathname) {
    return new File(pathname);
  }

  /**
   * Reads the contents from the given {@link File} into a {@link String}.
   *
   * @param file {@link File} to read.
   * @return a {@link String} containing the contents of the {@link File}.
   * @throws IOException if the given {@link File} cannot be read.
   * @throws IllegalArgumentException if the {@link File} is not valid.
   * @throws IllegalStateException if the {@link File} is not readable.
   * @see #readLines(File)
   * @see java.io.File
   */
  public static String read(File file) throws IOException {

    StringBuilder buffer = new StringBuilder();

    readLines(file).forEach(line -> {
      buffer.append(line);
      buffer.append(StringUtils.LINE_SEPARATOR);
    });

    return buffer.toString().trim();
  }

  /**
   * Reads the contents of the given {@link File} into a {@link String}.
   *
   * @param file the source {@link File} to read the contents from.
   * @return a {@link String} containing the contents of the given {@link File}.
   * @throws IOException if an I/O error occurs while reading the {@link File}.
   * @throws IllegalArgumentException if the {@link File} is not a valid file.
   * @throws IllegalStateException if the file cannot be read.
   * @see java.io.File#canRead()
   * @see #isFile(File)
   */
  public static List<String> readLines(File file) throws IOException {

    Assert.isTrue(isFile(file), "[%s] must be a valid file", file);
    Assert.state(file.canRead(), "[%s] is unreadable", tryGetCanonicalPathElseGetAbsolutePath(file));

    try (BufferedReader fileReader = new BufferedReader(new FileReader(file))) {
      return fileReader.lines().collect(Collectors.toList());
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
   * @see #isFile(File)
   */
  @NullSafe
  public static long size(File path) {
    return isFile(path) ? path.length() : 0L;
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
  public static File tryGetCanonicalFileElseGetAbsoluteFile(File file) {

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
  public static String tryGetCanonicalPathElseGetAbsolutePath(File file) {

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
   * @return the {@link File} be written.
   * @throws IOException if writing the contents of the {@link InputStream} to the {@link File}
   * results in an I/O error.
   * @throws NullPointerException if either the {@link InputStream} or {@link File} reference are null.
   * @throws IllegalStateException if the file exists and is not writable.
   * @see java.io.InputStream
   * @see java.io.File
   * @see #copy(InputStream, OutputStream)
   */
  public static File write(InputStream in, File file) throws IOException {

    Assert.notNull(in, "InputStream cannot be null");
    Assert.notNull(file, "File cannot be null");

    boolean isWritable = !isExisting(file) || file.canWrite();

    Assert.state(isWritable, "[%s] is not writable", tryGetCanonicalPathElseGetAbsolutePath(file));

    try (OutputStream out = new BufferedOutputStream(new FileOutputStream(file))) {
      copy(in, out);
    }

    return file;
  }
}
