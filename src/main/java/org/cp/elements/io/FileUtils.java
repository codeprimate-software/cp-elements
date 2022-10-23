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
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * Abstract utility class encapsulating methods used to process {@link File Files}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileOutputStream
 * @see java.io.FileReader
 * @see java.io.InputStream
 * @see java.io.OutputStream
 * @see org.cp.elements.io.IOUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileUtils extends IOUtils {

  public static final String CLASS_FILE_EXTENSION = ".class";
  public static final String GROOVY_FILE_EXTENSION = ".groovy";
  public static final String JAVA_FILE_EXTENSION = ".java";
  public static final String KOTLIN_FILE_EXTENSION = ".kt";

  protected static final String FILE_EXTENSION_SEPARATOR = StringUtils.DOT_SEPARATOR;
  protected static final String NO_FILE_EXTENSION = StringUtils.EMPTY_STRING;
  protected static final String NO_FILE_NAME = StringUtils.EMPTY_STRING;

  /**
   * Asserts that the given {@link File} exists.
   *
   * @param path {@link File} to assert for existence.
   * @return the given {@link File}; never {@literal null}.
   * @throws java.io.FileNotFoundException if the {@link File} does not exist.
   * @see #isExisting(File)
   * @see java.io.File
   */
  @NullSafe
  public static @NotNull File assertExists(@Nullable File path) throws FileNotFoundException {

    if (isExisting(path)) {
      return path;
    }

    throw new FileNotFoundException(String.format("[%s] was not found", path));
  }

  /**
   * Creates a file system directory with the given {@link File path}.
   *
   * @param path {@link File} referring to the file system path of the directory to create.
   * @return {@literal true} if the {@literal path} represented by the {@link File} is not {@literal null},
   * is not an existing {@link File#isFile()}, or the {@literal path} can be created as a directory.
   * Returns {@literal true} if the directory already exists.
   * @see java.io.File#mkdirs()
   */
  @NullSafe
  public static boolean createDirectory(@Nullable File path) {

    return path != null
      && !path.isFile()
      && (path.isDirectory() || path.mkdirs());
  }

  /**
   * Creates a file system file with the given {@link File path}.
   *
   * @param path {@link File} referring to the absolute location and name of the file to create.
   * @return {@literal true} if the path represented by the {@link File} is not {@literal null},
   * is not an existing {@link File#isDirectory()}, or the {@literal path} can be created as a file.
   * Returns {@literal true} if the file already exists.
   * @see java.io.File#createNewFile()
   */
  @NullSafe
  public static boolean createFile(@Nullable File path) {

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
   * @param path {@link File} to delete.
   * @return a boolean value indicating whether the given {@link File} was deleted successfully.
   * Returns {@literal false} if the {@link File} references is {@literal null}
   * or does not {@link #isExisting(File) exist}.
   * @see java.io.File#delete()
   * @see #isExisting(File)
   */
  @NullSafe
  public static boolean delete(@Nullable File path) {
    return isExisting(path) && path.delete();
  }

  /**
   * Returns the {@link String extension} of the given, required {@link File}.
   *
   * @param file {@link File} from which to get the {@link String extension}; must not be {@literal null}.
   * @return the {@link File} {@link String extension} or an empty {@link String} if the {@link File}
   * does not have an {@link String extension}.
   * @throws IllegalArgumentException if the {@link File} reference is {@literal null}.
   * @see java.io.File#getName()
   */
  public static @NotNull String getExtension(@NotNull File file) {

    Assert.notNull(file, "File is required");

    String filename = file.getName();

    int dotIndex = filename.indexOf(FILE_EXTENSION_SEPARATOR);

    return dotIndex > -1 ? filename.substring(dotIndex + 1) : NO_FILE_EXTENSION;
  }

  /**
   * Returns the {@link String absolute path (location)} of the given, required {@link File}.
   *
   * @param file {@link File} to determine {@link String absolute filesystem path}; must not be {@literal null}.
   * @return a {@link String} containing the {@literal absolute filesystem pathname (location)} of the given {@link File}.
   * @throws IllegalArgumentException if the {@link File} reference is {@literal null}.
   * @see #tryGetCanonicalPathElseGetAbsolutePath(java.io.File)
   * @see java.io.File#getParentFile()
   */
  public static @NotNull String getLocation(@NotNull File file) {

    Assert.notNull(file, "File is required");

    File parent = file.getParentFile();

    Assert.notNull(parent, "Location of file [%s] cannot be determined", file);

    return tryGetCanonicalPathElseGetAbsolutePath(parent);
  }

  /**
   * Returns the {@link String name} of the given, required {@link File}
   * without the {@link File File's} {@link String extension}.
   *
   * @param file {@link File} to get the {@link String name} of; must not be {@literal null}.
   * @return a {@link String} containing the {@literal name} of the {@link File}.
   * @throws IllegalArgumentException if the {@link File} reference is {@literal null}.
   * @see java.io.File#getName()
   */
  public static @NotNull String getName(@NotNull File file) {

    Assert.notNull(file, "File is required");

    String filename = file.getName();

    int dotIndex = filename.indexOf(FILE_EXTENSION_SEPARATOR);

    filename = dotIndex != -1 ? filename.substring(0, dotIndex) : filename;

    return filename.trim();
  }

  /**
   * Determines whether the given {@link File} exists and is a {@link File#isDirectory() directory}.
   *
   * @param path {@link File} to evaluate as a directory.
   * @return a boolean value indicating whether the given {@link File} is a {@link File#isDirectory() directory}.
   * @see java.io.File#isDirectory()
   */
  @NullSafe
  public static boolean isDirectory(@Nullable File path) {
    return path != null && path.isDirectory();
  }

  /**
   * Determines whether the given {@link File} has any content.
   *
   * If the {@link File} refers to a {@link File#isDirectory() directory}, then the directory is empty
   * if it contains not files or subdirectories.
   *
   * @param path {@link File} to evaluate for content.
   * @return a boolean value indicating whether the given {@link File} has any content.
   * @see java.io.File#length()
   * @see #isDirectory(File)
   * @see #size(File)
   */
  @NullSafe
  public static boolean isEmpty(@Nullable File path) {

    return isDirectory(path)
      ? ArrayUtils.nullSafeLength(path.listFiles()) == 0
      : size(path) == 0L;
  }

  /**
   * Determines whether the given {@link File} has any content.
   *
   * @param path {@link File} to evaluate for content.
   * @return a boolean value indicating whether the given {@link File} has any content.
   * @see #isEmpty(File)
   * @see java.io.File
   */
  @NullSafe
  public static boolean isNotEmpty(@Nullable File path) {
    return !isEmpty(path);
  }

  /**
   * Determines whether the given {@link File} {@link File#exists()}.
   *
   * @param path {@link File} to evaluate.
   * @return a boolean valued indicating whether the given {@link File} {@link File#exists()}.
   * @see java.io.File#exists()
   */
  @NullSafe
  public static boolean isExisting(@Nullable File path) {
    return path != null && path.exists();
  }

  /**
   * Determines whether the given {@link File} {@link File#exists()}.
   *
   * @param path {@link File} to evaluate.
   * @return a boolean valued indicating whether the given {@link File} {@link File#exists()}.
   * @see #isExisting(File)
   * @see java.io.File
   */
  public static boolean isNonExisting(@Nullable File path) {
    return !isExisting(path);
  }

  /**
   * Determines whether the given {@link File} exists and is a {@link File#isFile() file}.
   *
   * @param path {@link File} to evaluate as a file.
   * @return a boolean valued indicating whether the given {@link File} exists and is a {@link File#isFile() file}.
   * @see java.io.File#isFile()
   */
  @NullSafe
  public static boolean isFile(@Nullable File path) {
    return path != null && path.isFile();
  }

  /**
   * Constructs a new instance of {@link File} initialized with the given {@link String pathname}.
   *
   * @param pathname {@link String} containing the {@literal pathname} of the {@link File} to create;
   * must not be {@literal null}.
   * @return a new {@link File} with the given {@link String pathname}.
   * @throws IllegalArgumentException if the {@link String pathname} is {@literal null}.
   * @see java.io.File#File(String)
   */
  public static @NotNull File newFile(@NotNull String pathname) {
    return new File(ObjectUtils.requireObject(pathname, "A pathname for the File is required"));
  }

  /**
   * Reads the contents of the given, required {@link File} into a {@link String}.
   *
   * @param file {@link File} to read; must not be {@literal null}.
   * @return a {@link String} containing the contents of the {@link File}.
   * @throws IllegalArgumentException if the {@link File} is not a valid file.
   * @throws IllegalStateException if the {@link File} is not readable.
   * @throws IOException if the given {@link File} cannot be read.
   * @see #readLines(File)
   * @see java.io.File
   */
  public static @NotNull String read(@NotNull File file) throws IOException {

    StringBuilder buffer = new StringBuilder();

    readLines(file).forEach(line -> {
      buffer.append(line);
      buffer.append(StringUtils.LINE_SEPARATOR);
    });

    return buffer.toString().trim();
  }

  /**
   * Reads the contents of the given, required {@link File} into a {@link List} containing an {@link String element}
   * for each {@link String line} in the {@link File}.
   *
   * @param file {@link File} to read the contents from; must not be {@literal null}.
   * @return a {@link List} of {@link String Strings} containing the contents of the {@link File}.
   * @throws IOException if an I/O error occurs while reading the {@link File}.
   * @throws IllegalArgumentException if the {@link File} is not a valid file.
   * @throws IllegalStateException if the {@link File} cannot be read.
   * @see java.io.File#canRead()
   * @see #isFile(File)
   */
  public static List<String> readLines(@NotNull File file) throws IOException {

    Assert.isTrue(isFile(file), "[%s] must be a file", file);
    Assert.state(file.canRead(), "[%s] is not readable", tryGetCanonicalPathElseGetAbsolutePath(file));

    try (BufferedReader fileReader = new BufferedReader(new FileReader(file))) {
      return fileReader.lines().collect(Collectors.toList());
    }
  }

  /**
   * Determines the size in bytes of the given {@link File}.
   *
   * If the {@link File} is {@literal null} or does not {@link File#exists() exist}, then {@literal 0} is returned.
   *
   * @param path {@link File} to evaluate.
   * @return a {@link Long#TYPE} value for the size of the {@link File} in bytes. If the {@link File} is {@literal null}
   * or does not {@link File#exists() exist}, then {@literal 0} is returned.
   * @see java.io.File#length()
   * @see #isFile(File)
   */
  @NullSafe
  public static long size(@Nullable File path) {
    return isFile(path) ? path.length() : 0L;
  }

  /**
   * Attempts to the get the {@link File#getCanonicalFile() canonical form} of the given {@link File},
   * otherwise returns the {@link File#getAbsoluteFile() absolute form} of the {@link File}.
   *
   * @param file {@link File} from which to return the {@link File#getCanonicalFile()}; must not be {@literal null}.
   * @return the {@link File#getCanonicalFile() canonical form} of the {@link File} unless an {@link IOException} occurs
   * then return the {@link File#getAbsoluteFile() absolute form} of the {@link File}.
   * @throws IllegalArgumentException if the {@link File} reference is {@literal null}.
   * @see java.io.File#getCanonicalFile()
   * @see java.io.File#getAbsoluteFile()
   */
  public static @NotNull File tryGetCanonicalFileElseGetAbsoluteFile(@NotNull File file) {

    try {
      return ObjectUtils.requireObject(file, "File is required").getCanonicalFile();
    }
    catch (IOException ignore) {
      return file.getAbsoluteFile();
    }
  }

  /**
   * Attempts to the get the {@link File#getCanonicalPath() canonical form} of the given {@link File},
   * otherwise returns the {@link File#getAbsolutePath() absolute form} of the {@link File}.
   *
   * @param file {@link File} from which to return the {@link File#getCanonicalPath()}; must not be {@literal null}.
   * @return the {@link File#getCanonicalPath() canonical form} of the {@link File} unless an {@link IOException} occurs
   * then return the {@link File#getAbsolutePath() absolute form} of the {@link File}.
   * @throws IllegalArgumentException if the {@link File} reference is {@literal null}.
   * @see java.io.File#getCanonicalPath()
   * @see java.io.File#getAbsolutePath()
   */
  public static @NotNull String tryGetCanonicalPathElseGetAbsolutePath(@NotNull File file) {

    try {
      return ObjectUtils.requireObject(file, "File is required").getCanonicalPath();
    }
    catch (IOException ignore) {
      return file.getAbsolutePath();
    }
  }

  /**
   * Writes the contents of the given, required {@link InputStream} to the given, required {@link File}.
   *
   * @param in {@link InputStream} used as the source of the {@link File} content; must not be {@literal null}.
   * @param file {@link File} to write the contents of the {@link InputStream} to; must not be {@literal null}.
   * @return the given {@link File} reference containing the contents of the {@link InputStream}.
   * @throws IllegalArgumentException if either the {@link InputStream} or {@link File} reference are {@literal null}.
   * @throws IllegalStateException if the {@link File} {@link File#exists()} and is not {@link File#canWrite() writable}.
   * @throws IOException if an I/O error occurs while writing the contents of the {@link InputStream}
   * to the {@link File}.
   * @see #copy(InputStream, OutputStream)
   * @see java.io.InputStream
   * @see java.io.File
   */
  public static File write(@NotNull InputStream in, @NotNull File file) throws IOException {

    Assert.notNull(in, "InputStream is required");
    Assert.notNull(file, "File is required");

    boolean isWritable = isNonExisting(file) || file.canWrite();

    Assert.state(isWritable, "[%s] is not writable", tryGetCanonicalPathElseGetAbsolutePath(file));

    try (OutputStream out = new BufferedOutputStream(new FileOutputStream(file))) {
      copy(in, out);
    }

    return file;
  }
}
