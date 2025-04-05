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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * Abstract utility class for processing {@link File Files} in the operating system's (OS) file system.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.io.FileUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileSystemUtils extends FileUtils {

  // Java File System Properties
  public static final File JAVA_HOME = new File(SystemUtils.JAVA_HOME);
  public static final File JAVA_EXE = new File(new File(JAVA_HOME, "bin"), "java");

  // Well-Known File System Directories
  public static final File TEMPORARY_DIRECTORY = new File(SystemUtils.TEMPORARY_DIRECTORY);
  public static final File USER_HOME_DIRECTORY = new File(SystemUtils.USER_HOME);
  public static final File WORKING_DIRECTORY = new File(SystemUtils.USER_DIRECTORY);

  public static final File[] NO_FILES = new File[0];

  public static final String FILE_SEPARATOR = File.separator;
  public static final String WINDOWS_FILE_SEPARATOR = "\\";
  public static final String WINDOWS_FILE_SEPARATOR_PATTERN = "\\+";
  public static final String UNIX_FILE_SEPARATOR = "/";
  public static final String UNIX_FILE_SEPARATOR_PATTERN = "/+";

  /**
   * Creates a {@link String file system path} by appending the array of {@link String path elements}
   * to the {@link String base path} separated by the {@link File#separator}.
   * <p>
   * If the array of {@link String path elements} is {@literal null} or {@literal empty}
   * then {@link String base path} is returned.
   *
   * @param basePath base of the file system path expressed as a {@link String pathname};
   * must not be {@literal null}.
   * @param pathElements array of path elements to append to the base path.
   * @return the path elements appended to the base path separated by the {@link File#separator}.
   * @throws IllegalArgumentException if {@link String base path} is {@literal null}.
   * @see #createPath(String...)
   * @see java.io.File#separator
   */
  public static @NotNull String appendToPath(@NotNull String basePath, String... pathElements) {

    Assert.notNull(basePath, "Base path is required");

    List<String> path = new ArrayList<>(ArrayUtils.nullSafeLength(pathElements) + 1);

    path.add(StringUtils.trim(basePath));

    Arrays.stream(ArrayUtils.nullSafeArray(pathElements, String.class))
      .filter(StringUtils::hasText)
      .map(StringUtils::trim)
      .forEach(path::add);

    return createPath(path.toArray(new String[0]));
  }

  /**
   * Creates a {@link String file system path} with the given array of {@link String path elements}
   * delimited by {@link File#separator}.
   *
   * @param pathElements array of path elements used to construct a file system path.
   * @return an absolute file system path composed of the individual path elements from the given array
   * delimited by the {@link File#separator}.
   * @see #appendToPath(String, String...)
   * @see java.io.File#separator
   */
  @NullSafe
  public static @NotNull String createPath(String... pathElements) {

    String fileSeparator = resolveFileSeparator();

    StringBuilder buffer = new StringBuilder(fileSeparator);

    Arrays.stream(ArrayUtils.nullSafeArray(pathElements, String.class))
      .filter(StringUtils::hasText)
      .forEach(pathElement -> buffer.append(fileSeparator).append(pathElement.trim()));

    return buffer.toString().replaceAll(resolveFileSeparatorPattern(), fileSeparator);
  }

  static @NotNull String resolveFileSeparator() {
    return SystemUtils.isWindows() ? WINDOWS_FILE_SEPARATOR : File.separator;
  }

  private static @NotNull String resolveFileSeparatorPattern() {
    return SystemUtils.isWindows() ? WINDOWS_FILE_SEPARATOR_PATTERN : UNIX_FILE_SEPARATOR_PATTERN;
  }

  /**
   * Counts the number of files in the given {@link File file system path}.
   * <p>
   * If {@link File path} is a {@link File#isFile() file} then this method returns {@literal 1}.
   * If {@link File path} is a {@link File#isDirectory() directory} then this method will recursively visit
   * all {@link File subdirectories}, counting all the {@link File files} contained in the given {@link File path}
   * excluding {@link File subdirectories} (in the count) and the {@link File path} itself.
   * <p>
   * The {@link File} must simply {@link File#exists() exist} in order to be counted.
   *
   * @param path {@link File file system path} to evaluate.
   * @return an integer value counting the number of files contained in the given {@link File file system path}.
   * If {@link File path} is a {@literal non-null} {@link File}, then this method returns {@literal 1}.
   * @see #count(File, FileFilter)
   * @see #isExisting(File)
   * @see java.io.File
   */
  public static int count(@Nullable File path) {
    return count(path, FileUtils::isExisting);
  }

  /**
   * Counts the number of files in the given {@link File file system path} accepted by the given {@link FileFilter}.
   * <p>
   * If {@link File path} is a {@link File#isFile() file} then this method returns {@literal 1}.
   * If {@link File path} is a {@link File#isDirectory() directory} then this method will recursively visit
   * all {@link File subdirectories}, counting all the {@link File files} contained in the given {@link File path}
   * excluding {@link File subdirectories} (in the count) and the {@link File path} itself.
   * <p>
   * The {@link File} must simply {@link File#exists() exist} in order to be counted.
   *
   * @param path {@link File file system path} to evaluate.
   * @param fileFilter {@link FileFilter} used to evaluate a {@link File} to determine whether the {@link File}
   * will be included in the count.
   * @return an integer value counting the number of files contained in the given {@link File file system path}.
   * If {@link File path} is a {@literal non-null} {@link File}, then this method returns {@literal 1}.
   * @see #safeListFiles(File)
   * @see #isDirectory(File)
   * @see java.io.FileFilter
   * @see java.io.File
   */
  public static int count(@Nullable File path, @Nullable FileFilter fileFilter) {

    int count = 0;

    for (File file : safeListFiles(path, fileFilter)) {
      count += isDirectory(file) ? count(file, fileFilter) : 1;
    }

    return ComposableFileFilter.and(FileUtils::isFile, fileFilter).accept(path) ? 1 : count;
  }

  /**
   * Deletes the given {@link File} from the file system.
   * <p>
   * If {@link File path} is a {@link File#isDirectory() directory}, then this method recursively deletes
   * all {@link File files} and {@link File subdirectories} contained in the given {@link File path} along with
   * the {@link File directory} itself.
   * <p>
   * If {@link File path} is just a plain old {@link File#isFile() file}, then only the given {@link File}
   * will be deleted.
   * <p>
   * This method attempts to delete as many {@link File files} as possible up to and including
   * the given {@link File path}.
   *
   * @param path {@link File} to delete from the file system.
   * @return a boolean value indicating whether the given {@link File} was successfully deleted from the file system.
   * @see #deleteRecursive(File, FileFilter)
   * @see java.io.File
   */
  @NullSafe
  public static boolean deleteRecursive(@Nullable File path) {
    return deleteRecursive(path, FileUtils::isExisting);
  }

  /**
   * Deletes the given {@link File} from the file system if accepted by the given {@link FileFilter}.
   * <p>
   * If {@link File path} is a {@link File#isDirectory() directory}, then this method recursively deletes
   * all {@link File files} and {@link File subdirectories} contained in the given {@link File path} along with
   * the {@link File directory} itself.
   * <p>
   * If {@link File path} is just a plain old {@link File#isFile() file}, then only the given {@link File}
   * will be deleted.
   * <p>
   * This method attempts to delete as many {@link File files} as possible up to and including
   * the given {@link File path}.
   *
   * @param path {@link File} to delete from the file system.
   * @param fileFilter {@link FileFilter} used to identify the {@link File Files} to delete.
   * @return a boolean value indicating whether the given {@link File} was successfully deleted from the file system.
   * Returns {@literal true} if and only if (iff) the {@link File path} and all of its contents
   * were successfully deleted.
   * @see #safeListFiles(File)
   * @see #isDirectory(File)
   * @see #delete(File)
   * @see java.io.FileFilter
   * @see java.io.File
   */
  public static boolean deleteRecursive(@Nullable File path, @Nullable FileFilter fileFilter) {

    boolean success = true;

    for (File file : safeListFiles(path, fileFilter)) {
      success &= isDirectory(file) ? deleteRecursive(file, fileFilter) : delete(file);
    }

    return success && nullSafeFileFilter(fileFilter, false).accept(path) && delete(path);
  }

  /**
   * Determines whether the given {@link File#isDirectory() directory} is empty.
   * <p>
   * A {@link File#isDirectory() directory} is {@literal empty} if it does not contain any {@link File#isFile() files}
   * or {@link File#isDirectory() subdirectories}, or is not a {@link File#isDirectory() directory}.
   *
   * @param directory {@link File} referring to a {@link File#isDirectory() directory} to evaluate.
   * @return a boolean value indicating whether the given {@link File#isDirectory() directory} is empty.
   * @see java.io.File#listFiles()
   * @see #isDirectory(File)
   */
  @NullSafe
  public static boolean isEmptyDirectory(@Nullable File directory) {
    return isDirectory(directory) && ArrayUtils.nullSafeLength(directory.listFiles()) == 0;
  }

  /**
   * Determines whether the given {@link File} is relative to the {@link File#isDirectory() current working directory}.
   *
   * @param path {@link File} to evaluate.
   * @return a boolean value indicating whether the given {@link File} is relative to
   * the {@link File#isDirectory() current working directory}.
   * @see #tryGetCanonicalPathElseGetAbsolutePath(File)
   */
  @NullSafe
  public static boolean isRelativeToWorkingDirectory(@Nullable File path) {
    return path != null && tryGetCanonicalPathElseGetAbsolutePath(path)
      .startsWith(tryGetCanonicalPathElseGetAbsolutePath(WORKING_DIRECTORY));
  }

  /**
   * Lists all {@link File files} in the given {@link File#isDirectory() directory}.
   * <p>
   * This method uses recursion to list all {@link File files} in the given {@link File#isDirectory() directory}
   * as well as any {@link File#isDirectory() subdirectories} of the given {@link File#isDirectory() directory}.
   *
   * @param directory {@link File#isDirectory() directory} to search for {@link File files}.
   * @return an array of all {@link File files} in the given {@link File#isDirectory() directory}
   * and any {@link File#isDirectory() subdirectories} of the given {@link File#isDirectory() directory}.
   * @see #listFiles(File, FileFilter)
   * @see #isExisting(File)
   * @see java.io.File
   */
  @NullSafe
  public static File[] listFiles(@Nullable File directory) {
    return listFiles(directory, FileUtils::isExisting);
  }

  /**
   * Lists all {@link File files} in the given {@link File#isDirectory() directory}
   * accepted by the given {@link FileFilter}.
   * <p>
   * This method uses recursion to list all {@link File files} in the given {@link File#isDirectory() directory}
   * as well as any {@link File#isDirectory() subdirectories} of the given {@link File#isDirectory() directory}.
   *
   * @param directory {@link File#isDirectory() directory} to search for {@link File files}.
   * @param fileFilter the {@link FileFilter} used to filter and match {@link File files}.
   * @return an array of all {@link File files} in the given {@link File#isDirectory() directory}
   * and any {@link File#isDirectory() subdirectories} of the given {@link File#isDirectory() directory}
   * accepted by the given {@link FileFilter}.
   * @see #safeListFiles(File, FileFilter)
   * @see #isDirectory(File)
   * @see java.io.FileFilter
   * @see java.io.File
   */
  @NullSafe
  public static File[] listFiles(@Nullable File directory, @Nullable FileFilter fileFilter) {

    List<File> files = new ArrayList<>();

    for (File file : safeListFiles(directory, fileFilter)) {
      files.addAll(isDirectory(file) ? Arrays.asList(listFiles(file, fileFilter)) : Collections.singletonList(file));
    }

    return files.isEmpty() ? NO_FILES : files.toArray(new File[0]);
  }

  @NullSafe
  static File[] safeListFiles(@Nullable File path) {
    return safeListFiles(path, FileUtils::isExisting);
  }

  @NullSafe
  static File[] safeListFiles(@Nullable File path, @Nullable FileFilter fileFilter) {
    return isDirectory(path) ? path.listFiles(fileFilter) : NO_FILES;
  }

  /**
   * Searches for a {@link File} with the given, required {@link String filename} in the {@literal working directory}.
   *
   * @param filename {@link String} containing the {@literal name} of the {@link File} to find;
   * must not be {@literal null}.
   * @return a {@link File} object referring to the file in the file system with the given {@link String filename}
   * if found, otherwise returns {@literal null}.
   * @see #search(File, String)
   * @see java.io.File
   */
  public static @Nullable File search(@NotNull String filename) {
    return search(FileSystemUtils.WORKING_DIRECTORY, filename);
  }

  /**
   * Searches for a {@link File} with the given, required {@link String filename} in the given,
   * required {@link File directory}.
   *
   * @param path {@link File} referring to the file system path to begin the search; must not be {@literal null}.
   * @param filename {@link String} containing the {@literal name} of the {@link File} to find;
   * must not be {@literal null}.
   * @return a {@link File} object referring to the file in the file system with the given {@link String filename}
   * if found, otherwise returns {@literal null}.
   * @see java.io.File
   */
  public static @Nullable File search(@NotNull File path, @NotNull String filename) {

    File foundFile = null;

    for (File file : safeListFiles(path)) {

      foundFile = isDirectory(file) ? search(file, filename)
        : file.getName().equals(filename) ? file
        : null;

      if (Objects.nonNull(foundFile)) {
        break;
      }
    }

    return foundFile;
  }

  /**
   * Determines the size of the given {@link File path} in bytes.
   * <p>
   * The {@link Long path size} is determined by the {@link File#length() byte size}
   * of all the {@link File#isFile() files} contained within the {@link File path} itself
   * as well as all {@link File#isFile() files} in {@link File#isDirectory() subdirectories}.
   *
   * @param path {@link File} referring to the path to evaluate.
   * @return a {@link Long value} specifying the size of the {@link File path} in bytes.
   * @see #size(File, FileFilter)
   * @see java.io.File#length()
   * @see #isExisting(File)
   */
  public static long size(@Nullable File path) {
    return size(path, FileUtils::isExisting);
  }

  /**
   * Determines the size of the given {@link File path} in bytes.
   * <p>
   * The {@link Long path size} is determined by the {@link File#length() byte size}
   * of all the {@link File#isFile() files} contained within the {@link File path} itself
   * as well as all {@link File#isFile() files} in {@link File#isDirectory() subdirectories}
   * that are accepted by the given {@link FileFilter}.
   *
   * @param path {@link File} referring to the path to evaluate.
   * @param fileFilter {@link FileFilter} used to determine which {@link File#isFile() files}
   * will be included in the {@link Long size}.
   * @return a {@link Long value} specifying the size of the {@link File path} in bytes.
   * @see #safeListFiles(File, FileFilter)
   * @see java.io.FileFilter#accept(File)
   * @see java.io.File#length()
   */
  public static long size(@Nullable File path, @Nullable FileFilter fileFilter) {

    long size = 0;

    for (File file : safeListFiles(path, fileFilter)) {
      size += isDirectory(file) ? size(file, fileFilter) : file.length();
    }

    return ComposableFileFilter.and(FileUtils::isFile, fileFilter).accept(path) ? path.length() : size;
  }
}
