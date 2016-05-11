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
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.NullSafe;
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
  public static final String WINDOWS_FILE_SEPARATOR = "\\";
  public static final String WINDOWS_FILE_SEPARATOR_PATTERN = "\\+";
  public static final String UNIX_FILE_SEPARATOR = "/";
  public static final String UNIX_FILE_SEPARATOR_PATTERN = "/+";

  /**
   * Creates a file system path by appending the array of path elements to the base path separated by
   * {@link File#separator}.  If the array of path elements is null or empty then base path is returned.
   *
   * @param basePath base of the file system path expressed as a pathname {@link String}.
   * @param pathElements array of path elements to append to the base path.
   * @return the path elements appended to the base path separated by {@link File#separator}.
   * @throws NullPointerException if basePath is null.
   * @see java.io.File#separator
   */
  public static String appendToPath(String basePath, String... pathElements) {
    Assert.notNull(basePath, "basePath cannot be null");

    String fileSeparator = (SystemUtils.isWindows() ? WINDOWS_FILE_SEPARATOR : File.separator);

    for (String pathElement : ArrayUtils.nullSafeArray(pathElements, String.class)) {
      if (StringUtils.hasText(pathElement)) {
        basePath = String.format("%1$s%2$s%3$s", basePath.trim(), fileSeparator, pathElement.trim());
      }
    }

    String fileSeparatorPattern = (SystemUtils.isWindows() ? WINDOWS_FILE_SEPARATOR_PATTERN
      : UNIX_FILE_SEPARATOR_PATTERN);

    return basePath.trim().replaceAll(fileSeparatorPattern, fileSeparator);
  }

  /**
   * Creates a file system path with the given array of path elements delimited by {@link File#separator}.
   *
   * @param pathElements array of path elements used to construct a file system path.
   * @return an absolute file system pathname composed of the individual path elements
   * from the given array delimited by the {@link File#separator}.
   * @see java.io.File#separator
   */
  @NullSafe
  public static String createPath(String... pathElements) {
    String fileSeparator = (SystemUtils.isWindows() ? WINDOWS_FILE_SEPARATOR : File.separator);

    StringBuilder buffer = new StringBuilder(fileSeparator);

    for (String pathElement : ArrayUtils.nullSafeArray(pathElements, String.class)) {
      if (StringUtils.hasText(pathElement)) {
        buffer.append(fileSeparator).append(pathElement.trim());
      }
    }

    String fileSeparatorPattern = (SystemUtils.isWindows() ? WINDOWS_FILE_SEPARATOR_PATTERN
      : UNIX_FILE_SEPARATOR_PATTERN);

    return buffer.toString().replaceAll(fileSeparatorPattern, fileSeparator);
  }

  /**
   * Counts the number of files in the given file system path.  If path is a file then this method returns 1.
   * If path is a directory then this method will recursively visit all sub-directories, counting all the files
   * contained in the given path excluding the path itself.
   *
   * @param path the file system path to evaluate.
   * @return an integer value indicating the number of files contained in the given file system path.
   * If path is a non-null file then this method returns 1.
   * @see java.io.File
   * @see org.cp.elements.io.FileOnlyFilter
   * @see #count(File, FileFilter)
   */
  public static int count(File path) {
    return count(path, FileOnlyFilter.INSTANCE);
  }

  /**
   * Counts the number of files in the given file system path accepted by the given {@link FileFilter}.  If path
   * is a file then this method returns 1. If path is a directory then this method will recursively visit all
   * sub-directories, counting all the files contained within the given path excluding the path itself.
   *
   * @param path the file system path to evaluate.
   * @param fileFilter the {@link FileFilter} used to evaluate the {@link File} and determine whether
   * it is to be included in the count.
   * @return an integer value indicating the number of files contained in the given file system path.
   * If path is a non-null file then this method returns 1.
   * @see java.io.File
   * @see java.io.FileFilter
   * @see #safeListFiles(File)
   * @see #isDirectory(File)
   */
  public static int count(File path, FileFilter fileFilter) {
    int count = 0;

    for (File file : safeListFiles(path)) {
      count += (isDirectory(file) ? count(file, fileFilter) : (fileFilter.accept(file) ? 1 : 0));
    }

    return (fileFilter.accept(path) ? ++count : count);
  }

  /**
   * Deletes the given {@link File} from the file system.  If the {@link File} is a directory, then this method
   * recursively deletes all files and subdirectories in the given directory along with the directory itself.
   * If the {@link File} is just a plain old file, then only the given file will be deleted.  This method attempts
   * to delete as many files as possible.
   *
   * @param path the {@link File} to delete from the file system.
   * @return a boolean value indicating whether the given {@link File} was successfully deleted from the file system.
   * @see java.io.File
   * @see #deleteRecursive(File, FileFilter)
   */
  @NullSafe
  public static boolean deleteRecursive(File path) {
    return deleteRecursive(path, FileUtils::isExisting);
  }

  /**
   * Deletes the given {@link File} from the file system if accepted by the given {@link FileFilter}.
   *
   * If the {@link File} is a directory, then this method recursively deletes all files and subdirectories
   * accepted by the {@link FileFilter} in the given directory along with the directory itself.
   *
   * If the {@link File} is just a plain old file, then only the given file will be deleted if accepted
   * by the {@link FileFilter}.
   *
   * This method attempts to delete as many files as possible.
   *
   * @param path the {@link File} to delete from the file system.
   * @param fileFilter the {@link FileFilter} used to identify the {@link File}s to delete.
   * @return a boolean value indicating whether the given {@link File} was successfully deleted from the file system.
   * @see java.io.File
   * @see java.io.FileFilter
   * @see #safeListFiles(File)
   * @see #isDirectory(File)
   * @see #delete(File)
   */
  public static boolean deleteRecursive(File path, FileFilter fileFilter) {
    boolean success = true;

    for (File file : safeListFiles(path, fileFilter)) {
      success &= (isDirectory(file) ? deleteRecursive(file, fileFilter) : delete(file));
    }

    return (success && fileFilter.accept(path) && delete(path));
  }

  /**
   * Determines whether the given directory is empty.  A directory is empty if it does not contain any files
   * or sub-directories, or is not a directory.
   *
   * @param directory a {@link File} reference pointing to a directory to evaluate.
   * @return a boolean value indicating whether the given directory is empty.
   * @see java.io.File#listFiles()
   * @see #isDirectory(File)
   */
  @SuppressWarnings("all")
  public static boolean isEmptyDirectory(File directory) {
    return (isDirectory(directory) && directory.listFiles().length == 0);
  }

  /**
   * Determines whether the given {@link File} is relative to the current working directory.
   *
   * @param path the {@link File} to evaluate for relativity to the current working directory.
   * @return a boolean value indicating whether the given {@link File} is relative to the current working directory.
   * @see #tryGetCanonicalPathElseGetAbsolutePath(File)
   */
  @NullSafe
  public static boolean isRelativeToWorkingDirectory(File path) {
    return (path != null && tryGetCanonicalPathElseGetAbsolutePath(path).startsWith(
      tryGetCanonicalPathElseGetAbsolutePath(WORKING_DIRECTORY)));
  }

  /**
   * Lists all files in the given directory.  This method uses recursion to list all files in the given directory
   * as well as any sub-directories of the given directory.
   *
   * @param directory the directory to search for files.
   * @return an array of all files in the given directory and any sub-directories of the given directory.
   * @see #listFiles(File, FileFilter)
   * @see #isExisting(File)
   * @see java.io.File
   */
  @NullSafe
  public static File[] listFiles(File directory) {
    return listFiles(directory, FileUtils::isExisting);
  }

  /**
   * Lists all files in the given directory accepted by the {@link FileFilter}.  This method uses recursion to list
   * all files in the given directory as well as any sub-directories of the given directory accepted by
   * the {@link FileFilter}.
   *
   * @param directory the directory to search for files.
   * @param fileFilter the {@link FileFilter} used to filter and match files.
   * @return an array of all files in the given directory and any sub-directories of the given directory accepted by
   * the {@link FileFilter}.
   * @see #safeListFiles(File, FileFilter)
   * @see java.io.FileFilter
   * @see java.io.File
   */
  @NullSafe
  public static File[] listFiles(File directory, FileFilter fileFilter) {
    List<File> files = new ArrayList<>();

    for (File file : safeListFiles(directory, fileFilter)) {
      files.addAll(isDirectory(file) ? Arrays.asList(listFiles(file, fileFilter)) : Collections.singletonList(file));
    }

    return (files.isEmpty() ? NO_FILES : files.toArray(new File[files.size()]));
  }

  /* (non-Javadoc) */
  @NullSafe
  static File[] safeListFiles(File path) {
    return safeListFiles(path, FileUtils::isExisting);
  }

  /* (non-Javadoc) */
  @NullSafe
  static File[] safeListFiles(File path, FileFilter fileFilter) {
    return (isDirectory(path) ? path.listFiles(fileFilter) : NO_FILES);
  }

  /**
   * Determines the size of the path in bytes.  The path size is determined by the byte size of all the files
   * contained within the path itself as well as all files in sub-directories.
   *
   * @param path the {@link File} denoting the path to evaluate.
   * @return a long value indicating the size of the path in bytes.
   * @see java.io.File#length()
   * @see org.cp.elements.io.FileOnlyFilter
   * @see #size(File, FileFilter)
   */
  public static long size(File path) {
    return size(path, FileOnlyFilter.INSTANCE);
  }

  /**
   * Determines the size of the path in bytes.  The path size is determined by the byte size of all the files
   * contained within the path itself as well as all files in sub-directories, which are accepted
   * by the {@link FileFilter}.
   *
   * @param path the {@link File} denoting the path to evaluate.
   * @param fileFilter the {@link FileFilter} used to determine whether files identified will be included in the size.
   * @return a long value indicating the size of the path in bytes.
   * @see java.io.File#length()
   * @see java.io.FileFilter#accept(File)
   */
  public static long size(File path, FileFilter fileFilter) {
    long size = 0;

    FileFilter composedFileFilter = ComposableFileFilter.and(FileOnlyFilter.INSTANCE, fileFilter);

    for (File file : safeListFiles(path)) {
      size += size(file, fileFilter);
    }

    return (composedFileFilter.accept(path) ? (path.length() + size) : size);
  }

}
