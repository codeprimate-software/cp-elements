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
   * Constructs a file system path with the given array of path elements appended to the given base pathname using
   * the {@link File#separator}.  If the array of path elements is null or empty then base pathname is returned.
   *
   * @param basePathname a {@link String} indicating the base pathname.
   * @param pathElements an array of path elements to append to the base pathname.
   * @return the path elements appended to the base pathname.
   * @throws NullPointerException if the basePathname is null.
   * @see java.io.File#separator
   */
  public static String appendToPath(String basePathname, String... pathElements) {
    Assert.notNull(basePathname, "basePathname cannot be null");

    String fileSeparator = (SystemUtils.isWindows() ? WINDOWS_FILE_SEPARATOR : File.separator);

    for (String pathElement : ArrayUtils.nullSafeArray(pathElements, String.class)) {
      if (StringUtils.hasText(pathElement)) {
        basePathname = String.format("%1$s%2$s%3$s", basePathname.trim(), fileSeparator, pathElement.trim());
      }
    }

    String pattern = (SystemUtils.isWindows() ? WINDOWS_FILE_SEPARATOR_PATTERN : UNIX_FILE_SEPARATOR_PATTERN);

    return basePathname.replaceAll(pattern, fileSeparator);
  }

  /**
   * Creates a file system path with the given array of path elements delimited by {@link File#separator}.
   *
   * @param pathElements an array of {@link String}s constituting elements of the file system path.
   * @return an absolute pathname containing path elements from the given array delimited by {@link File#separator}.
   * @see java.io.File#separator
   */
  @NullSafe
  public static String createPath(String... pathElements) {
    StringBuilder buffer = new StringBuilder();

    String fileSeparator = (SystemUtils.isWindows() ? WINDOWS_FILE_SEPARATOR : File.separator);

    for (String pathElement : ArrayUtils.nullSafeArray(pathElements, String.class)) {
      if (StringUtils.hasText(pathElement)) {
        buffer.append(fileSeparator).append(pathElement.trim());
      }
    }

    String pattern = (SystemUtils.isWindows() ? WINDOWS_FILE_SEPARATOR_PATTERN : UNIX_FILE_SEPARATOR_PATTERN);

    return buffer.toString().replaceAll(pattern, fileSeparator);
  }

  /**
   * Counts the number of files in the given file system path.  If path is a file, then this method returns 1.
   * If path is a directory, then this method recursively visits any subdirectories counting all files contained
   * within the given path including the directory itself.
   *
   * @param path the file system path to evaluate.
   * @return an integer value indicating the number of files contained in the given file system path.
   * If path is a non-null file, then this method will return 1.
   * @see java.io.File
   * @see org.cp.elements.io.FileOnlyFilter
   * @see #count(File, FileFilter)
   */
  public static int count(final File path) {
    return count(path, FileOnlyFilter.INSTANCE);
  }

  /**
   * Counts the number of files in the given file system path accepted by the given {@link FileFilter}.  If path
   * is a file, then this method returns 1. If path is a directory, then this method recursively visits any
   * subdirectories counting all files contained within the given path including the directory itself.
   *
   * @param path the file system path to evaluate.
   * @param fileFilter the {@link FileFilter} used to evaluate the {@link File} and determine whether
   * it is to be included in the count.
   * @return an integer value indicating the number of files contained in the given file system path.
   * If path is a non-null file, then this method will return 1.
   * @see java.io.File
   * @see java.io.FileFilter
   * @see #safeListFiles(File)
   * @see #isDirectory(File)
   */
  public static int count(final File path, final FileFilter fileFilter) {
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
   * @see org.cp.elements.io.AcceptingAllNonNullFilesFilter
   * @see #deleteRecursive(File, FileFilter)
   */
  @NullSafe
  public static boolean deleteRecursive(final File path) {
    return deleteRecursive(path, AcceptingAllNonNullFilesFilter.INSTANCE);
  }

  /**
   * Deletes the given {@link File} from the file system if accepted by the given {@link FileFilter}
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
   * @param fileFilter the {@link FileFilter} used to identify the desired {@link File}s to delete.
   * @return a boolean value indicating whether the given {@link File} was successfully deleted from the file system.
   * @see java.io.File
   * @see java.io.FileFilter
   * @see #safeListFiles(File)
   * @see #isDirectory(File)
   * @see #delete(File)
   */
  public static boolean deleteRecursive(final File path, final FileFilter fileFilter) {
    boolean success = true;

    for (File file : safeListFiles(path)) {
      success &= (isDirectory(file) ? deleteRecursive(file, fileFilter) : (!fileFilter.accept(file) || delete(file)));
    }

    return (fileFilter.accept(path) && delete(path) && success);
  }

  /**
   * Determines whether the given directory is empty.  A directory is empty if it does not contain any files
   * or subdirectories.
   *
   * @param directory a {@link File} reference pointing to a directory that is evaluated for emptiness.
   * @return a boolean value indicating whether the given directory is empty.
   * @see java.io.File#listFiles()
   * @see #isDirectory(File)
   */
  @SuppressWarnings("all")
  public static boolean isEmptyDirectory(final File directory) {
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
  public static boolean isRelativeToWorkingDirectory(final File path) {
    return (path != null && tryGetCanonicalPathElseGetAbsolutePath(path).startsWith(
      tryGetCanonicalPathElseGetAbsolutePath(WORKING_DIRECTORY)));
  }

  public static File[] listFiles(final File directory) {
    return listFiles(directory, AcceptingAllNonNullFilesFilter.INSTANCE);
  }

  public static File[] listFiles(final File directory, final FileFilter fileFilter) {
    List<File> results = new ArrayList<>();

    for (File file : safeListFiles(directory, fileFilter)) {
      if (isDirectory(file)) {
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
  static File[] safeListFiles(final File path) {
    return safeListFiles(path, AcceptingAllNonNullFilesFilter.INSTANCE);
  }

  /* (non-Javadoc) */
  @NullSafe
  static File[] safeListFiles(final File path, final FileFilter fileFilter) {
    return (isDirectory(path) ? path.listFiles(fileFilter) : NO_FILES);
  }

  /**
   * Determines the size of the path in bytes.  The path size is determined by the byte size of all the files
   * contained within the path itself as well as it's subdirectories.
   *
   * @param path the {@link File} denoting the path to evaluate.
   * @return a long value indicating the size of the path in number of bytes.
   * @see java.io.File
   * @see org.cp.elements.io.FileOnlyFilter
   * @see #size(File, FileFilter)
   */
  public static long size(final File path) {
    return size(path, FileOnlyFilter.INSTANCE);
  }

  /**
   * Determines the size of the path in bytes.  The path size is determined by the byte size of all the files
   * contained within the path itself as well as it's subdirectories that are accepted by the {@link FileFilter}.
   *
   * @param path the {@link File} denoting the path to evaluate.
   * @param fileFilter the {@link FileFilter} used to determine whether files identified will be included in the size.
   * @return a long value indicating the size of the path in number of bytes.
   * @see java.io.File#length()
   * @see java.io.FileFilter#accept(File)
   * @see #isDirectory(File)
   * @see #size(File, FileFilter)
   */
  public static long size(final File path, final FileFilter fileFilter) {
    long size = 0;

    for (File file : safeListFiles(path)) {
      size += (isDirectory(path) ? size(file, fileFilter) : (fileFilter.accept(file) ? size(file) : 0));
    }

    return (ComposableFileFilter.and(FileOnlyFilter.INSTANCE, fileFilter).accept(path)
      ? (path.length() + size) : size);
  }

}
