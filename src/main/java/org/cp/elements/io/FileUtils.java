/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.io;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.StringUtils;

/**
 * The FileUtils class is a utility class for working with File objects and the file system.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.cp.elements.io.IOUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileUtils extends IOUtils {

  /**
   * Gets the extension of the given file.
   *
   * @param file the File from which to pull the file extension.
   * @return the file extension of the given file or an empty String if the file does not have an extension.
   * @throws java.lang.NullPointerException if the file reference is null.
   * @see java.io.File#getName()
   */
  public static String getExtension(final File file) {
    Assert.notNull(file, "The File from which to get the extension cannot be null!");
    String filename = file.getName();
    int index = filename.indexOf(StringUtils.DOT_SEPARATOR);
    return (index == -1 ? StringUtils.EMPTY_STRING : filename.substring(index + 1));
  }

  /**
   * Gets the absolute path of the given file.
   *
   * @param file the File from which to pull the file path.
   * @return a String indicating the absolute pathname (location) of the given file.
   * @throws java.lang.NullPointerException if the file reference is null.
   * @see #tryGetCanonicalPathElseGetAbsolutePath(java.io.File)
   * @see java.io.File#getParentFile()
   */
  public static String getLocation(final File file) {
    Assert.notNull(file, "The File to get the location of cannot be null!");
    File parent = file.getParentFile();
    Assert.notNull(parent, new IllegalArgumentException(String.format("Unable to find the location of file (%1$s)!",
      file)));
    return tryGetCanonicalPathElseGetAbsolutePath(parent);
  }

  /**
   * Gets the name of the given file without the extension.
   *
   * @param file the File from which to get the name.
   * @return a String indicating the name of the file without the extension.
   * @throws java.lang.NullPointerException if the file reference is null.
   * @see java.io.File#getName()
   */
  public static String getNameWithoutExtension(final File file) {
    Assert.notNull(file, "The File from which to get the name of without extension cannot be null!");
    String filename = file.getName();
    int index = filename.indexOf(StringUtils.DOT_SEPARATOR);
    return (index == -1 ? filename : filename.substring(0, index));
  }

  /**
   * Determines whether the given File path actually exists and is a directory.
   *
   * @param path the File used to determine whether the path is a directory.
   * @return a boolean valued indicating whether the given File path actually exists and is a directory.
   * @see java.io.File#isDirectory()
   */
  @NullSafe
  public static boolean isDirectory(final File path) {
    return (path != null && path.isDirectory());
  }

  /**
   * Determines whether the given File path actually exists.
   *
   * @param path the File used to determine whether the path exists.
   * @return a boolean valued indicating whether the given File path actually exists.
   * @see java.io.File#exists()
   */
  @NullSafe
  public static boolean isExisting(final File path) {
    return (path != null && path.exists());
  }

  /**
   * Determines whether the given File path actually exists and is a file.
   *
   * @param path the File used to determine whether the path is a file.
   * @return a boolean valued indicating whether the given File path actually exists and is a file.
   * @see java.io.File#isFile()
   */
  @NullSafe
  public static boolean isFile(final File path) {
    return (path != null && path.isFile());
  }

  /**
   * Attempts to the get the canonical representation of the specified File, otherwise returns
   * the absolute representation of the File.
   *
   * @param file the File from which the canonical or absolute File will be returned.
   * @return the canonical form of the File, otherwise returns the absolute form of the File if an IOException occurs.
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
   * Attempts to the get the canonical path of the specified File, otherwise returns the absolute path of the File.
   *
   * @param file the File from which the canonical or absolute path will be returned.
   * @return the canonical path of the File, otherwise returns the absolute path of the File if an IOException occurs.
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
