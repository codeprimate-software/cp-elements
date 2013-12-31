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
import java.io.IOException;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.StringUtils;

/**
 * The FileUtils class is a utility class for working with File objects and the file system.
 * <p/>
 * @author John J. Blum
 * @see java.io.File
 * @see org.cp.elements.io.IOUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileUtils extends IOUtils {

  public static String getNameWithoutExtension(final File file) {
    Assert.notNull(file, "The File to get the name of without extension cannot be null!");
    final String filename = file.getName();
    final int index = filename.indexOf(StringUtils.DOT_SEPARATOR);
    return (index == -1 ? filename : filename.substring(0, index));
  }

  @NullSafe
  public static boolean isDirectory(final File path) {
    return (path != null && path.isDirectory());
  }

  @NullSafe
  public static boolean isExisting(final File path) {
    return (path != null && path.exists());
  }

  @NullSafe
  public static boolean isFile(final File path) {
    return (path != null && path.isFile());
  }

  /**
   * Attempts to the get the canonical representation of the specified File, otherwise returns the absolute
   * representation of the File.
   * </p>
   * @param file the File object for which the canonical or absolute File will be returned.
   * @return the canonical form of the File, otherwise return the absolute form of the File if an IOException
   * occurs.
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

}
