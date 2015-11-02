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
import java.io.FileFilter;
import java.util.Set;
import java.util.TreeSet;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.ArrayUtils;

/**
 * The FileExtensionFilter class is a FileFilter implementation filtering Files by extension.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class FileExtensionFilter implements FileFilter, Filter<File> {

  private final Set<String> fileExtensions;

  /**
   * Constructs an instance of the FileExtensionFilter class initialized with the specified array of file extensions
   * defining the filtering criteria used by this FileFilter accept files.
   *
   * @param fileExtensions an array of file extensions used as the filtering criteria by this FileFilter
   * to accept files.
   * @see #FileExtensionFilter(Iterable)
   */
  public FileExtensionFilter(final String... fileExtensions) {
    this(ArrayUtils.iterable(fileExtensions));
  }

  /**
   * Constructs an instance of the FileExtensionFilter class initialized with the specified Iterable of file extensions
   * defining the filtering criteria used by this FileFilter to accept files.
   *
   * @param fileExtensions an Iterable of file extensions used as the filtering criteria by this FileFilter
   * to accept files.
   * @see java.lang.Iterable
   */
  public FileExtensionFilter(final Iterable<String> fileExtensions) {
    this.fileExtensions = new TreeSet<>();

    if (fileExtensions != null) {
      for (String fileExtension : fileExtensions) {
        if (StringUtils.hasText(fileExtension)) {
          this.fileExtensions.add((fileExtension.startsWith(StringUtils.DOT_SEPARATOR) ? fileExtension.substring(1)
            : fileExtension).toLowerCase().trim());
        }
      }
    }
  }

  /**
   * Gets an array of Strings specifying the file extensions used as filtering criteria by this FileFilter
   * when evaluation files.
   *
   * @return a String array containing the file extensions used as filtering criteria by this FileFilter
   * when evaluating and accepting files.
   */
  public String[] getFileExtensions() {
    return fileExtensions.toArray(new String[fileExtensions.size()]);
  }

  /**
   * Determines whether the given file is accepted by this FileFilter based on it's file extension.
   *
   * @param file the File evaluated by this FileFilter.
   * @return a boolean value indicating whether the file's extension match the filtering criteria set
   * by this FileFilter.
   * @see java.io.File
   * @see org.cp.elements.io.FileUtils#getExtension(java.io.File)
   */
  @Override
  public boolean accept(final File file) {
    String fileExtension = FileUtils.getExtension(file).toLowerCase().trim();
    return ((fileExtensions.isEmpty() && StringUtils.isEmpty(fileExtension)) || fileExtensions.contains(fileExtension));
  }

}
