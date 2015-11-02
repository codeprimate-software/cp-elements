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
import java.math.BigDecimal;
import java.util.Comparator;

/**
 * The FileComparatorFactory class is a factory class returning File Comparator implementations based on various
 * File properties and attributes.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.util.Comparator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileComparatorFactory {

  public static Comparator<File> fileExtensionComparator() {
    return (fileOne, fileTwo) -> FileUtils.getExtension(fileOne).compareTo(FileUtils.getExtension(fileTwo));
  }

  public static Comparator<File> fileLastModifiedComparator() {
    return (fileOne, fileTwo) -> new BigDecimal(fileOne.lastModified() - fileTwo.lastModified()).intValue();
  }

  public static Comparator<File> fileNameComparator() {
    return (fileOne, fileTwo) -> fileOne.getName().compareTo(fileTwo.getName());
  }

  public static Comparator<File> filePathComparator() {
    return (fileOne, fileTwo) -> fileOne.getAbsolutePath().compareTo(fileTwo.getAbsolutePath());
  }

  public static Comparator<File> fileSizeComparator() {
    return (fileOne, fileTwo) -> new BigDecimal(fileOne.length() - fileTwo.length()).intValue();
  }

}
