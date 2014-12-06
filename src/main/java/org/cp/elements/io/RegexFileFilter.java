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
import java.util.regex.Pattern;

import org.cp.elements.lang.Filter;

/**
 * The RegexFileFilter class is a FileFilter implementation that uses a regular expression to match and filter files.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see java.util.regex.Matcher
 * @see java.util.regex.Pattern
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class RegexFileFilter implements FileFilter, Filter<File> {

  private final Pattern pattern;

  /**
   * Constructs an instance of the RegexFileFilter class initialized with the specified regular expression (regex).
   *
   * @param regularExpression a String value representing the regular expression used to match and filter files.
   * @see java.util.regex.Pattern#compile(String)
   */
  public RegexFileFilter(final String regularExpression) {
    pattern = Pattern.compile(regularExpression);
  }

  /**
   * Gets the regular expression (regex) used in the comparison of this RegexFileFilter's accept operation.
   *
   * @return a String value indicating the regular expression (regex) used in the comparison of this RegexFileFilter's
   * accept operation.
   */
  public String getRegularExpression() {
    return pattern.pattern();
  }

  /**
   * Determines whether the given File matches the criteria specified by the regular expression.
   *
   * @param file the File object to filter.
   * @return a boolean value indicating whether the given File matches the criteria specified by the regular expression.
   * @see java.util.regex.Pattern#matcher(CharSequence)
   * @see java.util.regex.Matcher#matches()
   * @see org.cp.elements.io.FileUtils#tryGetCanonicalPathElseGetAbsolutePath(java.io.File)
   */
  @Override
  public boolean accept(final File file) {
    return pattern.matcher(FileUtils.tryGetCanonicalPathElseGetAbsolutePath(file)).matches();
  }

}
