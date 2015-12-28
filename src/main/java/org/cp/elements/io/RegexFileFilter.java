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
