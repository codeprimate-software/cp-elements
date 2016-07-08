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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.NullSafe;

/**
 * The RegexFileFilter class is a {@link FileFilter} and {@link Filter} implementation that uses a regular expression
 * to match and filter {@link File}s.
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

  private final Pattern regularExpression;

  /**
   * Constructs an instance of the {@link RegexFileFilter} class initialized with the given regular expression (regex),
   * expressed as a {@link String}.
   *
   * @param regularExpression regular expression (regex) used to match and filter {@link File}s.
   * @see java.util.regex.Pattern#compile(String)
   */
  public RegexFileFilter(String regularExpression) {
    this(Pattern.compile(regularExpression));
  }

  /**
   * Constructs an instance of the {@link RegexFileFilter} class initialized with the given regular expression (regex),
   * expressed as a {@link Pattern}.
   *
   * @param regularExpression regular expression (regex) used to match and filter {@link File}s.
   * @throws IllegalArgumentException if the regular expression {@link Pattern} is null.
   * @see java.util.regex.Pattern
   */
  public RegexFileFilter(Pattern regularExpression) {
    Assert.notNull(regularExpression, "The Regular Expression (Pattern) cannot be null");
    this.regularExpression = regularExpression;
  }

  /**
   * Returns the regular expression (regex) {@link Pattern} used in the filtering operation of the accept method.
   *
   * @return the regular expression {@link Pattern} used in the filtering operation of the accept method.
   * @see java.util.regex.Pattern
   */
  protected Pattern getPattern() {
    return regularExpression;
  }

  /**
   * Returns the regular expression (regex) used in the filtering operation of the accept method.
   *
   * @return a {@link String} value representing the regular expression (regex) used in the filtering operation
   * of the accept method.
   * @see java.util.regex.Pattern#pattern()
   */
  public String getRegularExpression() {
    return regularExpression.pattern();
  }

  /**
   * Determines whether the given {@link File} matches the regular expression used by this {@link FileFilter}
   * to accept or reject {@link File}s.
   *
   * @param file {@link File} to filter.
   * @return a boolean value indicating whether the given {@link File} matches the regular expression
   * used by this {@link FileFilter} to accept or reject {@link File}s.
   * @see java.util.regex.Pattern#matcher(CharSequence)
   * @see java.util.regex.Matcher#matches()
   * @see org.cp.elements.io.FileUtils#tryGetCanonicalPathElseGetAbsolutePath(java.io.File)
   */
  @Override
  @NullSafe
  public boolean accept(File file) {
    return (file != null && getPattern().matcher(FileUtils.tryGetCanonicalPathElseGetAbsolutePath(file)).matches());
  }
}
