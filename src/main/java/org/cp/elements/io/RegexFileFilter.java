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
import java.util.regex.Pattern;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Java {@link FileFilter} and Elements {@link Filter} implementation that uses a {@link Pattern Regular Expression}
 * to match and filter {@link File Files}.
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
   * Constructs a new instance of {@link RegexFileFilter} initialized with the given {@literal Regular Expression}
   * expressed as a {@literal pattern} contained in the given, required {@link String}.
   *
   * @param regularExpression {@link String} containing the {@literal Regular Expression (REGEX) Pattern} used to
   * match and filter {@link File Files}; must not be {@literal null} or {@literal empty}.
   * @throws IllegalArgumentException if the given {@link String pattern} used in the Regular Expression (REGEX)
   * is {@literal null} or {@literal empty}.
   * @see java.util.regex.Pattern#compile(String)
   */
  public RegexFileFilter(@NotNull String regularExpression) {
    this(Pattern.compile(StringUtils.requireText(regularExpression,
      "Regular Expression Pattern [%s] is required")));
  }

  /**
   * Constructs a new instance of {@link RegexFileFilter} initialized with a compiled {@literal Regular Expression}
   * expressed in the given, required {@link Pattern} object.
   *
   * @param regularExpression {@link Pattern} containing the compiled {@literal Regular Expression (REGEX)} used to
   * match and filter {@link File Files}; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link Pattern} is {@literal null}.
   * @see java.util.regex.Pattern
   */
  public RegexFileFilter(@NotNull Pattern regularExpression) {
    this.regularExpression = ObjectUtils.requireObject(regularExpression,
      "Regular Expression Pattern is required");
  }

  /**
   * Returns a reference to the configured, compiled {@literal Regular Expression (REGEX)} {@link Pattern} used to
   * match and filter {@link File Files}.
   *
   * @return the configured, compiled {@literal Regular Expression (REGEX)} {@link Pattern} used to match and filter
   * {@link File Files}.
   * @see java.util.regex.Pattern
   */
  protected @NotNull Pattern getPattern() {
    return this.regularExpression;
  }

  /**
   * Returns the configured {@literal Regular Expression (REGEX)} {@link String Patten} used to
   * match and filter {@link File Files}.
   *
   * @return a {@link String} containing the {@literal pattern} of the {@literal Regular Expression (REGEX)} used to
   * match and filter {@link File Files}.
   * @see java.util.regex.Pattern#pattern()
   * @see #getPattern()
   */
  public @NotNull String getRegularExpression() {
    return getPattern().pattern();
  }

  /**
   * Determines whether the given {@link File} matches the {@literal Regular Expression (REGEX)} used by
   * this {@link FileFilter} to match and filter {@link File Files}.
   * <p>
   * The {@link File File's} {@link File#getCanonicalPath()} or {@link File#getAbsolutePath()} are used in
   * the match against the {@literal Regular Expression (REGEX} {@link Pattern}.
   *
   * @param file {@link File} to filter.
   * @return a boolean value indicating whether the given {@link File} matches the {@literal Regular Expression (REGEX)}
   * used by this {@link FileFilter} to accept or reject {@link File Files}.
   * @see org.cp.elements.io.FileUtils#tryGetCanonicalPathElseGetAbsolutePath(java.io.File)
   * @see java.util.regex.Pattern#matcher(CharSequence)
   * @see java.util.regex.Matcher#matches()
   * @see #getPattern()
   * @see java.io.File
   */
  @NullSafe
  @Override
  public boolean accept(@Nullable File file) {
    return getPattern().matcher(getPath(file)).matches();
  }

  private @NotNull String getPath(@Nullable File file) {

    return file != null
      ? FileUtils.tryGetCanonicalPathElseGetAbsolutePath(file)
      : StringUtils.EMPTY_STRING;
  }
}
