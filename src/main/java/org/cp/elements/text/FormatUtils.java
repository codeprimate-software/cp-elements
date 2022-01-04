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
package org.cp.elements.text;

import java.text.MessageFormat;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.ArrayUtils;

/**
 * The {@link FormatUtils} class is an abstract utility class for working with text formatting.
 *
 * @author John J. Blum
 * @see java.lang.String
 * @see java.text.MessageFormat
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FormatUtils {

  /**
   * Formats the given {@link String} of text.
   *
   * @param textPattern {@link String} text pattern to format.
   * @param args array of {@link Object} arguments to apply to the text pattern.
   * @return a formatted {@link String} of text with the arguments applied to the text pattern.
   * @see #messageFormat(String, Object...)
   * @see #stringFormat(String, Object...)
   * @see java.lang.String
   */
  public static @NotNull String format(@NotNull String textPattern, Object... args) {
    return messageFormat(stringFormat(textPattern, args), args);
  }

  /**
   * Formats the given {@link String} of text using the {@link MessageFormat} class.
   *
   * @param textPattern {@link String} text pattern to format.
   * @param args array of {@link Object} arguments to apply to the text pattern.
   * @return a formatted {@link String} of text with the arguments applied to the text pattern
   * using the {@link MessageFormat} class.
   * @see java.text.MessageFormat
   * @see java.lang.String
   */
  protected static @NotNull String messageFormat(@NotNull String textPattern, Object... args) {
    return MessageFormat.format(textPattern, ArrayUtils.nullSafeArray(args));
  }

  /**
   * Formats the given {@link String} of text using the {@link String#format(String, Object...)} method.
   *
   * @param textPattern {@link String} text pattern to format.
   * @param args array of {@link Object} arguments to apply to the text pattern.
   * @return a formatted {@link String} of text with the arguments applied to the text pattern
   * using the {@link String#format(String, Object...)} method.
   * @see java.lang.String#format(String, Object...)
   * @see java.lang.String
   */
  protected static @NotNull String stringFormat(@NotNull String textPattern, Object... args) {
    return String.format(textPattern, ArrayUtils.nullSafeArray(args));
  }
}
