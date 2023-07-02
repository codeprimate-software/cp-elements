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
package org.cp.elements.lang;

import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract utility class providing methods for processing {@link Character Characters}.
 *
 * @author John J. Blum
 * @see java.lang.Character
 * @see java.lang.Character#TYPE
 * @since 1.0.0
 */
public abstract class CharacterUtils {

  /**
   * Determines whether the given {@link Character} is {@link Character#isWhitespace(char) blank}.
   * <p>
   * A {@link Character} is considered {@literal blank} if it is one of the whitespace
   * characters or is the {@literal null} character/char value {@literal '\0'}.
   *
   * @param value {@link Character} to evaluate.
   * @return a boolean value indicating whether the given {@literal Character}
   * is {@link Character#isWhitespace(char) blank}.
   * @see java.lang.Character#isWhitespace(char)
   * @see java.lang.Character
   * @see #valueOf(Character)
   */
  @NullSafe
  public static boolean isBlank(@Nullable Character value) {
    char chr = valueOf(value);
    return chr == '\0' || Character.isWhitespace(chr);
  }

  /**
   * Determines whether the given {@link Character} is a {@link Character#isDigit(char) digit},
   * a character value in the range of {@literal (0..9)}.
   *
   * @param value {@link Character} to evaluate.
   * @return a boolean value indicating whether the given {@link Character}
   * is a {@link Character#isDigit(char) digit}.
   * @see java.lang.Character#isDigit(char)
   * @see java.lang.Character
   * @see #valueOf(Character)
   */
  @NullSafe
  public static boolean isDigit(@Nullable Character value) {
    return Character.isDigit(valueOf(value));
  }

  /**
   * Determines whether the given {@link Character} is a {@link Character#isLetter(char) letter},
   * a character value in the range of {@literal (a..z)} case-insensitive.
   *
   * @param value {@link Character} being evaluated.
   * @return a boolean value indicating whether the given {@link Character}
   * is a {@link Character#isLetter(char) letter}.
   * @see java.lang.Character#isLetter(char)
   * @see java.lang.Character
   * @see #valueOf(Character)
   */
  @NullSafe
  public static boolean isLetter(@Nullable Character value) {
    return Character.isLetter(valueOf(value));
  }

  /**
   * Gets the {@link Character#TYPE char primitive value} for the given {@link Character} wrapper object.
   * <p>
   * Handles {@literal null} values by returning the {@literal null} char value {@literal '\0'}.
   *
   * @param value {@link Character} to convert into an equivalent {@link Character#TYPE primitive char value}.
   * @return a {@link Character#TYPE primitive char value} for the given {@link Character},
   * or {@literal '\0'} for {@literal null}.
   * @see java.lang.Character
   */
  @NullSafe
  public static char valueOf(@Nullable Character value) {
    return value != null ? value : '\0';
  }
}
