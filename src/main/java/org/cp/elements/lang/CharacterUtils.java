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

package org.cp.elements.lang;

/**
 * The CharacterUtils class provides utility methods for working with Characters.
 * 
 * @author John J. Blum
 * @see java.lang.Character
 * @see java.lang.Character#TYPE
 * @since 1.0.0
 */
public abstract class CharacterUtils {

  /**
   * Determines whether the specified Character is blank.  A Character is blank if it is one of the whitespace
   * characters or is the null character/char value '\0'.
   * 
   * @param value the Character being evaluated as blank.
   * @return a boolean value indicating whether the specified Character is blank.
   * @see java.lang.Character#isWhitespace(char)
   * @see #valueOf(Character)
   */
  public static boolean isBlank(final Character value) {
    final char chr = valueOf(value);
    return (chr == '\0' || Character.isWhitespace(chr));
  }

  /**
   * Determines whether the specified Character is a digit, a character value in the range of (0..9).
   * 
   * @param value the Character being evaluated as a digit.
   * @return a boolean value indicating whether the specified Character is a digit.
   * @see java.lang.Character#isDigit(char)
   * @see #valueOf(Character)
   */
  public static boolean isDigit(final Character value) {
    return Character.isDigit(valueOf(value));
  }

  /**
   * Determines whether the specified Character is a letter, a character value in the range of (a..z) case-insensitive.
   * 
   * @param value the Character being evaluated as a letter.
   * @return a boolean value indicating whether the specified Character is a letter.
   * @see java.lang.Character#isLetter(char)
   * @see #valueOf(Character)
   */
  public static boolean isLetter(final Character value) {
    return Character.isLetter(valueOf(value));
  }

  /**
   * Gets the char primitive value for the specified Character wrapper object, handling null values by returning
   * the null char value '\0'.
   * 
   * @param value the Character to convert into an equivalent primitive char value.
   * @return a primitive char value for the specified Character, or '\0' for null.
   * @see java.lang.Character
   */
  public static char valueOf(final Character value) {
    return (value == null ? '\0' : value);
  }

}
