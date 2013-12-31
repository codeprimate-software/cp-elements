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

package org.cp.elements.lang;

/**
 * The CharacterUtils class provides utility methods for working with Characters.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Character
 * @since 1.0.0
 */
public abstract class CharacterUtils {

  /**
   * Determines whether the specified Character is blank.  A Character is blank if it is one of the whitespace
   * characters or is the null character/char value '\0'.
   * <p/>
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
   * <p/>
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
   * <p/>
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
   * <p/>
   * @param value the Character to convert into an equivalent primitive char value.
   * @return a primitive char value for the specified Character, or '\0' for null.
   * @see java.lang.Character
   */
  public static char valueOf(final Character value) {
    return (value == null ? '\0' : value);
  }

}
