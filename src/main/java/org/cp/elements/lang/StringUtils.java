/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import java.util.ArrayList;
import java.util.List;

/**
 * The StringUtils class performs utility operations on Strings.
 * 
 * @author John J. Blum
 * @see java.lang.String
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class StringUtils {

  public static final String COMMA_DELIMITER = ",";
  public static final String COMMA_SPACE_DELIMITER = ", ";
  public static final String DOT_SEPARATOR = ".";
  public static final String EMPTY_STRING = "";
  public static final String LINE_SEPARATOR = System.getProperty("line.separator");
  public static final String NOT_IMPLEMENTED = Constants.NOT_IMPLEMENTED; // TODO remove!
  public static final String SINGLE_SPACE = " ";

  public static final char[] EMPTY_CHAR_ARRAY = new char[0];

  public static final String[] EMPTY_STRING_ARRAY = new String[0];

  public static final String[] SPACES = {
    "",
    " ",
    "  ",
    "   ",
    "    ",
    "     ",
    "      ",
    "       ",
    "        ",
    "         ",
    "          ",
  };

  /**
   * Concatenates the array of Strings into a single String value delimited by a comma and space.
   * 
   * @param values an array of Strings to concatenate.
   * @return a single String value containing all Strings from the array concatenated by a comma and space.
   * @throws NullPointerException if the String array is null.
   * @see #concat(String[], String)
   */
  public static String concat(final String... values) {
    return concat(values, COMMA_SPACE_DELIMITER);
  }

  /**
   * Concatenates the array of Strings into a single String value delimited by the specified delimiter.
   * 
   * @param values an array of Strings to concatenate.
   * @param delimiter the String used as the delimiter separating the String values from the array.
   * @return a single String value containing all Strings from the array concatenated by the specified delimiter.
   * @throws NullPointerException if the String array is null.
   * @see java.lang.StringBuilder
   */
  public static String concat(final String[] values, final String delimiter) {
    Assert.notNull(values, "The array of String values to concatenate cannot be null!");

    StringBuilder buffer = new StringBuilder();

    for (String value : values) {
      buffer.append(buffer.length() > 0 ? delimiter : EMPTY_STRING);
      buffer.append(value);
    }

    return buffer.toString();
  }

  /**
   * Determines whether the String value contains the specified text, guarding against null values.
   * 
   * @param value the String value being evaluated for text containment.
   * @param text the String used to determine if the text is contained in value.
   * @return a boolean value if the String value contains the text.
   * @see java.lang.String#contains(CharSequence)
   */
  @NullSafe
  public static boolean contains(final String value, final String text) {
    return (value != null && text != null && value.contains(text));
  }

  /**
   * Determines whether the String value contains any digits, guarding against null values.
   * 
   * @param value the String value being evaluated for digits containment.
   * @return a boolean value indicating whether the String value contains any digits.
   * @see #toCharArray(String)
   * @see java.lang.Character#isDigit(char)
   */
  @NullSafe
  public static boolean containsDigits(final String value) {
    for (char chr : toCharArray(value)) {
      if (Character.isDigit(chr)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Determines whether the String value contains any letters, guarding against null values.
   * 
   * @param value the String value being evaluated for letter containment.
   * @return a boolean value indicating whether the String value contains any letters.
   * @see #toCharArray(String)
   * @see java.lang.Character#isLetter(char)
   */
  @NullSafe
  public static boolean containsLetters(final String value) {
    for (char chr: toCharArray(value)) {
      if (Character.isLetter(chr)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Determines whether the String value contains any whitespace, guarding against null values.
   *
   * @param value the String value being evaluated for whitespace containment.
   * @return a boolean value indicating whether the String value contains any whitespace.
   * @see #toCharArray(String)
   * @see java.lang.Character#isWhitespace(char)
   */
  @NullSafe
  public static boolean containsWhitespace(final String value) {
    for (char chr : toCharArray(value)) {
      if (Character.isWhitespace(chr)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Determines whether two String values are equal in value ignoring case and guarding against null values.
   * 
   * @param str1 the first String value in the case-insensitive equality comparison.
   * @param str2 the second String value in the case-insensitive equality comparison.
   * @return a boolean value indicating if the two String values are equal in value ignore case.
   * @see java.lang.String#equalsIgnoreCase(String)
   */
  @NullSafe
  public static boolean equalsIgnoreCase(final String str1, final String str2) {
    return (str1 != null && str1.equalsIgnoreCase(str2));
  }

  /**
   * Extracts numbers from the String value.
   * 
   * @param value the String value from which to extract numbers.
   * @return only numbers from the String value.
   * @throws NullPointerException if the String value is null.
   * @see java.lang.Character#isDigit(char)
   */
  public static String getDigits(final String value) {
    StringBuilder digits = new StringBuilder(value.length());

    for (char chr : value.toCharArray()) {
      if (Character.isDigit(chr)) {
        digits.append(chr);
      }
    }

    return digits.toString();
  }

  /**
   * Extracts letters from the String value.
   * 
   * @param value the String value from which to extract letters.
   * @return only letters from the String value.
   * @throws NullPointerException if the String value is null.
   * @see java.lang.Character#isLetter(char)
   */
  public static String getLetters(final String value) {
    StringBuilder letters = new StringBuilder(value.length());

    for (char chr : value.toCharArray()) {
      if (Character.isLetter(chr)) {
        letters.append(chr);
      }
    }

    return letters.toString();
  }

  /**
   * Constructs a String with only spaces up to the specified length.
   * 
   * @param number an integer value indicating the number of spaces in the String.
   * @return a String containing the specified number of spaces.
   */
  public static String getSpaces(int number) {
    Assert.argument(number >= 0, "The number ({0}) must be greater than equal to 0!", number);

    StringBuilder spaces = new StringBuilder(Math.max(number, 0));

    while (number > 0) {
      int count = Math.min(SPACES.length - 1, number);
      spaces.append(SPACES[count]);
      number -= count;
    }

    return spaces.toString();
  }

  /**
   * Determines whether the String value has text.  A String has text if it is not blank, implying the String is not
   * null, is not the empty String and contains at least 1 non-whitespace character.
   * 
   * @param value the String being evaluated for containing text.
   * @return a boolean value if the String contains text.
   * @see #isBlank(String)
   */
  @NullSafe
  public static boolean hasText(final String value) {
    return !isBlank(value);
  }

  /**
   * Determines the index of the first occurrence of token in the String value.  This indexOf operation is null-safe
   * and returns a -1 if the String value is null, or the token does not exist in the String value.
   * 
   * @param value the String value used to search for the token.
   * @param text the text to search for in the String value.
   * @return the index of the first occurrence of the token in the String value, or -1 if the token does not exist, or
   * the String value is blank, empty or null.
   * @see java.lang.String#indexOf(String)
   * @see #lastIndexOf(String, String)
   */
  @NullSafe
  public static int indexOf(final String value, final String text) {
    return (value != null && text != null ? value.indexOf(text) : -1);
  }

  /**
   * Determines whether the String value is blank.  A String is blank if it is null, an empty String or contains only
   * whitespace.
   * 
   * @param value the String being evaluated as a blank String value.
   * @return a boolean value indicating whether the String value is blank.
   * @see #isEmpty(String)
   * @see #hasText(String)
   */
  @NullSafe
  public static boolean isBlank(final String value) {
    return (value == null || value.trim().isEmpty());
  }

  /**
   * Determines whether the String value represents a number, i.e. consists entirely of digits.
   * 
   * @param value the String value for determination as a number.
   * @return a boolean value indicating if the String value represents a number.
   * @see java.lang.Character#isDigit(char)
   * @see #hasText(String)
   * @see #toCharArray(String)
   */
  @NullSafe
  public static boolean isDigits(final String value) {
    for (char chr : toCharArray(value)) {
      if (!Character.isDigit(chr)) {
        return false;
      }
    }

    return hasText(value);
  }

  /**
   * Determines whether the String value is empty, which is true if and only if the String value is the empty String.
   * This method guards against null.
   * 
   * @param value the String being evaluated as the empty String.
   * @return a boolean value indicating if the String value is the empty String.
   * @see StringUtils#isBlank(String)
   * @see java.lang.String#isEmpty()
   */
  @NullSafe
  public static boolean isEmpty(final String value) {
    return EMPTY_STRING.equals(value);
  }

  /**
   * Determines whether the String value consists entirely of letters.
   * 
   * @param value the String value for determination as text.
   * @return a boolean value indicating if the String value consists entirely of letters.
   * @see java.lang.Character#isLetter(char)
   * @see #hasText(String)
   * @see #toCharArray(String)
   */
  @NullSafe
  public static boolean isLetters(final String value) {
    for (char chr : toCharArray(value)) {
      if (!Character.isLetter(chr)) {
        return false;
      }
    }

    return hasText(value);
  }

  /**
   * Determines the index of the last occurrence of token in the String value.  This lastIndexOf operation is null-safe
   * and returns a -1 if the String value is null, or the token does not exist in the String value.
   * 
   * @param value the String value used to search for the token.
   * @param text the text to search for in the String value.
   * @return the index of the last occurrence of the token in the String value, or -1 if the token does not exist, or
   * the String value is blank, empty or null.
   * @see java.lang.String#lastIndexOf(String)
   * @see #indexOf(String, String)
   */
  @NullSafe
  public static int lastIndexOf(final String value, final String text) {
    return (value != null && text != null ? value.lastIndexOf(text) : -1);
  }

  /**
   * Determines the length of the String value, which is based on the number of characters in the String.
   * If the String value is null, then the length is zero.
   * 
   * @param value the String who's length will be determined.
   * @return a integer value indicating the number of characters in the String.
   * @see java.lang.String#length()
   */
  @NullSafe
  public static int length(final String value) {
    return (value == null ? 0 : value.length());
  }

  /**
   * Single spaces the tokens in the specified String value.  A token is defined as any non-whitespace character.
   * 
   * @param value the String value for which the tokens will be single spaced.
   * @return a modified String where the tokens in the String value have been single spaced.
   * @see #concat(String[], String)
   */
  public static String singleSpaceString(final String value) {
    Assert.notBlank(value, "A String value must be specified!");

    final String[] tokens = value.split("\\s+");

    return trim(concat(tokens, SINGLE_SPACE));
  }

  /**
   * Single spaces the elements in the Object array and converts all values into a String representation using
   * Object.toString to be placed in a single String.
   * 
   * @param values an array of Object values to be converted and combined into a String.
   * @return a String value containing all the Object values in String form single spaced.
   * @throws NullPointerException if the values Object array reference is null!
   * @see #concat(String[], String)
   * @see java.lang.String#valueOf(Object)
   */
  public static String singleSpaceValues(final Object... values) {
    final List<String> valueList = new ArrayList<String>(values.length);

    for (final Object value : values) {
      valueList.add(String.valueOf(value));
    }

    return trim(concat(valueList.toArray(new String[valueList.size()]), SINGLE_SPACE));
  }

  /**
   * Null-safe implementation to convert the String into an array of characters.
   * 
   * @param value the String value to convert to a character array.
   * @return a character array for the String value or null if the String value is null.
   * @see java.lang.String#toCharArray()
   */
  @NullSafe
  public static char[] toCharArray(final String value) {
    return (value == null ? EMPTY_CHAR_ARRAY : value.toCharArray());
  }

  /**
   * Converts the String value to all lower case characters.  toLowerCase is a null-safe operation.
   * 
   * @param value the String value who's characters are all converted to lower case.
   * @return the String value with all lower case characters.
   * @see #toUpperCase(String)
   * @see java.lang.String#toLowerCase()
   */
  @NullSafe
  public static String toLowerCase(final String value) {
    return (value != null ? value.toLowerCase() : null);
  }

  /**
   * Converts the String value to all UPPER case characters.  toUpperCase is a null-safe operation.
   * 
   * @param value the String value who's characters are all converted to UPPER case.
   * @return the String value with all UPPER case characters.
   * @see #toLowerCase(String)
   * @see java.lang.String#toUpperCase()
   */
  @NullSafe
  public static String toUpperCase(final String value) {
    return (value != null ? value.toUpperCase() : null);
  }

  /**
   * Trims the specified String value, removing any whitespace from the beginning or end of a String.
   * 
   * @param value the String value to trim.
   * @return a trimmed version of the specified String value.
   * @see java.lang.String#trim()
   */
  @NullSafe
  public static String trim(final String value) {
    return (value != null ? value.trim() : null);
  }

  /**
   * Trims all whitespace characters from the String value.  The whitespace can occur in any position (beginning, end,
   * between characters) within th String value.
   * 
   * @param value the String value to fully trim.
   * @return a String value containing now whitespace.
   * @see java.lang.Character#isWhitespace(char)
   */
  @NullSafe
  public static String trimAll(final String value) {
    if (value != null) {
      StringBuilder trimmedValue = new StringBuilder(value.length());

      for (char chr : value.toCharArray()) {
        if (!Character.isWhitespace(chr)) {
          trimmedValue.append(chr);
        }
      }

      return trimmedValue.toString();
    }

    return null;
  }

}
