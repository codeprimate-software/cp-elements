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

import static org.cp.elements.lang.LangExtensions.assertThat;

import java.util.ArrayList;
import java.util.List;

import org.cp.elements.util.ArrayUtils;

/**
 * The StringUtils class performs utility operations on Strings.
 * 
 * @author John J. Blum
 * @see java.lang.String
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class StringUtils {

  public static final char COMMA_DELIMITER_CHAR = ',';
  public static final char DOT_SEPARATOR_CHAR = '.';
  public static final char EMPTY_CHAR = '\0';
  public static final char SINGLE_SPACE_CHAR = ' ';

  public static final String COMMA_DELIMITER = ",";
  public static final String COMMA_SPACE_DELIMITER = ", ";
  public static final String DOT_SEPARATOR = ".";
  public static final String EMPTY_STRING = "";
  public static final String LINE_SEPARATOR = System.getProperty("line.separator");
  public static final String SINGLE_SPACE = " ";
  public static final String UTF_8 = "UTF-8";

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
   * Defaults the given String to the first non-blank default value if the given String is blank, otherwise returns
   * the given String.
   *
   * @param value the String to evaluate if blank.
   * @param defaultValues an array of default String values to use if the given String is blank.
   * @return the first non-blank default String value if the given String is blank.  If the given String
   * and all default String values are blank, then the given String is returned.
   * @see #isBlank(String)
   * @see #hasText(String)
   */
  @NullSafe
  public static String defaultIfBlank(final String value, final String... defaultValues) {
    if (isBlank(value)) {
      for (String defaultValue : defaultValues) {
        if (hasText(defaultValue)) {
          return defaultValue;
        }
      }
    }

    return value;
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
   * Pads the given String with the specified number of spaces to the right.
   *
   * @param value the String to pad.
   * @param length the total length of the given String with padding.
   * @return the given String padded with spaces up to the specified length.
   * @see #pad(String, char, int)
   */
  @NullSafe
  public static String pad(final String value, final int length) {
    return pad(value, SINGLE_SPACE_CHAR, length);
  }

  /**
   * Pads the given String with the specified number of characters to the right.
   *
   * @param value the String to pad.
   * @param padding the character used for padding.
   * @param length the total length of the given String with padding.
   * @return the given String padded with the specified character up to the specified length.
   * @throws IllegalArgumentException if length is less than 0.
   */
  @NullSafe
  public static String pad(final String value, final char padding, final int length) {
    assertThat(length).throwing(new IllegalArgumentException(String.format(
      "(%1$d) must be greater than equal to 0", length))).isGreaterThanEqualTo(0);

    if (length > 0) {
      StringBuilder builder = new StringBuilder(ObjectUtils.defaultIfNull(value, EMPTY_STRING));

      while (length - builder.length() > 0) {
        builder.append(padding);
      }

      return builder.toString();
    }

    return value;
  }

  /**
   * Replaces the given characters in the given {@link String} value with the replacement {@link String}.
   *
   * @param value the {@link String} value to perform the search and replace operation.
   * @param charsToReplace the characeters in the {@link String} value to replace.
   * @param replacement the replacement {@link String} used to replace the characters.
   * @return a modified {@link String} value with the characters replaced with the replacement.
   * Returns null if the {@link String} value is null.
   * @throws NullPointerException if the charsToReplace or the replacement {@link String}s are null.
   * @see java.lang.String#replaceAll(String, String)
   */
  public static String replace(String value, final String charsToReplace, final String replacement) {
    Assert.notNull(charsToReplace, "charsToReplace cannot be null");
    Assert.notNull(replacement, "replacement cannot be null");

    if (value != null) {
      String newValue = value;

      do {
        value = newValue;
        newValue = newValue.replaceAll(charsToReplace, replacement);
      }
      while (!value.equals(newValue));
    }

    return value;
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
    List<String> valueList = new ArrayList<>(values.length);

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
    return (value != null ? value.toCharArray() : EMPTY_CHAR_ARRAY);
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
   * Tokenizes the given delimited String into an array of individually trimmed Strings.  If String is blank,
   * empty or null, then a 0 length String array is returned. If the String is not delimited with the specified
   * delimiter then a String array of size 1 is returned with the given String value as the only element.
   *
   * @param commaDelimitedValue the comma delimited String to tokenize.
   * @return an array of individually tokenized and trimmed Strings from the given String.
   * @see #toStringArray(String, String)
   */
  @NullSafe
  public static String[] toStringArray(final String commaDelimitedValue) {
    return toStringArray(commaDelimitedValue, COMMA_DELIMITER);
  }

  /**
   * Tokenizes the given delimited String into an array of individually trimmed Strings.  If String is blank,
   * empty or null, then a 0 length String array is returned. If the String is not delimited with the specified
   * delimiter then a String array of size 1 is returned with the given String value as the only element.
   *
   * @param delimitedValue the String to tokenize based on the delimiter.
   * @param delimiter the delimiter used to tokenize the String value.
   * @return an array of individually tokenized and trimmed Strings from the given String.
   * @see java.lang.String#split(String)
   * @see org.cp.elements.lang.ObjectUtils#defaultIfNull(Object[])
   * @see org.cp.elements.util.ArrayUtils#transform(Object[], Transformer)
   * @see #defaultIfBlank(String, String...)
   * @see #trim(String)
   */
  @NullSafe
  public static String[] toStringArray(final String delimitedValue, final String delimiter) {
    return ArrayUtils.transform(ObjectUtils.defaultIfNull(delimitedValue, EMPTY_STRING).split(
        defaultIfBlank(delimiter, COMMA_DELIMITER)), StringUtils::trim);
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

  /**
   * Truncates the given String to the desired length.  If the String is blank, empty or null, then value is returned,
   * otherwise the String is truncated to the maximum length determined by the value's length and desired length.
   *
   * @param value the String to truncate.
   * @param length an integer specifying the length to truncate the String to.
   * @return the given String truncated to length, or the entire String if the desired length exceeds
   * the String's length.
   * @throws IllegalArgumentException if length is less than 0.
   * @see java.lang.String#substring(int, int)
   * @see java.lang.String#length()
   */
  @NullSafe
  public static String truncate(String value, final int length) {
    assertThat(length).throwing(new IllegalArgumentException(String.format(
      "(%1$d) must be greater than equal to 0", length))).isGreaterThanEqualTo(0);

    return (value != null ? value.substring(0, Math.min(value.length(), length)) : null);
  }

  /**
   * Wraps a line of text to no longer than the specified width, measured by the number of characters in each line,
   * indenting all subsequent lines with the indent.  If the indent is null, then an empty String is used.
   *
   * @param line a String containing the line of text to wrap.
   * @param widthInCharacters an integer value indicating the width of each line measured by the number of characters.
   * @param indent the String value used to indent all subsequent lines.
   * @return the line of text wrapped.
   * @throws IndexOutOfBoundsException if widthInCharacters is less than 0, or there are no word boundaries within
   * the given width on any given split.
   * @throws NullPointerException if the line of text is null.
   */
  public static String wrap(String line, final int widthInCharacters, String indent) {
    StringBuilder buffer = new StringBuilder();

    int lineCount = 1;
    int spaceIndex;

    // if indent is null, then do not indent the wrapped lines
    indent = (indent != null ? indent : EMPTY_STRING);

    while (line.length() > widthInCharacters) {
      spaceIndex = line.substring(0, widthInCharacters).lastIndexOf(SINGLE_SPACE);
      buffer.append(lineCount++ > 1 ? indent : EMPTY_STRING);
      // throws IndexOutOfBoundsException if spaceIndex is -1, implying no word boundary was found within
      // the given width; this also avoids the infinite loop
      buffer.append(line.substring(0, spaceIndex));
      buffer.append(LINE_SEPARATOR);
      // possible infinite loop if spaceIndex is -1, see comment above
      line = line.substring(spaceIndex + 1);
    }

    buffer.append(lineCount > 1 ? indent : EMPTY_STRING);
    buffer.append(line);

    return buffer.toString();
  }

}
