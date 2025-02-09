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

import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newNoSuchElementException;

import java.text.CharacterIterator;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * Abstract utility class used to process {@link String Strings}.
 *
 * @author John J. Blum
 * @see java.lang.String
 * @see java.util.regex.Pattern
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class StringUtils {

  public static final char COMMA_DELIMITER_CHAR = ',';
  public static final char DOT_SEPARATOR_CHAR = '.';
  public static final char EMPTY_CHAR = '\0';
  public static final char SINGLE_SPACE_CHAR = ' ';
  public static final char UNDERSCORE_CHAR = '_';

  public static final String COLON_DELIMITER = ":";
  public static final String COLON_SPACE_DELIMITER = ": ";
  public static final String COMMA_DELIMITER = ",";
  public static final String COMMA_SPACE_DELIMITER = ", ";
  public static final String DOT_SEPARATOR = ".";
  public static final String EMPTY_STRING = "";
  public static final String LINE_SEPARATOR = System.lineSeparator();
  public static final String SEMICOLON_SEPARATOR = ";";
  public static final String SEMICOLON_SPACE_SEPARATOR = "; ";
  public static final String SINGLE_SPACE = " ";
  public static final String UNDERSCORE = "_";
  public static final String UTF_8 = "UTF-8";

  public static final char[] EMPTY_CHAR_ARRAY = new char[0];

  public static final String[] EMPTY_STRING_ARRAY = new String[0];

  static final String[] SPACES = {
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
   * Capitalizes the first {@link Character letter} in the {@link String}.
   *
   * @param value {@link String} to capitalize.
   * @return the given {@link String} with the first {@link Character letter} capitalized.
   * @throws IllegalArgumentException if the {@link String value} does not contain any {@link String text}.
   */
  public static String capitalize(String value) {

    Assert.hasText(value, "Value [%s] is required", value);

    String firstCharacter = String.valueOf(value.charAt(0));

    return firstCharacter.toUpperCase().concat(value.substring(1));
  }

  /**
   * Concatenates the array of {@link String Strings} into a single {@link String value} delimited by a comma and space.
   *
   * @param values array of {@link String Strings} to concatenate.
   * @return a single {@link String value} containing all {@link String Strings} from the array concatenated by
   * a comma and space.
   * @throws NullPointerException if the given array of {@link String Strings} is {@literal null}.
   * @see #concat(String[], String)
   */
  public static String concat(String... values) {
    return concat(values, COMMA_SPACE_DELIMITER);
  }

  /**
   * Concatenates the array of {@link String Strings} into a single {@link String value} delimited by
   * the given {@link String delimiter}.
   *
   * @param values array of {@link String Strings} to concatenate.
   * @param delimiter {@link String} used as the delimiter separating the {@link String values} in the given array.
   * @return a single {@link String value} containing all {@link String Strings} from the array concatenated by
   * the given {@link String delimiter}.
   * @throws NullPointerException if the array of {@link String Strings} is {@literal null}.
   * @see java.lang.StringBuilder
   */
  public static String concat(String[] values, String delimiter) {

    Assert.notNull(values, "An array of String values to concatenate is required");

    StringBuilder buffer = new StringBuilder();

    for (String value : values) {
      buffer.append(buffer.length() > 0 ? delimiter : EMPTY_STRING);
      buffer.append(value);
    }

    return buffer.toString();
  }

  /**
   * Determines whether the {@link String value} contains the specified {@link String text},
   * guarding against {@literal null} values.
   *
   * @param text {@link String} evaluated for value containment.
   * @param value {@link String} used to determine if the value is contained in the text.
   * @return a boolean value if the {@link String text} contains the {@link String value}.
   * @see java.lang.String#contains(CharSequence)
   */
  @NullSafe
  public static boolean contains(@Nullable String text, @Nullable String value) {
    return text != null && value != null && text.contains(value);
  }

  /**
   * Determines whether the {@link String value} contains any digits, guarding against {@literal null} values.
   *
   * @param value {@link String value} being evaluated for digits containment.
   * @return a boolean value indicating whether the String value contains any digits.
   * @see java.lang.Character#isDigit(char)
   * @see #toCharArray(String)
   */
  @NullSafe
  public static boolean containsDigits(@Nullable String value) {

    for (char chr : toCharArray(value)) {
      if (Character.isDigit(chr)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Determines whether the {@link String value} contains any letters, guarding against {@literal null} values.
   *
   * @param value {@link String value} being evaluated for letter containment.
   * @return a boolean value indicating whether the {@link String value} contains any letters.
   * @see java.lang.Character#isLetter(char)
   * @see #toCharArray(String)
   */
  @NullSafe
  public static boolean containsLetters(@Nullable String value) {

    for (char chr : toCharArray(value)) {
      if (Character.isLetter(chr)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Determines whether the {@link String value} contains any whitespace, guarding against {@literal null} values.
   *
   * @param value {@link String value} being evaluated for whitespace containment.
   * @return a boolean value indicating whether the {@link String value} contains any whitespace.
   * @see java.lang.Character#isWhitespace(char)
   * @see #toCharArray(String)
   */
  @NullSafe
  public static boolean containsWhitespace(@Nullable String value) {

    for (char chr : toCharArray(value)) {
      if (Character.isWhitespace(chr)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Defaults the given {@link String} to the first non-blank {@link String default value} if the given {@link String}
   * is blank, otherwise returns the given {@link String}.
   *
   * @param value {@link String} to evaluate.
   * @param defaultValues array of {@link String default values} to use if the given {@link String} is blank.
   * @return the first non-blank default {@link String value} if the given {@link String} is blank.
   * If the given {@link String} and all {@link String default values} are blank, then the given {@link String}
   * is returned.
   * @see #isBlank(String)
   * @see #hasText(String)
   */
  public static @Nullable String defaultIfBlank(@Nullable String value, String... defaultValues) {

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
   * Determines whether two {@link String values} are equal in value ignoring case
   * and guarding against {@literal null} values.
   *
   * @param stringOne first {@link String} in the case-insensitive equality comparison.
   * @param stringTwo second {@link String} in the case-insensitive equality comparison.
   * @return a boolean value indicating if the two {@link String values} are equal in value ignoring case.
   * @see java.lang.String#equalsIgnoreCase(String)
   */
  @NullSafe
  public static boolean equalsIgnoreCase(@Nullable String stringOne, @Nullable String stringTwo) {
    return stringOne != null && stringOne.equalsIgnoreCase(stringTwo);
  }

  /**
   * Extracts numbers from the {@link String value}.
   *
   * @param value {@link String} from which to extract numbers.
   * @return only numbers from the given {@link String value}.
   * Returns empty {@link String} if the {@link String value}
   * contains no digits.
   * @see java.lang.Character#isDigit(char)
   */
  @NullSafe
  public static @NotNull String getDigits(@Nullable String value) {

    StringBuilder digits = new StringBuilder(length(value));

    for (char chr : toCharArray(value)) {
      if (Character.isDigit(chr)) {
        digits.append(chr);
      }
    }

    return digits.toString();
  }

  /**
   * Extracts letters from the {@link String value}.
   *
   * @param value {@link String} from which to extract letters.
   * @return only letters from the {@link String value}.
   * Returns empty {@link String} if the {@link String value}
   * contains no letters.
   * @see java.lang.Character#isLetter(char)
   */
  @NullSafe
  public static @NotNull String getLetters(@Nullable String value) {

    StringBuilder letters = new StringBuilder(length(value));

    for (char chr : toCharArray(value)) {
      if (Character.isLetter(chr)) {
        letters.append(chr);
      }
    }

    return letters.toString();
  }

  /**
   * Constructs a {@link String} with only spaces up to the specified length.
   *
   * @param number {@link Integer value} indicating the number of spaces in the returned {@link String}.
   * @return a {@link String} containing the specified number of spaces.
   */
  public static @NotNull String getSpaces(int number) {

    Assert.argument(number, argument -> argument >= 0,
      "The number [{0}] of desired spaces must be greater than equal to 0", number);

    StringBuilder spaces = new StringBuilder(Math.max(number, 0));

    int remainingCount = number;

    while (remainingCount > 0) {
      int count = Math.min(SPACES.length - 1, remainingCount);
      spaces.append(SPACES[count]);
      remainingCount -= count;
    }

    return spaces.toString();
  }

  /**
   * Determines whether the {@link String} has text.
   * <p>
   * A {@link String} has {@link String text} if it is {@link String#isBlank() not blank}, implying the {@link String}
   * is not {@literal null}, is not {@link String#isEmpty()} and contains at least 1 non-whitespace character.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value if the {@link String} contains text.
   * @see #isBlank(String)
   */
  @NullSafe
  public static boolean hasText(@Nullable String value) {
    return !isBlank(value);
  }

  /**
   * Determines the {@link Integer index} of the first occurrence of token in the {@link String}.
   * <p>
   * This {@code indexOf} operation is null-safe and returns a {@literal -1} if the {@link String} is {@literal null},
   * or the token does not exist in the {@link String}.
   *
   * @param text {@link String text} to search.
   * @param value {@link String value} to search for in the {@link String}.
   * @return the {@link Integer index} of the first occurrence of the token in the {@link String}, or {@literal -1}
   * if the token does not exist, or the {@link String} is {@link String#isBlank() blank},
   * {@link String#isEmpty() empty} or {@literal null}.
   * @see java.lang.String#indexOf(String)
   * @see #lastIndexOf(String, String)
   */
  @NullSafe
  public static int indexOf(@Nullable String text, @Nullable String value) {
    return text != null && value != null ? text.indexOf(value) : -1;
  }

  /**
   * Determines whether the {@link String} is blank.
   * <p>
   * A {@link String} is blank if it is {@literal null}, {@link String#isEmpty() empty} or contains only whitespace.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating whether the {@link String} is blank.
   * @see java.lang.String#isBlank()
   * @see #isEmpty(String)
   * @see #hasText(String)
   */
  @NullSafe
  public static boolean isBlank(@Nullable String value) {
    return value == null || value.trim().isEmpty();
  }

  /**
   * Determines whether the {@link String} represents a number, i.e. consists entirely of digits.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating if the {@link String} represents a number.
   * @see java.lang.Character#isDigit(char)
   * @see #toCharArray(String)
   * @see #hasText(String)
   */
  @NullSafe
  public static boolean isDigits(@Nullable String value) {

    for (char chr : toCharArray(value)) {
      if (!Character.isDigit(chr)) {
        return false;
      }
    }

    return hasText(value);
  }

  /**
   * Determines whether the {@link String} is empty.
   * <p>
   * A {@link String} is empty iff the {@link String} is the {@link String#isEmpty() empty String}
   * and not {@literal null}.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating if the {@link String} is empty.
   * @see java.lang.String#isEmpty()
   * @see #isBlank(String)
   * @see #hasText(String)
   */
  @NullSafe
  public static boolean isEmpty(@Nullable String value) {
    return EMPTY_STRING.equals(value);
  }

  /**
   * Determines whether the {@link String} represents letters; i.e. consists entirely of letters.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating if the {@link String} consists entirely of letters.
   * @see java.lang.Character#isLetter(char)
   * @see #toCharArray(String)
   * @see #hasText(String)
   */
  @NullSafe
  public static boolean isLetters(@Nullable String value) {

    for (char chr : toCharArray(value)) {
      if (!Character.isLetter(chr)) {
        return false;
      }
    }

    return hasText(value);
  }

  /**
   * Determines the {@link Integer index} of the last occurrence of token in the {@link String}.
   * <p>
   * This {@code lastIndexOf} operation is null-safe and returns a {@literal -1} if the {@link String}
   * is {@literal null}, or the token does not exist in the {@link String}.
   *
   * @param text {@link String text} to search.
   * @param value {@link String value} to search for in the {@link String}.
   * @return the {@link Integer index} of the last occurrence of the token in the {@link String}, or {@literal -1}
   * if the token does not exist, or the {@link String} is {@link String#isBlank() blank},
   * {@link String#isEmpty() empty} or {@literal null}.
   * @see java.lang.String#lastIndexOf(String)
   * @see #indexOf(String, String)
   */
  @NullSafe
  public static int lastIndexOf(@Nullable String text, @Nullable String value) {
    return text != null && value != null ? text.lastIndexOf(value) : -1;
  }

  /**
   * Determines the length of the {@link String} based on the number of characters in the {@link String}.
   * <p>
   * If the {@link String} is {@literal null}, then the length is {@literal zero}.
   *
   * @param value {@link String value} whose length is determined.
   * @return a {@link Integer value} indicating the number of characters in the {@link String}.
   * @see java.lang.String#length()
   */
  @NullSafe
  public static int length(@Nullable String value) {
    return value != null ? value.length() : 0;
  }

  /**
   * Determines whether the given {@link String} has no text.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating whether the given {@link String} has text.
   * Returns {@literal true} if the {@link String} has no text, or {@literal false}
   * if the {@link String} contains text.
   * @see #hasText(String)
   */
  public static boolean noText(@Nullable String value) {
    return !hasText(value);
  }

  /**
   * Pads the given {@link String value} with spaces on the left.
   *
   * @param value {@link String value} to pad.
   * @param length {@link Integer total length} of the given {@link String value} with spaces.
   * @return the padded {@link String value}.
   * @throws IllegalArgumentException if {@link Integer length} is less than {@literal 0}.
   * @see #padLeft(String, char, int)
   */
  public static @Nullable String padLeft(@Nullable String value, int length) {
    return padLeft(value, SINGLE_SPACE_CHAR, length);
  }

  /**
   * Pads the given {@link String value} with {@link Character padding} on the left.
   *
   * @param value {@link String value} to pad.
   * @param padding {@link Character} to use as padding.
   * @param length {@link Integer total length} of the given {@link String value} with {@link Character padding}.
   * @return the padded {@link String value}.
   * @throws IllegalArgumentException if {@link Integer length} is less than {@literal 0}.
   */
  public static @Nullable String padLeft(@Nullable String value, char padding, int length) {

    assertThat(length)
      .throwing(newIllegalArgumentException("[%d] must be greater than equal to 0", length))
      .isGreaterThanEqualTo(0);

    String resolvedValue = ObjectUtils.returnValueOrDefaultIfNull(value, EMPTY_STRING);

    return length > 0
      ? String.valueOf(padding).repeat(Math.max(length - resolvedValue.length(), 0)).concat(resolvedValue)
      : value;
  }

  /**
   * Pads the given {@link String value} with spaces on the right.
   *
   * @param value {@link String value} to pad.
   * @param length {@link Integer total length} of the given {@link String value} with spaces.
   * @return the padded {@link String value}.
   * @throws IllegalArgumentException if {@link Integer length} is less than {@literal 0}.
   * @see #padRight(String, char, int)
   */
  public static @Nullable String padRight(@Nullable String value, int length) {
    return padRight(value, SINGLE_SPACE_CHAR, length);
  }

  /**
   * Pads the given {@link String value} with {@link Character padding} to the right.
   *
   * @param value {@link String} to pad.
   * @param padding {@link Character} to use as padding.
   * @param length {@link Integer total length} of the given {@link String value} with {@link Character padding}.
   * @return the padded {@link String value}.
   * @throws IllegalArgumentException if {@link Integer length} is less than {@literal 0}.
   */
  public static @Nullable String padRight(@Nullable String value, char padding, int length) {

    assertThat(length)
      .throwing(newIllegalArgumentException("[%d] must be greater than equal to 0", length))
      .isGreaterThanEqualTo(0);

    String resolvedValue = ObjectUtils.returnValueOrDefaultIfNull(value, EMPTY_STRING);

    return length > 0
      ? resolvedValue.concat(String.valueOf(padding).repeat(Math.max(length - resolvedValue.length(), 0)))
      : value;
  }

  /**
   * Null-safe method to replace the specified {@link String pattern} in the given {@link String} with
   * the replacement {@link String}.
   * <p>
   * For example, given the following String...
   * <p>
   * <code>
   * "///absolute//path/to/////some////file.ext"
   * </code>
   * <p>
   * When 'pattern' is "/+" and 'replacement' is "/", the resulting, returned String value will be...
   * <p>
   * <code>
   * "/absolute/path/to/some/file.ext"
   * </code>
   *
   * @param value {@link String} on which to perform the pattern search and replace operation.
   * @param pattern {@link String pattern of characters} in the {@link String} to replace.
   * @param replacement the replacement {@link String} used to replace all occurrences of the pattern of characters.
   * @return a modified {@link String} with the pattern of characters replaced with the replacement.
   * Returns the original {@link String} if the pattern of characters is not present in the given {@link String}.
   * @see java.util.regex.Pattern#compile(String)
   * @see java.util.regex.Pattern#matcher(CharSequence)
   * @see java.util.regex.Matcher#replaceAll(String)
   */
  @NullSafe
  public static String replaceAll(String value, String pattern, String replacement) {

    String result = value;

    if (!ObjectUtils.areAnyNull(value, pattern, replacement)) {
      result = Pattern.compile(pattern).matcher(value).replaceAll(replacement);
    }

    return result;
  }

  /**
   * Requires the given {@link String value} to have text.
   *
   * @param value {@link String} to evaluate.
   * @param message {@link String} containing the message used to describe the {@link IllegalArgumentException}.
   * @param args array of {@link Object arguments} used to format (replace) placeholders in the {@link String message}.
   * @return the given {@link String value}.
   * @throws IllegalArgumentException if the {@link String} is {@literal null} or {@literal empty} (blank).
   */
  public static @NotNull String requireText(@Nullable String value, String message, Object... args) {
    Object[] resolvedArguments = ArrayUtils.isNotEmpty(args) ? args : ArrayUtils.asArray(value);
    Assert.hasText(value, message, resolvedArguments);
    return value;
  }

  /**
   * Reverses the characters in the given {@link String}.
   *
   * @param value {@link String} to reverse; required.
   * @return a {@link String} from the given {@link String} with the characters reversed.
   * @throws IllegalArgumentException if {@link String} is {@literal null} or {@literal empty}.
   */
  public static String reverse(@NotNull String value) {

    Assert.hasText(value, "String to reverse is required");

    StringBuilder reverse = new StringBuilder();

    for (int index = value.length(); --index > -1; ) {
      reverse.append(value.charAt(index));
    }

    return reverse.toString();
  }

  /**
   * Single spaces the elements in the {@link Object array} and converts all values into a {@link String} representation
   * using {@link Object#toString()} to be placed in a single {@link String}.
   *
   * @param values array of {@link Object values} to be converted and combined into a {@link String}.
   * @return a {@link String} containing all the {@link Object values} in {@link String} form single spaced.
   * @throws NullPointerException if the given {@link Object array} reference is {@literal null}.
   * @see java.lang.String#valueOf(Object)
   * @see #concat(String[], String)
   */
  public static String singleSpaceObjects(Object... values) {

    List<String> valueList = new ArrayList<>(values.length);

    for (Object value : values) {
      valueList.add(String.valueOf(value));
    }

    return trim(concat(valueList.toArray(new String[0]), SINGLE_SPACE));
  }

  /**
   * Single spaces the tokens in the given {@link String}.
   * <p>
   * A token is defined as any non-whitespace character.
   *
   * @param value {@link String value} from which the tokens will be single spaced.
   * @return a modified {@link String} where the tokens in the original, given {@link String}
   * have been single spaced.
   * @see #concat(String[], String)
   */
  public static String singleSpaceString(String value) {

    Assert.hasText(value, "String value must contain text");

    return trim(concat(value.split("\\s+"), SINGLE_SPACE));
  }

  /**
   * Null-safe implementation to convert the {@link String} into an array of characters.
   *
   * @param value {@link String value} to convert to an array of characters.
   * @return an array of characters for the given {@link String} or return an empty array of characters
   * if the {@link String} is {@literal null}.
   * @see java.lang.String#toCharArray()
   */
  @NullSafe
  public static char[] toCharArray(@Nullable String value) {
    return value != null ? value.toCharArray() : EMPTY_CHAR_ARRAY;
  }

  /**
   * Converts the given, required {@link CharacterIterator} into regular {@link Iterator}.
   *
   * @param characterIterator {@link CharacterIterator} to convert into a {@link Iterator};
   * must not be {@literal null}.
   * @return an {@link Iterator} backed by the given {@link CharacterIterator}.
   * @throws IllegalArgumentException if {@link CharacterIterator} is {@literal null}.
   * @see java.text.CharacterIterator
   * @see java.util.Iterator
   */
  public static @NotNull Iterator<Character> toIterator(@NotNull CharacterIterator characterIterator) {

    Assert.notNull(characterIterator, "CharacterIterator is required");

    return new Iterator<>() {

      int index;

      @Override
      public boolean hasNext() {
        return characterIterator.getIndex() + 1 < characterIterator.getEndIndex();
      }

      @Override
      public Character next() {

        char character = characterIterator.setIndex(this.index++);

        if (character == CharacterIterator.DONE) {
          throw newNoSuchElementException("No more characters available");
        }

        return character;
      }
    };
  }

  /**
   * Converts the {@link String} to all lowercase characters.
   * <p>
   * {@code toLowerCase} is a null-safe operation.
   *
   * @param value {@link String value} whose characters are all converted to lowercase.
   * @return the given {@link String} in all lowercase characters.
   * @see java.lang.String#toLowerCase()
   * @see #toUpperCase(String)
   */
  @NullSafe
  public static @Nullable String toLowerCase(@Nullable String value) {
    return value != null ? value.toLowerCase() : null;
  }

  /**
   * Tokenizes the given, delimited {@link String} into an array of individually trimmed {@link String Strings}.
   * <p>
   * If {@link String} is {@link String#isBlank() blank}, {@link String#isEmpty() empty} or {@literal null},
   * then a {@literal 0} length {@link String array} is returned. If the {@link String} is not delimited with
   * the specified delimiter then a {@link String array} of size {@literal 1} is returned with
   * the given {@link String} as the only element.
   *
   * @param commaDelimitedValue comma-delimited {@link String} to tokenize.
   * @return an array of individually tokenized and trimmed {@link String Strings} from the given {@link String}.
   * @see #toStringArray(String, String)
   */
  @NullSafe
  public static String[] toStringArray(String commaDelimitedValue) {
    return toStringArray(commaDelimitedValue, COMMA_DELIMITER);
  }

  /**
   * Tokenizes the given delimited {@link String} into an array of individually trimmed {@link String Strings}.
   * <p>
   * If {@link String} is {@link String#isBlank() blank}, {@link String#isEmpty() empty} or {@literal null},
   * then a {@literal 0} length {@link String array} is returned. If the {@link String} is not delimited with
   * the specified delimiter then a {@link String array} of size {@literal 1} is returned with
   * the given {@link String} as the only element.
   *
   * @param delimitedValue {@link String} to tokenize based on the delimiter.
   * @param delimiter the delimiter used to tokenize the {@link String}.
   * @return an array of individually tokenized and trimmed {@link String Strings} from the given {@link String}.
   * @see org.cp.elements.lang.ObjectUtils#returnFirstNonNullValue(Object[])
   * @see org.cp.elements.util.ArrayUtils#transform(Object[], Transformer)
   * @see java.lang.String#split(String)
   * @see #trim(String)
   */
  @NullSafe
  @SuppressWarnings("all")
  public static String[] toStringArray(String delimitedValue, String delimiter) {
    return ArrayUtils.transform(ObjectUtils.returnFirstNonNullValue(delimitedValue, EMPTY_STRING).split(
      defaultIfBlank(delimiter, COMMA_DELIMITER)), StringUtils::trim);
  }

  /**
   * Converts the {@link String} to all uppercase characters.
   * <p>
   * {@code toUpperCase} is a null-safe operation.
   *
   * @param value {@link String value} whose characters are all converted to uppercase.
   * @return the given {@link String} in all uppercase characters.
   * @see java.lang.String#toUpperCase()
   * @see #toLowerCase(String)
   */
  @NullSafe
  public static @Nullable String toUpperCase(@Nullable String value) {
    return value != null ? value.toUpperCase() : null;
  }

  /**
   * Trims the {@link String}, removing any and all whitespace from the beginning and end of the {@link String}.
   *
   * @param value {@link String} to trim.
   * @return a trimmed version of the given {@link String}.
   * @see java.lang.String#trim()
   */
  @NullSafe
  public static String trim(String value) {
    return value != null ? value.trim() : null;
  }

  /**
   * Trims all whitespace characters in the {@link String}.
   * <p>
   * The whitespace can occur in any position (beginning, end, or between characters) within the {@link String}.
   *
   * @param value {@link String} to trim.
   * @return a {@link String} containing no whitespace.
   * @see java.lang.Character#isWhitespace(char)
   */
  @NullSafe
  public static String trimAll(String value) {

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
   * Truncates the {@link String} to the desired {@link Integer length}.
   * <p>
   * If the {@link String} is {@link String#isBlank() blank}, {@link String#isEmpty() empty} or {@literal null},
   * then {@link String} is returned, otherwise the {@link String} is truncated to the maximum {@link Integer length}
   * determined by the value's {@link String#length() length} and desired {@link Integer length}.
   *
   * @param value {@link String} to truncate.
   * @param length {@link Integer} specifying the length to truncate the {@link String} to.
   * @return the given {@link String} truncated to {@link Integer length}, or the entire {@link String}
   * if the desired {@link Integer length} exceeds the {@link String#length() String's length}.
   * @throws IllegalArgumentException if {@link Integer length} is less than {@literal 0}.
   * @see java.lang.String#substring(int, int)
   * @see java.lang.String#length()
   */
  @NullSafe
  public static String truncate(String value, int length) {

    assertThat(length).throwing(new IllegalArgumentException(String.format(
      "[%d] must be greater than equal to 0", length))).isGreaterThanEqualTo(0);

    return (value != null ? value.substring(0, Math.min(value.length(), length)) : null);
  }

  /**
   * Wraps a line of {@link String text} to no longer than the specified {@link Integer width}, measured by
   * the number of characters in each {@link String line}, indenting all subsequent lines with an {@link String indent}.
   * <p>
   * If the {@link String indent} is {@literal null}, then an empty {@link String} is used.
   *
   * @param line {@link String} containing the line of text to wrap.
   * @param widthInCharacters {@link Integer} indicating the width of each line measured by the number of characters.
   * @param indent {@link String} used to indent all subsequent lines.
   * @return the {@link String line of text} wrapped.
   * @throws IndexOutOfBoundsException if given {@code widthInCharacters} is less than {@literal 0},
   * or there are no word boundaries within the given width on any given split.
   * @throws NullPointerException if the given {@link String line of text} is {@literal null}.
   */
  @SuppressWarnings("all")
  public static String wrap(String line, int widthInCharacters, String indent) {

    StringBuilder buffer = new StringBuilder();

    int lineCount = 1;
    int spaceIndex;

    // if indent is null, then do not indent the wrapped lines
    String resolvedIndent = indent != null ? indent : EMPTY_STRING;
    String lineToProcess = line;

    while (lineToProcess.length() > widthInCharacters) {
      spaceIndex = lineToProcess.substring(0, widthInCharacters).lastIndexOf(SINGLE_SPACE);
      buffer.append(lineCount++ > 1 ? resolvedIndent : EMPTY_STRING);
      // throws IndexOutOfBoundsException if spaceIndex is -1, implying no word boundary was found within
      // the given width; this also avoids the infinite loop
      buffer.append(lineToProcess.substring(0, spaceIndex));
      buffer.append(LINE_SEPARATOR);
      // possible infinite loop if spaceIndex is -1, see comment above
      lineToProcess = lineToProcess.substring(spaceIndex + 1);
    }

    buffer.append(lineCount > 1 ? resolvedIndent : EMPTY_STRING);
    buffer.append(lineToProcess);

    return buffer.toString();
  }
}
