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

import java.math.BigInteger;
import java.util.regex.Pattern;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * The {@link NumberUtils} class is an abstract utility class encapsulating common functionality
 * for working with {@link Number Numbers}.
 *
 * @author John J. Blum
 * @see java.lang.Number
 * @see java.math.BigDecimal
 * @see java.math.BigInteger
 * @see java.util.regex.Pattern
 * @since 1.0.0
 */
public abstract class NumberUtils {

  public static final String BINARY_PREFIX_NOTATION = "b";
  public static final String HEXADECIMAL_PREFIX_NOTATION = "0x";

  protected static final Pattern binaryPattern = Pattern.compile("b?[01]+");
  protected static final Pattern hexadecimalPattern = Pattern.compile("0x[0-9A-Fa-f]+");

  /**
   * Determines whether the given {@link String value} represents a binary value, consisting of
   * only {@literal 1s} and {@literal 0s}.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating whether the given {@link String value} represents a binary value,
   * consisting of only {@literal 1s} and {@literal 0s}.
   */
  public static boolean isBinaryString(@Nullable String value) {
    return StringUtils.hasText(value) && binaryPattern.matcher(value).matches();
  }

  /**
   * Converts the given, required {@link String binary value} into an {@link Integer}.
   *
   * @param binaryValue {@link String} containing only 1s and 0s.
   * @return an {@link Integer} representing the {@link String binary value}.
   * @throws IllegalArgumentException if the {@link String binary value} is {@literal null} or {@literal empty},
   * or the value is not a valid {@link String binary string}.
   * @see #isBinaryString(String)
   */
  public static @NotNull Integer fromBinaryString(@NotNull String binaryValue) {

    Assert.hasText(binaryValue, "Binary String [%s] is required", binaryValue);

    Assert.argument(binaryValue, NumberUtils::isBinaryString,
      "Binary String [%s] must contain only 1s and 0s", binaryValue);

    String resolvedBinaryValue = stripBinaryNumberPrefixNotation(binaryValue);

    int result = 0;
    int pow = resolvedBinaryValue.length() - 1;

    for (char digit : resolvedBinaryValue.toCharArray()) {
      result += parseInt(digit) * (int) Math.pow(2.0d, pow--);
    }

    return result;
  }

  /**
   * Determines whether the given {@link String value} is a hexadecimal value, consisting of
   * only numbers {@literal [0-9]} and letters {@literal [AaBbCcDdEeFf]}.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating whether the given {@link String value} is a hexadecimal value, consisting of
   * only numbers {@literal [0-9]} and letters {@literal [AaBbCcDdEeFf]}.
   */
  public static boolean isHexadecimalString(@Nullable String value) {
    return StringUtils.hasText(value) && hexadecimalPattern.matcher(value).matches();
  }

  /**
   * Converts the given, required {@link String hexadecimal value} into an {@link Integer}.
   *
   * @param hexadecimalValue {@link String} containing digits 0-9 and letters A-F.
   * @return an {@link Integer} representing the {@link String hexadecimal value}.
   * @throws IllegalArgumentException if the {@link String hexadecimal value} is {@literal null} or {@literal empty},
   * or the value is not a valid {@link String hexadecimal string}.
   * @see #isHexadecimalString(String)
   */
  public static @NotNull Integer fromHexadecimalString(@NotNull String hexadecimalValue) {

    Assert.hasText(hexadecimalValue, "Hexadecimal String [%s] is required", hexadecimalValue);

    Assert.argument(hexadecimalValue, NumberUtils::isHexadecimalString,
      "Hexadecimal String [%s] must contain only digits [0-9] and letters [A-F]", hexadecimalValue);

    String resolvedHexadecimalValue = stripHexadecimalNumberPrefixNotation(hexadecimalValue);

    int result = 0;
    int pow = resolvedHexadecimalValue.length() - 1;

    for (char digit : resolvedHexadecimalValue.toCharArray()) {
      result += parseInt(digit) * Math.pow(16.0d, pow--);
    }

    return result;
  }

  private static int parseInt(char digit) {

    switch (digit) {
      case'A':
      case 'a':
        return 10;
      case 'B':
      case 'b':
        return 11;
      case 'C':
      case 'c':
        return 12;
      case 'D':
      case 'd':
        return 13;
      case 'E':
      case 'e':
        return 14;
      case 'F':
      case 'f':
        return 15;
      default:
        return Integer.parseInt(String.valueOf(digit));
    }
  }

  private static @NotNull String stripBinaryNumberPrefixNotation(@NotNull String value) {
    return stripNumberPrefixNotation(value, BINARY_PREFIX_NOTATION);
  }

  private static @NotNull String stripHexadecimalNumberPrefixNotation(@NotNull String value) {
    return stripNumberPrefixNotation(value, HEXADECIMAL_PREFIX_NOTATION);
  }

  private static @NotNull String stripNumberPrefixNotation(@NotNull String value, @NotNull String prefix) {
    return value.startsWith(prefix) ? value.substring(prefix.length()) : value;
  }

  /**
   * Gets the individual bytes of an integer (int) value.  An integer is a 32-bit value consisting of 4 bytes
   * where each byte of the integer (int) value is returned in an element of a 4-element byte array.
   *
   * @param value the integer (int) value to convert into a byte array consisting of the int value's 4 bytes
   * (or 32-bit value).
   * @return a byte array containing the individual bytes making up the value of the integer (int).
   */
  public static byte[] getBytes(int value) {

    byte[] valueBytes = new byte[4];

    valueBytes[0] = (byte) (value >>> 24 & 0xFF);
    valueBytes[1] = (byte) (value >>> 16 & 0xFF);
    valueBytes[2] = (byte) (value >>> 8 & 0xFF);
    valueBytes[3] = (byte) (value & 0xFF);

    return valueBytes;
  }

  /**
   * Determines whether the specified double is a floating-point number, which is defined as a double value having a
   * fractional value (a non-zero value after the decimal point).
   *
   * @param value the double value being evaluated as a decimal value (floating-point value).
   * @return a boolean value indicating whether the specified double value is a floating-point number.
   * @see #isWhole(double)
   */
  public static boolean isDecimal(double value) {
    return Math.floor(value) != value;
  }

  /**
   * Determines whether the given {@link Number} is a decimal value.
   *
   * The {@link Number} is considered a decimal value if it is instance of {@link Double} or {@link Float}.
   *
   * @param value {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a decimal value.
   * @see java.lang.Double
   * @see java.lang.Float
   * @see java.lang.Number
   */
  @NullSafe
  public static boolean isDecimal(@Nullable Number value) {
    return value instanceof Float || value instanceof Double;
  }

  /**
   * Determines whether the specified long value is an even number using modulus, which is any number divisible by 2
   * with a remainder of 0.
   *
   * @param value the long value being evaluated as an even number.
   * @return a boolean value indicating whether the specified long value is even.
   * @see #isBitwiseEven(long)
   * @see #isOdd(long)
   */
  public static boolean isEven(long value) {
    return Math.abs(value) % 2L == 0;
  }

  /**
   * Determines whether the specified long value is an even number using bitwise 'AND', which is any number
   * with a 0 in the 0 position of the binary representation of the number.
   *
   * @param value the long value being evaluated as an even number.
   * @return a boolean value indicating whether the specified long value is even.
   * @see #isEven(long)
   */
  public static boolean isBitwiseEven(long value) {
    return (1L & Math.abs(value)) == 0L;
  }

  /**
   * Determines whether the specified double value is negative (less than 0).
   *
   * @param value a double who's value is evaluated as a negative value (less than 0).
   * @return a boolean value indicating whether the specified double value is negative (less than 0).
   * @see #isPositive(double)
   */
  public static boolean isNegative(double value) {
    return value < 0.0d;
  }

  /**
   * Determines whether the specified long value is an odd number using modulus, which is any number
   * having a remainder of 1 when divided by 2.
   *
   * @param value the long value being evaluated as an odd number.
   * @return a boolean value indicating whether the specified long value is odd.
   * @see #isBitwiseOdd(long)
   * @see #isEven(long)
   */
  public static boolean isOdd(long value) {
    return (Math.abs(value) % 2L == 1);
  }

  /**
   * Determines whether the specified long value is an odd number using bitwise 'AND', which is any number
   * with a 1 in the 0 position of the binary representation of the number.
   *
   * @param value the long value being evaluated as an odd number.
   * @return a boolean value indicating whether the specified long value is odd.
   * @see #isOdd(long)
   */
  public static boolean isBitwiseOdd(long value) {
    return (1L & Math.abs(value)) == 1L;
  }

  /**
   * Determines whether the specified double value is positive (greater than 0).
   *
   * @param value a double who's value is evaluated as a positive value (greater than 0).
   * @return a boolean value indicating whether the specified double value is positive (greater than 0).
   * @see #isNegative(double)
   */
  public static boolean isPositive(double value) {
    return value > 0.0d;
  }

  /**
   * Determines whether the specified double is a whole number, which is defined as a double value having no
   * fractional value (containing only zeroes after the decimal point).
   *
   * @param value the double value being evaluated as a whole value (integral value).
   * @return a boolean value indicating whether the specified double value is a whole number.
   * @see #isDecimal(double)
   */
  public static boolean isWhole(double value) {
    return Math.floor(value) == value;
  }

  /**
   * Determines whether the given {@link Number} is {@literal whole}.
   *
   * @param value {@link Number} to evaluate.
   * @return a boolean value indicating whether the the given {@link Number} is {@literal whole}.
   * @see java.lang.Number
   */
  @NullSafe
  public static boolean isWhole(@Nullable Number value) {

    return value instanceof Byte
      || value instanceof Short
      || value instanceof Integer
      || value instanceof Long
      || value instanceof BigInteger;
  }

  /**
   * Determines whether the given double value is zero.
   *
   * @param value double value to evaluate.
   * @return a boolean value indicating whether the given double value is zero.
   */
  public static boolean isZero(double value) {
    return value == 0.0d;
  }

  /**
   * Determines whether the given {@link Number} is a {@link Byte}.
   *
   * If the {@link Number} is not an instance of {@link Byte} then the numerical value must be between
   * {@link Byte#MIN_VALUE} and {@link Byte#MAX_VALUE} inclusive.
   *
   * @param value {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@link Byte}.
   * @see #isWhole(Number)
   * @see java.lang.Number
   * @see java.lang.Byte
   */
  @NullSafe
  public static boolean isByte(@Nullable Number value) {
    return value instanceof Byte
      || (isWhole(value) && value.longValue() >= Byte.MIN_VALUE && value.longValue() <= Byte.MAX_VALUE);
  }

  /**
   * Determines whether the given {@link Number} is a {@link Short}.
   *
   * If the {@link Number} is not an instance of {@link Short} then the numerical value must be between
   * {@link Short#MIN_VALUE} and {@link Short#MAX_VALUE} inclusive.
   *
   * @param value {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@link Short}.
   * @see #isWhole(Number)
   * @see java.lang.Number
   * @see java.lang.Short
   */
  @NullSafe
  public static boolean isShort(@Nullable Number value) {
    return value instanceof Short
      || (isWhole(value) && value.longValue() >= Short.MIN_VALUE && value.longValue() <= Short.MAX_VALUE);
  }

  /**
   * Determines whether the given {@link Number} is a {@link Integer}.
   *
   * If the {@link Number} is not an instance of {@link Integer} then the numerical value must be between
   * {@link Integer#MIN_VALUE} and {@link Integer#MAX_VALUE} inclusive.
   *
   * @param value {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@link Integer}.
   * @see #isWhole(Number)
   * @see java.lang.Number
   * @see java.lang.Integer
   */
  @NullSafe
  public static boolean isInteger(@Nullable Number value) {
    return value instanceof Integer
      || (isWhole(value) && value.longValue() >= Integer.MIN_VALUE && value.longValue() <= Integer.MAX_VALUE);
  }

  /**
   * Determines whether the given {@link Number} is a {@link Long}.
   *
   * The {@link Number} must be an instance of {@link Long}.
   *
   * @param value {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@link Long}.
   * @see java.lang.Number
   * @see java.lang.Long
   */
  @NullSafe
  public static boolean isLong(@Nullable Number value) {
    return value instanceof Long;
  }

  /**
   * Determines whether the given {@link Number} is a {@link Float}.
   *
   * If the {@link Number} is not an instance of {@link Float} then the numerical value must be between
   * {@link Float#MIN_VALUE} and {@link Float#MAX_VALUE} inclusive.
   *
   * @param value {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@link Float}.
   * @see #isDecimal(Number)
   * @see java.lang.Number
   * @see java.lang.Float
   */
  @NullSafe
  public static boolean isFloat(@Nullable Number value) {
    return value instanceof Float
      || (isDecimal(value) && value.doubleValue() >= Float.MIN_VALUE && value.doubleValue() <= Float.MAX_VALUE);
  }

  /**
   * Determines whether the given {@link Number} is a {@link Double}.
   *
   * The {@link Number} must be an instance of {@link Double}.
   *
   * @param value {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@link Double}.
   * @see java.lang.Number
   * @see java.lang.Double
   */
  @NullSafe
  public static boolean isDouble(@Nullable Number value) {
    return value instanceof Double;
  }

  /**
   * Returns the byte value of the given {@link Number} or 0 if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the byte value of the given {@link Number} or 0 if the {@link Number} is {@literal null}.
   * @see java.lang.Number#byteValue()
   */
  @NullSafe
  public static byte byteValue(@Nullable Number number) {
    return number != null ? number.byteValue() : 0;
  }

  /**
   * Returns the short value of the given {@link Number} or 0 if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the short value of the given {@link Number} or 0 if the {@link Number} is {@literal null}.
   * @see java.lang.Number#shortValue()
   */
  @NullSafe
  public static short shortValue(@Nullable Number number) {
    return number != null ? number.shortValue() : 0;
  }

  /**
   * Returns the int value of the given {@link Number} or 0 if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the int value of the given {@link Number} or 0 if the {@link Number} is {@literal null}.
   * @see java.lang.Number#intValue()
   */
  @NullSafe
  public static int intValue(@Nullable Number number) {
    return number != null ? number.intValue() : 0;
  }

  /**
   * Returns the long value of the given {@link Number} or 0L if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the long value of the given {@link Number} or 0L if the {@link Number} is {@literal null}.
   * @see java.lang.Number#longValue()
   */
  @NullSafe
  public static long longValue(@Nullable Number number) {
    return number != null ? number.longValue() : 0L;
  }

  /**
   * Returns the float value of the given {@link Number} or 0.0f if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the float value of the given {@link Number} or 0.0f if the {@link Number} is {@literal null}.
   * @see java.lang.Number#floatValue()
   */
  @NullSafe
  public static float floatValue(@Nullable Number number) {
    return number != null ? number.floatValue() : 0.0f;
  }

  /**
   * Returns the double value of the given {@link Number} or 0.0d if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the double value of the given {@link Number} or 0.0d if the {@link Number} is {@literal null}.
   * @see java.lang.Number#doubleValue()
   */
  @NullSafe
  public static double doubleValue(@Nullable Number number) {
    return number != null ? number.doubleValue() : 0.0d;
  }

  /**
   * Return the primitive byte value of the specified {@link Byte} wrapper object, or 0
   * if the {@link Byte} wrapper object is {@literal null}.
   *
   * @param value {@link Byte} to evaluate.
   * @return a primitive byte value for the specified {@link Byte}, or 0 if the {@link Byte} is {@literal null}.
   * @see #byteValue(Number)
   * @see java.lang.Byte
   */
  @NullSafe
  public static byte valueOf(@Nullable Byte value) {
    return byteValue(value);
  }

  /**
   * Return the primitive short value of the specified {@link Short} wrapper object, or 0
   * if the {@link Short} wrapper object is {@literal null}.
   *
   * @param value {@link Short} to evaluate.
   * @return a primitive short value for the specified {@link Short}, or 0 if the {@link Short} is {@literal null}.
   * @see #shortValue(Number)
   * @see java.lang.Short
   */
  @NullSafe
  public static short valueOf(@Nullable Short value) {
    return shortValue(value);
  }

  /**
   * Return the primitive int value of the specified {@link Integer} wrapper object, or 0
   * if the {@link Integer} wrapper object is {@literal null}.
   *
   * @param value {@link Integer} to evaluate.
   * @return a primitive int value for the specified {@link Integer}, or 0 if the {@link Integer} is {@literal null}.
   * @see #intValue(Number)
   * @see java.lang.Integer
   */
  @NullSafe
  public static int valueOf(@Nullable Integer value) {
    return intValue(value);
  }

  /**
   * Return the primitive long value of the specified {@link Long} wrapper object, or 0
   * if the {@link Long} wrapper object is {@literal null}.
   *
   * @param value {@link Long} to evaluate.
   * @return a primitive long value for the specified {@link Long}, or 0 if the {@link Long} is {@literal null}.
   * @see #longValue(Number)
   * @see java.lang.Long
   */
  @NullSafe
  public static long valueOf(@Nullable Long value) {
    return longValue(value);
  }

  /**
   * Return the primitive float value of the specified {@link Float} wrapper object, or 0
   * if the {@link Float} wrapper object is {@literal null}.
   *
   * @param value {@link Float} to evaluate.
   * @return a primitive float value for the specified {@link Float}, or 0 if the {@link Float} is {@literal null}.
   * @see #floatValue(Number)
   * @see java.lang.Float
   */
  @NullSafe
  public static float valueOf(@Nullable Float value) {
    return floatValue(value);
  }

  /**
   * Return the primitive double value of the specified {@link Double} wrapper object, or 0
   * if the {@link Double} wrapper object is {@literal null}.
   *
   * @param value {@link Double} to evaluate.
   * @return a primitive double value for the specified {@link Double}, or 0 if the {@link Double} is {@literal null}.
   * @see #doubleValue(Number)
   * @see java.lang.Double
   */
  @NullSafe
  public static double valueOf(@Nullable Double value) {
    return doubleValue(value);
  }
}
