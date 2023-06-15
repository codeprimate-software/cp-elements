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
import java.util.function.Predicate;
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

  private static final int BIT_MASK = 0xFF;

  public static final String BINARY_PREFIX_NOTATION = "b";
  public static final String HEXADECIMAL_PREFIX_NOTATION = "0x";

  protected static final Pattern BINARY_PATTERN = Pattern.compile("b?[01]+");
  protected static final Pattern HEXADECIMAL_PATTERN = Pattern.compile("0x[0-9A-Fa-f]+");

  private static final Predicate<Number> IS_DECIMAL_NUMBER = NumberUtils::isDecimal;
  private static final Predicate<Number> IS_WHOLE_NUMBER = NumberUtils::isWhole;

  private static final Predicate<Number> IS_BYTE_SIZE = IS_WHOLE_NUMBER.and(number ->
    number.longValue() >= Byte.MIN_VALUE && number.longValue() <= Byte.MAX_VALUE);

  private static final Predicate<Number> IS_SHORT_SIZE = IS_WHOLE_NUMBER.and(number ->
    number.longValue() >= Short.MIN_VALUE && number.longValue() <= Short.MAX_VALUE);

  private static final Predicate<Number> IS_INT_SIZE = IS_WHOLE_NUMBER.and(number ->
    number.longValue() >= Integer.MIN_VALUE && number.longValue() <= Integer.MAX_VALUE);

  private static final Predicate<Number> IS_FLOAT_SIZE = IS_DECIMAL_NUMBER.and(number ->
    number.doubleValue() >= Float.MIN_VALUE && number.doubleValue() <= Float.MAX_VALUE);

  /**
   * Determines whether the given {@link String value} represents a {@literal binary value},
   * consisting of only {@literal 1s} and {@literal 0s}.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating whether the given {@link String value} represents a {@literal binary value},
   * consisting of only {@literal 1s} and {@literal 0s}.
   * @see #isHexadecimalString(String)
   */
  public static boolean isBinaryString(@Nullable String value) {
    return StringUtils.hasText(value) && BINARY_PATTERN.matcher(value).matches();
  }

  /**
   * Converts the given, required {@link String binary string} into an {@link Integer value}.
   *
   * @param binaryValue {@link String} containing a {@literal binary value} consisting of
   * {@literal 1s} and {@literal 0s}; must not be {@literal null} or {@literal empty}.
   * @return an {@link Integer value} equivalent to the {@link String binary string}.
   * @throws IllegalArgumentException if the {@link String binary string} is {@literal null} or {@literal empty},
   * or the {@literal value} of the {@link String binary string} is not valid.
   * @see #fromHexadecimalString(String)
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
   * Determines whether the given {@link String value} is a {@literal hexadecimal value},
   * consisting of only numbers {@literal [0-9]} and letters {@literal [AaBbCcDdEeFf]}.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating whether the given {@link String value} is a {@literal hexadecimal value},
   * consisting of only numbers {@literal [0-9]} and letters {@literal [AaBbCcDdEeFf]}.
   * @see #isBinaryString(String)
   */
  public static boolean isHexadecimalString(@Nullable String value) {
    return StringUtils.hasText(value) && HEXADECIMAL_PATTERN.matcher(value).matches();
  }

  /**
   * Converts the given, required {@link String hexadecimal string} into an {@link Integer value}.
   *
   * @param hexadecimalValue {@link String} containing a {@literal hexadecimal value} consisting of
   * digits {@literal 0-9} and letters {@literal A-F}; must not be {@literal null} or {@literal empty}.
   * @return an {@link Integer value} equivalent to the {@link String hexadecimal string}.
   * @throws IllegalArgumentException if the {@link String hexadecimal string} is {@literal null} or {@literal empty},
   * or the {@literal value} of the {@link String hexadecimal string} is not valid.
   * @see #isHexadecimalString(String)
   * @see #fromBinaryString(String)
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

    return switch (digit) {
      case 'A', 'a' -> 10;
      case 'B', 'b' -> 11;
      case 'C', 'c' -> 12;
      case 'D', 'd' -> 13;
      case 'E', 'e' -> 14;
      case 'F', 'f' -> 15;
      default -> Integer.parseInt(String.valueOf(digit));
    };
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
   * Gets the individual bytes of an {@link Integer value} or {@link Integer#TYPE int value}.
   *
   * An {@link Integer} is a 32-bit value consisting of 4 bytes where each byte of the {@link Integer}
   * is returned in an element of a 4-element byte array.
   *
   * @param value {@link Integer} to convert into a byte array consisting of the {@link Integer Integer's}
   * 4 bytes, or 32-bit value.
   * @return a byte array containing the individual bytes making up the value of
   * the {@link Integer} of {@link Integer#TYPE int}.
   */
  public static byte[] getBytes(int value) {

    byte[] valueBytes = new byte[4];

    valueBytes[0] = (byte) (value >>> 24 & BIT_MASK);
    valueBytes[1] = (byte) (value >>> 16 & BIT_MASK);
    valueBytes[2] = (byte) (value >>> 8 & BIT_MASK);
    valueBytes[3] = (byte) (value & BIT_MASK);

    return valueBytes;
  }

  /**
   * Determines whether the given {@link Double value} is a {@literal floating-point number}, which is defined as
   * any number having a fractional value, or non-zero value after the decimal point.
   *
   * @param value {@link Double value} being evaluated as a {@literal decimal}, or {@literal floating-point number}.
   * @return a boolean value indicating whether the given {@link Double value} is a {@literal floating-point number}.
   * @see java.lang.Math#floor(double)
   * @see #isWhole(double)
   */
  public static boolean isDecimal(double value) {
    return Math.floor(value) != value;
  }

  /**
   * Determines whether the given {@link Number} is a {@literal decimal} or {@literal floating-point number}.
   *
   * The {@link Number} is considered a {@literal floating-point value} only if the {@link Number}
   * is an instance of {@link Double} or {@link Float}.
   *
   * @param value {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@literal decimal value}
   * or a {@literal floating-point number}
   * @see java.lang.Double
   * @see java.lang.Float
   * @see java.lang.Number
   */
  @NullSafe
  public static boolean isDecimal(@Nullable Number value) {
    return value instanceof Float || value instanceof Double;
  }

  /**
   * Determines whether the given {@link Long} is an even number.
   *
   * The {@link Long number} is evaluated using the {@literal modulus operator}, which determines that any number
   * divisible by {@literal 2} with a remainder of {@literal 0} is an even number.
   *
   * @param number {@link Long value} to evaluate as an even number.
   * @return a boolean value indicating whether the given {@link Long} is an even number.
   * @see java.lang.Math#abs(long)
   * @see #isBitwiseEven(long)
   * @see #isOdd(long)
   */
  public static boolean isEven(long number) {
    return Math.abs(number) % 2L == 0;
  }

  /**
   * Determines whether the specified {@link Long} is an even number.
   *
   * The {@link Long number} is evaluated using the {@literal bitwise AND operator}, which determines that any number
   * with a {@literal 0} in the {@literal 0 position} of the binary representation of the number is an even number.
   *
   * @param value {@link Long value} to evaluate as an even number.
   * @return a boolean value indicating whether the given {@link Long} is an even number.
   * @see java.lang.Math#abs(double)
   * @see #isBitwiseOdd(long)
   * @see #isEven(long)
   */
  public static boolean isBitwiseEven(long value) {
    return (1L & Math.abs(value)) == 0L;
  }

  /**
   * Determines whether the given {@link Double value} is a negative number, or less than {@literal 0}.
   *
   * @param value {@link Double number} to evaluate as a negative value.
   * @return a boolean value indicating whether the given {@link Double value} is a negative number.
   * @see #isPositive(double)
   */
  public static boolean isNegative(double value) {
    return value < 0.0d;
  }

  /**
   * Determines whether the given {@link Long} is an odd number.
   *
   * The {@link Long number} is evaluated using the {@literal modulus operator}, which determines that any number
   * divisible by {@literal 2} with a remainder of {@literal 1} is an odd number.
   *
   * @param number {@link Long value} to evaluate as an odd number.
   * @return a boolean value indicating whether the given {@link Long} is an odd number.
   * @see java.lang.Math#abs(long)
   * @see #isBitwiseOdd(long)
   * @see #isEven(long)
   */
  public static boolean isOdd(long number) {
    return Math.abs(number) % 2L == 1;
  }

  /**
   * Determines whether the specified {@link Long} is an odd number.
   *
   * The {@link Long number} is evaluated using the {@literal bitwise AND operator}, which determines that any number
   * with a {@literal 1} in the {@literal 0 position} of the binary representation of the number is an odd number.
   *
   * @param value {@link Long value} to evaluate as an odd number.
   * @return a boolean value indicating whether the given {@link Long} is an odd number.
   * @see java.lang.Math#abs(double)
   * @see #isBitwiseEven(long)
   * @see #isOdd(long)
   */
  public static boolean isBitwiseOdd(long value) {
    return (1L & Math.abs(value)) == 1L;
  }

  /**
   * Determines whether the given {@link Double value} is a positive number, or greater than {@literal 0}.
   *
   * @param value {@link Double number} to evaluate as a positive value.
   * @return a boolean value indicating whether the given {@link Double value} is a positive number.
   * @see #isNegative(double)
   */
  public static boolean isPositive(double value) {
    return value > 0.0d;
  }

  /**
   * Determines whether a given {@link Integer number} is a {@literal prime number}.
   *
   * @param number {@link Integer number} to evaluate as prime.
   * @return a {@literal true} if the given {@link Integer} is {@literal prime}, otherwise return {@literal false}.
   * @see <a href="https://en.wikipedia.org/wiki/Primality_test">Primality test</a>
   */
  public static boolean isPrime(int number) {

    if (number <= 1) {
      return false;
    }

    if (number == 2 || number == 3) {
      return true;
    }

    if (number % 2 == 0 || number % 3 == 0) {
      return false;
    }

    for (int num = 5; num <= Math.sqrt(number); num += 6) {
      if (number % num == 0 || number % (num + 2) == 0) {
        return false;
      }
    }

    return true;
  }

  /**
   * Determines whether the given {@link Double value} is a {@literal whole number}, which is defined as
   * any {@link Double value} having no fractional value, or containing only zeroes after the decimal point.
   *
   * @param value {@link Double value} being evaluated as a {@literal whole number}, or an {@literal integral value}.
   * @return a boolean value indicating whether the given {@link Double value} is a {@literal whole number}.
   * @see java.lang.Math#floor(double)
   * @see #isDecimal(double)
   */
  public static boolean isWhole(double value) {
    return Math.floor(value) == value;
  }

  /**
   * Determines whether the given {@link Number} is {@literal whole}.
   *
   * @param value {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is {@literal whole}.
   * @see #isDecimal(Number)
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
   * Determines whether the given {@link Double value} is zero.
   *
   * @param value {@link Double value} to evaluate.
   * @return a boolean value indicating whether the given {@link Double value} is zero.
   */
  public static boolean isZero(double value) {
    return value == 0.0d;
  }

  /**
   * Determines whether the given {@link Number} is a {@link Byte}.
   *
   * If the given {@link Number} is not an instance of {@link Byte} then the numerical value
   * must be between {@link Byte#MIN_VALUE} and {@link Byte#MAX_VALUE} inclusive.
   *
   * If the given {@link Number} is a {@literal floating-point value}, then the {@link Number}
   * must be {@link #isWhole(Number) whole}.
   *
   * @param number {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@link Byte}.
   * @see java.lang.Number
   * @see java.lang.Byte
   */
  @NullSafe
  public static boolean isByte(@Nullable Number number) {
    return number instanceof Byte || IS_BYTE_SIZE.test(number);
  }

  /**
   * Determines whether the given {@link Number} is a {@link Short}.
   *
   * If the given {@link Number} is not an instance of {@link Short} then the numerical value
   * must be between {@link Short#MIN_VALUE} and {@link Short#MAX_VALUE} inclusive.
   *
   * If the given {@link Number} is a {@literal floating-point value}, then the {@link Number}
   * must be {@link #isWhole(Number) whole}.
   *
   * @param number {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@link Short}.
   * @see java.lang.Number
   * @see java.lang.Short
   */
  @NullSafe
  public static boolean isShort(@Nullable Number number) {
    return number instanceof Short || IS_SHORT_SIZE.test(number);
  }

  /**
   * Determines whether the given {@link Number} is a {@link Integer}.
   *
   * If the given {@link Number} is not an instance of {@link Integer} then the numerical value
   * must be between {@link Integer#MIN_VALUE} and {@link Integer#MAX_VALUE} inclusive.
   *
   * If the given {@link Number} is a {@literal floating-point value}, then the {@link Number}
   * must be {@link #isWhole(Number) whole}.
   *
   * @param number {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@link Integer}.
   * @see java.lang.Number
   * @see java.lang.Integer
   */
  @NullSafe
  public static boolean isInteger(@Nullable Number number) {
    return number instanceof Integer || IS_INT_SIZE.test(number);
  }

  /**
   * Determines whether the given {@link Number} is a {@link Long}.
   *
   * The {@link Number} must be an instance of {@link Long}.
   *
   * If the given {@link Number} is a {@literal floating-point value}, then the {@link Number}
   * must be {@link #isWhole(Number)} whole}.
   *
   * @param number {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@link Long}.
   * @see java.lang.Number
   * @see java.lang.Long
   */
  @NullSafe
  public static boolean isLong(@Nullable Number number) {
    return number instanceof Long;
  }

  /**
   * Determines whether the given {@link Number} is a {@link Float}.
   *
   * If the given {@link Number} is not an instance of {@link Float} then the numerical value
   * must be between {@link Float#MIN_VALUE} and {@link Float#MAX_VALUE} inclusive.
   *
   * The given {@link Number} must contain a {@link #isDecimal(Number) decimal value}.
   *
   * @param number {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@link Float}.
   * @see java.lang.Number
   * @see java.lang.Float
   */
  @NullSafe
  public static boolean isFloat(@Nullable Number number) {
    return number instanceof Float || IS_FLOAT_SIZE.test(number);
  }

  /**
   * Determines whether the given {@link Number} is a {@link Double}.
   *
   * The {@link Number} must be an instance of {@link Double}
   * and contain a {@link #isDecimal(Number) decimal value}.
   *
   * @param number {@link Number} to evaluate.
   * @return a boolean value indicating whether the given {@link Number} is a {@link Double}.
   * @see java.lang.Number
   * @see java.lang.Double
   */
  @NullSafe
  public static boolean isDouble(@Nullable Number number) {
    return number instanceof Double;
  }

  /**
   * Returns the value of the given {@link Number} as a {@link Byte} or {@literal 0}
   * if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the {@link Byte value} of the given {@link Number} or {@literal 0}
   * if the {@link Number} is {@literal null}.
   * @see java.lang.Number#byteValue()
   * @see java.lang.Byte#TYPE
   */
  @NullSafe
  public static byte byteValue(@Nullable Number number) {
    return number != null ? number.byteValue() : 0;
  }

  /**
   * Returns the value of the given {@link Number} as a {@link Short} or {@literal 0}
   * if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the {@link Short value} of the given {@link Number} or {@literal 0}
   * if the {@link Number} is {@literal null}.
   * @see java.lang.Number#shortValue()
   * @see java.lang.Short#TYPE
   */
  @NullSafe
  public static short shortValue(@Nullable Number number) {
    return number != null ? number.shortValue() : 0;
  }

  /**
   * Returns the value of the given {@link Number} as a {@link Integer} or {@literal 0}
   * if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the {@link Integer value} of the given {@link Number} or {@literal 0}
   * if the {@link Number} is {@literal null}.
   * @see java.lang.Number#intValue()
   * @see java.lang.Integer#TYPE
   */
  @NullSafe
  public static int intValue(@Nullable Number number) {
    return number != null ? number.intValue() : 0;
  }

  /**
   * Returns the value of the given {@link Number} as a {@link Long} or {@literal 0}
   * if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the {@link Long value} of the given {@link Number} or {@literal 0}
   * if the {@link Number} is {@literal null}.
   * @see java.lang.Number#longValue()
   * @see java.lang.Long#TYPE
   */
  @NullSafe
  public static long longValue(@Nullable Number number) {
    return number != null ? number.longValue() : 0L;
  }

  /**
   * Returns the value of the given {@link Number} as a {@link Float} or {@literal 0}
   * if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the {@link Float value} of the given {@link Number} or {@literal 0}
   * if the {@link Number} is {@literal null}.
   * @see java.lang.Number#floatValue()
   * @see java.lang.Float#TYPE
   */
  @NullSafe
  public static float floatValue(@Nullable Number number) {
    return number != null ? number.floatValue() : 0.0f;
  }

  /**
   * Returns the value of the given {@link Number} as a {@link Double} or {@literal 0}
   * if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the {@link Double value} of the given {@link Number} or {@literal 0}
   * if the {@link Number} is {@literal null}.
   * @see java.lang.Number#doubleValue()
   * @see java.lang.Double#TYPE
   */
  @NullSafe
  public static double doubleValue(@Nullable Number number) {
    return number != null ? number.doubleValue() : 0.0d;
  }

  /**
   * Return the primitive byte value of the given {@link Byte} wrapper object or {@literal 0}
   * if the {@link Byte} wrapper object is {@literal null}.
   *
   * @param value {@link Byte} to evaluate.
   * @return a primitive byte value for the given {@link Byte} or {@literal 0}
   * if the {@link Byte} is {@literal null}.
   * @see #byteValue(Number)
   * @see java.lang.Byte#TYPE
   * @see java.lang.Byte
   */
  @NullSafe
  public static byte valueOf(@Nullable Byte value) {
    return byteValue(value);
  }

  /**
   * Return the primitive short value of the given {@link Short} wrapper object or {@literal 0}
   * if the {@link Short} wrapper object is {@literal null}.
   *
   * @param value {@link Short} to evaluate.
   * @return a primitive short value for the given {@link Short} or {@literal 0}
   * if the {@link Short} is {@literal null}.
   * @see #shortValue(Number)
   * @see java.lang.Short#TYPE
   * @see java.lang.Short
   */
  @NullSafe
  public static short valueOf(@Nullable Short value) {
    return shortValue(value);
  }

  /**
   * Return the primitive int value of the given {@link Integer} wrapper object or {@literal 0}
   * if the {@link Integer} wrapper object is {@literal null}.
   *
   * @param value {@link Integer} to evaluate.
   * @return a primitive int value for the given {@link Integer} or {@literal 0}
   * if the {@link Integer} is {@literal null}.
   * @see java.lang.Integer#TYPE
   * @see java.lang.Integer
   * @see #intValue(Number)
   */
  @NullSafe
  public static int valueOf(@Nullable Integer value) {
    return intValue(value);
  }

  /**
   * Return the primitive long value of the given {@link Long} wrapper object or {@literal 0}
   * if the {@link Long} wrapper object is {@literal null}.
   *
   * @param value {@link Long} to evaluate.
   * @return a primitive long value for the given {@link Long} or {@literal 0}
   * if the {@link Long} is {@literal null}.
   * @see #longValue(Number)
   * @see java.lang.Long#TYPE
   * @see java.lang.Long
   */
  @NullSafe
  public static long valueOf(@Nullable Long value) {
    return longValue(value);
  }

  /**
   * Return the primitive float value of the given {@link Float} wrapper object or {@literal 0}
   * if the {@link Float} wrapper object is {@literal null}.
   *
   * @param value {@link Float} to evaluate.
   * @return a primitive float value for the given {@link Float} or {@literal 0}
   * if the {@link Float} is {@literal null}.
   * @see #floatValue(Number)
   * @see java.lang.Float#TYPE
   * @see java.lang.Float
   */
  @NullSafe
  public static float valueOf(@Nullable Float value) {
    return floatValue(value);
  }

  /**
   * Return the primitive double value of the given {@link Double} wrapper object or {@literal 0}
   * if the {@link Double} wrapper object is {@literal null}.
   *
   * @param value {@link Double} to evaluate.
   * @return a primitive double value for the given {@link Double} or {@literal 0}
   * if the {@link Double} is {@literal null}.
   * @see #doubleValue(Number)
   * @see java.lang.Double#TYPE
   * @see java.lang.Double
   */
  @NullSafe
  public static double valueOf(@Nullable Double value) {
    return doubleValue(value);
  }
}
