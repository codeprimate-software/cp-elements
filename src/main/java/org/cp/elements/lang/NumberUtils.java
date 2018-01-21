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

import java.math.BigInteger;

import org.cp.elements.lang.annotation.NullSafe;

/**
 * The {@link NumberUtils} class is an abstract utility class encapsulating common functionality
 * for working with {@link Number Numbers}.
 *
 * @author John J. Blum
 * @see java.lang.Number
 * @since 1.0.0
 */
public abstract class NumberUtils {

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
    return (Math.floor(value) != value);
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
  public static boolean isDecimal(Number value) {
    return (value instanceof Float || value instanceof Double);
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
    return (Math.abs(value) % 2L == 0);
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
    return ((1L & Math.abs(value)) == 0L);
  }

  /**
   * Determines whether the specified double value is negative (less than 0).
   *
   * @param value a double who's value is evaluated as a negative value (less than 0).
   * @return a boolean value indicating whether the specified double value is negative (less than 0).
   * @see #isPositive(double)
   */
  public static boolean isNegative(double value) {
    return (value < 0.0d);
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
    return ((1L & Math.abs(value)) == 1L);
  }

  /**
   * Determines whether the specified double value is positive (greater than 0).
   *
   * @param value a double who's value is evaluated as a positive value (greater than 0).
   * @return a boolean value indicating whether the specified double value is positive (greater than 0).
   * @see #isNegative(double)
   */
  public static boolean isPositive(double value) {
    return (value > 0.0d);
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
    return (Math.floor(value) == value);
  }

  /* (non-Javadoc) */
  @NullSafe
  public static boolean isWhole(Number value) {
    return (value instanceof Byte || value instanceof Short || value instanceof Integer || value instanceof Long
      || value instanceof BigInteger);
  }

  /**
   * Determines whether the given double value is zero.
   *
   * @param value double value to evaluate.
   * @return a boolean value indicating whether the given double value is zero.
   */
  public static boolean isZero(double value) {
    return (value == 0.0d);
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
  public static boolean isByte(Number value) {
    return (value instanceof Byte
      || (isWhole(value) && value.longValue() >= Byte.MIN_VALUE && value.longValue() <= Byte.MAX_VALUE));
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
  public static boolean isShort(Number value) {
    return (value instanceof Short
      || (isWhole(value) && value.longValue() >= Short.MIN_VALUE && value.longValue() <= Short.MAX_VALUE));
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
  public static boolean isInteger(Number value) {
    return (value instanceof Integer
      || (isWhole(value) && value.longValue() >= Integer.MIN_VALUE && value.longValue() <= Integer.MAX_VALUE));
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
  public static boolean isLong(Number value) {
    return (value instanceof Long);
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
  public static boolean isFloat(Number value) {
    return (value instanceof Float
      || (isDecimal(value) && value.doubleValue() >= Float.MIN_VALUE && value.doubleValue() <= Float.MAX_VALUE));
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
  public static boolean isDouble(Number value) {
    return (value instanceof Double);
  }

  /**
   * Returns the byte value of the given {@link Number} or 0 if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the byte value of the given {@link Number} or 0 if the {@link Number} is {@literal null}.
   * @see java.lang.Number#byteValue()
   */
  @NullSafe
  public static byte byteValue(Number number) {
    return (number != null ? number.byteValue() : 0);
  }

  /**
   * Returns the short value of the given {@link Number} or 0 if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the short value of the given {@link Number} or 0 if the {@link Number} is {@literal null}.
   * @see java.lang.Number#shortValue()
   */
  @NullSafe
  public static short shortValue(Number number) {
    return (number != null ? number.shortValue() : 0);
  }

  /**
   * Returns the int value of the given {@link Number} or 0 if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the int value of the given {@link Number} or 0 if the {@link Number} is {@literal null}.
   * @see java.lang.Number#intValue()
   */
  @NullSafe
  public static int intValue(Number number) {
    return (number != null ? number.intValue() : 0);
  }

  /**
   * Returns the long value of the given {@link Number} or 0L if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the long value of the given {@link Number} or 0L if the {@link Number} is {@literal null}.
   * @see java.lang.Number#longValue()
   */
  @NullSafe
  public static long longValue(Number number) {
    return (number != null ? number.longValue() : 0L);
  }

  /**
   * Returns the float value of the given {@link Number} or 0.0f if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the float value of the given {@link Number} or 0.0f if the {@link Number} is {@literal null}.
   * @see java.lang.Number#floatValue()
   */
  @NullSafe
  public static float floatValue(Number number) {
    return (number != null ? number.floatValue() : 0.0f);
  }

  /**
   * Returns the double value of the given {@link Number} or 0.0d if {@link Number} is {@literal null}.
   *
   * @param number {@link Number} to evaluate.
   * @return the double value of the given {@link Number} or 0.0d if the {@link Number} is {@literal null}.
   * @see java.lang.Number#doubleValue()
   */
  @NullSafe
  public static double doubleValue(Number number) {
    return (number != null ? number.doubleValue() : 0.0d);
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
  public static byte valueOf(Byte value) {
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
  public static short valueOf(Short value) {
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
  public static int valueOf(Integer value) {
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
  public static long valueOf(Long value) {
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
  public static float valueOf(Float value) {
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
  public static double valueOf(Double value) {
    return doubleValue(value);
  }
}
