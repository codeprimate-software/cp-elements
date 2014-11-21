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

/**
 * The NumberUtils class is a utility class encapsulating common functionality for working with Numbers.
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
  public static byte[] getBytes(final int value) {
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
  public static boolean isDecimal(final double value) {
    return (Math.floor(value) != value);
  }

  /**
   * Determines whether the specified long value is an even number, which is any number divisible by 2.
   * 
   * @param value the long value being evaluated as an even value.
   * @return a boolean value indicating whether the specified long value is even.
   * @see #isOdd(long)
   */
  public static boolean isEven(final long value) {
    return (Math.abs(value) % 2l == 0);
  }

  /**
   * Determines whether the specified double value is negative (less than 0).
   * 
   * @param value a double who's value is evaluated as a negative value (less than 0).
   * @return a boolean value indicating whether the specified double value is negative (less than 0).
   * @see #isPositive(double)
   */
  public static boolean isNegative(final double value) {
    return (value < 0.0d);
  }

  /**
   * Determines whether the specified long value is an odd number, which is any number having a remainder of 1
   * when divided by 2
   * 
   * @param value the long value being evaluated as an odd value.
   * @return a boolean value indicating whether the specified long value is odd.
   * @see #isEven(long)
   */
  public static boolean isOdd(final long value) {
    return (Math.abs(value) % 2l == 1);
  }

  /**
   * Determines whether the specified double value is positive (greater than 0).
   * 
   * @param value a double who's value is evaluated as a positive value (greater than 0).
   * @return a boolean value indicating whether the specified double value is positive (greater than 0).
   * @see #isNegative(double)
   */
  public static boolean isPositive(final double value) {
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
  public static boolean isWhole(final double value) {
    return (Math.floor(value) == value);
  }

  /**
   * Gets the primitive byte value of the specified Byte wrapper object, or 0 if the Byte wrapper object is null.
   * 
   * @param value the Byte to determine the byte primitive value for.
   * @return a primitive byte value for the specified Byte, or 0 if the Byte wrapper object is null.
   * @see java.lang.Byte
   */
  public static byte valueOf(final Byte value) {
    return (value == null ? 0 : value);
  }

  /**
   * Gets the primitive short value of the specified Short wrapper object, or 0 if the Short wrapper object is null.
   * 
   * @param value the Short to determine the short primitive value for.
   * @return a primitive short value for the specified Short, or 0 if the Short wrapper object is null.
   * @see java.lang.Short
   */
  public static short valueOf(final Short value) {
    return (value == null ? 0 : value);
  }

  /**
   * Gets the primitive int value of the specified Integer wrapper object, or 0 if the Integer wrapper object is null.
   * 
   * @param value the Integer to determine the int primitive value for.
   * @return a primitive int value for the specified Integer, or 0 if the Integer wrapper object is null.
   * @see java.lang.Integer
   */
  public static int valueOf(final Integer value) {
    return (value == null ? 0 : value);
  }

  /**
   * Gets the primitive long value of the specified Long wrapper object, or 0l if the Long wrapper object is null.
   * 
   * @param value the Long to determine the long primitive value for.
   * @return a primitive long value for the specified Long, or 0L if the Long wrapper object is null.
   * @see java.lang.Long
   */
  public static long valueOf(final Long value) {
    return (value == null ? 0l : value);
  }

  /**
   * Gets the primitive float value of the specified Float wrapper object, or 0.0f if the Float wrapper object is null.
   * 
   * @param value the Float to determine the float primitive value for.
   * @return a primitive float value for the specified Float, or 0.0f if the Float wrapper object is null.
   * @see java.lang.Float
   */
  public static float valueOf(final Float value) {
    return (value == null ? 0.0f : value);
  }

  /**
   * Gets the primitive double value of the specified Double wrapper object, or 0.0d if the Double wrapper object
   * is null.
   * 
   * @param value the Double to determine the double primitive value for.
   * @return a primitive double value for the specified Double, or 0.0d if the Double wrapper object is null.
   * @see java.lang.Double
   */
  public static double valueOf(final Double value) {
    return (value == null ? 0.0d : value);
  }

}
