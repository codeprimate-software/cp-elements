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
 * The BooleanUtils class provides utility methods for working with boolean values.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Boolean
 * @since 1.0.0
 */
public abstract class BooleanUtils {

  /**
   * Performs a logical AND on all the Boolean values and returns true if an only if all Boolean values evaluate
   * to true, as determined by the BooleanUtils.valueOf method on the Boolean wrapper object.  If the Boolean array
   * is null, then the result of the AND operation is false.
   * <p/>
   * @param values the array of Boolean values evaluated with the logical AND operator.
   * @return a boolean value of true if the Boolean array is not null and all Boolean wrapper object evaluate to true.
   * @see #valueOf(Boolean)
   */
  public static boolean and(final Boolean... values) {
    boolean result = (values != null); // innocent until proven guilty

    if (result) {
      for (final Boolean value : values) {
        result &= valueOf(value);
        if (!result) { // short-circuit if we find a false value
          break;
        }
      }
    }

    return result;
  }

  /**
   * Negates the specified evaluation of the Boolean value, which is determined by the BooleanUtils.valueOf method.
   * If the Boolean wrapper object is null or false, then this method returns true.  If the Boolean wrapper object
   * is true, then this method returns false.
   * <p/>
   * @param value the Boolean value being negated.
   * @return a negated boolean value of the specified Boolean wrapper object.
   * @see #valueOf(Boolean)
   */
  public static boolean negate(final Boolean value) {
    return !valueOf(value);
  }

  /**
   * Performs a logical OR on all the Boolean values and returns true if just one Boolean value evaluates
   * to true, as determined by the BooleanUtils.valueOf method on the Boolean wrapper object.  If the Boolean array
   * is null, the result of the OR operation is false.
   * <p/>
   * @param values the array of Boolean values evaluated with the logical OR operator.
   * @return a boolean value of true if the Boolean array is not null and any of the Boolean values is true.
   * @see #valueOf(Boolean)
   */
  public static boolean or(final Boolean... values) {
    boolean result = false; // guilty until proven innocent

    if (values != null) {
      for (final Boolean value : values) {
        result |= valueOf(value);
        if (result) { // short-circuit if we find a true value
          break;
        }
      }
    }

    return result;
  }

  /**
   * Converts the specified primitive boolean value into a Boolean wrapper object.
   * <p/>
   * @param value the primitive boolean value to convert into the equivalent Boolean wrapper object.
   * @return a Boolean wrapper object with the same value as the primitive boolean value.
   * @see java.lang.Boolean
   */
  public static Boolean toBoolean(final boolean value) {
    return (value ? Boolean.TRUE : Boolean.FALSE);
  }

  /**
   * Returns a String representation of the specified Boolean value.  If the Boolean value is true, then trueValue
   * is returned, otherwise falseValue is returned.
   * <p/>
   * @param value the Boolean value to evaluate and represent as one of the two String values representing true
   * and false.
   * @param trueValue String value representation for true.
   * @param falseValue String value representation of false.
   * @return a String representation of the specified Boolean value customized with values for true and false.
   * @see #valueOf(Boolean)
   */
  public static String toString(final Boolean value, final String trueValue, final String falseValue) {
    return (valueOf(value) ? trueValue : falseValue);

  }
  /**
   * Determines the primitive boolean value of the specified Boolean wrapper object.  The Boolean value is true
   * if and only if it is equal to Boolean.TRUE.  This method handles null values.
   * <p/>
   * @param value the Boolean wrapper value to convert to a primitive boolean value.
   * @return a primitive boolean value equivalent to the value of the Boolean wrapper object.
   * @see java.lang.Boolean
   */
  public static boolean valueOf(final Boolean value) {
    return Boolean.TRUE.equals(value);
  }

  /**
   * Performs a logical exclusive OR on the array of Boolean values and returns true if and only if the Boolean array
   * is not null and 1 and only 1 Boolean wrapper object evaluates to true, as determined by the BooleanUtils.valueof
   * method.
   * <p/>
   * @param values the array of Boolean values evaluated with the logical exclusive OR operator.
   * @return a boolean value of true if and only if the Boolean array is not null and 1 and only 1 of the Boolean
   * wrapper objects evaluates to true.
   * @see #valueOf(Boolean)
   */
  public static boolean xor(final Boolean... values) {
    boolean result = false; // guilty until proven innocent.

    if (values != null) {
      for (final Boolean value : values) {
        final boolean primitiveValue = valueOf(value);

        if (result && primitiveValue) {
          return false; // short-circuit if we find more than 1 true value
        }

        result |= primitiveValue;
      }
    }

    return result;
  }

}
