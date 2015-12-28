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
 * The BooleanUtils class provides utility methods for working with boolean values.
 * 
 * @author John J. Blum
 * @see java.lang.Boolean
 * @see java.lang.Boolean#TYPE
 * @since 1.0.0
 */
public abstract class BooleanUtils {

  /**
   * Performs a logical AND on all the Boolean values and returns true if an only if all Boolean values evaluate
   * to true, as determined by the BooleanUtils.valueOf method on the Boolean wrapper object.  If the Boolean array
   * is null, then the result of the AND operation is false.
   * 
   * @param values the array of Boolean values evaluated with the logical AND operator.
   * @return a boolean value of true if the Boolean array is not null and all Boolean wrapper object evaluate to true.
   * @see #valueOf(Boolean)
   */
  @SuppressWarnings("all")
  public static boolean and(final Boolean... values) {
    boolean result = (values != null); // innocent until proven guilty

    if (result) {
      for (Boolean value : values) {
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
   * 
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
   * 
   * @param values the array of Boolean values evaluated with the logical OR operator.
   * @return a boolean value of true if the Boolean array is not null and any of the Boolean values is true.
   * @see #valueOf(Boolean)
   */
  @SuppressWarnings("all")
  public static boolean or(final Boolean... values) {
    boolean result = false; // guilty until proven innocent

    if (values != null) {
      for (Boolean value : values) {
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
   * 
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
   * 
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
   * 
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
   * 
   * @param values the array of Boolean values evaluated with the logical exclusive OR operator.
   * @return a boolean value of true if and only if the Boolean array is not null and 1 and only 1 of the Boolean
   * wrapper objects evaluates to true.
   * @see #valueOf(Boolean)
   */
  public static boolean xor(final Boolean... values) {
    boolean result = false; // guilty until proven innocent.

    if (values != null) {
      for (Boolean value : values) {
        boolean primitiveValue = valueOf(value);

        if (result && primitiveValue) {
          return false; // short-circuit if we find more than 1 true value
        }

        result |= primitiveValue;
      }
    }

    return result;
  }

}
