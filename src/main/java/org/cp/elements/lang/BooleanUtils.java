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

import static org.cp.elements.util.ArrayUtils.nullSafeArray;

import java.util.concurrent.atomic.AtomicBoolean;

import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.util.ArrayUtils;

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
   * Performs a logical AND on all {@link Boolean} values in the array and returns {@literal true} if and only if (iff)
   * all {@link Boolean} values evaluate to {@literal true}, as determined by the {@link BooleanUtils#valueOf(Boolean)}
   * method for each {@link Boolean} value in the array.
   *
   * If the {@literal Boolean} array is {@literal null}, then the result of this logical AND operation
   * is {@literal false}.
   *
   * @param values array of {@literal Boolean} values to evaluate with the logical AND operator.
   * @return a boolean value of {@literal true} if the {@link Boolean} array is not {@literal null}
   * and all {@literal Boolean} values in the array evaluate to {@literal true}.
   * @see #valueOf(Boolean)
   * @see #or(Boolean...)
   */
  @NullSafe
  public static boolean and(Boolean... values) {

    boolean result = values != null; // innocent until proven guilty

    if (result) {
      for (Boolean value : values) {
        result &= valueOf(value);
        if (!result) { // short-circuit if we find a false value; guilty!
          break;
        }
      }
    }

    return result;
  }

  /**
   * Negates the evaluation of the given {@link Boolean} value determined by the {@link BooleanUtils#valueOf(Boolean)}
   * method.
   *
   * If the {@link Boolean} wrapper object is {@literal false}, then this method returns {@literal true}.
   * If the {@link Boolean} wrapper object is {@literal null} or {@literal true}, then this method
   * returns {@literal false}.
   *
   * @param value {@link Boolean} value to negate.
   * @return a negated boolean value for the given {@link Boolean} wrapper object.
   * @see #not(Boolean)
   */
  @NullSafe
  public static boolean negate(Boolean value) {
    return not(value);
  }

  /**
   * Perform the logical NOT operation on the given {@link Boolean} value.
   *
   * If the {@link Boolean} wrapper object is {@literal false}, then this method returns {@literal true}.
   * If the {@link Boolean} wrapper object is {@literal null} or {@literal true}, then this method
   * returns {@literal false}.
   *
   * @param value {@link Boolean} wrapper object to evaluate.
   * @return a logical NOT of the given {@link Boolean} value.
   */
  @NullSafe
  public static boolean not(Boolean value) {
    return value != null && !valueOf(value);
  }

  /**
   * Performs a logical OR on all {@link Boolean} values in the array and returns {@literal true} if and only if (iff)
   * at least one {@link Boolean} value evaluates to {@literal true}, as determined by
   * the {@link BooleanUtils#valueOf(Boolean)} method for each {@link Boolean} value in the array.
   *
   * If the {@literal Boolean} array is {@literal null}, then the result of this logical OR operation
   * is {@literal false}.
   *
   * @param values array of {@literal Boolean} values to evaluate with the logical OR operator.
   * @return a boolean value of {@literal true} if the {@link Boolean} array is not {@literal null}
   * and at least one {@literal Boolean} value in the array evaluates to {@literal true}.
   * @see #valueOf(Boolean)
   * @see #and(Boolean...)
   */
  @NullSafe
  public static boolean or(Boolean... values) {

    boolean result = false; // guilty until proven innocent

    for (Boolean value : nullSafeArray(values, Boolean.class)) {
      result |= valueOf(value);
      if (result) { // short-circuit if we find a true value; innocent
        break;
      }
    }

    return result;
  }

  /**
   * Converts the specified primitive boolean value into a {@link Boolean} wrapper object.
   *
   * @param value primitive boolean value to convert into the equivalent {@link Boolean} wrapper object.
   * @return a {@link Boolean} wrapper object with the same value as the primitive boolean.
   * @see java.lang.Boolean
   */
  public static Boolean toBoolean(boolean value) {
    return value ? Boolean.TRUE : Boolean.FALSE;
  }

  /**
   * Returns a {@link String} representation of the specified {@link Boolean} value.
   *
   * If the {@link Boolean} value is {@literal true}, then {@link String trueValue} is returned,
   * otherwise {@link String falseValue} is returned.
   *
   * @param value {@link Boolean} value to evaluate and represent as one of the two {@link String} values
   * representing {@literal true} and {@literal false}.
   * @param trueValue {@link String} value representation for {@literal true}.
   * @param falseValue {@link String} value representation of {@literal false}.
   * @return a {@link String} representation for the given {@link Boolean} value
   * customized with values for {@literal true} and {@literal false}.
   * @see #valueOf(Boolean)
   */
  @NullSafe
  public static String toString(Boolean value, String trueValue, String falseValue) {
    return valueOf(value) ? trueValue : falseValue;

  }

  /**
   * Determines the boolean value of the given {@link AtomicBoolean}.
   *
   * @param value {@link AtomicBoolean} to evaluate.
   * @return {@literal true} if the given {@link AtomicBoolean} is not {@literal null} and is equal to {@literal true}.
   * @see java.util.concurrent.atomic.AtomicBoolean
   */
  @NullSafe
  public static boolean valueOf(AtomicBoolean value) {
    return value != null && value.get();
  }

  /**
   * Determines the primitive boolean value of the given {@link  Boolean} wrapper object.
   *
   * The {@link Boolean} value is {@literal true} if and only if (iff) the value is equal to {@link Boolean#TRUE}.
   *
   * @param value {@link Boolean} wrapper object to convert to a primitive boolean value.
   * @return a primitive boolean value equivalent to the value of the {@link Boolean wrapper} object.
   * @see java.lang.Boolean
   */
  @NullSafe
  public static boolean valueOf(Boolean value) {
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
  @NullSafe
  public static boolean xor(Boolean... values) {

    boolean result = false; // guilty until proven innocent.

    for (Boolean value : ArrayUtils.nullSafeArray(values, Boolean.class)) {

      boolean primitiveValue = valueOf(value);

      if (result && primitiveValue) {
        return false; // short-circuit if we find more than 1 true value
      }

      result |= primitiveValue;
    }

    return result;
  }
}
