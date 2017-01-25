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

import static org.cp.elements.util.ArrayUtils.nullSafeArray;

import java.lang.reflect.Constructor;

import org.cp.elements.lang.reflect.ConstructorNotFoundException;
import org.cp.elements.lang.reflect.ReflectionUtils;
import org.cp.elements.util.ArrayUtils;

/**
 * The ObjectUtils utility class performs various operations on {@link java.lang.Object}.
 *
 * @author John J. Blum
 * @see java.lang.Object
 * @see org.cp.elements.lang.reflect.ReflectionUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ObjectUtils extends ReflectionUtils {

  public static final Object[] EMPTY_OBJECT_ARRAY = new Object[0];

  public static final String CLONE_METHOD_NAME = "clone";

  /**
   * Null-safe method to determine if all the values in the array are null.
   *
   * @param values an array of values being evaluated for null.
   * @return true if and only if the array is null, empty or all elements in the array are null.
   * @see #areAnyNull(Object...)
   */
  @NullSafe
  @SuppressWarnings("all")
  public static boolean areAllNull(Object... values) {
    for (Object value : nullSafeArray(values)) {
      if (value != null) {
        return false;
      }
    }

    return true;
  }

  /**
   * Null-safe method to determine if any of the values in the array are null.
   *
   * @param values an array of values being evaluated for null
   * @return true if and only if the array is not null, not empty and contains 1 null element.
   * @see #areAllNull(Object...)
   */
  @NullSafe
  public static boolean areAnyNull(Object... values) {
    for (Object value : nullSafeArray(values)) {
      if (value == null) {
        return true;
      }
    }

    return false;
  }

  /**
   * Null-safe clone operation on Objects that implement the Cloneable interface or implement a copy constructor.
   *
   * @param <T> the Cloneable class type of the value.
   * @param value the Object value to clone.
   * @return a "clone" of a given Cloneable Object or value if the Object is not Cloneable but implements
   * a copy constructor.
   * @see java.lang.Cloneable
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  public static <T> T clone(T value) {
    if (value instanceof Cloneable) {
      return (T) invoke(value, CLONE_METHOD_NAME, value.getClass());
    }
    else if (value != null) {
      try {
        Class<T> valueType = (Class<T>) value.getClass();
        Constructor<T> copyConstructor = resolveConstructor(valueType, new Class<?>[] { valueType }, value);
        return copyConstructor.newInstance(value);
      }
      catch (ConstructorNotFoundException ignore) {
        // a copy constructor was not found in the value's class type
      }
      catch (Exception e) {
        throw new CloneException("'clone' using 'copy constructor' was unsuccessful", e);
      }
    }

    throw new UnsupportedOperationException(new CloneNotSupportedException(String.format(
      "'clone' is not supported for (%1$s) value", getClassSimpleName(value))));
  }

  /**
   * Gets the first non-null value in the array of values.
   *
   * @param values an array of Object values from which the first non-null Object value in the array is returned.
   * @param <T> the Class type of values in the array.
   * @return the first non-null value in the array of Object values.
   */
  @NullSafe
  @SuppressWarnings({ "unchecked", "varargs" })
  public static <T> T defaultIfNull(T... values) {
    for (T value : nullSafeArray(values)) {
      if (value != null) {
        return value;
      }
    }

    return null;
  }

  /**
   * Determines whether {@code obj1} is {@literal null} or equal to {@code obj2}.
   *
   * @param obj1 {@link Object} being evaluated in the equality comparison.
   * @param obj2 {@link Object} to compare for equality with {@code obj1} if {@code obj1} is not {@literal null}.
   * @return a boolean value indicating whether {@code obj1} is {@literal null} or equal to {@code obj2}.
   * @see java.lang.Object#equals(Object)
   */
  public static boolean isNullOrEqualTo(Object obj1, Object obj2) {
    return (obj1 == null || obj1.equals(obj2));
  }

  /**
   * Determines whether two objects are equal in value as determined by the Object.equals method in addition to
   * guarding against null values.  Both objects are considered equal if and only if both are non-null
   * and obj1.equals(obj2).
   *
   * @param obj1 the first Object in the equality comparison.
   * @param obj2 the second Object in the equality comparison.
   * @return a boolean value indicating whether the two Objects are equal in value.
   * @see java.lang.Object#equals(Object)
   */
  @NullSafe
  public static boolean equals(Object obj1, Object obj2) {
    return (obj1 != null && obj1.equals(obj2));
  }

  /**
   * Determines whether two objects are equal in value as determined by the Object.equals method.  Both objects are
   * considered equal if and only if both are null or obj1.equals(obj2).
   *
   * @param obj1 the first Object in the equality comparison.
   * @param obj2 the second Object in the equality comparison.
   * @return a boolean value indicating whether the two Objects are equal in value where two null Object references
   * are considered equal as well.
   * @see java.lang.Object#equals(Object)
   */
  @NullSafe
  public static boolean equalsIgnoreNull(Object obj1, Object obj2) {
    return (obj1 == null ? obj2 == null : obj1.equals(obj2));
  }

  /**
   * Calculates the hash code of an object by invoking Object.hashCode for non-null objects and returning 0 if the
   * object is null.
   *
   * @param obj the Object who's hash code is computed and returned.
   * @return an integer value with the calculated hash code of the object.
   * @see java.lang.Object#hashCode()
   */
  @NullSafe
  public static int hashCode(Object obj) {
    return (obj != null ? obj.hashCode() : 0);
  }

  /**
   * Transforms the object into a String by invoking Object.toString for non-null objects and returning null for
   * null object references.
   *
   * @param obj the Object who's toString method will be called.
   * @return a String representation of the object.
   * @see java.lang.Object#toString()
   */
  @NullSafe
  public static String toString(Object obj) {
    return (obj != null ? obj.toString() : null);
  }
}
