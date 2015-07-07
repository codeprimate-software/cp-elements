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

import java.lang.reflect.Constructor;

import org.cp.elements.lang.reflect.ConstructorNotFoundException;
import org.cp.elements.lang.reflect.ReflectionUtils;

/**
 * The ObjectUtils utility class performs various operations on java.lang.Objects.
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
  public static <T> T clone(final T value) {
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
  public static <T> T defaultIfNull(final T... values) {
    if (values != null) {
      for (T value : values) {
        if (value != null) {
          return value;
        }
      }
    }

    return null;
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
  public static boolean equals(final Object obj1, final Object obj2) {
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
  public static boolean equalsIgnoreNull(final Object obj1, final Object obj2) {
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
  public static int hashCode(final Object obj) {
    return (obj == null ? 0 : obj.hashCode());
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
  public static String toString(final Object obj) {
    return (obj == null ? null : obj.toString());
  }

}
