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

package org.cp.elements.lang.reflect;

import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.cp.elements.lang.NullSafe;

/**
 * {@link ModifierUtils} is an abstract utility class for determining the language modifiers
 * on a {@link Class} or a {@link Member}.
 *
 * @author John Blum
 * @see java.lang.Class
 * @see java.lang.reflect.Field
 * @see java.lang.reflect.Member
 * @see java.lang.reflect.Method
 * @see java.lang.reflect.Modifier
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ModifierUtils {

  /**
   * Determines whether the given object is {@literal abstract}.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Class} or a {@link Member}.
   * @return a boolean value indicating whether the given object is {@literal abstract}.
   * @see java.lang.reflect.Modifier#isAbstract(int)
   * @see #getModifiers(Object)
   */
  @NullSafe
  public static boolean isAbstract(Object obj) {
    return Modifier.isAbstract(getModifiers(obj));
  }

  /**
   * Determines whether the given object is {@literal final}.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Class} or a {@link Member}.
   * @return a boolean value indicating whether the given object is {@literal final}.
   * @see java.lang.reflect.Modifier#isFinal(int)
   * @see #getModifiers(Object)
   */
  @NullSafe
  public static boolean isFinal(Object obj) {
    return Modifier.isFinal(getModifiers(obj));
  }

  /**
   * Determines whether the given object is an {@link Class interface}.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Class interface}.
   * @return a boolean value indicating whether the given object is an {@link Class interface}.
   * @see java.lang.reflect.Modifier#isInterface(int)
   * @see #getModifiers(Object)
   */
  @NullSafe
  public static boolean isInterface(Object obj) {
    return Modifier.isInterface(getModifiers(obj));
  }

  /**
   * Determines whether the given object is {@literal native}.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Method}.
   * @return a boolean value indicating whether the given object is {@literal native}.
   * @see java.lang.reflect.Modifier#isNative(int)
   * @see #getModifiers(Object)
   */
  @NullSafe
  public static boolean isNative(Object obj) {
    return Modifier.isNative(getModifiers(obj));
  }

  /**
   * Determines whether the given object has {@literal package-private} access.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Class} or a {@link Member}.
   * @return a boolean value indicating whether the given object has {@literal package-private} access.
   * @see #isPrivate(Object)
   * @see #isProtected(Object)
   * @see #isPublic(Object)
   */
  @NullSafe
  public static boolean isPackagePrivate(Object obj) {
    return !(isPrivate(obj) || isProtected(obj) || isPublic(obj));
  }

  /**
   * Determines whether the given object has {@literal private} access.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Class} or a {@link Member}.
   * @return a boolean value indicating whether the given object is {@literal private}.
   * @see java.lang.reflect.Modifier#isPrivate(int)
   * @see #getModifiers(Object)
   */
  @NullSafe
  public static boolean isPrivate(Object obj) {
    return Modifier.isPrivate(getModifiers(obj));
  }

  /**
   * Determines whether the given object has {@literal protected} access.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Class} or a {@link Member}.
   * @return a boolean value indicating whether the given object is {@literal protected}.
   * @see java.lang.reflect.Modifier#isProtected(int)
   * @see #getModifiers(Object)
   */
  @NullSafe
  public static boolean isProtected(Object obj) {
    return Modifier.isProtected(getModifiers(obj));
  }

  /**
   * Determines whether the given object has {@literal public} access.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Class} or a {@link Member}.
   * @return a boolean value indicating whether the given object is {@literal public}.
   * @see java.lang.reflect.Modifier#isPublic(int)
   * @see #getModifiers(Object)
   */
  @NullSafe
  public static boolean isPublic(Object obj) {
    return Modifier.isPublic(getModifiers(obj));
  }

  /**
   * Determines whether the given object is {@literal static}.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Class} or a {@link Member}.
   * @return a boolean value indicating whether the given object is {@literal static}.
   * @see java.lang.reflect.Modifier#isStatic(int)
   * @see #getModifiers(Object)
   */
  @NullSafe
  public static boolean isStatic(Object obj) {
    return Modifier.isStatic(getModifiers(obj));
  }

  /**
   * Determines whether the given object is {@literal strict}.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Class} or a {@link Member}.
   * @return a boolean value indicating whether the given object is {@literal strict}.
   * @see java.lang.reflect.Modifier#isStrict(int)
   * @see #getModifiers(Object)
   */
  @NullSafe
  public static boolean isStrict(Object obj) {
    return Modifier.isStrict(getModifiers(obj));
  }

  /**
   * Determines whether the given object is {@literal synchronized}.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Method}.
   * @return a boolean value indicating whether the given object is {@literal synchronized}.
   * @see java.lang.reflect.Modifier#isSynchronized(int)
   * @see #getModifiers(Object)
   */
  @NullSafe
  public static boolean isSynchronized(Object obj) {
    return Modifier.isSynchronized(getModifiers(obj));
  }

  /**
   * Determines whether the given object is {@literal transient}.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Field}.
   * @return a boolean value indicating whether the given object is {@literal transient}.
   * @see java.lang.reflect.Modifier#isTransient(int)
   * @see #getModifiers(Object)
   */
  @NullSafe
  public static boolean isTransient(Object obj) {
    return Modifier.isTransient(getModifiers(obj));
  }

  /**
   * Determines whether the given object is {@literal volatile}.
   *
   * @param obj {@link Object} to evaluate.  The {@link Object} must be a {@link Field}.
   * @return a boolean value indicating whether the given object is {@literal volatile}.
   * @see java.lang.reflect.Modifier#isVolatile(int)
   * @see #getModifiers(Object)
   */
  @NullSafe
  public static boolean isVolatile(Object obj) {
    return Modifier.isVolatile(getModifiers(obj));
  }

  /**
   * Determines the {@link Modifier modifiers} for a given {@link Class} or {@link Member}.
   *
   * @param obj {@link Object} to evaluate. The {@link Object} must be a {@link Class} or a {@link Member}.
   * @return the {@link Modifier modifiers} for the given {@link Class} or {@link Member}.
   * Returns {@literal 0} if the object has no modifiers.
   * @see java.lang.reflect.Member#getModifiers()
   * @see java.lang.Class#getModifiers()
   */
  @NullSafe
  static int getModifiers(Object obj) {
    return (obj instanceof Class ? ((Class<?>) obj).getModifiers()
      : (obj instanceof Member ? ((Member) obj).getModifiers() : 0));
  }
}
