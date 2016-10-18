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
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.BooleanUtils;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.DslExtension;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.annotation.DSL;
import org.cp.elements.lang.support.ComposableFilter;
import org.cp.elements.util.ComparatorUtils;

/**
 * The ReflectionUtils class is an abstract utility base class encapsulating commons operations used in Java
 * Introspection and Reflection.
 *
 * @author John J. Blum
 * @see java.lang.Class
 * @see java.lang.Object
 * @see java.lang.reflect.AccessibleObject
 * @see java.lang.reflect.Field
 * @see java.lang.reflect.Member
 * @see java.lang.reflect.Method
 * @see java.lang.reflect.Modifier
 * @see org.cp.elements.lang.ClassUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ReflectionUtils extends ClassUtils {

  /**
   * Determines the class types for all the given arguments.
   *
   * @param arguments the array of (object) arguments from which to determine their class types.
   * @return an array of class types for each object in the array of arguments, or null if the given array is null.
   * This method is careful to guard against null elements given a non-null array of arguments.
   * @see org.cp.elements.lang.ClassUtils#getClass(Object)
   */
  @NullSafe
  public static Class[] getArgumentTypes(Object... arguments) {
    Class[] argumentTypes = null;

    if (arguments != null) {
      argumentTypes = new Class[arguments.length];

      for (int index = 0; index < arguments.length; index++) {
        argumentTypes[index] = getClass(arguments[index]);
      }
    }

    return argumentTypes;
  }

  /**
   * Gets the value of the field with the specified name on the given class type cast to the desired field type.
   * This method assumes the field is a static (class) member field.
   *
   * @param <T> the desired return type in which the field's value will be cast; should be compatible with
   * the field's declared type.
   * @param type the Class type on which the field is declared and defined.
   * @param fieldName a String indicating the name of the field from which to get the value.
   * @param fieldType the declared type of the class field.
   * @return the value of the specified field on the given class type cast to the desired type.
   * @throws IllegalArgumentException if the given class type does not declare a static member field
   * with the specified name.
   * @throws FieldAccessException if the value for the specified field could not be retrieved.
   * @see #getField(Class, String)
   * @see #getValue(Object, java.lang.reflect.Field, Class)
   */
  @SuppressWarnings("unchecked")
  public static <T> T getValue(Class<?> type, String fieldName, Class<T> fieldType) {
    try {
      return getValue(null, getField(type, fieldName), fieldType);
    }
    catch (FieldNotFoundException e) {
      throw new IllegalArgumentException(String.format("Field with name (%1$s) does not exist on class type (%2$s)!",
        fieldName, type.getName()), e);
    }
  }

  /**
   * Gets the value of the field with the specified name on the given object cast to the desired field type.
   * This method assumes the field is a instance (object) member field.
   *
   * @param <T> the desired return type in which the field's value will be cast; should be compatible with
   * the field's declared type.
   * @param obj the Object on which the field is defined.
   * @param fieldName a String indicating the name of the field from which to get the value.
   * @param fieldType the declared type of the object's field.
   * @return the value of the specified field on the given object cast to the desired type.
   * @throws IllegalArgumentException if the given object's class type does not declare a instance member field
   * with the specified name.
   * @throws FieldAccessException if the value for the specified field could not be retrieved.
   * @see #getField(Class, String)
   * @see #getValue(Object, java.lang.reflect.Field, Class)
   */
  public static <T> T getValue(Object obj, String fieldName, Class<T> fieldType) {
    try {
      return getValue(obj, getField(obj.getClass(), fieldName), fieldType);
    }
    catch (FieldNotFoundException e) {
      throw new IllegalArgumentException(String.format("Field with name (%1$s) does not exist on object of type (%2$s)!",
        fieldName, obj.getClass().getName()), e);
    }
  }

  /**
   * Gets the value of the field on the given object cast to the desired class type.  If the "target" object is null,
   * then this method assumes the field is a static (class) member field; otherwise the field is considered
   * an instance (object) member field.  This method is not null-safe!
   *
   * @param <T> the desired return type in which the field's value will be cast; should be compatible with
   * the field's declared type.
   * @param target the Object on which the field is defined.
   * @param field the specified Field from which to get the value.
   * @param type the desired return type of the field's value; should be compatible with the field's declared type.
   * @return the value of the given field on the given object cast to the desired type.
   * @throws FieldAccessException if the value for the specified field could not be retrieved.
   * @throws NullPointerException if the field or type parameter arguments are null.
   */
  public static <T> T getValue(Object target, Field field, Class<T> type) {
    try {
      boolean currentAccessible = field.isAccessible();
      field.setAccessible(true);
      Object value = field.get(target);
      field.setAccessible(currentAccessible);
      return type.cast(value);
    }
    catch (NullPointerException e) {
      throw e;
    }
    catch (Exception e) {
      throw new FieldAccessException(String.format("Failed to get value of field (%1$s) from %2$s type (%3$s)!",
        field.getName(), BooleanUtils.toString(target == null, "class", "object of"), getClassName(target)), e);
    }
  }

  /**
   * Sets the field with the specified name on the given class type to the given value. This method assumes the field
   * is a static (class) member field.
   *
   * @param type the Class type on which the field is declared and defined.
   * @param fieldName a String indicating the name of the field on which to set the value.
   * @param value the Object value to set the specified field to.
   * @throws IllegalArgumentException if the given class type does not declare a static member field
   * with the specified name.
   * @throws FieldAccessException if the value for the specified field could not be set.
   * @see #getField(Class, String)
   * @see #setField(Object, java.lang.reflect.Field, Object)
   */
  public static void setField(Class<?> type, String fieldName, Object value) {
    try {
      setField(null, getField(type, fieldName), value);
    }
    catch (FieldNotFoundException e) {
      throw new IllegalArgumentException(String.format("Field with name (%1$s) does not exist on class type (%2$s)!",
        fieldName, type.getName()), e);
    }
  }

  /**
   * Sets the field with the specified name on the given object to the given value. This method assumes the field
   * is an instance (object) member field.
   *
   * @param obj the Object on which the field is defined.
   * @param fieldName a String indicating the name of the field on which to set the value.
   * @param value the Object value to set the specified field to.
   * @throws IllegalArgumentException if the given object's class type does not declare an instance member field
   * with the specified name.
   * @throws FieldAccessException if the value for the specified field could not be set.
   * @see #getField(Class, String)
   * @see #setField(Object, java.lang.reflect.Field, Object)
   */
  public static void setField(Object obj, String fieldName, Object value) {
    try {
      setField(obj, getField(obj.getClass(), fieldName), value);
    }
    catch (FieldNotFoundException e) {
      throw new IllegalArgumentException(String.format("Field with name (%1$s) does not exist on object of type (%2$s)!",
        fieldName, obj.getClass().getName()), e);
    }
  }

  /**
   * Sets the field on the given object to the given value. If the "target" object is null, then this method assumes
   * the field is a static (class) member field; otherwise the field is considered an instance (object) member field.
   * This method is not null-safe!
   *
   * @param target the Object on which the field is defined.
   * @param field the specified Field on which to set the value.
   * @param value the Object value to set the specified field to.
   * @throws FieldAccessException if the field is final, or the value for the specified field could not be set.
   * @throws NullPointerException if the field parameter argument is null.
   */
  @SuppressWarnings("all")
  public static void setField(Object target, Field field, Object value) {
    try {
      Assert.isFalse(Modifier.isFinal(field.getModifiers()), new FieldAccessException(String.format(
        "Cannot set the value of a final field (%1$s) on %2$s type (%3$s)!", field.getName(),
          BooleanUtils.toString(target == null, "class", "object of"), field.getDeclaringClass().getName())));

      boolean currentAccessible = field.isAccessible();

      field.setAccessible(true);
      field.set(target, value);
      field.setAccessible(currentAccessible);
    }
    catch (FieldAccessException e) {
      throw e;
    }
    catch (NullPointerException e) {
      throw e;
    }
    catch (Exception e) {
      throw new FieldAccessException(String.format("Failed to set field (%1$s) to value (%2$s) on %3$s type (%4$s)!",
        field.getName(), value, BooleanUtils.toString(target == null, "class", "object of"),
          field.getDeclaringClass().getName()), e);
    }
  }

  /**
   * Calls the no argument method with the specified name on the given class type.  This method assumes
   * the "method" to invoke is a static (class) member method with not return value.
   *
   * @param type the Class type on which the method to invoke is declared and defined.
   * @param methodName a String indicating the name of the method to invoke.
   * @throws IllegalArgumentException if the method with the specified name is not declared on the given class type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @see #getMethod(Class, String, Class[])
   * @see #invoke(Class, String, Class[], Object[], Class)
   */
  public static void invoke(Class<?> type, String methodName) {
    invoke(type, methodName, null, null, Void.class);
  }

  /**
   * Calls the no argument method with the specified name on the given class type, casting the method's return value
   * to the desired return type.  This method assumes the "method" to invoke is a static (class) member method.
   *
   * @param <T> the desired return type in which the method's return value will be cast; should be compatible with
   * the method's return type.
   * @param type the Class type on which the method to invoke is declared and defined.
   * @param methodName a String indicating the name of the method to invoke.
   * @param returnType the desired Class type in which to cast the method's return value.
   * @return the specified method's return value cast to the desired return type.
   * @throws IllegalArgumentException if the method with the specified name is not declared on the given class type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @see #getMethod(Class, String, Class[])
   * @see #invoke(Class, String, Class[], Object[], Class)
   */
  public static <T> T invoke(Class<?> type, String methodName, Class<T> returnType) {
    return invoke(type, methodName, null, null, returnType);
  }

  /**
   * Calls the method with the specified name on the given class type, passing the given arguments.
   * This method assumes the "method" to invoke is a static (class) member method with no return value.
   *
   * @param type the Class type on which the method to invoke is declared and defined.
   * @param methodName a String indicating the name of the method to invoke.
   * @param arguments an array of objects constituting the method's signature as well as the arguments
   * to the method's parameters.
   * @throws IllegalArgumentException if the method with the specified name is not declared on the given class type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @see #getArgumentTypes(Object...)
   * @see #getMethod(Class, String, Class[])
   * @see #invoke(Class, String, Class[], Object[], Class)
   */
  public static void invoke(Class<?> type, String methodName, Object... arguments) {
    invoke(type, methodName, getArgumentTypes(arguments), arguments, Void.class);
  }

  /**
   * Calls the method with the specified name on the given class type, passing the given arguments and casting
   * the method's return value to the desired class type.  This method assumes the "method" to invoke
   * is a static (class) member method.
   *
   * @param <T> the desired return type in which the method's return value will be cast; should be compatible with
   * the method's return type.
   * @param type the Class type on which the method to invoke is declared and defined.
   * @param methodName a String indicating the name of the method to invoke.
   * @param arguments an array of objects constituting the method's signature as well as the arguments
   * to the method's parameters.
   * @param returnType the desired Class type in which to cast the method's return value.
   * @return the specified method's return value cast to the desired return type.
   * @throws IllegalArgumentException if the method with the specified name is not declared on the given class type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @see #getArgumentTypes(Object...)
   * @see #invoke(Class, String, Class[], Object[], Class)
   */
  public static <T> T invoke(Class<?> type, String methodName, Object[] arguments, Class<T> returnType) {
    return invoke(type, methodName, getArgumentTypes(arguments), arguments, returnType);
  }

  /**
   * Calls the method with the specified name and signature on the given class type, passing the given arguments.
   * This method assumes the "method" to invoke is a static (class) member method with no return value.
   *
   * @param type the Class type on which the method to invoke is declared and defined.
   * @param methodName a String indicating the name of the method to invoke.
   * @param parameterTypes an array of Class types corresponding to the method's parameter types that specifies
   * the exact signature (order, type and number of parameters) of the method to invoke.  This is necessary
   * in cases where the method maybe overloaded.
   * @param arguments an array of objects constituting the method's signature as well as the arguments
   * to the method's parameters.
   * @throws IllegalArgumentException if the method with the specified name is not declared on the given class type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @see #getMethod(Class, String, Class[])
   * @see #getMethodSignature(String, Class[], Class)
   * @see #invoke(Object, java.lang.reflect.Method, Object[], Class)
   */
  public static void invoke(Class<?> type, String methodName, Class<?>[] parameterTypes, Object... arguments) {
    try {
      invoke(null, resolveMethod(type, methodName, parameterTypes, arguments, Void.class), arguments, Void.class);
    }
    catch (MethodNotFoundException e) {
      throw new IllegalArgumentException(String.format("No method with signature (%1$s) exists on class type (%2$s)!",
        getMethodSignature(methodName, parameterTypes, Void.class), type.getName()), e);
    }
  }

  /**
   * Calls the method with the specified name and signature on the given class type, passing the given arguments
   * and then the method's return value to the desired class type.  This method assumes the "method" to invoke
   * is a static (class) member method.
   *
   * @param <T> the desired return type in which the method's return value will be cast; should be compatible with
   * the method's return type.
   * @param type the Class type on which the method to invoke is declared and defined.
   * @param methodName a String indicating the name of the method to invoke.
   * @param parameterTypes an array of Class types corresponding to the method's parameter types that specifies
   * the exact signature (order, type and number of parameters) of the method to invoke.  This is necessary
   * in cases where the method maybe overloaded.
   * @param arguments an array of objects constituting the method's signature as well as the arguments
   * to the method's parameters.
   * @param returnType the desired Class type in which to cast the method's return value.
   * @return the specified method's return value cast to the desired return type.
   * @throws IllegalArgumentException if the method with the specified name is not declared on the given class type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @see #getMethod(Class, String, Class[])
   * @see #getMethodSignature(String, Class[], Class)
   * @see #invoke(Object, java.lang.reflect.Method, Object[], Class)
   */
  public static <T> T invoke(Class<?> type, String methodName, Class<?>[] parameterTypes, Object[] arguments,
      Class<T> returnType) {

    try {
      return invoke(null, resolveMethod(type, methodName, parameterTypes, arguments, returnType),
        arguments, returnType);
    }
    catch (MethodNotFoundException e) {
      throw new IllegalArgumentException(String.format("No method with signature (%1$s) exists on class type (%2$s)!",
        getMethodSignature(methodName, parameterTypes, returnType), type.getName()), e);
    }
  }

  /**
   * Calls the method with the specified name on the given object.  This method assumes the "method" to invoke
   * is an instance (object) member method.
   *
   * @param obj the Object on which the method to invoke is defined.
   * @param methodName a String indicating the name of the method to invoke.
   * @throws IllegalArgumentException if the method with the specified name is not declared and defined
   * on the given object's class type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @see #invoke(Object, String, Class[], Object[], Class)
   */
  public static void invoke(Object obj, String methodName) {
    invoke(obj, methodName, null, null, Void.class);
  }

  /**
   * Calls the method with the specified name on the given object, casting the method's return value
   * to the desired class type.  This method assumes the "method" to invoke is an instance (object) member method.
   *
   * @param <T> the desired return type in which the method's return value will be cast; should be compatible with
   * the method's return type.
   * @param obj the Object on which the method to invoke is defined.
   * @param methodName a String indicating the name of the method to invoke.
   * @param returnType the desired Class type in which to cast the method's return value.
   * @return the specified method's return value cast to the desired return type.
   * @throws IllegalArgumentException if the method with the specified name is not declared and defined
   * on the given object's class type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @see #invoke(Object, String, Class[], Object[], Class)
   */
  public static <T> T invoke(Object obj, String methodName, Class<T> returnType) {
    return invoke(obj, methodName, null, null, returnType);
  }

  /**
   * Calls the method with the specified name on the given object, passing the given arguments.
   * This method assumes the "method" to invoke is an instance (object) member method.
   *
   * @param obj the Object on which the method to invoke is defined.
   * @param methodName a String indicating the name of the method to invoke.
   * @param arguments an array of objects constituting the method's signature as well as the arguments
   * to the method's parameters.
   * @throws IllegalArgumentException if the method with the specified name is not declared and defined
   * on the given object's class type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @see #getArgumentTypes(Object...)
   * @see #invoke(Object, String, Class[], Object[], Class)
   */
  public static void invoke(Object obj, String methodName, Object... arguments) {
    invoke(obj, methodName, getArgumentTypes(arguments), arguments, Void.class);
  }

  /**
   * Calls the method with the specified name on the given object, passing the given arguments and casting
   * the method's return value to the desired class type.  This method assumes the "method" to invoke
   * is an instance (object) member method.
   *
   * @param <T> the desired return type in which the method's return value will be cast; should be compatible with
   * the method's return type.
   * @param obj the Object on which the method to invoke is defined.
   * @param methodName a String indicating the name of the method to invoke.
   * @param arguments an array of objects constituting the method's signature as well as the arguments
   * to the method's parameters.
   * @param returnType the desired Class type in which to cast the method's return value.
   * @return the specified method's return value cast to the desired return type.
   * @throws IllegalArgumentException if the method with the specified name is not declared and defined
   * on the given object's class type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @see #getArgumentTypes(Object...)
   * @see #invoke(Object, String, Class[], Object[], Class)
   */
  public static <T> T invoke(Object obj, String methodName, Object[] arguments, Class<T> returnType) {
    return invoke(obj, methodName, getArgumentTypes(arguments), arguments, returnType);
  }

  /**
   * Calls the method with the specified name and signature on the given object, passing the given arguments.
   * This method assumes the "method" to invoke is an instance (object) member method.
   *
   * @param obj the Object on which the method to be invoked is defined.
   * @param methodName a String indicating the name of the method to invoke.
   * @param parameterTypes an array of Class types corresponding to the method's parameter types in order to specify
   * the exact signature (order, type and number) of the method to invoke.  This is necessary in cases where the
   * method maybe overloaded.
   * @param arguments an array of objects constituting the method's signature as well as the arguments
   * to the method's parameters.
   * @throws IllegalArgumentException if the method with the specified name is not declared and defined
   * on the given object's class type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @see #getMethod(Class, String, Class[])
   * @see #getMethodSignature(String, Class[], Class)
   * @see #invoke(Object, java.lang.reflect.Method, Object[], Class)
   */
  public static void invoke(Object obj, String methodName, Class<?>[] parameterTypes, Object... arguments) {
    try {
      invoke(obj, resolveMethod(obj.getClass(), methodName, parameterTypes, arguments, Void.class),
        arguments, Void.class);
    }
    catch (MethodNotFoundException e) {
      throw new IllegalArgumentException(String.format("No method with signature (%1$s) exists on object of type (%2$s)!",
        getMethodSignature(methodName, parameterTypes, Void.class), obj.getClass().getName()), e);
    }
  }

  /**
   * Calls the method with the specified name and signature on the given object, passing the given arguments
   * and casting the method's return value to the desired class type.  This method assumes the "method" to invoke
   * is an instance (object) member method.
   *
   * @param <T> the desired return type in which the method's return value will be cast; should be compatible with
   * the method's return type.
   * @param obj the Object on which the method to be invoked is defined.
   * @param methodName a String indicating the name of the method to invoke.
   * @param parameterTypes an array of Class types corresponding to the method's parameter types in order to specify
   * the exact signature (order, type and number) of the method to invoke.  This is necessary in cases where the
   * method maybe overloaded.
   * @param arguments an array of objects constituting the method's signature as well as the arguments
   * to the method's parameters.
   * @param returnType the desired Class type in which to cast the method's return value.
   * @return the specified method's return value cast to the desired return type.
   * @throws IllegalArgumentException if the method with the specified name is not declared and defined
   * on the given object's class type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @see #getMethod(Class, String, Class[])
   * @see #getMethodSignature(String, Class[], Class)
   * @see #invoke(Object, java.lang.reflect.Method, Object[], Class)
   */
  public static <T> T invoke(Object obj, String methodName, Class<?>[] parameterTypes, Object[] arguments,
      Class<T> returnType) {

    try {
      return invoke(obj, resolveMethod(obj.getClass(), methodName, parameterTypes, arguments, returnType),
        arguments, returnType);
    }
    catch (MethodNotFoundException e) {
      throw new IllegalArgumentException(String.format("No method with signature (%1$s) exists on object of type (%2$s)!",
        getMethodSignature(methodName, parameterTypes, returnType), obj.getClass().getName()), e);
    }
  }

  /**
   * Calls the method with the specified name on the given object, passing the given arguments and casting
   * the method's return value to the desired class type.  If the target object is null, then this method assumes
   * the "method" to invoke is a static (class) member method, otherwise the "method" to invoke is considered
   * an instance (object) member method.
   *
   * @param <T> the desired return type in which the method's return value will be cast; should be compatible with
   * the method's return type.
   * @param target the Object on which the method is defined and will be invoked.
   * @param method the Method to invoke.
   * @param arguments an array of objects constituting the method's signature as well as the arguments
   * to the method's parameters.
   * @param returnType the desired Class type in which to cast the method's return value.
   * @return the specified method's return value cast to the desired return type.
   * @throws MethodInvocationException if the method invocation (call) fails to be executed successfully.
   * @throws NullPointerException if the method or returnType parameter arguments are null.
   * @see #getMethodSignature(String, Class[], Class)
   * @see java.lang.reflect.Method#invoke(Object, Object...)
   * @see java.lang.reflect.Method#setAccessible(boolean)
   */
  public static <T> T invoke(Object target, Method method, Object[] arguments, Class<T> returnType) {
    try {
      boolean currentAccessible = method.isAccessible();
      method.setAccessible(true);
      Object returnValue = method.invoke(target, arguments);
      method.setAccessible(currentAccessible);
      return returnType.cast(returnValue);
    }
    catch (NullPointerException e) {
      throw e;
    }
    catch (Exception e) {
      throw new MethodInvocationException(String.format("Failed to invoke method (%1$s) on %2$s type (%3$s)!",
        getMethodSignature(method), BooleanUtils.toString(target == null, "class", "object of"),
          method.getDeclaringClass().getName()), e);
    }
  }

  /**
   * Domain-specific language (DSL) expression for performing callbacks on the given fields.
   *
   * @param fields the specific collection of Fields on which to operate; maybe null or empty.
   * @return an instance of the WithFields DSL expression.
   * @see org.cp.elements.lang.reflect.ReflectionUtils.WithFields
   * @see org.cp.elements.lang.NullSafe
   * @see org.cp.elements.lang.annotation.DSL
   */
  @DSL @NullSafe
  public static WithFields withFields(Field... fields) {
    return new WithFields(fields);
  }

  /**
   * Domain-specific language (DSL) expression for performing callbacks on the given methods.
   *
   * @param methods the specific collection of Methods on which to operate; maybe null or empty.
   * @return an instance of the WithMethods DSL expression.
   * @see org.cp.elements.lang.reflect.ReflectionUtils.WithMethods
   * @see org.cp.elements.lang.NullSafe
   * @see org.cp.elements.lang.annotation.DSL
   */
  @DSL @NullSafe
  public static WithMethods withMethods(Method... methods) {
    return new WithMethods(methods);
  }

  /**
   * Callback interface for members declared and defined on a given class/object type.
   *
   * @param <T> the specific member class type on which the callback is performed.
   * @see org.cp.elements.lang.reflect.ReflectionUtils.FieldCallback
   * @see org.cp.elements.lang.reflect.ReflectionUtils.MethodCallback
   */
  public interface MemberCallback<T extends Member> {
    void with(T member);
  }

  /**
   * Callback interface for Fields declared and defined on a given class/object type.
   *
   * @see java.lang.reflect.Field
   * @see org.cp.elements.lang.reflect.ReflectionUtils.MemberCallback
   */
  public interface FieldCallback extends MemberCallback<Field> {
  }

  /**
   * Callback interface for Methods declared and defined on a given class/object type.
   *
   * @see java.lang.reflect.Method
   * @see org.cp.elements.lang.reflect.ReflectionUtils.MemberCallback
   */
  public interface MethodCallback extends MemberCallback<Method> {
  }

  /**
   * The WithExpression class defines the domain-specific language (DSL) expression for class/object type
   * member callbacks.
   *
   * @param <T> the specific member class type on which the callback is performed.
   * @see org.cp.elements.lang.DslExtension
   * @see org.cp.elements.lang.reflect.ReflectionUtils.WithFields
   * @see org.cp.elements.lang.reflect.ReflectionUtils.WithMethods
   */
  public static abstract class WithExpression<T extends Member> implements DslExtension {

    private volatile boolean accepted = false;

    private final Filter<T> defaultFilter = (obj) -> (obj != null);

    private Filter<T> filter;

    private final Set<T> members = newMemberSet();

    /* (non-Javadoc) */
    @SuppressWarnings({ "unchecked", "varargs" })
    protected WithExpression(T... members) {
      if (members != null) {
        Collections.addAll(this.members, members);
      }
    }

    /* (non-Javadoc) */
    protected Filter<T> getFilter() {
      return ComposableFilter.and(defaultFilter, filter);
    }

    /* (non-Javadoc) */
    protected Iterable<T> getMembers() {
      return members;
    }

    /* (non-Javadoc) */
    protected boolean accepts(T member) {
      boolean localAccepted = getFilter().accept(member);
      this.accepted |= localAccepted;
      return localAccepted;
    }

    /* (non-Javadoc) */
    public WithExpression<T> call(MemberCallback<T> callback) {
      for (T member : getMembers()) {
        if (accepts(member)) {
          callback.with(member);
        }
      }

      return this;
    }

    /* (non-Javadoc) */
    public WithExpression<T> matching(Filter<T> filter) {
      this.filter = filter;
      return this;
    }

    /* (non-Javadoc) */
    protected abstract T[] members(Class<?> type);

    /* (non-Javadoc) */
    protected Set<T> newMemberSet() {
      return new HashSet<>();
    }

    /* (non-Javadoc) */
    public WithExpression<T> on(Object obj) {
      return on(ClassUtils.getClass(obj));
    }

    /* (non-Javadoc) */
    public WithExpression<T> on(Class<?> type) {
      Assert.notNull(type, "The class type must not be null!");

      while (type != null) {
        Collections.addAll(this.members, members(type));
        type = type.getSuperclass();
      }

      return this;
    }

    /* (non-Javadoc) */
    public WithExpression<T> throwing(RuntimeException e) {
      if (!accepted) {
        throw e;
      }

      return this;
    }
  }

  /**
   * The WithFields class defines the domain-specific language (DSL) expression for class/object type
   * Field callbacks.
   *
   * @see org.cp.elements.lang.reflect.ReflectionUtils.WithExpression
   */
  public static class WithFields extends WithExpression<Field> {

    /* (non-Javadoc) */
    public WithFields(Field... fields) {
      super(fields);
    }

    /**
     * @inheritDoc
     */
    @Override
    protected Field[] members(Class<?> type) {
      return type.getDeclaredFields();
    }

    /**
     * @inheritDoc
     */
    @Override
    protected Set<Field> newMemberSet() {
      return new TreeSet<>(ComparatorUtils.nullSafeDelegatingComparator((Field field1, Field field2) -> {
        String fullyQualfiedFieldOneName = field1.getDeclaringClass().getName().concat(field1.getName());
        String fullyQualifiedFieldTwoName = field2.getDeclaringClass().getName().concat(field2.getName());

        return fullyQualfiedFieldOneName.compareTo(fullyQualifiedFieldTwoName);
      }));
    }
  }

  /**
   * The WithMethods class defines the domain-specific language (DSL) expression for class/object type
   * Method callbacks.
   *
   * @see org.cp.elements.lang.reflect.ReflectionUtils.WithMethods
   */
  public static class WithMethods extends WithExpression<Method> {

    /* (non-Javadoc) */
    public WithMethods(Method... methods) {
      super(methods);
    }

    /**
     * @inheritDoc
     */
    @Override
    protected Method[] members(Class<?> type) {
      return type.getDeclaredMethods();
    }
  }
}
