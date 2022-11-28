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
package org.cp.elements.lang.reflect;

import static org.cp.elements.lang.ElementsExceptionsFactory.newFieldAccessException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newMethodInvocationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newMethodNotFoundException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;

import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.BooleanUtils;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.DslExtension;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.FluentApiExtension;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.support.ComposableFilter;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.ComparatorUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Abstract base class encapsulating operations commonly used with the Java Introspection and Reflection APIs.
 *
 * @author John J. Blum
 * @see java.lang.Class
 * @see java.lang.Object
 * @see java.lang.reflect.Field
 * @see java.lang.reflect.Member
 * @see java.lang.reflect.Method
 * @see java.lang.reflect.Modifier
 * @see java.util.Objects
 * @see java.util.function.BiPredicate
 * @see java.util.function.Function
 * @see java.util.function.Predicate
 * @see org.cp.elements.lang.ClassUtils
 * @see org.cp.elements.lang.DslExtension
 * @see org.cp.elements.lang.FluentApiExtension
 * @see org.cp.elements.lang.annotation.Dsl
 * @see org.cp.elements.lang.annotation.FluentApi
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ReflectionUtils extends ClassUtils {

  /**
   * Determines if the given {@link Method} is {@literal overloaded} in its {@link Class type} hierarchy.
   *
   * @param method {@link MethodReference} referring to the {@link Method} to evaluate.
   * @return a boolean value indicating whether the given {@link Method} is {@literal overloaded}
   * in its {@link Class type} hierarchy.
   * @see org.cp.elements.lang.reflect.ReflectionUtils.MethodReference
   * @see org.cp.elements.lang.reflect.ReflectionUtils.MethodResolver
   * @see #overloadedMethodEquals(MethodReference, Method)
   * @see java.lang.reflect.Method
   */
  @NullSafe
  public static boolean isOverloaded(@Nullable MethodReference method) {
    return method != null && resolveAllMatchingDeclaredMethods(method).stream()
      .anyMatch(declaredMethod -> overloadedMethodEquals(method, declaredMethod));
  }

  /**
   * Determines if the given {@link Method} is {@literal overridden} in its {@link Class type} hierarchy.
   *
   * @param method {@link MethodReference} referring to the {@link Method} to evaluate.
   * @return a boolean value indicating whether the given {@link Method} is {@literal overridden}
   * in its {@link Class type} hierarchy.
   * @see org.cp.elements.lang.reflect.ReflectionUtils.MethodReference
   * @see org.cp.elements.lang.reflect.ReflectionUtils.MethodResolver
   * @see #overriddenMethodEquals(MethodReference, Method)
   * @see java.lang.reflect.Method
   */
  @NullSafe
  public static boolean isOverridden(@Nullable MethodReference method) {
    return method != null && resolveAllMatchingDeclaredMethods(method).stream()
      .anyMatch(declaredMethod -> overriddenMethodEquals(method, declaredMethod));
  }

  private static boolean isMatchingMethods(@NotNull MethodReference methodReference, @NotNull Method method,
      @NotNull BiPredicate<Class<?>[], Class<?>[]> methodParametersEqualityPredicate) {

    // Null Method references are not equal even if both Method references are null
    if (ArrayUtils.noNullElements(methodReference, method)) {

      // Short circuit if the Method references refer to the same Object;
      //  a Method is clearly equal to itself.
      if (methodReference.get() == method) {
        return true;
      }

      // Methods must have the same (equal) "name" (case-sensitive)
      if (methodReference.getName().equals(method.getName())) {

        Class<?> methodOneDeclaredType = methodReference.fromType();
        Class<?> methodTwoDeclaredType = method.getDeclaringClass();

        // Methods must come from the same Class type hierarchy
        // Clearly, two Methods with the same signature (name, then number, order and type of parameters)
        //  cannot be declared in the same Class when overriding
        // Two Methods with the same name but different parameters (number, order or type)
        //  can be declared in the same Class when overloading
        if (assignableTo(methodOneDeclaredType, methodTwoDeclaredType)) {

          Class<?>[] methodOneParameterTypes = methodReference.getParameterTypes();
          Class<?>[] methodTwoParameterTypes = method.getParameterTypes();

          return methodParametersEqualityPredicate.test(methodOneParameterTypes, methodTwoParameterTypes);
        }
      }
    }

    return false;
  }

  /**
   * Determines whether the first {@link Method} is an {@literal overload} of the second {@link Method}.
   *
   * Overloading compliments a {@link Method}.
   *
   * @param methodOne first method {@link Method} in the equality comparison.
   * @param methodTwo second method {@link Method} in the equality comparison.
   * @return a boolean value indicating whether the first {@link Method}
   * is an {@literal overload} of the second {@link Method}.
   * @see #isMatchingMethods(MethodReference, Method, BiPredicate)
   * @see org.cp.elements.lang.reflect.ReflectionUtils.MethodReference
   * @see java.lang.reflect.Method
   */
  protected static boolean overloadedMethodEquals(@NotNull MethodReference methodOne, @NotNull Method methodTwo) {

    // Methods with the same name must have a different number, order or type of parameters.
    BiPredicate<Class<?>[], Class<?>[]> methodParameterEqualityPredicate = Arrays::equals;

    return methodOne.get() != methodTwo
      && isMatchingMethods(methodOne, methodTwo, methodParameterEqualityPredicate.negate());
  }

  /**
   * Determines whether the first {@link Method} is an {@literal override} of the second {@link Method}.
   *
   * Overriding hides a {@link Method}.
   *
   * @param methodOne first method {@link Method} in the equality comparison.
   * @param methodTwo second method {@link Method} in the equality comparison.
   * @return a boolean value indicating whether the first {@link Method}
   * is an {@literal override} of the second {@link Method}.
   * @see org.cp.elements.lang.reflect.ReflectionUtils.MethodReference
   * @see #isMatchingMethods(MethodReference, Method, BiPredicate)
   * @see java.lang.reflect.Method
   */
  @NullSafe
  protected static boolean overriddenMethodEquals(@NotNull MethodReference methodOne, @NotNull Method methodTwo) {
    // Methods with the same name must have the same number, order and type of parameters.
    return methodOne.get() != methodTwo
      && isMatchingMethods(methodOne, methodTwo, Arrays::equals);
  }

  private static List<Method> resolveAllMatchingDeclaredMethods(@NotNull MethodReference methodReference) {

    Predicate<Method> methodPredicate = Objects::nonNull;

    methodPredicate = methodPredicate.and(target -> !target.equals(methodReference.get()));
    methodPredicate = methodPredicate.and(target -> target.getName().equals(methodReference.getName()));

    return resolveAllMatchingDeclaredMethods(methodReference.fromType(), methodPredicate);
  }

  private static List<Method> resolveAllMatchingDeclaredMethods(@NotNull Class<?> type,
      @NotNull Predicate<Method> methodPredicate) {

    List<Method> matchingDeclaredMethods = new ArrayList<>();

    if (isNonNullNonObjectType(type)) {

      Arrays.stream(ArrayUtils.nullSafeArray(type.getDeclaredMethods(), Method.class))
        .filter(methodPredicate)
        .forEach(matchingDeclaredMethods::add);

      for (Class<?> interfaceType : ArrayUtils.nullSafeArray(type.getInterfaces(), Class.class)) {
        matchingDeclaredMethods.addAll(resolveAllMatchingDeclaredMethods(interfaceType, methodPredicate));
      }

      matchingDeclaredMethods.addAll(resolveAllMatchingDeclaredMethods(type.getSuperclass(), methodPredicate));
    }

    return matchingDeclaredMethods;
  }

  private static boolean isNonNullNonObjectType(@Nullable Class<?> type) {
    return !(type == null || Object.class.equals(type));
  }


  /**
   * Determines the {@link Class types} for all the given arguments.
   *
   * @param arguments array of {@link Object arguments} from which to determine their {@link Class types}.
   * @return an array of {@link Class types} for each {@link Object} in the array of arguments, or {@literal null}
   * if the given array is {@literal null}. This method is careful to guard against {@literal null} elements when
   * given a {@literal non-null} array of {@link Object arguments}.
   * @see org.cp.elements.lang.ClassUtils#getClass(Object)
   */
  @NullSafe
  @SuppressWarnings("rawtypes")
  public static Class[] getArgumentTypes(Object... arguments) {

    Class[] argumentTypes = new Class[0];

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
  public static @Nullable <T> T getValue(@NotNull Class<?> type, @NotNull String fieldName,
      @NotNull Class<T> fieldType) {

    try {
      return getValue(null, getField(type, fieldName), fieldType);
    }
    catch (FieldNotFoundException cause) {
      throw newIllegalArgumentException(cause, "Field with name [%s] does not exist on class type [%s]",
        fieldName, type.getName());
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
  public static @Nullable <T> T getValue(@NotNull Object obj, @NotNull String fieldName, @NotNull Class<T> fieldType) {

    try {
      return getValue(obj, getField(obj.getClass(), fieldName), fieldType);
    }
    catch (FieldNotFoundException cause) {
      throw newIllegalArgumentException(cause, "Field with name [%s] does not exist on object of type [%s]",
        fieldName, obj.getClass().getName());
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
   * @param fieldType the desired return type of the field's value; should be compatible with the field's declared type.
   * @return the value of the given field on the given object cast to the desired type.
   * @throws FieldAccessException if the value for the specified field could not be retrieved.
   * @throws NullPointerException if the field or type parameter arguments are null.
   */
  public static @Nullable <T> T getValue(@Nullable Object target, @NotNull Field field, @NotNull Class<T> fieldType) {

    try {
      boolean currentAccessible = field.isAccessible();
      field.setAccessible(true);
      Object value = field.get(target);
      field.setAccessible(currentAccessible);
      return fieldType.cast(value);
    }
    catch (NullPointerException cause) {
      throw cause;
    }
    catch (Exception cause) {
      throw newFieldAccessException(cause, "Failed to get value of field [%s] from %s type [%s]", field.getName(),
        BooleanUtils.toString(target == null, "class", "object of"), getClassName(target));
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
  public static void setField(@NotNull Class<?> type, @NotNull String fieldName, @Nullable Object value) {

    try {
      setField(null, getField(type, fieldName), value);
    }
    catch (FieldNotFoundException cause) {
      throw newIllegalArgumentException(cause, "Field with name [%s] does not exist on class type [%s]",
        fieldName, type.getName());
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
  public static void setField(@NotNull Object obj, @NotNull String fieldName, @Nullable Object value) {

    try {
      setField(obj, getField(obj.getClass(), fieldName), value);
    }
    catch (FieldNotFoundException cause) {
      throw newIllegalArgumentException(cause, "Field with name [%s] does not exist on object of type [%s]",
        fieldName, obj.getClass().getName());
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
  public static void setField(@Nullable Object target, @NotNull Field field, @Nullable Object value) {

    try {
      Assert.isFalse(Modifier.isFinal(field.getModifiers()),
        newFieldAccessException("Cannot set the value of a final field [%s] on %s type [%s]", field.getName(),
          BooleanUtils.toString(target == null, "class", "object of"),
            field.getDeclaringClass().getName()));

      boolean currentAccessible = field.isAccessible();

      field.setAccessible(true);
      field.set(target, value);
      field.setAccessible(currentAccessible);
    }
    catch (FieldAccessException cause) {
      throw cause;
    }
    catch (NullPointerException cause) {
      throw cause;
    }
    catch (Exception cause) {
      throw newFieldAccessException(cause, "Failed to set field [%s] to value [%s] on %s type [%s]", field.getName(),
        value, BooleanUtils.toString(target == null, "class", "object of"),
          field.getDeclaringClass().getName());
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
  public static void invoke(@NotNull Class<?> type, @NotNull String methodName) {
    invoke(type, methodName, null, null, Void.class);
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
  public static void invoke(@NotNull Class<?> type, @NotNull String methodName, Object... arguments) {
    invoke(type, methodName, getArgumentTypes(arguments), arguments, Void.class);
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
  public static @Nullable <T> T invoke(@NotNull Class<?> type, @NotNull String methodName, Class<T> returnType) {
    return invoke(type, methodName, null, null, returnType);
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
  public static @Nullable <T> T invoke(@NotNull Class<?> type, @NotNull String methodName, Object[] arguments,
      Class<T> returnType) {

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
  public static void invoke(@NotNull Class<?> type, @NotNull String methodName, Class<?>[] parameterTypes,
      Object... arguments) {

    invoke(null, resolveMethod(type, methodName, parameterTypes, arguments, Void.class), arguments, Void.class);
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
  public static <T> T invoke(@NotNull Class<?> type, @NotNull String methodName,
      Class<?>[] parameterTypes, Object[] arguments, Class<T> returnType) {

    try {
      return invoke(null, resolveMethod(type, methodName, parameterTypes, arguments, returnType),
        arguments, returnType);
    }
    catch (MethodNotFoundException cause) {
      throw newIllegalArgumentException(cause, "No method with signature [%s] exists on class type [%s]",
        getMethodSignature(methodName, parameterTypes, returnType), type.getName());
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
  public static void invoke(@NotNull Object obj, @NotNull String methodName) {
    invoke(obj, methodName, null, null, Void.class);
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
  public static void invoke(@NotNull Object obj, @NotNull String methodName, Object... arguments) {
    invoke(obj, methodName, getArgumentTypes(arguments), arguments, Void.class);
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
  public static @Nullable <T> T invoke(@NotNull Object obj, @NotNull String methodName, Class<T> returnType) {
    return invoke(obj, methodName, null, null, returnType);
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
  public static @Nullable <T> T invoke(@NotNull Object obj, @NotNull String methodName, Object[] arguments,
      Class<T> returnType) {

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
  public static void invoke(@NotNull Object obj, @NotNull String methodName, Class<?>[] parameterTypes,
      Object... arguments) {

    invoke(obj, resolveMethod(obj.getClass(), methodName, parameterTypes, arguments, Void.class),
      arguments, Void.class);
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
  public static @Nullable <T> T invoke(@NotNull Object obj, @NotNull String methodName,
      Class<?>[] parameterTypes, Object[] arguments, Class<T> returnType) {

    try {
      return invoke(obj, resolveMethod(obj.getClass(), methodName, parameterTypes, arguments, returnType),
        arguments, returnType);
    }
    catch (MethodNotFoundException cause) {
      throw newIllegalArgumentException(cause, "No method with signature [%s] exists on object of type [%s]",
        getMethodSignature(methodName, parameterTypes, returnType), obj.getClass().getName());
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
  public static <T> T invoke(@NotNull Object target, @NotNull Method method, Object[] arguments, Class<T> returnType) {

    try {
      boolean currentAccessible = method.isAccessible();
      method.setAccessible(true);
      Object returnValue = method.invoke(target, arguments);
      method.setAccessible(currentAccessible);
      return returnType.cast(returnValue);
    }
    catch (NullPointerException cause) {
      throw cause;
    }
    catch (Exception cause) {
      throw newMethodInvocationException(cause, "Failed to invoke method [%s] on %s type [%s]",
        getMethodSignature(method), BooleanUtils.toString(target == null, "class", "object of"),
          method.getDeclaringClass().getName());
    }
  }

  /**
   * {@link FluentApi} object interface for performing callbacks on the given {@link Field fields}.
   *
   * @param fields collection of {@link Field Fields} on which to operate; maybe {@literal null} or empty.
   * @return a new instance of the {@link WithFields} {@link FluentApi} object interface.
   * @see org.cp.elements.lang.reflect.ReflectionUtils.WithFields
   * @see org.cp.elements.lang.annotation.FluentApi
   * @see NullSafe
   */
  @Dsl
  @NullSafe
  public static WithFields withFields(Field... fields) {
    return new WithFields(fields);
  }

  /**
   * {@link FluentApi} object interface for performing callbacks on the given {@link Method methods}.
   *
   * @param methods collection of {@link Method Methods} on which to operate; maybe {@literal null} or empty.
   * @return a new instance of the {@link WithMethods} {@link FluentApi} object interface.
   * @see org.cp.elements.lang.reflect.ReflectionUtils.WithMethods
   * @see org.cp.elements.lang.annotation.FluentApi
   * @see NullSafe
   */
  @Dsl
  @NullSafe
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

    /**
     * Performs a function on the given {@link Member}.
     *
     * @param member {@link Member} to process.
     * @see java.lang.reflect.Member
     */
    void with(T member);

  }

  /**
   * Callback interface for Fields declared and defined on a given class/object type.
   *
   * @see java.lang.reflect.Field
   * @see org.cp.elements.lang.reflect.ReflectionUtils.MemberCallback
   */
  public interface FieldCallback extends MemberCallback<Field> { }

  /**
   * Callback interface for Methods declared and defined on a given class/object type.
   *
   * @see java.lang.reflect.Method
   * @see org.cp.elements.lang.reflect.ReflectionUtils.MemberCallback
   */
  public interface MethodCallback extends MemberCallback<Method> { }

  /**
   * ADT used to resolve a {@link Method} by {@link String name} from a given, required {@link Class type}.
   *
   * @see org.cp.elements.lang.annotation.FluentApi
   * @see org.cp.elements.lang.FluentApiExtension
   * @see org.cp.elements.lang.DslExtension
   */
  @FluentApi
  public static class MethodResolver implements DslExtension, FluentApiExtension {

    /**
     * Factory method used to construct a new instance of {@link MethodResolver} initialized with
     * the given, required {@link Class type} used to resolve the {@link Method} by {@link String name}.
     *
     * @param type {@link Class type} from which to resolve the {@link Method} by {@link String name};
     * must not be {@literal null}.
     * @return a new {@link MethodResolver}.
     * @throws IllegalArgumentException if the {@link Class type} is {@literal null}.
     * @see #MethodResolver(Class)
     */
    @Dsl
    public static @NotNull MethodResolver fromType(@NotNull Class<?> type) {
      return new MethodResolver(type);
    }

    private static @NotNull <T> T requireNonNull(@NotNull T target, String message, Object... args) {
      Assert.notNull(target, message, args);
      return target;
    }

    private final Class<?> referenceType;

    /**
     * Constructs a new instance of {@link MethodResolver} initialized with the given, required {@link Class type}
     * from which to resolve the {@link Method} by {@link String name}.
     *
     * @param sourceType {@link Class} from which to resolve the {@link Method} by {@link String name};
     * must not be {@literal null}.
     * @throws IllegalArgumentException if the {@link Class type} is {@literal null}.
     */
    protected MethodResolver(@NotNull Class<?> sourceType) {
      this.referenceType = requireNonNull(sourceType, "Reference type is required");
    }

    /**
     * Returns the {@link Class type} from which the {@link Method} by {{@link String name} will be resolved.
     *
     * @return the {@link Class type} from which the {@link Method} by {{@link String name} will be resolved.
     */
    protected @NotNull Class<?> getReferenceType() {
      return this.referenceType;
    }

    /**
     * {@link Dsl} based builder method used to configure the {@link String name} of the {@link Method} to resolve.
     *
     * @param methodName {@link String} containing the {@literal name} of the {@link Method} to resolve;
     * must not be {@literal null} or {@literal empty}.
     * @return this {@link MethodReference}.
     * @throws IllegalArgumentException if the {@link String method name} is {@literal null} or {@literal empty}.
     */
    @Dsl
    public @NotNull MethodReference havingName(@NotNull String methodName) {
      Assert.hasText(methodName, "Method name [%s] is required", methodName);
      return new MethodReference(getReferenceType(), methodName);
    }
  }

  /**
   * ADT used to refer to a {@link Method}.
   *
   * @see org.cp.elements.lang.annotation.FluentApi
   * @see org.cp.elements.lang.FluentApiExtension
   * @see org.cp.elements.lang.DslExtension
   * @see org.cp.elements.lang.Nameable
   */
  @FluentApi
  public static final class MethodReference implements DslExtension, FluentApiExtension, Nameable<String> {

    private final AtomicReference<Method> methodReference = new AtomicReference<>(null);

    private final Class<?> referenceType;

    private Class<?>[] parameterTypes;

    private final String methodName;

    private final Function<Class<?>, Method> safeGetMethod = type -> {

      String methodName = getName();
      Class<?>[] parameterTypes = getParameterTypes();

      try {
        return type.getMethod(methodName, parameterTypes);
      }
      catch (NoSuchMethodException cause) {
        throw newMethodNotFoundException(cause, "Method [%s] with parameters of type [%s] not found",
          methodName, Arrays.stream(parameterTypes).map(Class::getName).collect(Collectors.toList()));
      }
    };

    /**
     * Default constructor used for testing/mocking purposes only!
     */
    private MethodReference() {
      this.referenceType = null;
      this.methodName = null;
    }

    /**
     * Constructs a new instance of {@link MethodReference} initialized with the given, required {@link Class type}
     * from which the {@link Method} will be resolved and {@link String method name}
     * used to identify the {@link Method}.
     *
     * @param referenceType {@link Class type} from which the {@link Method} by {@link String name} is resolved.
     * @param methodName {@link String} containing the {@literal name} of the {@link Method} to resolve.
     */
    private MethodReference(@NotNull Class<?> referenceType, @NotNull String methodName) {
      this.referenceType = referenceType;
      this.methodName = methodName;
    }

    /**
     * Gets the {@link String name} of the resolve {@link Method}.
     *
     * @return the {@link String name} of the resolve {@link Method}.
     */
    public @NotNull String getName() {
      return this.methodName;
    }

    /**
     * Gets an array containing the {@link Method Method's} parameter {@link Class types}.
     *
     * The number, order and {@link Class types} (along with the {@link Method Method's} {@link String name})
     * constitutes the {@link Method Method's} signature.
     *
     * @return an array containing the {@link Method Method's} parameter {@link Class types}.
     * @see #getName()
     */
    Class<?>[] getParameterTypes() {
      return ArrayUtils.nullSafeArray(this.parameterTypes, Class.class);
    }

    /**
     * Gets the {@link Class type} from which this {@link Method} was resolved.
     *
     * @return the {@link Class type} from which this {@link Method} was resolved.
     */
    public @NotNull Class<?> fromType() {
      Class<?> referenceType = this.referenceType;
      return referenceType != null ? referenceType : get().getDeclaringClass();
    }

    /**
     * Resolves the {@link Method} referred to by this {@link MethodReference}.
     *
     * @return the resolved {@link Method}.
     * @see java.lang.reflect.Method
     */
    public @NotNull Method get() {
      return this.methodReference.updateAndGet(method -> method != null ? method
        : this.safeGetMethod.apply(this.referenceType));
    }

    /**
     * {@link Dsl} based build method to configure the array of {@link Class parameter types} used in
     * identifying and resolving the {@link Method} by signature.
     *
     * @param parameterTypes array of {@link Class types} constituting the {@link Method Method's} signature.
     * @return this {@link MethodReference}.
     */
    @Dsl
    public @NotNull MethodReference withParameterTypes(Class<?>... parameterTypes) {
      this.parameterTypes = parameterTypes;
      return this;
    }
  }

  /**
   * The {@link WithExpression} class defines the {@link FluentApi} object interface for {@link Class}
   * and {@link Object} type member callbacks.
   *
   * @param <T> member {@link Class} type on which the callback is performed.
   * @see org.cp.elements.lang.reflect.ReflectionUtils.WithFields
   * @see org.cp.elements.lang.reflect.ReflectionUtils.WithMethods
   * @see org.cp.elements.lang.FluentApiExtension
   */
  @FluentApi
  public abstract static class WithExpression<T extends Member> implements FluentApiExtension {

    private volatile boolean accepted;

    private final Filter<T> defaultFilter = Objects::nonNull;

    private Filter<T> filter;

    private final Set<T> members = newMemberSet();

    /**
     * Constructs a new instance of {@link WithExpression} initialized with the given array of {@link Member Members}
     * to process.
     *
     * @param members array of {@link Member Members} to process.
     * @see java.lang.reflect.Member
     */
    @SuppressWarnings({ "unchecked", "varargs" })
    protected WithExpression(T... members) {
      if (members != null) {
        Collections.addAll(this.members, members);
      }
    }

    /**
     * Gets the configured {@link Filter} used to filter the {@link Member Members}.
     *
     * @return the configured {@link Filter} used to filter the {@link Member Members}.
     * @see org.cp.elements.lang.Filter
     */
    protected @NotNull Filter<T> getFilter() {
      return ComposableFilter.and(this.defaultFilter, this.filter);
    }

    /**
     * Gets the {@link Member Members} to process.
     *
     * @return an {@link Iterable} over the {@link Member Members} to process.
     * @see java.lang.reflect.Member
     * @see java.lang.Iterable
     */
    protected @NotNull Iterable<T> getMembers() {
      return this.members;
    }

    /**
     * Evaluates whether the given {@link Member} is accepted by this expression.
     *
     * @param member {@link Member} to evaluate.
     * @return a boolean value indicating whether the given {@link Member} is accepted by this expression.
     * @see #getFilter()
     */
    protected boolean accepts(T member) {
      boolean localAccepted = getFilter().accept(member);
      synchronized (this) {
        this.accepted |= localAccepted;
      }
      return localAccepted;
    }

    /**
     * Builder methods used to call the {@link MemberCallback} on all {@link Member Members}
     * configured in this expression.
     *
     * @param callback {@link MemberCallback} used to process the {@link Member Members}
     * configured in this expression.
     * @return this {@link WithExpression}.
     * @see org.cp.elements.lang.reflect.ReflectionUtils.MemberCallback
     */
    @Dsl
    public @NotNull WithExpression<T> call(@NotNull MemberCallback<T> callback) {

      StreamUtils.stream(getMembers())
        .filter(this::accepts)
        .forEach(callback::with);

      return this;
    }

    /**
     * Builder method used to configure the {@link Filter} used to match {@link Member Members} to process.
     *
     * @param filter {@link Filter} used to match {@link Member Members} to process.
     * @return this {@link WithExpression}.
     * @see org.cp.elements.lang.Filter
     */
    @Dsl
    public @NotNull WithExpression<T> matching(@Nullable Filter<T> filter) {
      this.filter = filter;
      return this;
    }

    /**
     * Gets an array of {@link Member Members} of the given {@link Class type}.
     *
     * @param type {@link Class type} of {@link Member Members} to return in the array.
     * @return an array of {@link Member Members} of the given {@link Class type}.
     */
    protected abstract T[] members(Class<?> type);

    /**
     * Constructs a new {@link Set} to hold {@link Member} objects to process.
     *
     * @return a new {@link Set} to hold {@link Member} objects to process.
     * @see java.util.Set
     */
    protected Set<T> newMemberSet() {
      return new HashSet<>();
    }

    /**
     * Builder method used to extract {@link Member Members} from the given, required {@link Object}
     * targeted to be processed.
     *
     * @param obj {@link Object} from which to extract {@link Member Members} to process.
     * @return this {@link WithExpression}.
     */
    @Dsl
    public @NotNull WithExpression<T> on(@NotNull Object obj) {
      return on(ClassUtils.getClass(obj));
    }

    /**
     * Builder method used to extract {@link Member Members} from the given, required {@link Class type}
     * targeted to be processed.
     *
     * @param type {@link Class type} from which to extract {@link Member Members} to process.
     * @return this {@link WithExpression}.
     * @throws IllegalArgumentException if the {@link Class type} is {@literal null}.
     */
    @Dsl
    public @NotNull WithExpression<T> on(@NotNull Class<?> type) {

      Assert.notNull(type, "Class type must not be null");

      Class<?> typeToProcess = type;

      while (typeToProcess != null) {
        Collections.addAll(this.members, members(typeToProcess));
        typeToProcess = typeToProcess.getSuperclass();
      }

      return this;
    }

    /**
     * Throws the given {@link RuntimeException} if the {@link Member Members} are not accepted by this expression.
     *
     * @param cause {@link RuntimeException} to throw if the {@link Member Members} are not accepted by this expression.
     * @return this {@link WithExpression}
     */
    @Dsl
    @SuppressWarnings("all")
    public @NotNull WithExpression<T> throwing(@NotNull RuntimeException cause) {

      if (!this.accepted) {
        throw cause;
      }

      return this;
    }
  }

  /**
   * The {@link WithFields} class defines the {@link FluentApi} object interface for {@link Class} and {@link Object}
   * type {@link Field} callbacks.
   *
   * @see org.cp.elements.lang.reflect.ReflectionUtils.WithExpression
   */
  @FluentApi
  public static class WithFields extends WithExpression<Field> {

    /**
     * Constructs a new instance of {@link WithFields} initialized with an array of {@link Field Fields} to process.
     *
     * @param fields array of {@link Field Fields} to process.
     * @see java.lang.reflect.Field
     */
    public WithFields(Field... fields) {
      super(fields);
    }

    /**
     * Returns all {@link Field} {@link Member Members} declared on the given {@link Class} type.
     *
     * @param type {@link Class} type declaring the {@link Field Fields} to return.
     * @return an array of {@link Field Fields} declared on the given {@link Class} type.
     * @see java.lang.Class#getDeclaredFields()
     * @see java.lang.reflect.Field
     */
    @Override
    protected Field[] members(@NotNull Class<?> type) {
      return type.getDeclaredFields();
    }

    /**
     * Constructs a {@link TreeSet} to hold the {@link Member Members} to process.
     *
     * @return a new {@link TreeSet}.
     * @see java.util.TreeSet
     */
    @Override
    protected @NotNull Set<Field> newMemberSet() {

      return new TreeSet<>(ComparatorUtils.nullSafeArgumentsComparator((Field field1, Field field2) -> {

        String fullyQualifiedFieldOneName = field1.getDeclaringClass().getName().concat(field1.getName());
        String fullyQualifiedFieldTwoName = field2.getDeclaringClass().getName().concat(field2.getName());

        return fullyQualifiedFieldOneName.compareTo(fullyQualifiedFieldTwoName);
      }));
    }
  }

  /**
   * The {@link WithMethods} class defines the {@link FluentApi} object interface for {@link Class} and {@link Object}
   * type {@link Method} callbacks.
   *
   * @see org.cp.elements.lang.reflect.ReflectionUtils.WithMethods
   */
  @FluentApi
  public static class WithMethods extends WithExpression<Method> {

    /**
     * Constructs a new instance of {@link WithMethods} initialized with an array of {@link Method Methods} to process.
     *
     * @param methods array of {@link Method Methods} to process.
     * @see java.lang.reflect.Method
     */
    public WithMethods(Method... methods) {
      super(methods);
    }

    /**
     * Returns all {@link Method} {@link Member Members} declared on the given {@link Class} type.
     *
     * @param type {@link Class} type declaring the {@link Method Methods} to return.
     * @return an array of {@link Method Methods} declared on the given {@link Class} type.
     * @see java.lang.Class#getDeclaredFields()
     * @see java.lang.reflect.Field
     */
    @Override
    protected Method[] members(@NotNull Class<?> type) {
      return type.getDeclaredMethods();
    }
  }
}
