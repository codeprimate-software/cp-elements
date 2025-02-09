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

import static org.cp.elements.lang.ElementsExceptionsFactory.newConstructorNotFoundException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newIllegalTypeException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newObjectInstantiationException;
import static org.cp.elements.lang.ObjectUtils.safeGetValue;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.util.stream.StreamUtils.stream;

import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.reflect.ConstructorNotFoundException;
import org.cp.elements.lang.reflect.FieldNotFoundException;
import org.cp.elements.lang.reflect.MethodNotFoundException;
import org.cp.elements.lang.reflect.ModifierUtils;
import org.cp.elements.util.ArrayUtils;

/**
 * Abstract utility class providing methods for processing {@link Class} types.
 *
 * @author John J. Blum
 * @see java.lang.Class
 * @see java.lang.Object
 * @see java.lang.annotation.Annotation
 * @see java.lang.reflect.AnnotatedElement
 * @see java.lang.reflect.Constructor
 * @see java.lang.reflect.Field
 * @see java.lang.reflect.Method
 * @see java.lang.reflect.ParameterizedType
 * @see java.lang.reflect.Type
 * @see java.lang.reflect.TypeVariable
 * @see java.net.URL
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ClassUtils {

  protected static final boolean DEFAULT_INITIALIZE_LOADED_CLASS = true;

  @SuppressWarnings("rawtypes")
  public static final Class[] EMPTY_CLASS_ARRAY = new Class[0];

  @SuppressWarnings("rawtypes")
  public static final Constructor[] EMPTY_CONSTRUCTOR_ARRAY = new Constructor[0];

  public static final Field[] EMPTY_FIELD_ARRAY = new Field[0];

  public static final Method[] EMPTY_METHOD_ARRAY = new Method[0];

  public static final String CLASS_FILE_EXTENSION = ".class";
  public static final String CLONE_METHOD_NAME = "clone";
  public static final String GETTER_METHOD_NAME_PREFIX = "get";
  public static final String IS_METHOD_NAME_PREFIX = "is";
  public static final String MAIN_METHOD_NAME = "main";
  public static final String SETTER_METHOD_NAME_PREFIX = "set";

  /**
   * Determines whether a given {@link Class type} is assignable to a declared {@link Class type}.
   * <p>
   * A given {@link Class type} is assignable to a declared {@link Class type} if it is in the same {@link Class type}
   * hierarchy, i.e. the given {@link Class type} is a {@link Class subclass}, or sub-interface of the declared
   * {@link Class type}. {@literal Null} is also assignable to the declared (to) {@link Class type}.
   *
   * @param fromType {@link Class type} evaluated for assignment compatibility with the declared {@link Class type}.
   * @param toType declared {@link Class type} defining the type hierarchy, or bounds on the given {@link Class type}
   * for assignment compatibility.
   * @return a boolean value indicating given {@link Class type} is assignable to the declared {@link Class type}.
   * @see java.lang.Class#isAssignableFrom(Class)
   * @see #instanceOf(Object, Class)
   */
  @NullSafe
  public static boolean assignableTo(@Nullable Class<?> fromType, @Nullable Class<?> toType) {
    return (toType != null && (fromType == null || toType.isAssignableFrom(fromType)));
  }

  /**
   * Casts the given {@link Object} to the given, required {@link Class type}.
   * <p>
   * It is safe to cast {@literal null} to an {@literal instance of} the given, required {@link Class type}.
   *
   * @param <T> {@link Class type} of the cast.
   * @param target {@link Object} to cast as an instance of the given, required {@link Class type}.
   * @param type {@link Class} used to cast the {@link Object} to; must not be {@literal null}.
   * @return the given {@link Object} cast to an instance of the given, required {@link Class type}.
   * @throws IllegalArgumentException if the {@link Class type} is {@literal null}.
   * @throws IllegalTypeException if the {@link Object} has value and is not an instance of {@link Class type}.
   */
  public static @Nullable <T> T castTo(@Nullable Object target, @NotNull Class<T> type) {

    Assert.notNull(type, "The Class type used to cast is required");

    Assert.isTrue(target == null || type.isInstance(target),
      newIllegalTypeException("Object [%s] is not an instance of Class [%s]", target, getName(type)));

    return type.cast(target);
  }

  /**
   * Constructs an {@link Object} of the given, required {@link Class type}.
   *
   * @param <T> {@link Class type} of {@link Object} to construct.
   * @param type {@link Class type} to construct (instantiate).
   * @param arguments array of {@link Object arguments} to the {@link Constructor}.
   * @return a new instance of {@link Object} of the given {@link Class type}.
   * @throws org.cp.elements.lang.factory.ObjectInstantiationException if an {@link Object} of {@link Class type}
   * could not be constructed.
   * @see #findConstructor(Class, Object...)
   * @see #findDefaultConstructor(Class)
   * @since 2.0.0
   */
  public static <T> T construct(@NotNull Class<T> type, Object... arguments) {

    try {
      return Optional.ofNullable(findConstructor(type, arguments))
        .orElseGet(() -> findDefaultConstructor(type))
        .newInstance(arguments);
    }
    catch (IllegalArgumentException cause) {
      throw newObjectInstantiationException(cause, "Failed to construct object of type [%1$s] with arguments %2$s",
        getName(type), Arrays.toString(arguments));
    }
    catch (Throwable cause) {
      throw newObjectInstantiationException(cause, "Failed to construct object of type [%s]", getName(type));
    }
  }

  /**
   * Get the {@link Class type} of the specified {@link Object}.
   * <p>
   * Returns {@literal null} if the {@link Object} reference is {@literal null}.
   *
   * @param obj {@link Object} who's {@link Object#getClass() type} is being determined.
   * @return a {@link Class} classifying the type of the given {@link Object}.
   * @see java.lang.Object#getClass()
   */
  @NullSafe
  public static @Nullable Class<?> getClass(@Nullable Object obj) {
    return obj != null ? obj.getClass() : null;
  }

  /**
   * Gets the {@link String fully-qualified name} of the {@link Class type} for the given {@link Object}.
   * <p>
   * Returns {@literal null} if the {@link Object} reference is {@literal null}.
   *
   * @param obj {@link Object} who's {@link Class#getName() class name} is determined.
   * @return a {@link String} specifying the {@link Class#getName() fully-qualified class name} of the {@link Object}.
   * @see java.lang.Object#getClass()
   * @see java.lang.Class#getName()
   */
  @NullSafe
  public static @Nullable String getClassName(@Nullable Object obj) {
    return obj != null ? obj.getClass().getName() : null;
  }

  /**
   * Gets the {@link Class#getSimpleName() unqualified, simple name} of the {@link Class type}
   * for the given {@link Object}.
   * <p>
   * Returns {@literal null} if the {@link Object} reference is {@literal null}.
   *
   * @param obj {@link Object} who's {@link Class#getSimpleName() simple class name} is determined.
   * @return a {@link String} specifying the {@link Class#getSimpleName() simple class name} of the {@link Object}.
   * @see java.lang.Class#getSimpleName()
   * @see java.lang.Object#getClass()
   */
  @NullSafe
  public static @Nullable String getClassSimpleName(@Nullable Object obj) {
    return obj != null ? obj.getClass().getSimpleName() : null;
  }

  /**
   * Determines all interfaces implemented by the given {@link Object}'s {@link Class type}.
   * <p>
   * This method performs a deep analysis of the object's {@link Class} along with all interfaces
   * implemented by the object's {@link Class superclass}, up the {@link Class} hierarchy until
   * the {@link Object} class is reached.
   *
   * @param obj {@link Object} to evaluate.
   * @return all interfaces implemented by the given object's {@link Class} and its {@link Class superclass}.
   * Returns an empty {@link Set} if the given {@link Object} does not implement any interfaces.
   * @see #getInterfaces(Class)
   * @see #getClass(Object)
   * @see java.lang.Object
   */
  @NullSafe
  public static Set<Class<?>> getInterfaces(@Nullable Object obj) {
    return getInterfaces(getClass(obj));
  }

  /**
   * Determines all the interfaces implemented by the given {@link Class type}.
   * <p>
   * This method performs a deep analysis of all the interfaces implemented by the given {@link Class} type
   * along with interfaces implemented by the {@link Class Class's} {@link Class superclass},
   * up the {@link Class} hierarchy until the {@link Object} class is reached.
   *
   * @param type {@link Class} to evaluate.
   * @return all interfaces implemented by the given {@link Class} and its {@link Class superclass}.
   * Returns an empty {@link Set} if the given {@link Class} does not implement any interfaces.
   * @see #getInterfaces(Class, Set)
   * @see java.lang.Class
   */
  @NullSafe
  public static Set<Class<?>> getInterfaces(@Nullable Class<?> type) {

    return Optional.ofNullable(type)
      .map(theType -> getInterfaces(type, new HashSet<>()))
      .orElse(Collections.emptySet());
  }

  /**
   * Determines all the interfaces implemented by the given, required {@link Class type}.
   * <p>
   * This method performs a deep analysis of all the interfaces implemented by the given {@link Class} type
   * along with interfaces implemented by the {@link Class Class's} {@link Class superclass},
   * up the {@link Class} hierarchy until the {@link Object} class is reached.
   *
   * @param type {@link Class} to evaluate; must not be {@literal null}.
   * @param interfaces {@link Set} containing all the {@link Class interfaces} implemented by
   * the given {@link Class} type; must not be {@literal null}.
   * @return all interfaces implemented by the given {@link Class} and its superclass.
   * Returns an empty {@link Set} if the given {@link Class} does not implement any interfaces.
   * @see java.lang.Class
   */
  private static Set<Class<?>> getInterfaces(@NotNull Class<?> type, @NotNull Set<Class<?>> interfaces) {

    Class<?> superclass = type.getSuperclass();

    if (!(superclass == null || Object.class.equals(superclass))) {
      getInterfaces(superclass, interfaces);
    }

    for (Class<?> implementedType : type.getInterfaces()) {
      if (!interfaces.contains(implementedType)) {
        interfaces.add(implementedType);
        getInterfaces(implementedType, interfaces);
      }
    }

    return interfaces;
  }

  /**
   * Attempts to find a compatible {@link Constructor} on the given {@link Class type} with a signature
   * having {@link Class parameter types} satisfying the given {@link Object arguments}.
   *
   * @param <T> generic {@link Class type} to search for the {@link Constructor}.
   * @param type {@link Class type} to search for the desired {@link Constructor}.
   * @param arguments an array of {@link Object arguments} used to match the {@link Constructor}'s signature.
   * @return a {@link Constructor} from the given {@link Class type} whose signature matches
   * the given {@link Object arguments}.
   * @see java.lang.Class#getDeclaredConstructors()
   * @see java.lang.reflect.Constructor
   * @see java.lang.Class
   */
  @SuppressWarnings({ "unchecked" })
  public static @Nullable <T> Constructor<T> findConstructor(@NotNull Class<T> type, Object... arguments) {

    for (Constructor<?> constructor : type.getDeclaredConstructors()) {

      Class<?>[] parameterTypes = constructor.getParameterTypes();

      if (ArrayUtils.nullSafeLength(arguments) == parameterTypes.length) {

        boolean match = true;

        for (int index = 0; match && index < parameterTypes.length; index++) {
          match = instanceOf(arguments[index], parameterTypes[index]);
        }

        if (match) {
          return (Constructor<T>) constructor;
        }
      }
    }

    return null;
  }

  /**
   * Finds the default, public no-argument {@link Constructor} from the given, required {@link Class type}.
   *
   * @param <T> {@link Class type} to evaluate.
   * @param type {@link Class type} used to find the default, public no-argument {@link Constructor};
   * must not be {@literal null}.
   * @return the default, public no-argument {@link Constructor} for the given {@link Class type}.
   * @throws ConstructorNotFoundException if the given {@link Class type} does not contain a default,
   * public no-argument {@link Constructor}.
   * @see java.lang.Class
   * @since 2.0.0
   */
  public static @NotNull <T> Constructor<T> findDefaultConstructor(@NotNull Class<T> type) {

    return Optional.ofNullable(findConstructor(type))
      .filter(ModifierUtils::isPublic)
      .orElseThrow(() -> newConstructorNotFoundException(
        "Failed to find a default, public no-argument constructor for type [%s]", getName(type)));
  }

  /**
   * Gets the {@link Constructor} with the given signature from the given {@link Class type}.
   *
   * @param <T> generic {@link Class type} from which to get the {@link Constructor}.
   * @param type {@link Class type} from which to get the {@link Constructor}.
   * @param parameterTypes array of {@link Class parameter types} matching the {@link Constructor} signature.
   * @return a {@link Constructor} from the given {@link Class type} with a matching signature.
   * @see java.lang.Class#getDeclaredConstructor(Class[])
   * @see java.lang.reflect.Constructor
   * @see java.lang.Class
   */
  public static @NotNull <T> Constructor<T> getConstructor(@NotNull Class<T> type, Class<?>... parameterTypes) {

    try {
      return type.getDeclaredConstructor(parameterTypes);
    }
    catch (NoSuchMethodException cause) {
      throw new ConstructorNotFoundException(cause);
    }
  }

  /**
   * Attempts to resolve the {@link Constructor} from the given {@link Class type} based on the {@link Constructor}'s
   * exact signature, otherwise finds a {@link Constructor} having a signature with the given
   * {@link Class parameter types} satisfying the given array of {@link Object arguments}.
   *
   * @param <T> generic {@link Class type} from which to resolve the {@link Constructor}.
   * @param type {@link Class type} from which to resolve the {@link Constructor}; must not be {@literal null}.
   * @param parameterTypes array of {@link Class types} specifying the desired {@link Constructor}'s signature.
   * @param arguments array of {@link Object arguments} used to match the {@link Constructor}'s signature.
   * @return a {@link Constructor} from the given {@link Class type} whose signature either matches
   * the given {@link Class parameter types} or satisfies the array of {@link Object arguments}.
   * @see #getConstructor(Class, Class[])
   * @see #findConstructor(Class, Object...)
   * @see java.lang.reflect.Constructor
   * @see java.lang.Class
   */
  public static @NotNull <T> Constructor<T> resolveConstructor(@NotNull Class<T> type, Class<?>[] parameterTypes,
      Object... arguments) {

    try {
      return getConstructor(type, parameterTypes);
    }
    catch (ConstructorNotFoundException cause) {

      Constructor<T> constructor = findConstructor(type, arguments);

      String message = String.format("Failed to resolve constructor with signature [%1$s] on class type [%2$s]",
        getMethodSignature(getSimpleName(type), parameterTypes, Void.class), getName(type));

      Assert.notNull(constructor, new ConstructorNotFoundException(message, cause.getCause()));

      return constructor;
    }
  }

  /**
   * Gets all the declared {@link Field fields} on the given {@link Object}.
   *
   * @param target {@link Object} to evaluate.
   * @return an array of {@link Field Fields} for all fields declared by the {@link Object Object's} {@link Class type}
   * and its {@link Class supertypes} in the {@link Class} hierarchy. If no {@link Field Fields} are declared, then
   * this method returns an empty {@link Field} array.
   * @see #getAllDeclaredFields(Class)
   */
  public static Field[] getAllDeclaredFields(@Nullable Object target) {
    return target != null ? getAllDeclaredFields(target.getClass()) : EMPTY_FIELD_ARRAY;
  }

  /**
   * Gets all the declared {@link Field fields} on the given {@link Class type}.
   * <p>
   * This method recursively searches up the {@link Class} hierarchy from the given {@link Class type},
   * introspecting each {@link Class superclass types} until the {@link Object} {@link Class} is reached.
   *
   * @param type {@link Class} to evaluate.
   * @return an array of {@link Field Fields} for all fields declared by the {@link Object Object's} {@link Class type}
   * including all {@link Class superclass types} in the {@link Class} hierarchy. If no {@link Field Fields}
   * were declared, then this method returns an empty {@link Field} array.
   * @throws IllegalArgumentException if the {@link Class type} is {@literal null}.
   * @see #getAllDeclaredFields(Class)
   */
  public static Field[] getAllDeclaredFields(@NotNull Class<?> type) {

    Assert.notNull(type, "Class type is required");

    List<Field> fields = new ArrayList<>(Arrays.asList(type.getDeclaredFields()));

    if (type.getSuperclass() != null) {
      fields.addAll(Arrays.asList(getAllDeclaredFields(type.getSuperclass())));
    }

    return fields.toArray(new Field[0]);
  }

  /**
   * Gets a {@link Field} object representing the {@link String named field} of the given {@link Class}.
   * <p>
   * This method recursively searches up the {@link Class} hierarchy of the given {@link Class}
   * until the {@link Object} class is reached. If the named field is found then a {@link Field} object
   * representing the {@link Class} field is returned, otherwise a {@link FieldNotFoundException} is thrown.
   *
   * @param type {@link Class} to search for a {@link Field} with the given {@link String name};
   * must not be {@literal null}.
   * @param fieldName {@link String} containing the {@literal name} of the {@link Field} on the {@link Class}
   * to search for.
   * @return a {@link Field} object representing the {@link String named field} of the given {@link Class}.
   * @throws FieldNotFoundException if the {@link String named field} does not exist on the given {@link Class}
   * or a superclass of the given {@link Class}.
   * @see java.lang.Class#getDeclaredField(String)
   * @see java.lang.reflect.Field
   * @see java.lang.Class
   */
  public static @NotNull Field getField(@NotNull Class<?> type, String fieldName) {

    try {
      return type.getDeclaredField(fieldName);
    }
    catch (NoSuchFieldException cause) {

      if (type.getSuperclass() != null) {
        return getField(type.getSuperclass(), fieldName);
      }

      throw new FieldNotFoundException(cause);
    }
  }

  /**
   * Attempts to find a method with the specified name on the given class type having a signature with parameter types
   * that are compatible with the given arguments.  This method searches recursively up the inherited class hierarchy
   * for the given class type until the desired method is found or the class type hierarchy is exhausted, in which case,
   * null is returned.
   *
   * @param type the Class type to search for the desired method.
   * @param methodName a String value indicating the name of the method to find.
   * @param arguments an array of object values indicating the arguments the method's parameters must accept.
   * @return a Method on the given class type with the specified name having a signature compatible with the arguments,
   * or null if no such Method exists on the given class type or one of it's inherited (parent) class types.
   * @throws NullPointerException if the given class type is null.
   * @see java.lang.Class
   * @see java.lang.Class#getDeclaredMethods()
   * @see java.lang.Class#getSuperclass()
   * @see java.lang.reflect.Method
   */
  @SuppressWarnings("all")
  public static @Nullable Method findMethod(@NotNull Class<?> type, String methodName, Object... arguments) {

    for (Method method : type.getDeclaredMethods()) {

      if (method.getName().equals(methodName)) {

        Class<?>[] parameterTypes = method.getParameterTypes();

        if (ArrayUtils.nullSafeLength(arguments) == parameterTypes.length) {

          boolean match = true;

          for (int index = 0; match && index < parameterTypes.length; index++) {
            match &= instanceOf(arguments[index], parameterTypes[index]);
          }

          if (match) {
            return method;
          }
        }
      }
    }

    return type.getSuperclass() != null
      ? findMethod(type.getSuperclass(), methodName, arguments)
      : null;
  }

  /**
   * Gets a Method object representing the named method on the specified class.  This method will recursively search
   * up the class hierarchy of the specified class until the Object class is reached.  If the named method is found
   * then a Method object representing the class method is returned, otherwise a NoSuchMethodException is thrown.
   *
   * @param type the Class type to search for the specified method.
   * @param methodName a String indicating the name of the method on the class.
   * @param parameterTypes an array of Class objects identifying the parameters and their types
   * based on the method's signature.
   * @return a Method object representing the named method on the specified class.
   * @throws MethodNotFoundException if the named method does not exist on the specified class
   * or a superclass of the specified class.
   * @see java.lang.Class
   * @see java.lang.Class#getDeclaredMethod(String, Class[])
   * @see java.lang.reflect.Method
   */
  public static @NotNull Method getMethod(@NotNull Class<?> type, String methodName, Class<?>... parameterTypes) {

    try {
      return type.getDeclaredMethod(methodName, parameterTypes);
    }
    catch (NoSuchMethodException cause) {

      if (type.getSuperclass() != null) {
        return getMethod(type.getSuperclass(), methodName, parameterTypes);
      }

      throw new MethodNotFoundException(cause);
    }
  }

  /**
   * Gets the most direct, declared {@link Method} from the given, required {@link Class type} using the given,
   * required {@link Method} reference.
   *
   * @param type {@link Class} to search for the most direct, declared {@link Method}.
   * @param method {@link Method} used as the reference to search in the {@link Class type}.
   * @return the most direct, declared {@link Method} of the given {@link Class type}.
   * @throws IllegalArgumentException if the {@link Class type} or {@link Method} are {@literal null}
   * or if the {@link Method#getDeclaringClass()} of the given {@link Method} is not assignable from
   * the given {@link Class type}.
   * @throws MethodNotFoundException if the {@link Method} cannot be found.
   * @see java.lang.reflect.Method
   * @see java.lang.Class
   */
  public static @NotNull Method getDeclaredMethod(@NotNull Class<?> type, @NotNull Method method) {

    Assert.notNull(type, "Class type is required");
    Assert.notNull(method, "Method is required");
    Assert.isTrue(method.getDeclaringClass().isAssignableFrom(type),
      "The declared Class type [%1$s] of Method [%2$s] is not assignable from the given Class type [%3$s]",
      method.getDeclaringClass().getName(), method.getName(), type.getName());

    return method.getDeclaringClass().equals(type) ? method
      : getMethod(type, method.getName(), method.getParameterTypes());
  }

  /**
   * Attempts to resolve the method with the specified name and signature on the given class type.  The named method's
   * resolution is first attempted by using the specified method's name along with the array of parameter types.
   * If unsuccessful, the method proceeds to lookup the named method by searching all "declared" methods
   * of the class type having a signature compatible with the given argument types.  This method operates recursively
   * until the method is resolved or the class type hierarchy is exhausted, in which case,
   * a MethodNotFoundException is thrown.
   *
   * @param type the Class type on which to resolve the method.
   * @param methodName a String indicating the name of the method to resolve.
   * @param parameterTypes an array of Class objects used to resolve the exact signature of the method.
   * @param arguments an array of Objects used in a method invocation serving as a fallback search/lookup strategy
   * if the method cannot be resolved using its parameter types. Maybe {@literal null}.
   * @param returnType the declared class type of the method's return value (used only for Exception message purposes).
   * @return the resolved method from the given class type given the name, parameter types (signature)
   * and calling arguments, if any.
   * @throws MethodNotFoundException if the specified method cannot be resolved on the given class type.
   * @throws NullPointerException if the class type is null.
   * @see #getMethod(Class, String, Class[])
   * @see #findMethod(Class, String, Object...)
   * @see java.lang.Class
   * @see java.lang.reflect.Method
   */
  public static @NotNull Method resolveMethod(@NotNull Class<?> type, String methodName,
      Class<?>[] parameterTypes, Object[] arguments, Class<?> returnType) {

    try {
      return getMethod(type, methodName, parameterTypes);
    }
    catch (MethodNotFoundException cause) {

      Method method = findMethod(type, methodName, arguments);

      Assert.notNull(method, new MethodNotFoundException(String.format(
        "Failed to resolve method with signature [%1$s] on class type [%2$s]",
          getMethodSignature(methodName, parameterTypes, returnType), getName(type)), cause.getCause()));

      return method;
    }
  }

  /**
   * Builds the signature of a given {@link Method}.
   *
   * @param method {@link Method} used to build a {@link String method signature}; must not be {@literal null}.
   * @return the {@link String signature} of the given {@link Method}.
   * @see #getMethodSignature(String, Class[], Class)
   */
  protected static @NotNull String getMethodSignature(@NotNull Method method) {
    return getMethodSignature(method.getName(), method.getParameterTypes(), method.getReturnType());
  }

  /**
   * Builds the signature of a given {@link Method} based on the {@link Method#getName() method's name},
   * {@link Class parameter types} and {@link Class return type}.
   *
   * @param methodName {@link String} containing the {@literal name} of the {@link Method}.
   * @param parameterTypes array of {@link Class types} specifying the {@link Class type}
   * of each {@link Method} parameter.
   * @param returnType a Class object indicating the methods return type.
   * @return the {@link String signature} of the {@link Method}.
   * @see #getSimpleName(Class)
   */
  protected static @NotNull String getMethodSignature(@NotNull String methodName,
      Class<?>[] parameterTypes, Class<?> returnType) {

    StringBuilder buffer = new StringBuilder(methodName);

    buffer.append("(");

    if (parameterTypes != null) {

      int index = 0;

      for (Class<?> parameterType : parameterTypes) {
        buffer.append(index++ > 0 ? ", :" : ":");
        buffer.append(getSimpleName(parameterType));
      }
    }

    buffer.append("):");
    buffer.append(returnType == null || Void.class.equals(returnType) ? "void" : getSimpleName(returnType));

    return buffer.toString();
  }

  /**
   * Gets the {@link Class#getName() fully-qualified name} of the {@link Class}.
   *
   * @param type {@link Class type} from which to return the {@link String fully-qualified name}.
   * @return a {@link String} containing the {@link Class#getName() fully-qualified name} of the {@link Class}.
   * @see java.lang.Class#getName()
   */
  @NullSafe
  public static @Nullable String getName(@Nullable Class<?> type) {
    return type != null ? type.getName() : null;
  }

  /**
   * Gets the {@link String resource name} of the given {@link Class type}.
   * <p>
   * The {@link String resource name} of a given {@link Class} is the {@link String pathname} of the class file
   * defining the {@link Class type} relative to the {@literal CLASSPATH} used by the {@literal JVM}.
   * <p>
   * For instance, if the {@link Class type} were {@literal java.lang.Object.class},
   * then the {@link String resource name} would be:
   * <p>
   * <code>
   *   java/lang/Object.class.
   * </code>
   *
   * @param type {@link Class} type from which to construct the {@link String resource name}.
   * @return a {@link String} specifying the {@literal resource name} of the given {@link Class type}.
   * @see java.lang.Class
   */
  @NullSafe
  public static @Nullable String getResourceName(@Nullable Class<?> type) {

    return type != null
      ? type.getName().replaceAll("\\.", "/").concat(CLASS_FILE_EXTENSION)
      : null;
  }

  /**
   * Gets the {@link Class#getSimpleName() simple name} of the {@link Class}.
   *
   * @param type {@link Class type} from which to return the {@link String unqualified, simple name}.
   * @return a {@link String} with the {@link Class#getSimpleName() simple name} of the {@link Class}.
   * @see java.lang.Class#getSimpleName()
   */
  @NullSafe
  public static @Nullable String getSimpleName(@Nullable Class<?> type) {
    return type != null ? type.getSimpleName() : null;
  }

  /**
   * Determines whether the given {@link Class} has a {@literal main} {@link Method}.
   *
   * @param type {@link Class} to evaluate.
   * @return a boolean value indicating whether the given {@link Class} has a {@literal main} {@link Method}.
   * @see java.lang.Class#getDeclaredMethods()
   * @see #isMainMethod(Method)
   * @see java.lang.Class
   */
  @NullSafe
  public static boolean hasMainMethod(@Nullable Class<?> type) {

    return Optional.ofNullable(type)
      .map(theType -> type.getDeclaredMethods())
      .map(methods -> stream(methods).anyMatch(ClassUtils::isMainMethod))
      .orElse(false);
  }

  /**
   * Determines whether the given {@link Object} implements any {@link Class interfaces}.
   *
   * @param obj {@link Object} to evaluate.
   * @return a boolean value indicating whether the given {@link Object} implements any {@link Class interfaces}.
   * @see #getInterfaces(Object)
   * @see java.lang.Object
   */
  @NullSafe
  public static boolean implementsInterfaces(@Nullable Object obj) {
    return !getInterfaces(obj).isEmpty();
  }

  /**
   * Determines whether the given {@link Class type} implements any {@link Class interfaces}.
   *
   * @param type {@link Class type} to evaluate.
   * @return a boolean value indicating whether the given {@link Class type} implements any {@link Class interfaces}.
   * @see #getInterfaces(Class)
   * @see java.lang.Class
   */
  @NullSafe
  public static boolean implementsInterfaces(@Nullable Class<?> type) {
    return !getInterfaces(type).isEmpty();
  }

  /**
   * Determines whether the given {@link Object} is an instance of the given {@link Class}.
   * <p>
   * Note, an {@link Object} cannot be an instance of {@literal null}, so this method returns {@literal false}
   * if the {@link Class type} is {@literal null} or the {@link Object} is {@literal null}.
   *
   * @param obj {@link Object} to test as an instance of the given {@link Class type}.
   * @param type {@link Class type} used in the {@literal instanceof} operation.
   * @return a boolean value indicating whether the {@link Object} is an instance of the given {@link Class}.
   * @see java.lang.Class#isInstance(Object)
   * @see #assignableTo(Class, Class)
   */
  @NullSafe
  public static boolean instanceOf(@Nullable Object obj, @Nullable Class<?> type) {
    return type != null && type.isInstance(obj);
  }

  /**
   * Determines whether the given {@link Class} represents an {@link Annotation}.
   *
   * @param type {@link Class} to evaluate.
   * @return {@literal true} iff the {@link Class} is not {@literal null} and represents an {@link Annotation}.
   * @see java.lang.annotation.Annotation
   * @see java.lang.Class#isAnnotation()
   */
  @NullSafe
  public static boolean isAnnotation(@Nullable Class<?> type) {
    return type != null && type.isAnnotation();
  }

  /**
   * Determines whether the given {@link Annotation} metadata is present on the given "annotated" members,
   * such as fields and methods.
   *
   * @param annotation {@link Annotation} used in the detection for presence on the given members.
   * @param elements members of a {@link Class} or {@link Object} to inspect for the presence of
   * the given {@link Annotation}.
   * @return a boolean value indicating whether the given {@link Annotation} is present on any of the given members.
   * @see java.lang.reflect.AccessibleObject#isAnnotationPresent(Class)
   * @see java.lang.annotation.Annotation
   */
  @NullSafe
  public static boolean isAnnotationPresent(@Nullable Class<? extends Annotation> annotation,
      AnnotatedElement... elements) {

    return stream(ArrayUtils.nullSafeArray(elements, AnnotatedElement.class))
      .anyMatch(member -> member != null && member.isAnnotationPresent(annotation));
  }

  /**
   * Determines whether the given {@link Class} represents an array type.
   *
   * @param type {@link Class} to evaluate.
   * @return {@literal true} iff the {@link Class} is not {@literal null} and represents an array type.
   * @see java.lang.Class#isArray()
   */
  @NullSafe
  public static boolean isArray(@Nullable Class<?> type) {
    return type != null && type.isArray();
  }

  /**
   * Determines whether the given {@link Class} represents an actual class, and not an {@link Annotation}, Array,
   * {@link Enum}, Interface or primitive type.
   *
   * @param type {@link Class} to evaluate.
   * @return {@literal true} iff the {@link Class} is not {@literal null} and represents an actual class.
   * @see java.lang.Class
   */
  @NullSafe
  public static boolean isClass(@Nullable Class<?> type) {

    return type != null
      && !(type.isAnnotation() || type.isArray() || type.isEnum() || type.isInterface() || type.isPrimitive());
  }

  /**
   * Null-safe method used to determine whether the given {@link Constructor} accepts a single argument
   * of type {@link Object array} used to pass arguments much like a Java {@link Class} {@literal main} {@link Method}.
   * <p>
   * This determination makes no effort to distinguish {@link Constructor Constructors} that accept an arbitrary
   * array of {@link Object objects} that do not represent arguments. Therefore, this method should only be used
   * when the developer knows such a {@link Constructor} exists for his/her particular UC.
   *
   * @param constructor {@link Constructor} to evaluate.
   * @return a boolean value indicating whether the given {@link Constructor}
   * accepts an array of {@link Object arguments}.
   * @see java.lang.reflect.Constructor
   */
  @NullSafe
  public static boolean isConstructorWithArrayParameter(@Nullable Constructor<?> constructor) {

    return constructor != null
      && constructor.getParameterCount() == 1
      && Object[].class.isAssignableFrom(constructor.getParameterTypes()[0]);
  }

  /**
   * Determines whether the given {@link Constructor} is a {@literal default} {@link Constructor}.
   * <p>
   * A {@link Constructor} is the {@literal default} {@link Constructor} if it is public and has no parameters.
   *
   * @param constructor {@link Constructor} to evaluate.
   * @return a boolean value indicating whether the given {@link Constructor}
   * is the {@literal default} {@link Constructor}.
   * @see java.lang.reflect.Constructor
   */
  @NullSafe
  public static boolean isDefaultConstructor(@Nullable Constructor<?> constructor) {
    return ModifierUtils.isPublic(constructor) && constructor.getParameterCount() == 0;
  }

  /**
   * Determines whether the given {@link Class} represents an {@link Enum}.
   *
   * @param type {@link Class} to evaluate.
   * @return {@literal true‘} iff the {@link Class} is not {@literal null} and represents an {@link Enum}.
   * @see java.lang.Class#isEnum()
   * @see java.lang.Enum
   */
  @NullSafe
  public static boolean isEnum(@Nullable Class<?> type) {
    return type != null && type.isEnum();
  }

  /**
   * Determines whether the {@link Class} represents an interface.
   *
   * @param type {@link Class} to evaluate.
   * @return {@literal true} iff the {@link Class} is not {@literal null} and represents an interface.
   * @see java.lang.Class#isInterface()
   */
  @NullSafe
  public static boolean isInterface(@Nullable Class<?> type) {
    return type != null && type.isInterface();
  }

  /**
   * Determines whether the given Java {@link Class} {@link Method} is the {@literal main} {@link Method}.
   *
   * @param method {@link Method} to evaluate.
   * @return a boolean value indicating whether the given {@link Method}
   * is a Java {@link Class} {@literal main} {@link Method}.
   * @see java.lang.reflect.Method
   */
  @NullSafe
  public static boolean isMainMethod(@Nullable Method method) {

    return Optional.ofNullable(method)
      .map(localMethod -> MAIN_METHOD_NAME.equals(localMethod.getName())
        && ModifierUtils.isPublic(localMethod)
        && ModifierUtils.isStatic(localMethod)
        && void.class.equals(localMethod.getReturnType())
        && localMethod.getParameterCount() == 1
        && localMethod.getParameterTypes()[0].equals(String[].class))
      .orElse(false);
  }

  /**
   * Determines whether the specified class identified by {@link String name} is available and present
   * on the application classpath.
   *
   * @param className {@link String fully qualified name} of the {@link Class} from which to determine the presence.
   * @return a boolean value indicating whether the class identified by {@link String name} is in the classpath.
   * @see #loadClass(String)
   */
  @SuppressWarnings("all")
  public static boolean isPresent(@NotNull String className) {

    try {
      return loadClass(className) != null;
    }
    catch (TypeNotFoundException ignore) {
      return false;
    }
  }

  /**
   * Determines whether the given {@link Class} represents a primitive type.
   *
   * @param type {@link Class} to evaluate.
   * @return {@literal true} iff the {@link Class} is not {@literal null} and represents a primitive type.
   * @see java.lang.Class#isPrimitive()
   */
  @NullSafe
  public static boolean isPrimitive(@Nullable Class<?> type) {
    return type != null && type.isPrimitive();
  }

  /**
   * Determines whether the given {@link Class type} is unclassified.
   * <p>
   * A {@link Class type} is unclassified if {@literal null} or equal to the {@link Class Object class}.
   *
   * @param type {@link Class} type of evaluate.
   * @return a boolean value indicating whether the given {@link Class type} is unclassified.
   */
  public static boolean isUnclassified(@Nullable Class<?> type) {
    return type == null || Object.class.equals(type);
  }

  /**
   * Loads the {@link Class} for the given, {@link String fully-qualified class name} using
   * the {@link Thread#currentThread() current Thread's} {@link Thread#getContextClassLoader() context ClassLoader},
   * followed by initializing the {@link Class}.
   *
   * @param <T> {@link Class type} of T.
   * @param fullyQualifiedClassName {@link String} containing the fully-qualified class name
   * of the {@link Class} to load.
   * @return a {@link Class} for the given, {@link String fully-qualified class name}.
   * @throws TypeNotFoundException if the {@link Class} identified by the {@link String fully-qualified class name}
   * could not be found.
   * @see java.lang.Thread#currentThread()
   * @see java.lang.Thread#getContextClassLoader()
   * @see #loadClass(String, boolean, ClassLoader)
   */
  public static @NotNull <T> Class<T> loadClass(@NotNull String fullyQualifiedClassName) {
    return loadClass(fullyQualifiedClassName, DEFAULT_INITIALIZE_LOADED_CLASS,
      Thread.currentThread().getContextClassLoader());
  }

  /**
   * Loads the {@link Class} for the given, {@link String fully-qualified class name}
   * using the provided {@link ClassLoader} with an option to initialize the class (calling any static initializers)
   * once loaded.
   *
   * @param <T> {@link Class type} of T.
   * @param fullyQualifiedClassName {@link String} indicating the {@link String fully-qualified class name}
   * of the {@link Class} to load.
   * @param initialize a boolean value indicating whether to initialize the {@link Class} after loading.
   * @param classLoader {@link ClassLoader} used to load the {@link Class}.
   * @return a {@link Class} for the given, {@link String fully-qualified class name}.
   * @throws TypeNotFoundException if the Class identified by the fully qualified class name could not be found.
   * @see java.lang.Class#forName(String, boolean, ClassLoader)
   * @see java.lang.ClassLoader
   * @see java.lang.Class
   */
  @SuppressWarnings("unchecked")
  public static @NotNull <T> Class<T> loadClass(@NotNull String fullyQualifiedClassName, boolean initialize,
      @NotNull ClassLoader classLoader) {

    try {
      return (Class<T>) Class.forName(fullyQualifiedClassName, initialize, classLoader);
    }
    catch (ClassNotFoundException | NoClassDefFoundError cause) {
      throw new TypeNotFoundException(String.format("Class [%s] was not found", fullyQualifiedClassName), cause);
    }
  }

  /**
   * Locates the class file resource given the binary name of the {@link Class}.
   *
   * @param binaryName {@link String} with the binary name of the {@link Class} that's class file resource
   * will be located.
   * @return a {@link URL} with the location of the class file resource containing the {@link Class} definition
   * for the given binary name.
   * @see #locateClass(String, ClassLoader)
   * @see java.net.URL
   */
  public static @Nullable URL locateClass(@NotNull String binaryName) {
    return locateClass(binaryName, Thread.currentThread().getContextClassLoader());
  }

  /**
   * Locates the class file resource given the binary name of the {@link Class}.
   * <p>
   * The {@link ClassLoader} used to search class file resource for the {@link Class} with the given binary name.
   *
   * @param binaryName {@link String} with the binary name of the {@link Class} that's class file resource
   * will be located.
   * @param classLoader {@link ClassLoader} used to locate the class file resource for the {@link Class}
   * with the given binary name.
   * @return a {@link URL} with the location of the class file resource containing the {@link Class} definition
   * for the given binary name.
   * @see java.lang.ClassLoader
   * @see java.net.URL
   */
  public static @Nullable URL locateClass(@NotNull String binaryName, @NotNull ClassLoader classLoader) {

    try {

      Class<?> type = loadClass(binaryName, false, classLoader);

      return type.getClassLoader().getResource(getResourceName(type));
    }
    catch (TypeNotFoundException ignore) {
      return null;
    }
  }

  /**
   * Determines whether the {@link Object} is an instance of any of the {@link Class types}
   * and returns {@literal false} if it is.
   *
   * @param obj {@link Object} passed to the {@literal instanceof} comparison.
   * @param types array of {@link Class types} used in the {@literal instanceof} comparison.
   * @return a {@literal true} boolean value iff the {@link Object} is not an instance of
   * any of the {@link Class types}.
   * @see #instanceOf(Object, Class)
   */
  @NullSafe
  @SuppressWarnings("all")
  public static boolean notInstanceOf(@Nullable Object obj, Class... types) {

    boolean result = true;

    for (int index = 0; result && index < ArrayUtils.nullSafeLength(types); index++) {
      result &= !instanceOf(obj, types[index]);
    }

    return result;
  }

  /**
   * Resolves the {@link Class type} of {@link Type}.
   *
   * @param type {@link Type} to resolve as a {@link Class}.
   * @return the resolved {@link Class type} of {@link Type}.
   * @throws IllegalArgumentException if the given {@link Type} cannot be resolved as a {@link Class type}.
   * @see java.lang.reflect.ParameterizedType
   * @see java.lang.reflect.Type
   */
  @NullSafe
  @SuppressWarnings("all")
  public static @NotNull Class<?> toRawType(@Nullable Type type) {

    Type resolvedType = type instanceof ParameterizedType parameterizedType ? parameterizedType.getRawType()
      : type instanceof TypeVariable typeVariable ? safeGetValue(() -> loadClass(typeVariable.getName()), Object.class)
      : type;

    if (resolvedType instanceof Class<?> classType) {
      return classType;
    }

    throw newIllegalArgumentException("[%1$s] is not resolvable as a %2$s", type, Class.class.getName());
  }
}
