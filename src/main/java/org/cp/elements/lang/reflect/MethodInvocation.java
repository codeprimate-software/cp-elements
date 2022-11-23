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

import static org.cp.elements.lang.ElementsExceptionsFactory.newMethodInvocationException;
import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Optional;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * The {@link MethodInvocation} class encapsulates all the necessary information to invoke a {@link Method}
 * on a given target {@link Object}.
 *
 * @author John Blum
 * @see java.lang.reflect.Method
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class MethodInvocation {

  /**
   * Factory method used to construct a new instance of {@link MethodInvocation} initialized with
   * the given {@link Method} and array of {@link Object arguments} passed to the {@link Method}
   * during invocation.
   *
   * The {@link Method} is expected to be a {@link java.lang.reflect.Modifier#STATIC},
   * {@link Class} member {@link Method}.
   *
   * @param method {@link Method} to invoke.
   * @param args array of {@link Object arguments} to pass to the {@link Method} during invocation.
   * @return an instance of {@link MethodInvocation} encapsulating all the necessary details
   * to invoke the {@link java.lang.reflect.Modifier#STATIC} {@link Method}.
   * @see #newMethodInvocation(Object, Method, Object...)
   * @see java.lang.reflect.Method
   */
  public static @NotNull MethodInvocation newMethodInvocation(@NotNull Method method, Object... args) {
    return newMethodInvocation(null, method, args);
  }

  /**
   * Factory method used to construct a new instance of {@link MethodInvocation} initialized with
   * the given target {@link Object}, instance {@link Method} and array of {@link Object arguments}
   * passed to the {@link Method} during invocation.
   *
   * The {@link Method} represents a {@link java.lang.reflect.Modifier#STATIC non-static} instance {@link Method}
   * that will be invoked on the given target {@link Object}.
   *
   * @param target {@link Object} on which the instance {@link Method} will be invoked.
   * @param method {@link Method} to invoke.
   * @param args array of {@link Object arguments} to pass to the {@link Method} during invocation.
   * @return an instance of {@link MethodInvocation} encapsulating all the necessary details
   * to invoke the {@link Method} on the given {@link Object target}.
   * @see #MethodInvocation(Object, Method, Object...)
   * @see java.lang.reflect.Method
   * @see java.lang.Object
   */
  public static @NotNull MethodInvocation newMethodInvocation(Object target, @NotNull Method method, Object... args) {
    return new MethodInvocation(target, method, args);
  }

  /**
   * Factory method used to construct a new instance of {@link MethodInvocation} initialized with the given,
   * required {@link Class type} on which the {@code methodName named} {@link Method} accepting the given
   * array of {@link Object arguments} is declared.
   *
   * The {@link String named} {@link Method} is expected to be a {@link java.lang.reflect.Modifier#STATIC},
   * {@link Class} member {@link Method}.
   *
   * @param type {@link Class type} declaring the {@link Method}.
   * @param methodName {@link String name} of the {@link Method}.
   * @param args array of {@link Object arguments} passed to the {@link Method}.
   * @return an instance of {@link MethodInvocation} encapsulating all the necessary details to invoke
   * the {@link java.lang.reflect.Modifier#STATIC} {@link Method}.
   * @throws IllegalArgumentException if the {@link Class type} is {@literal null}.
   * @see #MethodInvocation(Object, Method, Object...)
   * @see java.lang.Class
   */
  public static @NotNull MethodInvocation newMethodInvocation(@NotNull Class<?> type, String methodName,
      Object... args) {

    Assert.notNull(type, "Class type is required");

    return new MethodInvocation(null, ClassUtils.findMethod(type, methodName, args), args);
  }

  /**
   * Factory method used to construct an instance of {@link MethodInvocation} initialized with the given
   * {@link Object target} on which the {@code methodName named} {@link Method} accepting the given
   * array of {@link Object arguments} will be invoked.
   *
   * The {@link String named} {@link Method} represents a {@link java.lang.reflect.Modifier#STATIC non-static}
   * instance {@link Method} that will be invoked on the given target {@link Object}.
   *
   * @param target {@link Object} on which the {@link Method} will be invoked.
   * @param methodName {@link String name} of the {@link Method}.
   * @param args array of {@link Object arguments} passed to the {@link Method}.
   * @return an instance of {@link MethodInvocation} encapsulating all the necessary details to invoke
   * the instance {@link Method} on the target {@link Object}.
   * @throws IllegalArgumentException if the target {@link Object} is {@literal null}.
   * @see #MethodInvocation(Object, Method, Object...)
   * @see java.lang.Object
   */
  public static @NotNull MethodInvocation newMethodInvocation(@NotNull Object target, String methodName,
      Object... args) {

    Assert.notNull(target, "Target object is required");

    return new MethodInvocation(target, ClassUtils.findMethod(target.getClass(), methodName, args), args);
  }

  private final Method method;

  private Object target;

  private Object[] arguments;

  /**
   * Constructs a new instance of {@link MethodInvocation} initialized with the given {@link Object target}
   * on which the given {@link Method} will be invoked, passing the given array of {@link Object arguments}
   * to the {@link Method} during invocation.
   *
   * @param target {@link Object} on which the given {@link Method} is invoked.
   * @param method {@link Method} to invoke; must not be {@literal null}.
   * @param args array of {@link Object arguments} passed to the {@link Method}.
   * @throws IllegalArgumentException if the {@link Method} is {@literal null}, or {@link Object target}
   * is {@literal null} and the {@link Method} is not {@link java.lang.reflect.Modifier#STATIC}.
   * @see #validateArguments(Method, Object...)
   * @see java.lang.reflect.Method
   * @see java.lang.Object
   */
  public MethodInvocation(@Nullable Object target, @NotNull Method method, Object... args) {

    Assert.notNull(method, "Method is required");

    Assert.isTrue(target != null || ModifierUtils.isStatic(method),
      "Method must be static if target is null");

    this.arguments = validateArguments(method, args);
    this.method = method;
    this.target = target;
  }

  /**
   * Validates the array of {@link Object arguments} to be passed to the {@link Method} by comparing
   * the {@link Object arguments} to the given {@link Method Method's} declared parameters.
   *
   * @param method {@link Method} used to validate the array of {@link Object arguments}.
   * @param args array of {@link Object arguments} to validate.
   * @return the given array of {@link Object arguments}.
   * @throws IllegalArgumentException if {@link Method} is {@literal null}, or the number of arguments
   * does not equal the number of {@link Method} parameters, or an argument does not the match the type
   * of the corresponding {@link Method} parameter.
   * @see java.lang.reflect.Method
   */
  protected Object[] validateArguments(@NotNull Method method, Object... args) {

    Assert.notNull(method, "Method is required");

    Object[] arguments = ArrayUtils.nullSafeArray(args);

    int methodParameterCount = method.getParameterCount();

    String exceptionMessage =
      "The number of arguments [%1$d] does not match the number of parameters [%2$d] for method [%3$s] in class [%4$s]";

    assertThat(arguments.length)
      .throwing(newIllegalArgumentException(exceptionMessage, arguments.length, methodParameterCount, method.getName(),
        method.getDeclaringClass().getName()))
      .isEqualTo(methodParameterCount);

    Class<?>[] parameterTypes = method.getParameterTypes();

    int parameterIndex = 0;

    for (Object argument : arguments) {
      assertThat(argument)
        .throwing(newIllegalArgumentException("Argument [%1$s] is not assignable to parameter [%2$d] of type [%3$s]",
          argument, parameterIndex, parameterTypes[parameterIndex].getName()))
        .isAssignableTo(parameterTypes[parameterIndex++]);
    }

    return arguments;
  }

  /**
   * Returns the array of {@link Object arguments} passed to the {@link Method}.
   *
   * @return an array of {@link Object arguments} passed to the {@link Method}.
   * Returns an empty array if the {@link Method} takes no arguments.
   */
  @NullSafe
  public Object[] getArguments() {
    return ArrayUtils.nullSafeArray(this.arguments);
  }

  /**
   * Returns the {@link Class} type on which this {@link Method} is declared.
   *
   * @return the declaring {@link Class type} for this {@link Method}.
   * @see java.lang.reflect.Method#getDeclaringClass()
   * @see #getMethod()
   */
  @NullSafe
  public @NotNull Class<?> getDeclaringClass() {
    return getMethod().getDeclaringClass();
  }

  /**
   * Returns the target {@link Method} for this invocation.
   *
   * @return the target {@link Method} for this invocation.
   * @see java.lang.reflect.Method
   */
  public @NotNull Method getMethod() {
    return this.method;
  }

  /**
   * Returns the configured target {@link Object} on which the {@link Method} will be invoked.
   *
   * @return the configured target {@link Object} on which the {@link Method} will be invoked.
   * @see java.lang.Object
   */
  public @Nullable Object getTarget() {
    return this.target;
  }

  /**
   * Invokes the {@link Method} on the target {@link Object}.
   *
   * @param <T> {@link Class} type of the {@link Method} return value.
   * @return the result of the {@link Method} invocation on the given target {@link Object}
   * wrapped in a {@link Optional} to guard against {@literal null}.
   * @see java.util.Optional
   * @see #invoke(Object)
   * @see #getTarget()
   */
  public <T> Optional<T> invoke() {
    return invoke(getTarget());
  }

  /**
   * Invokes the {@link Method} on the given target {@link Object}.
   *
   * @param <T> {@link Class} type of the {@link Method} return value.
   * @param target {@link Object} on which the {@link Method} will be invoked.
   * @return the result of the {@link Method} invocation on the given target {@link Object}
   * wrapped in a {@link Optional} to guard against {@literal null}.
   * @throws MethodInvocationException if an error occurs during the invocation of the {@link Method}
   * on the target {@link Object}.
   * @see java.lang.reflect.Method#invoke(Object, Object...)
   * @see java.util.Optional
   * @see #resolveTarget(Object)
   * @see #getArguments()
   * @see #getMethod()
   */
  @SuppressWarnings("unchecked")
  public <T> Optional<T> invoke(Object target) {

    Object resolvedTarget = resolveTarget(target);

    Method method = getMethod();

    try {
      return Optional.ofNullable((T) method.invoke(resolvedTarget, getArguments()));
    }
    catch (IllegalAccessException | InvocationTargetException cause) {
      throw newMethodInvocationException(cause, "Failed to invoke method [%1$s] on target object [%2$s]",
        method.getName(), resolvedTarget);
    }
  }

  /**
   * Resolves the target {@link Object} on which the {@link Method} will be invoked.
   *
   * @param target {@link Object} to evaluate.
   * @return the resolved {@link Object} on which the {@link Method} will be invoked.
   * @see java.lang.Object
   * @see #getTarget()
   */
  protected Object resolveTarget(Object target) {
    return Optional.ofNullable(target).orElseGet(this::getTarget);
  }

  /**
   * Sets the accessibility of the {@link Method} to {@literal true}.
   *
   * @return this {@link MethodInvocation}.
   * @see java.lang.reflect.Method#setAccessible(boolean)
   * @see org.cp.elements.lang.reflect.MethodInvocation
   * @see #getMethod()
   */
  public MethodInvocation makeAccessible() {
    getMethod().setAccessible(true);
    return this;
  }

  /**
   * Sets the target {@link Object} on which the {@link Method} will be invoked.
   *
   * @param target {@link Object} on which the {@link Method} will be invoked.
   * @return this {@link MethodInvocation}.
   * @throws IllegalArgumentException if {@link Object target} is {@literal null} and {@link Method}
   * is not {@link java.lang.reflect.Modifier#STATIC}.
   * @see org.cp.elements.lang.reflect.MethodInvocation
   * @see java.lang.Object
   * @see #getMethod()
   */
  public MethodInvocation on(Object target) {

    Assert.isTrue(target != null || ModifierUtils.isStatic(getMethod()),
      "Method must be static if target is null");

    this.target = target;

    return this;
  }

  /**
   * Sets the array of {@link Object arguments} to pass to the {@link Method}.
   *
   * @param arguments array of {@link Object arguments} passed to the {@link Method}.
   * @return this {@link MethodInvocation}.
   * @throws IllegalArgumentException if the number of arguments is not equal to the number of {@link Method} parameters
   * or any argument {@link Class type} is not assignable to the corresponding parameter {@link Class type}.
   * @see org.cp.elements.lang.reflect.MethodInvocation
   * @see #validateArguments(Method, Object...)
   * @see #getMethod()
   */
  public MethodInvocation passing(Object... arguments) {
    this.arguments = validateArguments(getMethod(), arguments);
    return this;
  }
}
