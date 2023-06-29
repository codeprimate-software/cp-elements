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
package org.cp.elements.lang.reflect.support;

import java.lang.reflect.Method;
import java.util.Optional;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.reflect.MethodInterceptor;
import org.cp.elements.lang.reflect.MethodInvocation;

/**
 * {@link MethodInterceptor} implementation used to invoke the {@link Method} on a specified target {@link Object}.
 *
 * @author John Blum
 * @see java.lang.reflect.Method
 * @see org.cp.elements.lang.reflect.MethodInterceptor
 * @see org.cp.elements.lang.reflect.MethodInvocation
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class MethodInvokingMethodInterceptor implements MethodInterceptor<Object> {

  /**
   * Factory method used to construct a new {@link MethodInvokingMethodInterceptor} initialized with
   * the given target {@link Object} on which the {@link Method} invocation will be called.
   *
   * @param target {@link Object} on which the {@link Method} will be invoked.
   * @return a new instance of {@link MethodInvokingMethodInterceptor} initialized with the given {@link Object target}.
   * @see org.cp.elements.lang.reflect.support.MethodInvokingMethodInterceptor
   * @see #MethodInvokingMethodInterceptor(Object)
   * @see java.lang.Object
   */
  public static @NotNull MethodInvokingMethodInterceptor newMethodInvokingMethodInterceptor(@Nullable Object target) {
    return new MethodInvokingMethodInterceptor(target);
  }

  private final Object target;

  /**
   * Default constructor used to construct a new {@link MethodInvokingMethodInterceptor} with no {@link Object target}.
   * <p>
   * This constructor is used to invoke {@link java.lang.reflect.Modifier#STATIC} {@link Method methods}.
   *
   * @see #MethodInvokingMethodInterceptor(Object)
   */
  public MethodInvokingMethodInterceptor() {
    this(null);
  }

  /**
   * Constructs a new instance of {@link MethodInvokingMethodInterceptor} initialized with
   * the given target {@link Object} on which the {@link Method} will be invoked.
   *
   * @param target {@link Object} used as the target of the {@link Method} invocation.
   * @see java.lang.Object
   */
  public MethodInvokingMethodInterceptor(@Nullable Object target) {
    this.target = target;
  }

  /**
   * Return the target {@link Object} on which the {@link Method} will be intercepted.
   *
   * @return the target {@link Object} on which the {@link Method} will be intercepted.
   * @see java.lang.Object
   */
  @Override
  public Object getTarget() {
    return this.target;
  }

  /**
   * Intercepts the given {@link MethodInvocation} on the proxy of an {@link Object} and in turn invokes
   * the {@link Method} on the configured {@link #getTarget() target} object.  If {@link #getTarget() target}
   * is {@literal null} then the {@link Method} will be invoked on the configured target of
   * the {@link MethodInvocation}.
   *
   * @param <T> {@link Class type} of the {@link Object return value}.
   * @param methodInvocation {@link MethodInvocation} being intercepted.
   * @return an {@link Optional} return value containing the result of the interception.
   * @see org.cp.elements.lang.reflect.MethodInvocation
   * @see java.util.Optional
   * @see #getTarget()
   */
  @Override
  public <T> Optional<T> intercept(@NotNull MethodInvocation methodInvocation) {
    return methodInvocation.makeAccessible().invoke(getTarget());
  }
}
