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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Optional;

/**
 * Java {@link InvocationHandler} implementation that intercepts and handles {@link Method} invocations
 * on a {@literal Proxy} of a given {@link Object target}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object target}.
 * @see java.lang.Object
 * @see java.lang.reflect.InvocationHandler
 * @see java.lang.reflect.Method
 * @see org.cp.elements.lang.reflect.MethodInvocation
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface MethodInterceptor<T> extends InvocationHandler {

  /**
   * Gets the {@link Object target} on which the {@link Method} invocation will be intercepted.
   *
   * @return {@link Object target} on which the {@link Method} invocation will be intercepted.
   * @see org.cp.elements.lang.reflect.MethodInvocation#getTarget()
   * @see java.lang.Object
   */
  T getTarget();

  /**
   * Intercepts the {@link Method} identified and encapsulated by the given {@link MethodInvocation}.
   *
   * @param <R> {@link Class type} of the {@link Method Method's} return value.
   * @param methodInvocation {@link MethodInvocation} encapsulating details of the intercepted {@link Method} invocation
   * on the {@link Object target}.
   * @return the result of the intercepted {@link Method} invocation wrapped in an {@link Optional} instance
   * to guard against {@literal null}.
   * @see org.cp.elements.lang.reflect.MethodInvocation
   * @see #invoke(Object, Method, Object[])
   * @see java.lang.Object
   * @see java.util.Optional
   */
  <R> Optional<R> intercept(MethodInvocation methodInvocation);

  /**
   * Invokes the given {@link Method} with the given array of {@link Object} arguments.
   *
   * @param proxy {@link Object Proxy} on which the {@link Method} was invoked, intercepting the {@link Method} call.
   * @param method {@link Method} to invoke.
   * @param args array of {@link Object} arguments passed to the {@link Method} invocation.
   * @return the {@link Object return value} from the {@link Method} invocation, or {@literal null}
   * if the {@link Method} does not return a value.
   * @throws Throwable if the {@link Method} invocation fails.
   * @see org.cp.elements.lang.reflect.MethodInvocation#newMethodInvocation(Object, Method, Object...)
   * @see #intercept(MethodInvocation)
   * @see java.lang.reflect.Method
   * @see java.lang.Object
   * @see #getTarget()
   */
  @Override
  default Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
    return intercept(MethodInvocation.newMethodInvocation(getTarget(), method, args))
      .orElse(null);
  }
}
