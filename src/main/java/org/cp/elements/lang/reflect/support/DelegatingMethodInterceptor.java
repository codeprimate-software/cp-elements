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

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.concurrent.lock.GuardedBy;
import org.cp.elements.lang.reflect.MethodInterceptor;
import org.cp.elements.lang.reflect.MethodInvocation;

/**
 * {@link DelegatingMethodInterceptor} is a wrapper for an existing {@link MethodInterceptor} that delegates
 * all {@link Method} invocations and intercepting operations to the configured delegate.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the {@link Object target}.
 * @see org.cp.elements.lang.reflect.MethodInterceptor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DelegatingMethodInterceptor<T> implements MethodInterceptor<T> {

  private MethodInterceptor<T> delegate;

  /**
   * Factory method used to construct a new, uninitialized instance of {@link DelegatingMethodInterceptor}.
   *
   * @param <T> {@link Class type} of the {@link Object target}.
   * @return a new, uninitialized instance of {@link DelegatingMethodInterceptor}.
   * @see org.cp.elements.lang.reflect.support.DelegatingMethodInterceptor
   * @see #DelegatingMethodInterceptor()
   */
  public static @NotNull <T> DelegatingMethodInterceptor<T> newDelegatingMethodInterceptor() {
    return new DelegatingMethodInterceptor<>();
  }

  /**
   * Factory method used to construct a new {@link DelegatingMethodInterceptor} initialized with
   * the given {@link MethodInterceptor} used as the delegate in all {@link MethodInterceptor} operations.
   *
   * @param <T> {{@link Class type} of the {@link Object target}.
   * @param delegate {@link MethodInterceptor} wrapped as the delegate for the {@link DelegatingMethodInterceptor}.
   * @return a new {@link DelegatingMethodInterceptor} initialized with the given {@link MethodInterceptor} delegate.
   * @see org.cp.elements.lang.reflect.MethodInterceptor
   */
  public static @NotNull <T> DelegatingMethodInterceptor<T> newDelegatingMethodInterceptor(
      @Nullable MethodInterceptor<T> delegate) {

    return new DelegatingMethodInterceptor<>(delegate);
  }

  /**
   * Default constructor for {@link DelegatingMethodInterceptor}constructing an uninitialized instance
   * of {@link DelegatingMethodInterceptor}.
   */
  public DelegatingMethodInterceptor() { }

  /**
   * Constructs a new {@link DelegatingMethodInterceptor} initialized with
   * the given {@link MethodInterceptor} serving as the delegate.
   *
   * @param delegate {@link MethodInterceptor} wrapped by this {@link DelegatingMethodInterceptor}.
   * May be {@literal null} but must be {@literal non-null} when the delegate is used.
   * @see org.cp.elements.lang.reflect.MethodInterceptor
   */
  public DelegatingMethodInterceptor(@Nullable MethodInterceptor<T> delegate) {
    this.delegate = delegate;
  }

  /**
   * Sets the delegate used in the {@link MethodInterceptor} operations.
   *
   * @param delegate {@link MethodInterceptor} used in operations on this {@link MethodInterceptor}.
   * @see org.cp.elements.lang.reflect.MethodInterceptor
   */
  @GuardedBy("this")
  public synchronized void setDelegate(@Nullable MethodInterceptor<T> delegate) {
    this.delegate = delegate;
  }

  /**
   * Returns the delegate used in the {@link MethodInterceptor} operations.
   *
   * @return the {@link MethodInterceptor} used in operations on this {@link MethodInterceptor}.
   * @throws IllegalStateException if the {@code delegate} is {@literal null}.
   * @see org.cp.elements.lang.reflect.MethodInterceptor
   */
  @GuardedBy("this")
  protected synchronized @NotNull MethodInterceptor<T> getDelegate() {
    return ObjectUtils.requireState(this.delegate,
      "The delegate MethodInterceptor was not properly initialized");
  }

  /**
   * Return the target {@link Object} on which the {@link Method} will be intercepted.
   *
   * @return the target {@link Object} on which the {@link Method} will be intercepted.
   * @see #getDelegate()
   */
  @Override
  public T getTarget() {
    return getDelegate().getTarget();
  }

  /**
   * Intercepts the {@link MethodInvocation} and handles it by forwarding the invocation
   * to the required, configured {@link #getDelegate() delegate}.
   *
   * @param <R> {@link Class} type of the {@link Method} return value.
   * @param methodInvocation {@link MethodInvocation} being intercepted.
   * @see org.cp.elements.lang.reflect.MethodInterceptor#intercept(MethodInvocation)
   * @see org.cp.elements.lang.reflect.MethodInvocation
   * @see java.util.Optional
   * @see #getDelegate()
   */
  @Override
  public <R> Optional<R> intercept(MethodInvocation methodInvocation) {
    return getDelegate().intercept(methodInvocation);
  }
}
