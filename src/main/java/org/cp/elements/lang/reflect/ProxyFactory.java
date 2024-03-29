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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Abstract factory object used to create a proxy for a given target {@link Object} implementing the given set of
 * {@link Class interfaces}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the {@link Object} to proxy.
 * @see java.lang.reflect.Proxy
 * @see org.cp.elements.lang.reflect.ProxyService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ProxyFactory<T> {

  /**
   * Returns a reference to a {@link ProxyFactory} {@literal Service Provider Implementation (SPI)} provided by Elements
   * that delegates all proxy operations to any and all application configured {@link ProxyFactory}
   * Service Provider Implementations (SPI).
   *
   * @param <T> {@link Class type} of the {@link Object} to proxy.
   * @return a reference to the Elements-defined {@link ProxyFactory} Service Provider Implementation (SPI).
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see org.cp.elements.lang.reflect.ProxyService
   */
  @SuppressWarnings("all")
  protected static @NotNull <T> ProxyFactory<T> newProxyFactory() {

    return new ProxyFactory<T>() {

      @Override
      public boolean canProxy(Object target, Class<?>[] proxyInterfaces) {
        return ProxyService.<T>newProxyService().canProxy(target, proxyInterfaces);
      }

      @Override
      @SuppressWarnings("unchecked")
      protected <R> R newProxy(ClassLoader proxyClassLoader, T target, Class<?>[] proxyInterfaces,
          Iterable<MethodInterceptor<T>> methodInterceptors) {

        return (R) StreamUtils.stream(ProxyService.newProxyService())
          .filter(proxyFactory -> proxyFactory.canProxy(getTarget(), getProxyInterfaces()))
          .findFirst()
          .map(proxyFactory -> proxyFactory.<R>newProxy(getProxyClassLoader(), getTarget(),
            getProxyInterfaces(), getMethodInterceptors()))
          .orElse(getTarget());
      }
    };
  }

  /**
   * Factory method used to construct a new ={@link ProxyFactory} that will be used to create a {@literal Proxy}
   * for the given {@link Object} implementing the given array of {@link Class interfaces}.
   *
   * @param <T> {@link Class type} of the {@link Object} to proxy.
   * @param target {@link Object} to proxy.
   * @param proxyInterfaces array of {@link Class interfaces} implemented by the {@literal Proxy}.
   * @return a new instance of the Elements-defined {@link ProxyFactory} Service Provider Implementation (SPI)
   * used to create a {@literal Proxy} for the given {@link Object target}.
   * @see #newProxyFactory()
   * @see #implementing(Class[])
   * @see #proxy(Object)
   */
  public static @NotNull <T> ProxyFactory<T> newProxyFactory(@Nullable T target, Class<?>... proxyInterfaces) {

    return ProxyFactory.<T>newProxyFactory()
      .proxy(target)
      .implementing(proxyInterfaces);
  }

  /**
   * Resolves all the {@link Class interfaces} implemented by the given {@link Object object's} {@link Class}
   * including all the given {@link Class interfaces} in the {@link Class} array.
   *
   * @param obj {@link Object} to evaluate.
   * @param interfaces array of {@link Class interfaces} to includes in the result.
   * @return an array of {@link Class interfaces} implemented by the given {@link Object object's} {@link Class}
   * including all {@link Class interfaces} from the given {@link Class} array.
   * @see org.cp.elements.lang.ClassUtils#getInterfaces(Object)
   * @see java.lang.Class
   * @see java.lang.Object
   */
  @NullSafe
  protected static Class<?>[] resolveInterfaces(@Nullable Object obj, Class<?>... interfaces) {

    Set<Class<?>> allImplementedInterfaces = new HashSet<>(ClassUtils.getInterfaces(obj));

    allImplementedInterfaces.addAll(Arrays.stream(ArrayUtils.nullSafeArray(interfaces, Class.class))
      .filter(ClassUtils::isInterface)
      .toList());

    return allImplementedInterfaces.toArray(new Class<?>[0]);
  }

  private Class<?>[] proxyInterfaces;

  private ClassLoader proxyClassLoader;

  private List<MethodInterceptor<T>> methodInterceptors = Collections.emptyList();

  private T target;

  /**
   * Determines whether this {@link ProxyFactory} can proxy the given {@link Object}
   * with the specified {@link Class interfaces}.
   *
   * @param target {@link Object} to proxy.
   * @param proxyInterfaces array of {@link Class interfaces} to be implemented by the created Proxy {@link Object}.
   * @return a boolean value indicating whether this {@link ProxyFactory} can proxy the given {@link Object}
   * with the specified array of {@link Class interface}.
   * @see java.lang.Class
   * @see java.lang.Object
   */
  public abstract boolean canProxy(Object target, Class<?>... proxyInterfaces);

  /**
   * Returns an {@link Iterable} of {@link MethodInterceptor MethodInterceptors} used to advise
   * the {@link MethodInvocation method invocations} on the Proxy.
   *
   * @return an {@link Iterable} of {@link MethodInterceptor MethodInterceptors} used to advise
   * the {@link MethodInvocation method invocations} on the Proxy.
   * @see org.cp.elements.lang.reflect.MethodInterceptor
   * @see java.lang.Iterable
   */
  @NullSafe
  protected Iterable<MethodInterceptor<T>> getMethodInterceptors() {
    return Collections.unmodifiableList(this.methodInterceptors);
  }

  /**
   * Gets the Java {@link ClassLoader} used to resolve and define the Proxy {@link Class}.
   * <p>
   * If the {@link ClassLoader} was not configured then the current {@link Thread Thread's} {@link ClassLoader}
   * will be used.
   *
   * @return the Java {@link ClassLoader} used to resolve and define the Proxy {@link Class}.
   * @see java.lang.Thread#currentThread()
   * @see java.lang.Thread#getContextClassLoader()
   * @see java.lang.ClassLoader
   */
  @NullSafe
  protected @NotNull ClassLoader getProxyClassLoader() {

    ClassLoader proxyClassLoader = this.proxyClassLoader;

    return proxyClassLoader != null ? proxyClassLoader : Thread.currentThread().getContextClassLoader();
  }

  /**
   * Returns an array of {@link Class interfaces} implemented by the Proxy.
   *
   * @return an array of {@link Class interfaces} implemented by the Proxy.
   * Returns an empty array if no {@link Class interfaces} were configured.
   * @see java.lang.Class
   */
  @NullSafe
  public Class<?>[] getProxyInterfaces() {
    return ArrayUtils.nullSafeArray(this.proxyInterfaces, Class.class);
  }

  /**
   * Returns the {@link Object} to Proxy.
   *
   * @return the {@link Object} to Proxy.
   * @see java.lang.Object
   */
  public @Nullable T getTarget() {
    return this.target;
  }

  /**
   * Adds the array of {@link MethodInterceptor MethodInterceptors} to the Advice used to intercept
   * the {@link java.lang.reflect.Method} invocation and apply AOP-style advice
   * to the Proxy {@link java.lang.reflect.Method}.
   *
   * @param methodInterceptors array of {@link MethodInterceptor MethodInterceptors} used to advise the Proxy object
   * {@link java.lang.reflect.Method} invocation.
   * @return this {@link ProxyFactory}.
   * @see org.cp.elements.lang.reflect.MethodInterceptor
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see #getMethodInterceptors()
   */
  @NullSafe
  @SafeVarargs
  public final @NotNull ProxyFactory<T> adviseWith(MethodInterceptor<T>... methodInterceptors) {
    this.methodInterceptors = new ArrayList<>(ArrayUtils.nullSafeLength(methodInterceptors));
    Collections.addAll(this.methodInterceptors, ArrayUtils.nullSafeArray(methodInterceptors, MethodInterceptor.class));
    return this;
  }

  /**
   * Configures the {@link Class interfaces} to be implemented by the Proxy.
   *
   * @param proxyInterfaces array of {@link Class interfaces} to be implemented by the Proxy.
   * @return this {@link ProxyFactory}.
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see java.lang.Class
   * @see #getProxyInterfaces()
   */
  public @NotNull ProxyFactory<T> implementing(Class<?>... proxyInterfaces) {

    this.proxyInterfaces = Arrays.stream(ArrayUtils.nullSafeArray(proxyInterfaces, Class.class))
      .filter(Objects::nonNull)
      .toArray(Class[]::new);

    return this;
  }

  /**
   * Sets the {@link Object} to proxy.
   *
   * @param target {@link Object} to proxy.
   * @return this {@link ProxyFactory}
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see java.lang.Object
   * @see #getTarget()
   */
  public @NotNull ProxyFactory<T> proxy(T target) {
    this.target = target;
    return this;
  }

  /**
   * Configures the Java {@link ClassLoader} used to resolve and define the Proxy {@link Class}.
   *
   * @param proxyClassLoader Java {@link ClassLoader} used to resolve and define the Proxy {@link Class}.
   * @return this {@link ProxyFactory}.
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see java.lang.ClassLoader
   * @see #getProxyClassLoader()
   */
  public @NotNull ProxyFactory<T> using(@Nullable ClassLoader proxyClassLoader) {
    this.proxyClassLoader = proxyClassLoader;
    return this;
  }

  /**
   * Constructs, configures and initializes a new {@literal Proxy}.
   *
   * @param <R> {@link Class type} of the new {@literal Proxy} object.
   * @return a new {@literal Proxy} object of the given {@link Class type R}.
   * @see #newProxy(ClassLoader, Object, Class[], Iterable)
   * @see #getMethodInterceptors()
   * @see #getProxyClassLoader()
   * @see #getProxyInterfaces()
   * @see #getTarget()
   */
  public <R> R newProxy() {
    return newProxy(getProxyClassLoader(), getTarget(), getProxyInterfaces(), getMethodInterceptors());
  }

  /**
   * Constructs, configures and initializes a new {@literal Proxy}.
   *
   * @param <R> {@link Class type} of the new {@literal Proxy} object.
   * @param proxyClassLoader {@link ClassLoader} used to define the {@literal Proxy} {@link Class}.
   * @param target {@link Object} to proxy.
   * @param proxyInterfaces array of {@link Class interfaces} that will be implemented by
   * the {@literal Proxy} {@link Class}.
   * @param methodInterceptors array of {@link MethodInterceptor MethodInterceptors} used to advise
   * the {@literal Proxy} object {@link java.lang.reflect.Method} invocation.
   * @return a new {@literal Proxy} object of the given {@link Class type R}.
   * @see org.cp.elements.lang.reflect.MethodInterceptor
   * @see java.lang.ClassLoader
   * @see java.lang.Iterable
   * @see java.lang.Class
   * @see #newProxy()
   */
  protected abstract <R> R newProxy(ClassLoader proxyClassLoader, T target, Class<?>[] proxyInterfaces,
    Iterable<MethodInterceptor<T>> methodInterceptors);

}
