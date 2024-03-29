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
package org.cp.elements.lang.reflect.provider;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.HashSet;
import java.util.Set;

import org.cp.elements.lang.JavaType;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.reflect.MethodInterceptor;
import org.cp.elements.lang.reflect.ProxyFactory;
import org.cp.elements.lang.reflect.support.ComposableInvocationHandler;
import org.cp.elements.util.stream.StreamUtils;

/**
 * A {@link ProxyFactory} Service Provider Implementation (SPI) that uses JDK Dynamic Proxies
 * to proxy a given {@link Object target} with a set of {@link Class interfaces}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} to proxy.
 * @see java.lang.reflect.Method
 * @see java.lang.reflect.Proxy
 * @see org.cp.elements.lang.JavaType
 * @see org.cp.elements.lang.reflect.MethodInterceptor
 * @see org.cp.elements.lang.reflect.ProxyFactory
 * @since 1.0.0
 */
public class JdkDynamicProxiesFactory<T> extends ProxyFactory<T> {

  private static final Set<Class<?>> NON_PROXYABLE_TYPES = new HashSet<>();

  static {
    NON_PROXYABLE_TYPES.add(Serializable.class);
  }

  /**
   * Factory method used to construct a new {@link JdkDynamicProxiesFactory} to create JDK Dynamic Proxies
   * for a given target {@link Object target}.
   *
   * @param <T> preferred {@link Class type} of the {@literal Proxy} object.
   * @return a new {@link JdkDynamicProxiesFactory} used to create JDK Dynamic Proxies.
   * @see org.cp.elements.lang.reflect.provider.JdkDynamicProxiesFactory
   */
  public static @NotNull <T> JdkDynamicProxiesFactory<T> newJdkDynamicProxiesFactory() {
    return new JdkDynamicProxiesFactory<>();
  }

  /**
   * Determines whether the given {@link Object target} can be proxied by this {@link ProxyFactory}
   * using JDK Dynamic Proxies.
   *
   * @param target {@link Object} to evaluate.
   * @param proxyInterfaces array of {@link Class interfaces} to use in the {@literal Proxy}, which are also used
   * as part of the evaluation for determining whether the {@link Object target} can be proxied.
   * @return a boolean value indicating whether the given {@link Object target} can be proxied by
   * this {@link ProxyFactory} using JDK Dynamic Proxies.
   * @see #resolveInterfaces(Object, Class[])
   * @see #canProxy(Class[])
   * @see #canProxy(Object)
   */
  @Override
  public boolean canProxy(@Nullable Object target, Class<?>... proxyInterfaces) {
    return canProxy(target) && canProxy(resolveInterfaces(target, proxyInterfaces));
  }

  /**
   * Determines whether the given {@link Object target} can be proxied with a JDK Dynamic Proxy.
   *
   * @param target {@link Object} to evaluate.
   * @return a boolean value indicating whether the given {@link Object target} can be proxied
   * with a JDK Dynamic Proxy.
   * @see org.cp.elements.lang.JavaType#isJavaType(Object)
   * @see java.lang.Object
   */
  private boolean canProxy(@Nullable Object target) {
    return target == null || !JavaType.isJavaType(target);
  }

  /**
   * Determines whether any of the given {@link Class interfaces} can be proxied with a JDK Dynamic Proxy.
   *
   * @param proxyInterfaces array of {@link Class interfaces} to evaluate.
   * @return a boolean value indicating whether any of the given {@link Class interfaces} can be proxied
   * with a JDK Dynamic Proxy.
   * @see java.lang.Class
   */
  private boolean canProxy(Class<?>... proxyInterfaces) {

    return StreamUtils.stream(proxyInterfaces)
      .filter(proxyInterface -> !NON_PROXYABLE_TYPES.contains(proxyInterface))
      .anyMatch(Class::isInterface);
  }

  /**
   * Constructs a new {@literal Proxy} for the given {@link Object target} implementing the given array of
   * {@link Class interfaces}, using the {@link Iterable} collection of {@link MethodInterceptor} objects
   * to intercept {@link Method} invocations on the {@literal Proxy} and handle the proxied invocation
   * according to the interception.
   *
   * @param <R> desired {@link Class type} of the proxied {@link Object}.
   * @param proxyClassLoader {@link ClassLoader} used to create the {@literal Proxy} {@link Class type}.
   * @param target {@link Object} to proxy.
   * @param proxyInterfaces array of {@link Class interfaces} for the constructed, proxied {@link Object}
   * to implement.
   * @return a new JDK Dynamic Proxy instance.
   * @see org.cp.elements.lang.reflect.MethodInterceptor
   * @see #resolveInterfaces(Object, Class[])
   * @see java.lang.ClassLoader
   */
  @Override
  @SuppressWarnings("unchecked")
  public <R> R newProxy(ClassLoader proxyClassLoader, T target, Class<?>[] proxyInterfaces,
      Iterable<MethodInterceptor<T>> methodInterceptors) {

    return (R) Proxy.newProxyInstance(proxyClassLoader, resolveInterfaces(target, proxyInterfaces),
      ComposableInvocationHandler.compose(methodInterceptors));
  }
}
