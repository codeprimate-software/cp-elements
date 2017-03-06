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

package org.cp.elements.lang.reflect.provider;

import static org.cp.elements.lang.reflect.support.ComposableInvocationHandler.compose;
import static org.cp.elements.util.stream.StreamUtils.stream;

import java.io.Serializable;
import java.lang.reflect.Proxy;
import java.util.HashSet;
import java.util.Set;

import org.cp.elements.lang.JavaType;
import org.cp.elements.lang.reflect.MethodInterceptor;
import org.cp.elements.lang.reflect.ProxyFactory;

/**
 * The {@link JdkDynamicProxiesFactory} class is a {@link ProxyFactory} service provider implementation that uses
 * JDK Dynamic Proxies to proxy a given target {@link Object} with a set of {@link Class interfaces}.
 *
 * @author John Blum
 * @param <T> {@link Class} type of the {@link Object} to proxy.
 * @see java.lang.reflect.Proxy
 * @see org.cp.elements.lang.reflect.ProxyFactory
 * @since 1.0.0
 */
public class JdkDynamicProxiesFactory<T> extends ProxyFactory<T> {

  private static final Set<Class<?>> NON_PROXYABLE_TYPES = new HashSet<>();

  static {
    NON_PROXYABLE_TYPES.add(Serializable.class);
  }

  /**
   * Factory method used to construct a new instance of the {@link JdkDynamicProxiesFactory}
   * to create JDK Dynamic Proxies for a given target {@link Object}.
   *
   * @param <T> preferred {@link Class} type of the Proxy.
   * @return a new instance of the {@link JdkDynamicProxiesFactory} to create JDK Dynamic Proxies.
   * @see org.cp.elements.lang.reflect.provider.JdkDynamicProxiesFactory
   */
  @SuppressWarnings("unchecked")
  public static <T> JdkDynamicProxiesFactory<T> newJdkDynamicProxiesFactory() {
    return new JdkDynamicProxiesFactory<>();
  }

  /**
   * @inheritDoc
   * @see #canProxy(Object)
   * @see #canProxy(Class[])
   */
  @Override
  public boolean canProxy(Object target, Class<?>... proxyInterfaces) {
    return (canProxy(target) && canProxy(resolveInterfaces(target, proxyInterfaces)));
  }

  /**
   * Determines whether the given target {@link Object} can be proxied with a JDK Dynamic Proxy.
   *
   * @param target {@link Object} to evaluate.
   * @return a boolean value indicating whether the given target {@link Object} can be proxied
   * with a JDK Dynamic Proxy.
   * @see org.cp.elements.lang.JavaType#isJavaType(Object)
   * @see java.lang.Object
   */
  private boolean canProxy(Object target) {
    return (target == null || !JavaType.isJavaType(target));
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
    return stream(proxyInterfaces).filter(proxyInterface -> !NON_PROXYABLE_TYPES.contains(proxyInterface))
      .anyMatch(Class::isInterface);
  }

  /**
   * @inheritDoc
   */
  @Override
  @SuppressWarnings("unchecked")
  public <R> R newProxy(ClassLoader proxyClassLoader, T target, Class<?>[] proxyInterfaces,
      Iterable<MethodInterceptor<T>> methodInterceptors) {

    return (R) Proxy.newProxyInstance(proxyClassLoader, resolveInterfaces(target, proxyInterfaces),
      compose(methodInterceptors));
  }
}
