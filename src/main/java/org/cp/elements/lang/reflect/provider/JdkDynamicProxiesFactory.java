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

  private static final Set<Class<?>> NON_PROXY_INTERFACES = new HashSet<>();

  static {
    NON_PROXY_INTERFACES.add(Serializable.class);
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
   */
  @Override
  public boolean canProxy(Object target, Class<?>... proxyInterfaces) {
    return stream(resolveInterfaces(target, proxyInterfaces))
      .filter(proxyInterface -> !NON_PROXY_INTERFACES.contains(proxyInterface))
        .anyMatch(Class::isInterface);
  }

  /**
   * @inheritDoc
   */
  @Override
  @SuppressWarnings("unchecked")
  public <T> T newProxy() {
    return (T) Proxy.newProxyInstance(getProxyClassLoader(),
      resolveInterfaces(getTarget(), getProxyInterfaces()), compose(getMethodInterceptors()));
  }
}
