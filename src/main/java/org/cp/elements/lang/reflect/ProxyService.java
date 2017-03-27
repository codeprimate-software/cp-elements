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

package org.cp.elements.lang.reflect;

import static org.cp.elements.util.stream.StreamUtils.stream;

import java.util.Iterator;
import java.util.Optional;
import java.util.ServiceLoader;

import org.cp.elements.service.annotation.Service;
import org.cp.elements.service.support.ServiceSupport;

/**
 * The {@link ProxyService} class is a service component used to load {@link ProxyFactory}
 * service provider implementations at application runtime.
 *
 * @author John Blum
 * @param <T> {@link Class} type of the {@link Object} to proxy.
 * @see java.lang.Iterable
 * @see java.util.ServiceLoader
 * @see org.cp.elements.lang.reflect.ProxyFactory
 * @see org.cp.elements.service.annotation.Service
 * @see org.cp.elements.service.support.ServiceSupport
 * @since 1.0.0
 */
@Service
@SuppressWarnings("unused")
public final class ProxyService<T> implements Iterable<ProxyFactory>, ServiceSupport<ProxyFactory<T>> {

  private static ProxyService proxyServiceInstance;

  /**
   * Constructs, configures and initializes a single (Singleton) instance of the {@link ProxyService} class.
   *
   * @param <T> {@link Class} type of the {@link Object} to proxy.
   * @return a single instance of the {@link ProxyService} class.
   * @see org.cp.elements.lang.reflect.ProxyService
   */
  @SuppressWarnings("unchecked")
  public static synchronized <T> ProxyService<T> newProxyService() {
    proxyServiceInstance = Optional.ofNullable(proxyServiceInstance).orElseGet(ProxyService::new);
    return proxyServiceInstance;
  }

  private final ServiceLoader<ProxyFactory> proxyFactoriesLoader;

  /**
   * Constructs an instance of {@link ProxyService} that initializes all {@link ProxyFactory}
   * service provider implementations.
   *
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see java.util.ServiceLoader#load(Class)
   */
  private ProxyService() {
    proxyFactoriesLoader = ServiceLoader.load(ProxyFactory.class);
  }

  /**
   * Determines whether any of the {@link ProxyFactory} service provider implementations managed by this
   * {@link ProxyService} is able to proxy the given target {@link Object}, given the array of {@link Class interfaces}.
   *
   * @param target {@link Object} to proxy.
   * @param proxyInterfaces array of {@link Class interfaces} to be implemented by the Proxy.
   * @return a boolean value of {@literal true} if there exists at least one {@link ProxyFactory}
   * service provider implementation that is able to proxy the given target {@link Object} with the given
   * array of {@link Class interfaces}.
   * @see org.cp.elements.lang.reflect.ProxyFactory#canProxy(Object, Class[])
   * @see java.lang.Class
   * @see java.lang.Object
   */
  @SuppressWarnings("unchecked")
  public boolean canProxy(Object target, Class<?>... proxyInterfaces) {
    return stream(this).anyMatch(proxyFactory -> proxyFactory.canProxy(target, proxyInterfaces));
  }

  /**
   * Iterates over the {@link ProxyFactory} service provider implementations.
   *
   * @return an {@link Iterator} over the {@link ProxyFactory} service provider implementations.
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see java.util.ServiceLoader#iterator()
   * @see java.util.Iterator
   * @see java.lang.Iterable
   */
  @Override
  public Iterator<ProxyFactory> iterator() {
    return proxyFactoriesLoader.iterator();
  }

  /**
   * Clears the {@link ServiceLoader ServiceLoader's} provider cache of all service provider implementations
   * causing all {@link ProxyFactory} service provider implementations to be reloaded.
   *
   * @see java.util.ServiceLoader#reload()
   */
  public void reload() {
    this.proxyFactoriesLoader.reload();
  }
}
