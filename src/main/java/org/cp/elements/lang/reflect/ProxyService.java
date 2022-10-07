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

import java.util.Iterator;
import java.util.ServiceLoader;
import java.util.concurrent.atomic.AtomicReference;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.service.ServiceTemplate;
import org.cp.elements.service.annotation.Service;
import org.cp.elements.util.stream.StreamUtils;

/**
 * An Elements {@link Service} component used to load {@link ProxyFactory} Service Provider Implementations (SPI)
 * at application runtime.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} to proxy.
 * @see java.lang.Iterable
 * @see java.util.ServiceLoader
 * @see org.cp.elements.lang.reflect.ProxyFactory
 * @see org.cp.elements.service.annotation.Service
 * @see org.cp.elements.service.ServiceTemplate
 * @since 1.0.0
 */
@Service
@SuppressWarnings({ "rawtypes", "unused" })
public final class ProxyService<T> implements Iterable<ProxyFactory>, ServiceTemplate<ProxyFactory<T>> {

  private static final AtomicReference<ProxyService> proxyServiceInstance = new AtomicReference<>(null);

  /**
   * Factory method used to construct a new {@literal Singleton} instance of the {@link ProxyService} class.
   *
   * @param <T> {@link Class type} of {@link Object} to proxy.
   * @return a new, single instance of the {@link ProxyService} class.
   * @see org.cp.elements.lang.reflect.ProxyService
   */
  @SuppressWarnings("unchecked")
  public static @NotNull <T> ProxyService<T> newProxyService() {
    return proxyServiceInstance.updateAndGet(it -> it != null ? it : new ProxyService<T>());
  }

  private final ServiceLoader<ProxyFactory> proxyFactoriesLoader;

  /**
   * Constructs a new instance of {@link ProxyService} used to initialize all {@link ProxyFactory}
   * Service Provider Implementations (SPI).
   *
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see java.util.ServiceLoader#load(Class)
   */
  private ProxyService() {
    this.proxyFactoriesLoader = ServiceLoader.load(ProxyFactory.class);
  }

  /**
   * Determines whether any of the {@link ProxyFactory} Service Provider Implementations (SPI) managed by
   * this {@link ProxyService} is able to proxy the given {@link Object target}
   * for the given array of {@link Class interfaces}.
   *
   * @param target {@link Object} to proxy.
   * @param proxyInterfaces array of {@link Class interfaces} to be implemented by the {@literal Proxy}.
   * @return a boolean value of {@literal true} if there exists at least one {@link ProxyFactory}
   * Service Provider Implementation (SPI) that is able to proxy the given {@link Object target}
   * for the given array of {@link Class interfaces}.
   * @see org.cp.elements.lang.reflect.ProxyFactory#canProxy(Object, Class[])
   * @see java.lang.Class
   * @see java.lang.Object
   */
  @SuppressWarnings("unchecked")
  public boolean canProxy(Object target, Class<?>... proxyInterfaces) {
    return StreamUtils.stream(this).anyMatch(proxyFactory -> proxyFactory.canProxy(target, proxyInterfaces));
  }

  /**
   * Iterates over the {@link ProxyFactory} Service Provider Implementations (SPI) loaded by this {@link ProxyService}.
   *
   * @return an {@link Iterator} over the {@link ProxyFactory} Service Provider Implementations (SPI).
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see java.util.ServiceLoader#iterator()
   * @see java.util.Iterator
   * @see java.lang.Iterable
   */
  @Override
  public Iterator<ProxyFactory> iterator() {
    return this.proxyFactoriesLoader.iterator();
  }

  /**
   * Clears the {@link ServiceLoader ServiceLoader's} provider cache of all Service Provider Implementations (SPI)
   * causing all {@link ProxyFactory} Service Provider Implementations (SPI) to be reloaded.
   *
   * @see java.util.ServiceLoader#reload()
   */
  public void reload() {
    this.proxyFactoriesLoader.reload();
  }
}
