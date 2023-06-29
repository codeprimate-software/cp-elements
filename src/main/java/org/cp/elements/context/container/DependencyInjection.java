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
package org.cp.elements.context.container;

import java.util.concurrent.atomic.AtomicReference;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.service.loader.ServiceLoaderSupport;

/**
 * Interface defining a contract for {@literal Dependency Injection} as code.
 * <p>
 * Additionally, this interface defines a contract for a very lightweight and simple
 * {@literal Inversion of Control (IOC)} container.
 * <p>
 * This container service provider implementation (SPI) is configurable using
 * the Java {@link java.util.ServiceLoader} functionality.
 *
 * @author John Blum
 * @see <a href="https://en.wikipedia.org/wiki/Dependency_injection">Dependency Injection</a>
 * @see <a href="https://en.wikipedia.org/wiki/Inversion_of_control">Inversion of Control (IOC)</a>
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface DependencyInjection {

  AtomicReference<Loader> LOADER_REFERENCE = new AtomicReference<>(null);

  /**
   * Gets a reference to the {@link DependencyInjection.Loader} used to load
   * the {@literal Service Provider Implementation (SPI)} of this {@link DependencyInjection} interface.
   *
   * @return a reference to the {@link DependencyInjection.Loader} used to load
   * the {@literal Service Provider Implementation (SPI)} of this {@link DependencyInjection} interface.
   * @see org.cp.elements.context.container.DependencyInjection.Loader
   */
  static @NotNull DependencyInjection.Loader getLoader() {
    return LOADER_REFERENCE.updateAndGet(it -> it != null ? it : new DependencyInjection.Loader() { });
  }

  /**
   * Performs dependency injection to automatically configure, auto-wire (inject dependencies) and initialize
   * the given {@link Object target}.
   *
   * @param <T> {@link Class type} of the {@link Object target}.
   * @param target {@link Object} on which to inject dependencies.
   * @return the given {@link Object}.
   */
  <T> T inject(T target);

  /**
   * {@link ServiceLoaderSupport} implementation used to load the {@link DependencyInjection}
   * service provider implementation (SPI).
   *
   * @see org.cp.elements.service.loader.ServiceLoaderSupport
   */
  interface Loader extends ServiceLoaderSupport<DependencyInjection> {

    @Override
    default Class<DependencyInjection> getType() {
      return DependencyInjection.class;
    }
  }
}
