/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.cp.elements.service;

import static org.cp.elements.lang.ElementsExceptionsFactory.newCacheNotFoundException;

import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.ServiceLoader;
import java.util.stream.Stream;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.context.configure.ConfigurationService;
import org.cp.elements.context.container.DependencyInjection;
import org.cp.elements.data.caching.Cache;
import org.cp.elements.data.caching.CacheNotFoundException;
import org.cp.elements.data.caching.support.CachingTemplate;
import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * The {@link ServiceTemplate} interface defines a contract for declaring an application service component.
 *
 * Additionally, this {@link ServiceTemplate} interface defines a contract for application service components
 * encapsulating business logic and other service operations common to all services.
 *
 * @author John J. Blum
 * @see java.util.ServiceLoader
 * @see org.cp.elements.context.configure.ConfigurationService
 * @see org.cp.elements.context.container.DependencyInjection
 * @see org.cp.elements.data.caching.Cache
 * @see org.cp.elements.data.caching.support.CachingTemplate
 * @see org.cp.elements.data.conversion.ConversionService
 * @see <a href="https://en.wikipedia.org/wiki/Template_method_pattern">Template Method Software Design Pattern</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ServiceTemplate<T> {

  /**
   * Gets access to the {@link Cache} with the given, required {@link String name} that can then be used by
   * program (application) services to cache results of resource-intensive service operations.
   *
   * @param <KEY> {@link Comparable} {@link Class type} of the {@link Cache} {@link Object key}.
   * @param <VALUE> {@link Class type} of the {@link Cache} {@link Object value}.
   * @param name {@link String} containing the {@literal name} of the {@link Cache} to lookup.
   * @return the {@link String named} {@link Cache}.
   * @throws CacheNotFoundException if a {@link Cache} with the given {@link String name} cannot be found.
   * @see org.cp.elements.data.caching.Cache
   * @see java.util.ServiceLoader
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  @NullSafe
  default @NotNull <KEY extends Comparable<KEY>, VALUE> Cache<KEY, VALUE> getCache(@Nullable String name) {

    ServiceLoader<Cache> cacheServiceLoader = ServiceLoader.load(Cache.class);

    return StreamUtils.stream(CollectionUtils.nullSafeIterable(cacheServiceLoader))
      .filter(Objects::nonNull)
      .filter(cache -> ObjectUtils.equalsIgnoreNull(cache.getName(), name))
      .findFirst()
      .orElseThrow(() -> newCacheNotFoundException("Cache with name [%s] not found", name));
  }

  /**
   * Gets an instance of {@link CachingTemplate} wrapping the {@link Cache} identified by the given {@link String name}.
   *
   * @param <KEY> {@link Comparable} {@link Class type} of the {@link Cache} {@link Object key}.
   * @param <VALUE> {@link Class type} of the {@link Cache} {@link Object value}.
   * @param cacheName {@link String} containing the {@literal name} of the {@link Cache} to lookup.
   * @return an instance of {@link CachingTemplate} wrapping the {@link Cache} identified by
   * the given {@link String name}.
   * @see org.cp.elements.data.caching.support.CachingTemplate
   * @see org.cp.elements.data.caching.Cache
   * @see #getCache(String)
   */
  default @NotNull <KEY extends Comparable<KEY>, VALUE> CachingTemplate<KEY, VALUE> getCachingTemplate(
      @Nullable String cacheName) {

    return new CachingTemplate<>(this.<KEY, VALUE>getCache(cacheName));
  }

  /**
   * Gets access to an {@link Optional} {@link Configuration} object with the given {@link String name}.
   *
   * @param name {@link String} containing the {@literal name} of the {@link Configuration} to lookup.
   * @return an {@link Optional} {@link Configuration} object with the given {@link String name}.
   * @see org.cp.elements.context.configure.ConfigurationService
   * @see org.cp.elements.context.configure.Configuration
   * @see #getConfigurationService()
   * @see java.util.Optional
   */
  @NullSafe
  default Optional<Configuration> getConfiguration(@Nullable String name) {

    return getConfigurationService()
      .map(StreamUtils::stream)
      .orElseGet(Stream::empty)
      .filter(Objects::nonNull)
      .filter(configuration -> ObjectUtils.equals(configuration.getName(), name))
      .findFirst();
  }

  /**
   * Gets access to an {@link Optional} {@link ConfigurationService} used by program (application) services
   * to acquire access to configuration metadata, such as {@link Properties} in addition to
   * other configuration resources.
   *
   * @return an {@link Optional} reference to the configured {@link ConfigurationService}.
   * @see org.cp.elements.context.configure.ConfigurationService
   * @see java.util.ServiceLoader
   * @see java.util.Optional
   */
  default Optional<ConfigurationService> getConfigurationService() {
    return Optional.ofNullable(ConfigurationService.getLoader().getServiceInstance());
  }

  /**
   * Gets access to an {@link Optional} {@link ConversionService} used by program (application) services
   * to perform data {@link Class type} conversions.
   *
   * @return an {@link Optional} reference to the configured {@link ConversionService}.
   * @see org.cp.elements.data.conversion.ConversionService
   * @see java.util.ServiceLoader
   * @see java.util.Optional
   */
  default Optional<ConversionService> getConversionService() {
    return Optional.ofNullable(ConversionService.getLoader().getServiceInstance());
  }

  /**
   * Gets access to an {@link Optional} {@link DependencyInjection} container used by program (application) services
   * to auto-wire (configure and initialize) collaborators, or dependencies, required by the application components
   * and services to carry out it's contractual function.
   *
   * @return an {@link Optional} reference to the configured {@link DependencyInjection} container.
   * @see org.cp.elements.context.container.DependencyInjection
   * @see java.util.ServiceLoader
   * @see java.util.Optional
   */
  default Optional<DependencyInjection> getDependencyInjectionContainer() {
    return Optional.ofNullable(DependencyInjection.getLoader().getServiceInstance());
  }
}
