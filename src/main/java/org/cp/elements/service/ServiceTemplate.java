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

import java.util.Iterator;
import java.util.Optional;
import java.util.ServiceLoader;
import java.util.stream.StreamSupport;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.data.caching.CacheException;
import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.CollectionUtils;

/**
 * The {@link ServiceTemplate} interface defines a contract for declaring an application service component.
 *
 * Additionally, this {@link ServiceTemplate} interface defines a contract for application service components
 * encapsulating business logic and other service operations common to all services.
 *
 * @author John J. Blum
 * @see <a href="https://en.wikipedia.org/wiki/Template_method_pattern">Template Method Software Design Pattern</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ServiceTemplate<T> {

  /**
   * Gets access to the {@link Cache} with the given {@link String name} that can be used by application services
   * to cache results of service operations.
   *
   * @param <KEY> {@link Comparable} {@link Class type} of the {@link Cache} {@link Object key}.
   * @param <VALUE> {@link Class type} of the {@link Cache} {@link Object value}.
   * @param name {@link String} containing the {@literal name} of the {@link Cache} to lookup.
   * @return the {@link String named} {@link Cache}.
   * @throws CacheException if a {@link Cache} with {@link String name} cannot be found.
   * @see org.cp.elements.data.caching.Cache
   * @see java.util.ServiceLoader
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  default @NotNull <KEY extends Comparable<KEY>, VALUE> Cache<KEY, VALUE> getCache(String name) {

    ServiceLoader<Cache> cacheServiceLoader = ServiceLoader.load(Cache.class);

    Iterator<Cache> cacheServiceIterator = CollectionUtils.nullSafeIterator(cacheServiceLoader.iterator());

    return StreamSupport.stream(CollectionUtils.asIterable(cacheServiceIterator).spliterator(), false)
      .filter(cache -> ObjectUtils.equalsIgnoreNull(cache.getName(), name))
      .findFirst()
      .orElseThrow(() -> newCacheNotFoundException("Cache with name [%s] not found", name));
  }

  /**
   * Gets access to an {@link Optional} {@link ConversionService} that can be used by application services
   * to perform conversions.
   *
   * @return an {@link Optional} reference to the configured {@link ConversionService}.
   * @see org.cp.elements.data.conversion.ConversionService
   * @see java.util.ServiceLoader
   * @see java.util.Optional
   */
  default Optional<ConversionService> getConversionService() {

    try {

      ServiceLoader<ConversionService> conversionServiceLoader = ServiceLoader.load(ConversionService.class);

      Iterator<ConversionService> conversionServiceIterator =
        CollectionUtils.nullSafeIterator(conversionServiceLoader.iterator());

      return conversionServiceIterator.hasNext()
        ? Optional.of(conversionServiceIterator.next())
        : Optional.empty();
    }
    catch (Exception ignore) {
      throw new ServiceUnavailableException("Failed to load ConversionService");
    }
  }
}
