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
package org.cp.elements.service.loader;

import static org.cp.elements.lang.ElementsExceptionsFactory.newServiceUnavailableException;

import java.util.Iterator;
import java.util.Objects;
import java.util.ServiceLoader;
import java.util.function.Predicate;
import java.util.stream.StreamSupport;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.service.ServiceUnavailableException;
import org.cp.elements.util.CollectionUtils;

/**
 * Interface defining a contract for Java {@link Object Objects} and services
 * {@link ServiceLoader#load(Class, ClassLoader) loaded} with Java's {@link ServiceLoader}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the service instance loaded by this {@link ServiceLoader}.
 * @see java.util.ServiceLoader
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ServiceLoaderSupport<T> {

  /**
   * Declares the {@link Class type} used to {@link ServiceLoader#load(Class) load the service} with
   * the Java {@link ServiceLoader}.
   *
   * By default, returns the {@link Class type} of the {@link Class} implementing this interface.
   *
   * @return a {@link Class type} of the service to load.
   * @see java.lang.Class
   */
  @SuppressWarnings("unchecked")
  default Class<T> getType() {
    return (Class<T>) getClass();
  }

  /**
   * Gets the {@link ClassLoader} used to load the service provider {@link Class} files and resource files.
   *
   * By default, returns the {@link Thread#currentThread()} {@link Thread#getContextClassLoader()}.
   *
   * @return the {@link ClassLoader} used to load the service provider {@link Class} files and resource files.
   * @see java.lang.ClassLoader
   */
  default @NotNull ClassLoader getClassLoader() {
    return Thread.currentThread().getContextClassLoader();
  }

  /**
   * Gets the first configured and available {@literal service instance}.
   *
   * @return the first configured and available {@literal service instance}.
   * @throws ServiceUnavailableException if a {@literal service instance} was not configured
   * or is generally not available for service.
   * @see #getServiceInstance(Predicate)
   */
  default T getServiceInstance() {
    return getServiceInstance(serviceInstance -> true);
  }

  /**
   * Gets the first configured and available {@literal service instance} matching the given, required {@link Predicate}.
   *
   * @param serviceInstancePredicate {@link Predicate} defining the criteria
   * used to match the {@literal service provider}.
   * @return the first configured and available {@literal service instance} matching the given,
   * required {@link Predicate}.
   * @throws IllegalArgumentException if the {@link Predicate} is {@literal null}.
   * @throws ServiceUnavailableException if a {@literal service instance} cannot be found matching the criteria
   * of the given, required {@link Predicate}.
   * @see java.util.function.Predicate
   */
  default T getServiceInstance(@NotNull Predicate<T> serviceInstancePredicate) {

    Assert.notNull(serviceInstancePredicate, "A Predicate used to match the service instance is required");

    ServiceLoader<T> serviceLoader = ServiceLoader.load(getType(), getClassLoader());

    Predicate<T> nullSafeServiceInstancePredicate = Objects::nonNull;

    nullSafeServiceInstancePredicate = nullSafeServiceInstancePredicate.and(serviceInstancePredicate);

    Iterator<T> services = CollectionUtils.nullSafeIterator(serviceLoader.iterator());

    return StreamSupport.stream(CollectionUtils.asIterable(services).spliterator(), false)
      .filter(nullSafeServiceInstancePredicate)
      .findFirst()
      .orElseThrow(() -> newServiceUnavailableException("Failed to find a service instance matching Predicate [%s]",
        serviceInstancePredicate));
  }
}
