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
package org.cp.elements.data.conversion;

import java.util.concurrent.atomic.AtomicReference;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.service.annotation.Service;
import org.cp.elements.service.loader.ServiceLoaderSupport;

/**
 * The {@link ConversionService} interface defines a contract for application {@link Service} components
 * responsible for performing {@link Class type} conversions.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.conversion.AbstractConversionService
 * @see org.cp.elements.data.conversion.Converter
 * @see org.cp.elements.data.conversion.ConverterRegistry
 * @see org.cp.elements.service.annotation.Service
 * @see org.cp.elements.service.loader.ServiceLoaderSupport
 * @since 1.0.0
 */
@Service
@SuppressWarnings("unused")
public interface ConversionService extends ConverterRegistry {

  AtomicReference<Loader> LOADER_REFERENCE = new AtomicReference<>(null);

  /**
   * Gets a reference to the {@link ConversionService.Loader} used to load
   * the {@literal Service Provider Implementation (SPI)} of this {@link ConversionService}.
   *
   * @return a reference to the {@link ConversionService.Loader} used to load
   * the {@literal Service Provider Implementation (SPI)} of this {@link ConversionService}.
   * @see org.cp.elements.data.conversion.ConversionService.Loader
   */
  static @NotNull ConversionService.Loader getLoader() {
    return LOADER_REFERENCE.updateAndGet(it -> it != null ? it : new ConversionService.Loader() { });
  }

  /**
   * Determines whether this {@link ConversionService} can convert the given {@link Object}
   * into the desired {@link Class type}.
   *
   * @param value {@link Object} to convert.
   * @param toType {@link Class type} to convert the {@link Object} into.
   * @return a boolean value indicating whether this {@link ConversionService} can convert the given {@link Object}
   * into the desired {@link Class type}.
   * @see org.cp.elements.data.conversion.Converter#canConvert(Object, Class)
   * @see #canConvert(Class, Class)
   */
  default boolean canConvert(Object value, Class<?> toType) {
    return canConvert(ObjectUtils.getClass(value), toType);
  }

  /**
   * Determines whether this {@link ConversionService} can convert from the given {@link Class type}
   * into the desired {@link Class type}.
   *
   * @param fromType {@link Class type} to convert from.
   * @param toType {@link Class type} to convert to.
   * @return a boolean value indicating whether this {@link ConversionService} can convert from
   * the given {@link Class type} into the desired {@link Class type}.
   * @see org.cp.elements.data.conversion.Converter#canConvert(Class, Class)
   * @see #canConvert(Object, Class)
   */
  default boolean canConvert(Class<?> fromType, Class<?> toType) {

    for (Converter<?, ?> converter : this) {
      if (converter.canConvert(fromType, toType)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Converts the given {@link Object} into a {@link Object value} of the {@link Class target type}.
   *
   * @param <T> {@link Class target type} of the conversion.
   * @param value {@link Object} to convert.
   * @param toType {@link Class target type} to convert the {@link Object} into.
   * @return the converted {@link Object} of the {@link Class target type} with the given {@code value}.
   * @throws ConversionException if the {@link Object} could not be converted into a {@link Object value}
   * of the desired {@link Class target type}.
   * @see org.cp.elements.data.conversion.Converter#convert(Object, Class)
   * @see org.cp.elements.data.conversion.Converter#convert(Object)
   */
  <T> T convert(Object value, Class<T> toType);

  /**
   * {@link ServiceLoaderSupport} implementation used to load the {@link ConversionService}
   * provider implementation (SPI).
   *
   * @see org.cp.elements.service.loader.ServiceLoaderSupport
   */
  interface Loader extends ServiceLoaderSupport<ConversionService> {

    @Override
    default Class<ConversionService> getType() {
      return ConversionService.class;
    }
  }
}
