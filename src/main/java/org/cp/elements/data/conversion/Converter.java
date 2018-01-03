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

package org.cp.elements.data.conversion;

/**
 * The {@link Converter} interface defines a contract for objects that convert an {@link Object}
 * from one {@link Class type} to another.
 *
 * @author John J. Blum
 * @param <S> the {@link Class source type} to convert from.
 * @param <T> the {@link Class target type} to convert to.
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @see org.cp.elements.data.conversion.ConversionService
 * @see org.cp.elements.data.conversion.ConversionServiceAware
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Converter<S, T> extends ConversionServiceAware {

  /**
   * Determines whether this {@link Converter} can convert {@link Object Objects}
   * {@link Class from type} {@link Class to type}.
   *
   * @param fromType {@link Class type} to convert from.
   * @param toType {@link Class type} to convert to.
   * @return a boolean indicating whether this {@link Converter} can convert {@link Object Objects}
   * {@link Class from type} {@link Class to type}.
   * @see org.cp.elements.data.conversion.ConversionService#canConvert(Class, Class)
   */
  boolean canConvert(Class<?> fromType, Class<?> toType);

  /**
   * Converts an {@link Object} of {@link Class type S} into an {@link Object} of {@link Class type T}.
   *
   * @param value {@link Object} of {@link Class type S} to convert.
   * @return the converted {@link Object} of {@link Class type T}.
   * @throws ConversionException if the {@link Object} cannot be converted.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   * @see #convert(Object, Class)
   */
  T convert(S value);

  /**
   * Converts an {@link Object} of {@link Class type S} into an {@link Object} of {@link Class qualifying type QT}.
   *
   * @param <QT> {@link Class qualifying type} extending {@link Class type T}.
   * @param value {@link Object} of {@link Class type S} to convert.
   * @param qualifyingType the {@link Class qualifying type} of the {@link Object} resolved in the conversion.
   * @return the converted {@link Object} of {@link Class qualifying type QT}.
   * @throws ConversionException if the {@link Object} cannot be converted.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   * @see #convert(Object)
   */
  default <QT extends T> QT convert(S value, Class<QT> qualifyingType) {
    return qualifyingType.cast(convert(value));
  }
}
