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

import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;

import java.util.function.Function;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Interface defining a contract for application components that convert an {@link Object}
 * from one {@link Class type} to another {@link Class type}.
 *
 * @author John J. Blum
 * @param <S> {@link Class source type} to convert from.
 * @param <T> {@link Class target type} to convert to.
 * @see java.util.function.Function
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @see org.cp.elements.data.conversion.ConversionService
 * @see org.cp.elements.data.conversion.ConversionServiceAware
 * @since 1.0.0
 */
public interface Converter<S, T> extends ConversionServiceAware, Function<S, T> {

  /**
   * Applies this {@link Converter} and {@link Function} to the given {@link S argument}
   * converting the value into an {@link Object} of {@link Class type T}.
   *
   * @param value {@link Object} applied to this {@link Converter} and {@link Function}.
   * @return the converted {@link T value} from applying this {@link Converter} and {@link Function}
   * to the given {@link S argument}.
   * @see java.util.function.Function#apply(Object)
   * @see #convert(Object)
   */
  @Override
  default T apply(S value) {
    return convert(value);
  }

  /**
   * Determines whether this {@link Converter} can convert {@link Object Objects} of the given {@link Class from type}
   * into {@link Object Objects} of the given {@link Class to type}.
   *
   * By default, the {@link Class type} to convert {@literal from} must be assignable to the {@link Class type}
   * to convert to.
   *
   * @param fromType {@link Class type} to convert from.
   * @param toType {@link Class type} to convert to.
   * @return a boolean indicating whether this {@link Converter} can convert {@link Object Objects}
   * {@link Class from type} {@link Class to type}.
   * @see org.cp.elements.data.conversion.ConversionService#canConvert(Class, Class)
   * @see #canConvert(Object, Class)
   */
  @NullSafe
  default boolean canConvert(@Nullable Class<?> fromType, @Nullable Class<?> toType) {
    return ClassUtils.assignableTo(fromType, toType);
  }

  /**
   * Determines whether this {@link Converter} can convert the given {@link Object value}
   * into the specified {@link Class type}.
   *
   * @param value {@link Object} to convert.
   * @param toType {@link Class type} to convert the {@link Object value} to.
   * @return a boolean value indicating whether this {@link Converter} can convert
   * the given {@link Object value} into the specified {@link Class type}.
   * @see org.cp.elements.data.conversion.ConversionService#canConvert(Object, Class)
   * @see #canConvert(Class, Class)
   */
  @NullSafe
  default boolean canConvert(@Nullable Object value, @Nullable Class<?> toType) {
    return canConvert(ObjectUtils.getClass(value), toType);
  }

  /**
   * Converts the given {@link Object} of {@link Class type S} into an {@link Object} of {@link Class type T}.
   *
   * @param value {@link Object} to convert.
   * @return the converted {@link Object} of {@link Class type T}.
   * @throws ConversionException if the {@link Object} cannot be converted.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   * @see #convert(Object, Class)
   * @see #apply(Object)
   */
  T convert(S value);

  /**
   * Converts an {@link Object} of {@link Class type S} into an {@link Object} of {@link Class qualifying type QT}.
   *
   * @param <QT> {@link Class qualifying type} extending {@link Class type T}.
   * @param value {@link Object} to convert.
   * @param qualifyingType {@link Class qualifying type} of the {@link Object} produced by the conversion.
   * @return the converted {@link Object} of {@link Class qualifying type QT}.
   * @throws ConversionException if the {@link Object} cannot be converted.
   * @throws IllegalArgumentException if {@link Class qualifying type} is {@literal null}.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   * @see #convert(Object)
   */
  default <QT extends T> QT convert(S value, @NotNull Class<QT> qualifyingType) {

    Assert.notNull(qualifyingType, "Qualifying type is required");

    try {
      return qualifyingType.cast(convert(value));
    }
    catch (ClassCastException cause) {
      throw newConversionException(cause, "Cannot convert value [%1$s] into an Object of type [%2$s]",
        value, qualifyingType.getName());
    }
  }
}
