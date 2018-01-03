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

import static org.cp.elements.lang.LangExtensions.is;
import static org.cp.elements.util.ArrayUtils.nullSafeArray;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * The {@link AbstractConverter} class is an abstract base class encapsulating functionality and behavior
 * common to all {@link Converter} implementations.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.conversion.ConversionServiceAware
 * @see org.cp.elements.data.conversion.Converter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractConverter<S, T> implements Converter<S, T> {

  private ConversionService conversionService;

  /**
   * Sets a reference to the {@link ConversionService} used to perform conversions.
   *
   * @param conversionService reference to the {@link ConversionService} used to perform conversions.
   * @see org.cp.elements.data.conversion.ConversionService
   */
  public void setConversionService(ConversionService conversionService) {
    this.conversionService = conversionService;
  }

  /**
   * Returns a reference to the {@link ConversionService} used to perform conversions.
   *
   * @return a reference to the {@link ConversionService} used to perform conversions.
   * @see org.cp.elements.data.conversion.ConversionService
   */
  protected ConversionService getConversionService() {

    Assert.state(conversionService != null, "No ConversionService was configured");

    return this.conversionService;
  }

  /**
   * Determines whether the {@link Class from type} is {@link ClassUtils#assignableTo(Class, Class) assignable to}
   * any of the {@link Class to types}.
   *
   * @param fromType {@link Class type} to evaluate.
   * @param toTypes {@link Class types} checked for assignment compatibility.
   * @return {@literal true} if {@link Class from type} is assignable to any of the {@link Class to types}.
   * @see org.cp.elements.lang.ClassUtils#assignableTo(Class, Class)
   * @see java.lang.Class
   */
  @NullSafe
  protected boolean isAssignableTo(Class<?> fromType, Class<?>... toTypes) {

    for (Class toType : nullSafeArray(toTypes, Class.class)) {
      if (is(fromType).assignableTo(toType)) {
        return true;
      }
    }

    return false;
  }

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
  @Override
  public boolean canConvert(Class<?> fromType, Class<?> toType) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  /**
   * Converts an {@link Object} of {@link Class type S} into an {@link Object} of {@link Class type T}.
   *
   * @param value {@link Object} of {@link Class type S} to convert.
   * @return the converted {@link Object} of {@link Class type T}.
   * @throws ConversionException if the {@link Object} cannot be converted.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   * @see #convert(Object, Class)
   */
  @Override
  public T convert(S value) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

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
  @Override
  public <QT extends T> QT convert(S value, Class<QT> qualifyingType) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }
}
