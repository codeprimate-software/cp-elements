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

import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;

/**
 * The {@link DefaultableConverter} class is an abstract {@link Converter} implementation supporting
 * the use {@link Object default values} if the {@link Object value} to convert is {@literal null}.
 *
 * @author John Blum
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class DefaultableConverter<S, T> extends AbstractConverter<S, T> {

  private T defaultValue;

  /**
   * Sets the {@link Object default value} to use if the {@link Object value} to convert is {@literal null}.
   *
   * @param defaultValue {@link Object default value} returned if the {@link Object value} to convert
   * is {@literal null}.
   */
  public void setDefaultValue(T defaultValue) {
    this.defaultValue = defaultValue;
  }

  /**
   * Returns the {@link Object default value} used if the {@link Object value} to convert is {@literal null}.
   *
   * @return the {@link Object default value} used if the {@link Object value} to convert is {@literal null}.
   */
  protected T getDefaultValue() {
    return this.defaultValue;
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
    return (fromType == null && getDefaultValue() != null);
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

    T defaultValue = getDefaultValue();

    if (value == null && defaultValue != null) {
      return defaultValue;
    }

    throw newConversionException("Cannot convert [%1$s] to [%2$s]", value, getTargetType().getName());
  }

  /**
   * Builder method used to set the {@link Object default value} to use
   * if the {@link Object value} to convert is {@literal null}.
   *
   * @param <CONVERTER> {@link Class type} of this {@link Converter}.
   * @param defaultValue {@link Object default value} returned if the {@link Object value} to convert
   * is {@literal null}.
   * @return this {@link Converter}.
   * @see #setDefaultValue(Object)
   */
  @SuppressWarnings("unchecked")
  public <CONVERTER extends DefaultableConverter<S, T>> CONVERTER withDefaultValue(T defaultValue) {
    setDefaultValue(defaultValue);
    return (CONVERTER) this;
  }
}
