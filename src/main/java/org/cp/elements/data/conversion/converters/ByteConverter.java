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
package org.cp.elements.data.conversion.converters;

import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.data.conversion.DefaultableConverter;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.Order;

/**
 * {@link ByteConverter} converts an {@link Object} to a {@link Byte}.
 *
 * @author John J. Blum
 * @see java.lang.Byte
 * @see java.lang.Object
 * @see org.cp.elements.data.conversion.DefaultableConverter
 * @see org.cp.elements.lang.annotation.Order
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Order(-1_000_000)
public class ByteConverter extends DefaultableConverter<Object, Byte> {

  /**
   * Determines whether this {@link Converter} can convert {@link Object Objects}
   * {@link Class from type} {@link Class to type}.
   *
   * @param fromType {@link Class type} to convert from.
   * @param toType {@link Class type} to convert to.
   * @return a boolean indicating whether this {@link Converter} can convert {@link Object Objects}
   * {@link Class from type} {@link Class to type}.
   * @see org.cp.elements.data.conversion.ConversionService#canConvert(Class, Class)
   * @see #canConvert(Object, Class)
   */
  @Override
  public boolean canConvert(Class<?> fromType, Class<?> toType) {
    return isAssignableTo(fromType, Number.class, String.class) && Byte.class.equals(toType);
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
  public Byte convert(Object value) {

    if (value instanceof Number) {
      return ((Number) value).byteValue();
    }
    else if (value instanceof String && StringUtils.isDigits(value.toString().trim())) {
      try {
        return Byte.parseByte(value.toString().trim());
      }
      catch (NumberFormatException cause) {
        throw newConversionException(cause, "[%s] is not a valid byte", value);
      }
    }
    else {
      return super.convert(value);
    }
  }
}
